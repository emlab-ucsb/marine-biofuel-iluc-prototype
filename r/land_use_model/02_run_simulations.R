#################################
# 02_run_simulations.R
# "Parallelized deforestation simulation across all carbon price scenarios"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# This is the second of two scripts in the parallelized simulation workflow.
# It reads the chunk files prepared by 01_prepare_data.R, distributes them
# across parallel workers, runs all simulation scenarios for each chunk,
# and combines the results into a single output CSV.
#
# HOW IT WORKS:
# 1. The main process reads a small manifest (file paths + row counts) and
#    model parameters. No large data is loaded into the main process.
# 2. Each parallel worker receives only a file path string (a few bytes).
#    It reads its own ~7 MB chunk file, runs all simulation scenarios for BOTH crop-price
#    regimes (crp_price = "endog" and "exog") — each regime has 1 actual baseline D(0,V(0))
#    plus, per carbon price, the policy run D(P,V), the FI paid-for baseline, and the AI
#    paid-for baseline — and writes its results to a result parquet file. The worker returns a small
#    summary — NOT the full simulation data — so the main process never holds
#    more than one chunk's results in memory at a time.
# 3. After all workers finish, the main process reads and combines the result
#    parquet files and writes the final CSV.
#
# MEMORY DESIGN:
# - Main process: holds manifest + model params (~1 MB) + final combined results
# - Each worker: holds one chunk input (~7 MB) + one chunk results (~50 MB)
# - No large object is serialized across the worker boundary
#
# PREREQUISITES:
# Run 01_prepare_data.R first to generate chunk files and the manifest.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

# Toggle between sample (200 pixels) and full (~1.5M pixels) run.
# Must match the USE_SAMPLE_DATA setting used in 01_prepare_data.R and 02_run_simulations.R.
USE_SAMPLE_DATA <- FALSE

source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))

select <- dplyr::select
options(scipen = 999)
set.seed(93106)

# MEMORY-AWARE WORKER COUNT.
# At 56 scenarios, RAM — not cores — is the binding constraint: too many workers exhausts memory
# and the OS kills one, which surfaces as "Future (...) MultisessionFuture interrupted". We size
# workers so workers x gb_per_worker leaves ~12 GB headroom for the OS + main process.
# gb_per_worker is the measured per-worker peak (incl. R/package overhead) at the current chunk
# size: ~6 GB at 400 chunks (~3,625 px/chunk). If you change N_CHUNKS in 01_prepare_data.R,
# re-measure and update this (peak scales ~linearly with px/chunk; it was ~12 at 200 chunks).
gb_per_worker <- 6
mem_gb <- suppressWarnings(tryCatch(
  as.numeric(system("sysctl -n hw.memsize", intern = TRUE)) / 1024^3,
  error = function(e) NA_real_))
N_WORKERS <- if (is.na(mem_gb)) 2L else max(1L, floor((mem_gb - 12) / gb_per_worker))
N_WORKERS <- min(N_WORKERS, parallel::detectCores() - 1L)
# N_WORKERS <- 3   # uncomment to override manually
cat("Using", N_WORKERS, "parallel workers",
    if (!is.na(mem_gb)) sprintf("(detected %.0f GB RAM)", mem_gb) else "(RAM undetected; conservative default)", "\n")

# Directory paths
wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')


processing_dir  <- file.path(wdir, "2_processing")
data_subdir    <- if (USE_SAMPLE_DATA) "smpl" else "full"
chunks_dir     <- file.path(processing_dir, data_subdir, "chunks")
results_dir    <- file.path(processing_dir, data_subdir, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load shared parameters -----
# The manifest and model params are small (~1 MB total) and safe to load in the
# main process. Workers receive them via the closure in run_chunk().

cat("Loading manifest and model params from:", chunks_dir, "\n")

manifest_path     <- file.path(chunks_dir, "chunk_manifest.rds") #CL: note: this breaks if chunks were created with a different wdir - should be changed in the future
model_params_path <- file.path(chunks_dir, "model_params.rds")

if (!file.exists(manifest_path)) {
  stop("Manifest not found at: ", manifest_path,
       "\nPlease run 01_prepare_data.R first.")
}

manifest     <- readRDS(manifest_path)
model_params <- readRDS(model_params_path)

# Unpack model params into the local environment
alpha   <- model_params$alpha
beta    <- model_params$beta
linkinv <- model_params$linkinv
CP_LIST <- model_params$cp_list
YEARS   <- model_params$years
NYEARS  <- model_params$nyears
iota    <- model_params$iota
scc     <- model_params$scc
scc_dly <- model_params$scc_dly

# Derived simulation constants
# Scenarios = 2 crop-price regimes (endog, exog), each with 1 actual baseline D(0,V(0))
#   + 3 per carbon price (policy, FI paid-for baseline, AI paid-for baseline) = 2*(1 + 3*nCP).
N_SCENARIOS       <- 2L * (1L + 3L * length(CP_LIST))
N_YEARS_WITH_2020 <- NYEARS + 2L           # years vector covers 2021..2020+NYEARS, plus year 2020 added back

cat("Manifest: ", nrow(manifest), "chunks\n")
cat("CP_LIST: ", paste(CP_LIST, collapse = ", "), "\n")
cat("Years: ", min(YEARS), "to", max(YEARS), "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Define the per-chunk worker function -----
#
# run_chunk() is the function executed by each parallel worker.
# Design principles:
#   - Receives ONLY a file path (not data) to avoid large serialization overhead
#   - Reads, validates, simulates, and writes independently
#   - Returns a small summary list (not results data) back to the main process
#   - All failures are reported with informative error messages
#
# Parameters passed via closure (captured from Section 2):
#   alpha, beta, linkinv, CP_LIST, YEARS, results_dir

run_chunk <- function(chunk_file, expected_n_rows, alpha, beta, linkinv,
                      cp_list, years, results_dir) {
  # Wrap everything in tryCatch so a single failed chunk doesn't kill the whole run.
  # The error is returned as a structured list so the main process can report it clearly.
  tryCatch({

    # ---- READ ----
    chunk <- arrow::read_parquet(chunk_file)

    # ---- VALIDATE ON READ ----
    # These checks catch any corruption or mismatch between the manifest and files.
    if (nrow(chunk) != expected_n_rows) {
      stop(sprintf("Row count mismatch: expected %d, got %d in %s",
                   expected_n_rows, nrow(chunk), basename(chunk_file)))
    }

    critical_cols <- c("pixel_fe", "ai_pixel_fe", "yc_fe",
                       "lag_remaining_treecover", "remaining_treecover_share",
                       "pixel_count", "CF", "CF_pix",
                       paste0("revenue_cp", c(0, cp_list)))
    for (col in critical_cols) {
      if (!col %in% names(chunk)) {
        stop(sprintf("Missing required column '%s' in %s", col, basename(chunk_file)))
      }
      if (anyNA(chunk[[col]])) {
        stop(sprintf("NA values found in column '%s' in %s", col, basename(chunk_file)))
      }
    }

    n_pixels_chunk <- nrow(chunk)

    # ---- RUN ALL SIMULATION SCENARIOS ----
    # We produce results for BOTH crop-price regimes in one pass, tagged by crp_price:
    #   "endog": crop prices respond to the carbon price, shifting from V(0) to V(P).
    #   "exog" : crop prices are held at baseline V(0) for every carbon price (no feedback).
    #
    # Only 38 simulations are genuinely DISTINCT. Each is one simulate_10_dt() call (a forward
    # simulation over all pixels in the chunk for a given carbon price + fixed-effect assumption):
    #
    #   object              simulate_10_dt args                  meaning        runs
    #   ------------------  -----------------------------------  -------------  ----
    #   sim_base_v0         cp=0, "revenue_cp0", "pixel_fe"      D(0,V(0))         1
    #   sim_base_ai_v0      cp=0, "revenue_cp0", "ai_pixel_fe"   D_ai(0,V(0))      1
    #   endog_policy   (9)  cp=P, "revenue_cpP", "pixel_fe"      D(P,V(P))         9
    #   endog_fi_blvp  (9)  cp=0, "revenue_cpP", "pixel_fe"      D(0,V(P))         9
    #   endog_ai_blvp  (9)  cp=0, "revenue_cpP", "ai_pixel_fe"   D_ai(0,V(P))      9
    #   exog_policy    (9)  cp=P, "revenue_cp0", "pixel_fe"      D(P,V(0))         9
    #                                                            distinct total = 38
    #
    # These are assembled into 56 LABELED blocks (28 endog + 28 exog). The 18 "extra" exog blocks
    # are cheap RELABELED COPIES, not re-runs: under exog there is no leakage and no price-
    # anticipation gap, so the exog FI/AI paid-for baselines and the exog cp=0 actual baseline are
    # all P-independent and equal D(0,V(0)) / D_ai(0,V(0)) (i.e. sim_base_v0 / sim_base_ai_v0).
    #   endog (28): sim_base_v0 -> (fi, cp=0); endog_policy -> (fi, P);
    #               endog_fi_blvp -> (fi_bl_vp, P); endog_ai_blvp -> (ai_bl_vp, P).
    #   exog  (28): sim_base_v0 -> (fi, cp=0) and (fi_bl_vp, every P)   [copies];
    #               exog_policy -> (fi, P)                              [real runs];
    #               sim_base_ai_v0 -> (ai_bl_vp, every P)               [copies].
    #
    # cp=0 is passed to simulate_10_dt for every baseline scenario so no carbon opportunity cost
    # (cp*CF) is subtracted — only the revenue column carries the price regime.

    # Helpers. lab() stamps the four scenario-id columns with dplyr::mutate (NOT data.table :=):
    # dplyr verbs return a fresh frame and never modify the input in place, so relabeling a copy
    # cannot corrupt the shared sim_base_v0 / sim_base_ai_v0. (:= would mutate in place — avoid it.)
    sim_one <- function(cp, revenue_col, fe_col) {
      simulate_10_dt(
        starting_data = chunk, cp = cp,
        revenue_col   = revenue_col, fe_col = fe_col,
        years = years, alpha = alpha, beta = beta, linkinv = linkinv
      )
    }
    lab <- function(df, info, cprice, crp_price) {
      df %>% mutate(info = info, cprice = cprice, crp_price = crp_price,
                    scn = paste(cprice, info, crp_price, sep = "_"))
    }

    # --- 38 genuinely-distinct simulations ---
    sim_base_v0    <- sim_one(0L, "revenue_cp0", "pixel_fe")      # D(0, V(0))   shared actual baseline
    sim_base_ai_v0 <- sim_one(0L, "revenue_cp0", "ai_pixel_fe")   # D_ai(0, V(0)) exog AI baseline
    endog_policy   <- lapply(cp_list, function(cp) sim_one(cp, paste0("revenue_cp", cp), "pixel_fe"))     # D(P, V(P))
    endog_fi_blvp  <- lapply(cp_list, function(cp) sim_one(0L, paste0("revenue_cp", cp), "pixel_fe"))     # D(0, V(P))
    endog_ai_blvp  <- lapply(cp_list, function(cp) sim_one(0L, paste0("revenue_cp", cp), "ai_pixel_fe"))  # D_ai(0, V(P))
    exog_policy    <- lapply(cp_list, function(cp) sim_one(cp, "revenue_cp0", "pixel_fe"))                # D(P, V(0))

    # --- 56 labeled blocks (18 of the exog blocks are relabeled copies, not re-runs) ---
    endog_blocks <- c(
      list(lab(sim_base_v0, "fi", 0L, "endog")),
      Map(function(d, cp) lab(d, "fi",       cp, "endog"), endog_policy,  cp_list),
      Map(function(d, cp) lab(d, "fi_bl_vp", cp, "endog"), endog_fi_blvp, cp_list),
      Map(function(d, cp) lab(d, "ai_bl_vp", cp, "endog"), endog_ai_blvp, cp_list)
    )
    exog_blocks <- c(
      list(lab(sim_base_v0, "fi", 0L, "exog")),                                              # copy of D(0,V(0))
      Map(function(d, cp) lab(d, "fi", cp, "exog"), exog_policy, cp_list),                    # real D(P,V(0))
      lapply(cp_list, function(cp) lab(sim_base_v0,    "fi_bl_vp", cp, "exog")),              # copies: P-independent
      lapply(cp_list, function(cp) lab(sim_base_ai_v0, "ai_bl_vp", cp, "exog"))               # copies: P-independent
    )

    # MEMORY: free the 38 distinct simulations now. lab() copies (dplyr::mutate), so the labeled
    # blocks above are independent of these originals — dropping them here removes ~38 chunk-sized
    # frames from the peak before the large bind below. (Per-chunk peak is the binding constraint
    # at 56 scenarios; see N_WORKERS note in Section 1.)
    rm(sim_base_v0, sim_base_ai_v0, endog_policy, endog_fi_blvp, endog_ai_blvp, exog_policy)
    gc(FALSE)

    # ---- ASSEMBLE SIMULATION RESULTS ----
    # data.table::rbindlist is more memory-efficient than dplyr::bind_rows over many data.tables.
    # Free the block lists immediately after binding so only the combined frame remains.
    for_shr <- data.table::rbindlist(c(endog_blocks, exog_blocks))
    rm(endog_blocks, exog_blocks)
    gc(FALSE)
    for_shr <- for_shr[, .(scn, pixel_id, year, remaining_treecover_share,
                           cprice, info, crp_price)]

    # ---- ADD YEAR-2020 INITIAL ROWS ----
    # The simulation produces years 2021 through 2020+NYEARS. We add one row per
    # (scenario, pixel) for year 2020 using the initial forest cover from the chunk.
    # This matches the initial_rows_2020 logic in simulation_workflow.R.
    initial_rows_2020 <- for_shr %>%
      dplyr::select(scn, pixel_id, cprice, info, crp_price) %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        chunk %>% dplyr::select(pixel_id, remaining_treecover_share),
        by = "pixel_id"
      ) %>%
      dplyr::mutate(year = 2020L) %>%
      dplyr::filter(!is.na(remaining_treecover_share)) %>%
      dplyr::select(dplyr::all_of(names(for_shr)))

    for_shr <- dplyr::bind_rows(initial_rows_2020, for_shr) %>%
      dplyr::arrange(scn, pixel_id, year)

    # ---- CALCULATE BIOMASS ----
    # biomass = remaining forest cover fraction × carbon content per pixel (CF_pix) × pixel area (pixel_count)
    for_shr <- for_shr %>%
      dplyr::left_join(
        chunk %>% dplyr::select(pixel_id, CF_pix, pixel_count),
        by = "pixel_id"
      ) %>%
      dplyr::mutate(biomass = remaining_treecover_share * CF_pix * pixel_count)

    sim_biomass_chunk <- for_shr %>%
      dplyr::select(crp_price, cprice, pixel_id, year, info, biomass)

    # ---- VALIDATE CHUNK OUTPUT ----
    # Scenarios per pixel: 2 crop-price regimes (endog, exog), each with 1 actual baseline
    # D(0,V(0)) + 3 per carbon price (policy, FI paid-for baseline, AI paid-for baseline).
    n_scenarios       <- 2L * (1L + 3L * length(cp_list))
    n_years_with_2020 <- length(years) + 1L
    expected_rows     <- n_pixels_chunk * n_scenarios * n_years_with_2020

    if (nrow(sim_biomass_chunk) != expected_rows) {
      stop(sprintf(
        "Output row count mismatch in %s: expected %d (= %d px × %d scn × %d yr), got %d",
        basename(chunk_file),
        expected_rows, n_pixels_chunk, n_scenarios, n_years_with_2020,
        nrow(sim_biomass_chunk)
      ))
    }

    if (anyNA(sim_biomass_chunk$biomass)) {
      na_rows <- sim_biomass_chunk[is.na(sim_biomass_chunk$biomass), ]
      stop(sprintf(
        "NA values in biomass output for chunk %s: %d NA rows across scenarios %s, pixel_ids %s",
        basename(chunk_file),
        nrow(na_rows),
        paste(unique(na_rows$cprice), collapse = ", "),
        paste(head(unique(na_rows$pixel_id), 5), collapse = ", ")
      ))
    }

    # ---- WRITE CHUNK RESULTS ----
    chunk_id_str <- sub("chunk_(\\d+)\\.parquet", "\\1", basename(chunk_file))
    result_file  <- file.path(results_dir, paste0("results_chunk_", chunk_id_str, ".parquet"))
    arrow::write_parquet(sim_biomass_chunk, result_file)

    # Return a small summary only — NOT the full data.
    # Returning full data would serialize ~50 MB per chunk across the socket to the
    # main process, which would negate the memory benefits of parallelization.
    list(
      chunk_file  = chunk_file,
      result_file = result_file,
      n_rows      = nrow(sim_biomass_chunk),
      n_pixels    = n_pixels_chunk,
      success     = TRUE
    )

  }, error = function(e) {
    list(
      chunk_file = chunk_file,
      error      = conditionMessage(e),
      success    = FALSE
    )
  })
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Run simulations in parallel -----
#
# future::multisession spawns N_WORKERS independent R processes. Each worker:
#   1. Has its own clean R session (no shared memory with other workers)
#   2. Reads its chunk file from disk independently
#   3. Returns only a small summary to the main process
#
# We use furrr::future_map() (parallel version of purrr::map()) because it
# provides clean error propagation — if a chunk fails, you get a clear error
# message rather than a silent NA.

cat("Starting parallel simulation with", N_WORKERS, "workers...\n")
tictoc::tic("Total parallel simulation time")

future::plan(future::multisession, workers = N_WORKERS)

worker_results <- furrr::future_map(
  seq_len(nrow(manifest)),
  function(i) {
    run_chunk(
      chunk_file     = manifest$file_path[i],
      expected_n_rows = manifest$n_rows[i],
      alpha          = alpha,
      beta           = beta,
      linkinv        = linkinv,
      cp_list        = CP_LIST,
      years          = YEARS,
      results_dir    = results_dir
    )
  },
  .options  = furrr::furrr_options(seed = 93106L),
  .progress = TRUE
)

# Return to sequential execution to free worker processes
future::plan(future::sequential)
tictoc::toc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Check for worker failures -----
# Report any chunks that failed before trying to combine results.

failed <- Filter(function(r) !isTRUE(r$success), worker_results)

if (length(failed) > 0) {
  error_msgs <- sapply(failed, function(r) {
    sprintf("  Chunk %s: %s", basename(r$chunk_file), r$error)
  })
  stop("The following chunks failed:\n", paste(error_msgs, collapse = "\n"))
}

cat("All", length(worker_results), "chunks completed successfully.\n")

# Summarize worker output
total_pixels <- sum(sapply(worker_results, `[[`, "n_pixels"))
total_rows   <- sum(sapply(worker_results, `[[`, "n_rows"))
cat("Total pixels processed:", total_pixels, "\n")
cat("Total simulation rows:", total_rows, "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Validate total row count -----
# Before combining, confirm the total across all chunks matches expectations.

expected_total <- total_pixels * N_SCENARIOS * N_YEARS_WITH_2020

if (total_rows != expected_total) {
  warning(sprintf(
    "Total rows (%d) does not match expected (%d = %d px × %d scn × %d yr)",
    total_rows, expected_total, total_pixels, N_SCENARIOS, N_YEARS_WITH_2020
  ))
} else {
  cat("Row count validation PASSED:", total_rows, "rows\n")
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 7: Save results manifest -----
# Record the absolute path, row count, and file size of every result chunk.
# 03_aggregate_results.R reads this manifest to locate result files without
# relying on list.files(), which would silently include stale files from prior runs.

result_files <- sapply(worker_results, `[[`, "result_file")
results_manifest <- data.frame(
  chunk_id  = seq_along(result_files),
  file_path = result_files,
  n_rows    = sapply(worker_results, `[[`, "n_rows"),
  file_size = file.size(result_files)
)
manifest_out <- file.path(results_dir, "results_manifest.rds")
saveRDS(results_manifest, manifest_out)
cat("Saved results_manifest.rds:", nrow(results_manifest), "chunks,",
    round(sum(results_manifest$file_size) / 1024^2, 1), "MB total\n")
cat("  Manifest path:", manifest_out, "\n")


# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # SECTION 7: Combine chunk results and write final output -----
# #
# # We read result parquet files and combine them. arrow::open_dataset() + collect()
# # reads all files sequentially, keeping only the combined data in memory.

# cat("Combining chunk results...\n")
# tictoc::tic("Combining and writing output")

# result_files <- sapply(worker_results, `[[`, "result_file")

# # Read all result parquet files and collect into a single data frame
# sim_biomass_df <- arrow::open_dataset(result_files) %>%
#   dplyr::collect()

# cat("Combined dataset:", nrow(sim_biomass_df), "rows,", ncol(sim_biomass_df), "cols\n")

# # Write final output CSV
# output_file <- file.path(output_dir, "sim_biomass.csv")
# readr::write_csv(sim_biomass_df, output_file)
# cat("Output written to:", output_file, "\n")

# tictoc::toc()


# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # SECTION 8: Validate against reference output -----
# #
# # Compare results against sim_biomass_ref.csv, which was produced by the original
# # simulation_workflow.R on a 200-pixel sample. When running on the full dataset,
# # the current output is subset to the reference pixel_ids before comparison, so
# # this check always runs as long as the reference file exists.

# ref_file <- file.path(output_dir, "sim_biomass_ref.csv")

# if (!file.exists(ref_file)) {
#   cat("Reference file not found at", ref_file, "— skipping validation\n")
# } else {
#   cat("\nRunning validation against reference output...\n")

#   ref <- readr::read_csv(ref_file, show_col_types = FALSE)
#   ref_pixel_ids <- unique(ref$pixel_id)
#   cat("  Reference pixels:", length(ref_pixel_ids),
#       "| Current run pixels:", total_pixels, "\n")

#   # Subset the current output to only the reference pixels.
#   # On a sample run this is a no-op; on the full dataset it extracts the matching rows.
#   current <- sim_biomass_df %>%
#     dplyr::filter(pixel_id %in% ref_pixel_ids) %>%
#     dplyr::arrange(crp_price, cprice, info, pixel_id, year)

#   reference <- ref %>%
#     dplyr::arrange(crp_price, cprice, info, pixel_id, year)

#   # Verify the subset contains the expected number of rows
#   if (nrow(current) != nrow(reference)) {
#     warning(sprintf(
#       "After subsetting to reference pixels: current has %d rows, reference has %d rows. ",
#       nrow(current), nrow(reference),
#       "Some reference pixel_ids may be absent from the current run."
#     ))
#   } else if (!identical(sort(names(current)), sort(names(reference)))) {
#     warning("Column names differ between current output and reference:\n",
#             "  Current:   ", paste(sort(names(current)),   collapse = ", "), "\n",
#             "  Reference: ", paste(sort(names(reference)), collapse = ", "))
#   } else {
#     reference <- reference %>% dplyr::select(dplyr::all_of(names(current)))

#     biomass_diff <- max(abs(current$biomass - reference$biomass), na.rm = TRUE)
#     cat("  Max absolute biomass difference vs reference:", biomass_diff, "\n")

#     if (biomass_diff > 1e-6) {
#       warning(sprintf(
#         "Biomass values differ from reference by %.2e (threshold: 1e-6). Check for logic errors.",
#         biomass_diff
#       ))
#     } else {
#       cat("  VALIDATION PASSED: Reference pixels match within tolerance (diff < 1e-6)\n")
#     }
#   }
# }

# cat("\n=== 02_run_simulations.R COMPLETE ===\n")
# cat("Output file:", output_file, "\n")
# cat("Total rows: ", nrow(sim_biomass_df), "\n")
