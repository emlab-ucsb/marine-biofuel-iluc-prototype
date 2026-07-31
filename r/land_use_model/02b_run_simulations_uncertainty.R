#################################
# 02_run_simulations_uncertainty.R
# "Parallelized deforestation simulation, repeated across coefficient uncertainty draws"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# Variant of 02_run_simulations.R that propagates model-parameter uncertainty through the
# simulation: reruns the full chunked simulation once per (alpha, beta) draw produced by
# 01b_prepare_uncertainty_draws.R, using that draw's re-profiled fixed effects.
#
# This script no longer draws the coefficients or re-profiles fixed effects itself -- see
# 01b_prepare_uncertainty_draws.R for that (and for why it's a separate stage: refitting
# has a very different resource profile than chunk simulation, and splitting them means a
# chunk-sim rerun never has to redo the FE refits). This script just needs the outputs of
# that stage (draws.rds / fe_draws_manifest.rds) plus the chunk manifest.
#
# HOW IT WORKS:
# 1. The main process reads the chunk manifest, model params, and the FE-draws manifest
#    (produced by 01b). No large data is loaded into the main process.
# 2. One future_map() call PER DRAW (worker pool started once, reused across all draws).
#    Before each draw's call, the main process reads that draw's small FE table ONCE
#    (gamma_df/gamma_df_ai/delta_df) and passes it as an in-memory global -- future's
#    persistent-worker caching then sends it to each worker only once per draw, not once
#    per chunk. Each worker still receives just a chunk file path (not chunk data) and
#    reads its own ~7 MB chunk file, runs all simulation scenarios for the exog
#    crop-price regime only (crp_price = "exog", crop prices held at baseline V(0) for
#    every carbon price) -- 1 actual baseline D(0,V(0)) plus, per carbon price, the
#    policy run D(P,V(0)), the FI paid-for baseline, and the AI paid-for baseline -- and
#    writes its results to a result parquet file. The worker returns a small summary —
#    NOT the full simulation data — so the main process never holds more than one task's
#    results in memory at a time.
# 3. The gap between draws is cheap: one small readRDS() plus dispatching the next
#    future_map() call. This used to be expensive (refit_fes() ran serially there,
#    leaving workers idle) before FE re-profiling was moved to 01b and precomputed for
#    every draw up front -- see that script for why splitting it out this way matters.
# 4. After each draw's future_map() call, the main process writes that draw's results
#    manifest before moving to the next draw.
#
# NO CHECKPOINTING:
# Every run recomputes and overwrites ALL chunks for ALL draws from scratch -- there is
# no skip-if-exists logic. If any chunk fails, the script stops immediately (Section 5)
# rather than continuing past the failure, since a partial/incomplete draw_summary.rds
# would otherwise be silently fed to 03_aggregate_results_uncertainty.R. This is a
# deliberate simplification: an earlier version of this script added checkpointing
# (skip work whose output file already existed), but that silently treated stale result
# files from a PRIOR run with a different chunk definition (e.g. after 01_prepare_data.R
# was rerun with a different N_CHUNKS or different input data) as valid, since it only
# checked filename existence, not whether the file's contents still matched the current
# chunk manifest -- a real correctness bug. Removed rather than fixed for now.
#
# MEMORY DESIGN:
# - Main process: holds manifest + model params + FE-draws manifest (all small) + one
#   draw's FE table at a time + final combined summaries. Unlike before 01b existed, it
#   does NOT hold fe_panel (~1 GB on full data) -- that only lives in
#   01b_prepare_uncertainty_draws.R now.
# - Each worker: holds one chunk input (~7 MB) + one draw's FE tables (tens of MB,
#   received once per draw, not once per chunk) + one chunk's results (~50 MB)
#
# PREREQUISITES:
# Run 01_prepare_data.R, then 01b_prepare_uncertainty_draws.R first, to generate chunk
# files/manifest and the FE-draws manifest respectively.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

# Toggle between sample (200 pixels) and full (~1.5M pixels) run.
# Must match the USE_SAMPLE_DATA setting used in 01_prepare_data.R and
# 01b_prepare_uncertainty_draws.R (chunks/model_params/FE-draws must have been generated
# with the same setting).
USE_SAMPLE_DATA <- TRUE

# For quick testing, limit how many of the available draws get chunk-simulated. Set to
# NULL to run every draw in fe_draws_manifest.rds. Note: this only limits THIS script --
# 01b_prepare_uncertainty_draws.R still re-profiles FEs for all N_DRAWS draws regardless
# (that step isn't what this is testing; lower N_DRAWS in 01b instead if you want to
# limit it too).
MAX_DRAWS <- 10   #200 draws available drom 01b

source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))

select <- dplyr::select
options(scipen = 999)
set.seed(93106)

# MEMORY-AWARE WORKER COUNT.
# At 28 scenarios, RAM — not cores — is the binding constraint: too many workers exhausts
# memory and the OS kills one, which surfaces as "Future (...) MultisessionFuture
# interrupted". We size workers so workers x gb_per_worker leaves ~12 GB headroom for the
# OS + main process. gb_per_worker is the measured per-worker peak (incl. R/package
# overhead) at the current chunk size: ~6 GB at 400 chunks (~3,625 px/chunk). If you
# change N_CHUNKS in 01_prepare_data.R, re-measure and update this (peak scales
# ~linearly with px/chunk).
gb_per_worker <- 6
mem_gb <- suppressWarnings(tryCatch({
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Darwin") {
    as.numeric(system("sysctl -n hw.memsize", intern = TRUE)) / 1024^3
  } else if (sysname == "Linux") {
    as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)) / 1024^2
  } else {
    NA_real_
  }
}, error = function(e) NA_real_))
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

processing_dir <- file.path(wdir, "2_processing")
data_subdir    <- if (USE_SAMPLE_DATA) "smpl" else "full"
chunks_dir     <- file.path(processing_dir, data_subdir, "chunks")
results_dir    <- file.path(processing_dir, data_subdir, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load shared parameters -----
# The manifest and model params are small (~1 MB total) and safe to load in the main
# process. Workers receive them via the closure in run_chunk().

cat("Loading manifest and model params from:", chunks_dir, "\n")

manifest_path     <- file.path(chunks_dir, "chunk_manifest.rds") #CL: note: this breaks if chunks were created with a different wdir - should be changed in the future
model_params_path <- file.path(chunks_dir, "model_params.rds")

if (!file.exists(manifest_path)) {
  stop("Manifest not found at: ", manifest_path,
       "\nPlease run 01_prepare_data.R first.")
}

manifest     <- readRDS(manifest_path)
model_params <- readRDS(model_params_path)

# Unpack model params into the local environment. alpha/beta are NOT taken from here --
# each draw brings its own (see Section 3) -- these are only the scenario constants.
linkinv <- model_params$linkinv
YEARS   <- model_params$years
NYEARS  <- model_params$nyears
iota    <- model_params$iota
scc     <- model_params$scc
scc_dly <- model_params$scc_dly

# Full CP_LIST restored. The earlier cp=80-only restriction was needed because opp_cost
# (03_aggregate_results.R Section 9) integrates cumulatively over all carbon-price steps
# up to a given price -- collapsing to cp=80 alone turned that into a biased 2-point
# trapezoid. Now that this script runs the exog regime only (see run_chunk()), that
# concern no longer applies: exog_policy always uses "revenue_cp0" regardless of cp, so
# every carbon price here is cheap (no cobweb-derived revenue_cpX needed), and the full
# multi-point opp_cost integral works correctly again.
CP_LIST <- model_params$cp_list

# Derived simulation constants
# Scenarios = exog regime only, 1 actual baseline D(0,V(0))
#   + 3 per carbon price (policy, FI paid-for baseline, AI paid-for baseline) = 1 + 3*nCP.
N_SCENARIOS       <- 1L + 3L * length(CP_LIST)
N_YEARS_WITH_2020 <- NYEARS + 2L           # years vector covers 2021..2020+NYEARS, plus year 2020 added back

cat("Manifest: ", nrow(manifest), "chunks\n")
cat("CP_LIST: ", paste(CP_LIST, collapse = ", "), "\n")
cat("Years: ", min(YEARS), "to", max(YEARS), "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Load the FE-draws manifest from 01b_prepare_uncertainty_draws.R -----
# fe_draws_manifest.rds is all-or-nothing: 01b stops immediately if any draw's FE refit
# fails, so this file only ever exists once it covers every draw. All draws it lists are
# processed below (Section 5) -- there's no partial-completion case to handle here.

fe_draws_manifest_path <- file.path(results_dir, "fe_draws", "fe_draws_manifest.rds")
if (!file.exists(fe_draws_manifest_path)) {
  stop("fe_draws_manifest.rds not found at: ", fe_draws_manifest_path,
       "\nPlease run 01b_prepare_uncertainty_draws.R first.")
}
fe_draws_manifest <- readRDS(fe_draws_manifest_path)

if (nrow(fe_draws_manifest) == 0) {
  stop("fe_draws_manifest.rds has zero ready draws. Run 01b_prepare_uncertainty_draws.R.")
}

cat("Loaded fe_draws_manifest.rds:", nrow(fe_draws_manifest), "draws ready for simulation\n")

if (!is.null(MAX_DRAWS)) {
  fe_draws_manifest <- fe_draws_manifest[seq_len(min(MAX_DRAWS, nrow(fe_draws_manifest))), ]
  cat("MAX_DRAWS =", MAX_DRAWS, "-- limiting this run to", nrow(fe_draws_manifest), "draw(s).\n")
}

N_DRAWS <- nrow(fe_draws_manifest)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Define the per-chunk worker function -----
#
# run_chunk() is the function executed by each parallel worker, for one (draw, chunk)
# task. Design principles:
#   - Receives the chunk file path (not data) to avoid large serialization overhead, but
#     receives this draw's FE tables (gamma_df/gamma_df_ai/delta_df) as in-memory objects
#     rather than a file path -- Section 6 calls future_map() once PER DRAW, so these are
#     read from disk ONCE in the main process and then passed as futures' globals, which
#     future's persistent-worker caching sends to each worker only once per draw (not once
#     per chunk). Reading fe_draw_path fresh inside every task -- which an earlier version
#     of this script did -- redundantly reread and reparsed the same small file once per
#     chunk instead of once per draw.
#   - Reads, validates, simulates, and writes independently
#   - Returns a small summary list (not results data) back to the main process
#   - All failures are reported with informative error messages, tagged with draw_id so
#     failures are traceable back to a draw.

run_chunk <- function(chunk_file, expected_n_rows, alpha, beta, linkinv,
                      cp_list, years, results_dir, gamma_df, gamma_df_ai, delta_df, draw_id) {
  # Wrap everything in tryCatch so a single failed task doesn't kill the whole run.
  # The error is returned as a structured list so the main process can report it clearly.
  tryCatch({

    # Parallelism here comes from running N_WORKERS separate processes (Section 5), so
    # each process's own data.table calls (rbindlist/joins below) must not ALSO spawn
    # multiple OpenMP threads -- that would oversubscribe a core count already capped at
    # N_WORKERS, adding heat/contention without any speed benefit.
    data.table::setDTthreads(1)

    # ---- READ ----
    chunk <- arrow::read_parquet(chunk_file)

    # ---- VALIDATE ON READ ----
    # These checks catch any corruption or mismatch between the manifest and files.
    if (nrow(chunk) != expected_n_rows) {
      stop(sprintf("Row count mismatch: expected %d, got %d in %s",
                   expected_n_rows, nrow(chunk), basename(chunk_file)))
    }

    # pixel_fe/ai_pixel_fe/yc_fe are validated AFTER the FE-replacement join below,
    # since the chunk's baked-in (point-estimate) values are about to be dropped.
    critical_cols <- c("country",
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

    # ---- REPLACE FIXED EFFECTS WITH THIS DRAW'S RE-PROFILED VALUES ----
    # The chunk's baked-in pixel_fe/ai_pixel_fe/yc_fe (written once by 01_prepare_data.R
    # at the point estimate) are dropped and replaced with gamma_df/gamma_df_ai/delta_df,
    # re-profiled for THIS draw's (alpha, beta) by refit_fes()
    # (01b_prepare_uncertainty_draws.R).
    n_before_fe_join <- nrow(chunk)
    chunk <- chunk %>%
      dplyr::select(-pixel_fe, -ai_pixel_fe, -yc_fe) %>%
      dplyr::left_join(gamma_df,    by = "pixel_id") %>%
      dplyr::left_join(gamma_df_ai, by = "pixel_id") %>%
      dplyr::left_join(delta_df,    by = "country")

    if (nrow(chunk) != n_before_fe_join) {
      stop(sprintf(
        "Draw-specific FE join changed row count in %s: %d -> %d (gamma_df/gamma_df_ai/delta_df likely have duplicate keys)",
        basename(chunk_file), n_before_fe_join, nrow(chunk)
      ))
    }
    if (anyNA(chunk$pixel_fe) || anyNA(chunk$ai_pixel_fe) || anyNA(chunk$yc_fe)) {
      stop(sprintf(
        "Draw-specific FE join produced NA values in %s -- gamma_df/gamma_df_ai/delta_df may be missing pixel_ids or countries present in this chunk",
        basename(chunk_file)
      ))
    }

    n_pixels_chunk <- nrow(chunk)

    # ---- RUN ALL SIMULATION SCENARIOS ----
    # Exog regime only: crop prices are held at baseline V(0) for every carbon price (no
    # feedback) -- the endog (price-responsive) regime is intentionally dropped from this
    # uncertainty run (it needed cobweb-recomputed equilibrium prices per draw, and its
    # opp_cost integral has separate issues; see 02_run_simulations.R for endog + exog).
    #
    # 3 simulations are genuinely DISTINCT. Each is one simulate_10_dt() call (a forward
    # simulation over all pixels in the chunk for a given carbon price + fixed-effect assumption):
    #
    #   object              simulate_10_dt args                  meaning        runs
    #   ------------------  -----------------------------------  -------------  ----
    #   sim_base_v0         cp=0, "revenue_cp0", "pixel_fe"      D(0,V(0))         1
    #   sim_base_ai_v0      cp=0, "revenue_cp0", "ai_pixel_fe"   D_ai(0,V(0))      1
    #   exog_policy    (n)  cp=P, "revenue_cp0", "pixel_fe"      D(P,V(0))         n = length(cp_list)
    #                                                            distinct total = 2 + n
    #
    # These are assembled into (1 + 3n) LABELED blocks. Two of the three block families
    # (fi_bl_vp, ai_bl_vp) are cheap RELABELED COPIES, not re-runs: under exog there is no
    # leakage and no price-anticipation gap, so the exog FI/AI paid-for baselines and the
    # exog cp=0 actual baseline are all P-independent and equal D(0,V(0)) / D_ai(0,V(0))
    # (i.e. sim_base_v0 / sim_base_ai_v0):
    #   sim_base_v0 -> (fi, cp=0) and (fi_bl_vp, every P)   [copies];
    #   exog_policy -> (fi, P)                              [real runs];
    #   sim_base_ai_v0 -> (ai_bl_vp, every P)               [copies].
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

    # --- genuinely-distinct simulations ---
    sim_base_v0    <- sim_one(0L, "revenue_cp0", "pixel_fe")      # D(0, V(0))   shared actual baseline
    sim_base_ai_v0 <- sim_one(0L, "revenue_cp0", "ai_pixel_fe")   # D_ai(0, V(0)) exog AI baseline
    exog_policy    <- lapply(cp_list, function(cp) sim_one(cp, "revenue_cp0", "pixel_fe"))                # D(P, V(0))

    # --- labeled blocks (fi_bl_vp/ai_bl_vp blocks are relabeled copies, not re-runs) ---
    exog_blocks <- c(
      list(lab(sim_base_v0, "fi", 0L, "exog")),                                              # copy of D(0,V(0))
      Map(function(d, cp) lab(d, "fi", cp, "exog"), exog_policy, cp_list),                    # real D(P,V(0))
      lapply(cp_list, function(cp) lab(sim_base_v0,    "fi_bl_vp", cp, "exog")),              # copies: P-independent
      lapply(cp_list, function(cp) lab(sim_base_ai_v0, "ai_bl_vp", cp, "exog"))               # copies: P-independent
    )

    # MEMORY: free the distinct simulations now. lab() copies (dplyr::mutate), so the labeled
    # blocks above are independent of these originals.
    rm(sim_base_v0, sim_base_ai_v0, exog_policy)
    gc(FALSE)

    # ---- ASSEMBLE SIMULATION RESULTS ----
    # data.table::rbindlist is more memory-efficient than dplyr::bind_rows over many data.tables.
    # Free the block list immediately after binding so only the combined frame remains.
    for_shr <- data.table::rbindlist(exog_blocks)
    rm(exog_blocks)
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
    # Scenarios per pixel: exog regime only, 1 actual baseline D(0,V(0)) + 3 per carbon
    # price (policy, FI paid-for baseline, AI paid-for baseline).
    n_scenarios       <- 1L + 3L * length(cp_list)
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
    # Returning full data would serialize ~50 MB per task across the socket to the
    # main process, which would negate the memory benefits of parallelization.
    list(
      chunk_file  = chunk_file,
      draw_id     = draw_id,
      result_file = result_file,
      n_rows      = nrow(sim_biomass_chunk),
      n_pixels    = n_pixels_chunk,
      success     = TRUE
    )

  }, error = function(e) {
    list(
      chunk_file = chunk_file,
      draw_id    = draw_id,
      error      = conditionMessage(e),
      success    = FALSE
    )
  })
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Run simulations in parallel, once per draw -----
#
# future::multisession spawns N_WORKERS independent R processes ONCE, reused across every
# draw's future_map() call (no repeated pool spin-up/teardown). For each draw:
#   1. Read this draw's FE table ONCE in the main process.
#   2. Run ONE future_map() over ALL of this draw's chunks (no skip-if-exists -- see
#      header note), passing gamma_df/gamma_df_ai/delta_df as in-memory globals -- future
#      sends them to each worker once per draw, not once per chunk (Section 4 header note).
#   3. Stop immediately if any chunk fails, and write this draw's results_manifest.rds
#      otherwise, before moving to the next draw.
#
# This is a loop over draws, but the parallelism doing the actual work -- N_WORKERS
# processes churning through a draw's chunks -- is identical to a fully flattened
# (draw x chunk) grid; looping only changes how work is batched into future_map() calls,
# not how much of it runs concurrently. The gap between draws is cheap (one small
# readRDS() + dispatching the next future_map()) because FE re-profiling already
# happened up front in 01b_prepare_uncertainty_draws.R -- unlike the old design this
# replaced, where refit_fes() ran serially in that gap and left workers idle.

for (d in fe_draws_manifest$draw_id) {
  dir.create(file.path(results_dir, sprintf("draw_%02d", d)), recursive = TRUE, showWarnings = FALSE)
}

cat("Starting parallel simulation with", N_WORKERS, "workers across", N_DRAWS, "draws...\n")
tictoc::tic("Total parallel simulation time (all draws)")

future::plan(future::multisession, workers = N_WORKERS)

draw_summaries <- vector("list", nrow(fe_draws_manifest))

for (i in seq_len(nrow(fe_draws_manifest))) {

  draw_t0          <- Sys.time()
  d                <- fe_draws_manifest$draw_id[i]
  draw_alpha       <- fe_draws_manifest$alpha[i]
  draw_beta        <- fe_draws_manifest$beta[i]
  draw_results_dir <- file.path(results_dir, sprintf("draw_%02d", d))

  cat(sprintf("\n--- Draw %d/%d (id=%d): alpha = %.6f, beta = %.6f | %d chunks ---\n",
              i, nrow(fe_draws_manifest), d, draw_alpha, draw_beta, nrow(manifest)))

  # ---- Load this draw's re-profiled FE table ONCE (not once per chunk) ----
  fe_draw     <- readRDS(fe_draws_manifest$fe_draw_path[i])
  gamma_df    <- fe_draw$gamma_df
  gamma_df_ai <- fe_draw$gamma_df_ai
  delta_df    <- fe_draw$delta_df

  worker_results_d <- furrr::future_map(
    seq_len(nrow(manifest)),
    function(j) {
      run_chunk(
        chunk_file      = manifest$file_path[j],
        expected_n_rows = manifest$n_rows[j],
        alpha           = draw_alpha,
        beta            = draw_beta,
        linkinv         = linkinv,
        cp_list         = CP_LIST,
        years           = YEARS,
        results_dir     = draw_results_dir,
        gamma_df        = gamma_df,
        gamma_df_ai     = gamma_df_ai,
        delta_df        = delta_df,
        draw_id         = d
      )
    },
    .options  = furrr::furrr_options(seed = 93106L),
    .progress = TRUE
  )

  rm(fe_draw, gamma_df, gamma_df_ai, delta_df)

  # ---- Stop immediately on any chunk failure ----
  failed_d <- Filter(function(r) !isTRUE(r$success), worker_results_d)
  if (length(failed_d) > 0) {
    msgs <- sapply(failed_d, function(r) sprintf("  chunk %s: %s", basename(r$chunk_file), r$error))
    stop(sprintf("Draw %d: the following chunks failed:\n%s", d, paste(msgs, collapse = "\n")))
  }

  draw_secs <- as.numeric(difftime(Sys.time(), draw_t0, units = "secs"))
  cat(sprintf("  Draw %d: completed all %d chunks in %.1f s\n", d, length(worker_results_d), draw_secs))

  # ---- Save this draw's results manifest ----
  result_files_d <- sapply(worker_results_d, `[[`, "result_file")
  results_manifest_d <- data.frame(
    chunk_id  = seq_along(result_files_d),
    file_path = result_files_d,
    n_rows    = sapply(worker_results_d, `[[`, "n_rows"),
    file_size = file.size(result_files_d)
  )
  saveRDS(results_manifest_d, file.path(draw_results_dir, "results_manifest.rds"))

  draw_summaries[[i]] <- data.frame(
    draw_id      = d,
    alpha        = draw_alpha,
    beta         = draw_beta,
    total_pixels = sum(manifest$n_rows),
    results_dir  = draw_results_dir,
    runtime_secs = draw_secs
  )
}

future::plan(future::sequential)
tictoc::toc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Save the overall draw summary -----
#
# Every draw in fe_draws_manifest completes successfully by this point -- Section 5 stops
# the script immediately on any chunk failure, so draw_summaries has no gaps to handle.

draw_summary_df <- dplyr::bind_rows(draw_summaries)
saveRDS(draw_summary_df, file.path(results_dir, "draw_summary.rds"))
cat("\nSaved draw_summary.rds:", nrow(draw_summary_df), "draws\n")
print(draw_summary_df)

cat("\n=== 02_run_simulations_uncertainty.R COMPLETE ===\n")
