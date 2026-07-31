#################################
# 03_aggregate_results.R
# "Parallelized aggregation of LBCS simulation results"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# This is the third script in the parallelized simulation workflow.
# It reads the per-chunk result parquet files produced by 02_run_simulations.R,
# computes discounted present-value metrics and payment schemes for each pixel,
# and aggregates across 100 simulation years to produce one row per (pixel_id, cprice).
#
# WHY PARALLELIZED?
# The raw simulation output for 1.5M pixels × 10 scenarios × 102 years is too large
# to load into a single data frame. Because every pixel_id is contained entirely within
# one chunk file (no cross-chunk dependencies), each chunk can be aggregated independently.
# Workers return only the aggregated rows (~48K × 8 cprice = 384K rows per chunk),
# which are safe to serialize back to the main process. The main process never holds
# the full time-series data in memory.
#
# PREREQUISITES:
# Run 01_prepare_data.R and 02_run_simulations.R first, with the same USE_SAMPLE_DATA setting.
#
# TODO:
# 1. AI calculations were added in a rush. Need to carefully proof.
# 2. Currently calculating both delays and differences in emissions. 
# In the end, we only need one since they are equivalent with longer nyears. Can clean up.
# 3. Haven't yet built out the testing / double checking i'd like to see in this script.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

# Toggle between sample (200 pixels) and full (~1.5M pixels) run.
# Must match the USE_SAMPLE_DATA setting used in 01_prepare_data.R and 02_run_simulations.R.
USE_SAMPLE_DATA <- TRUE

# Load packages and functions
source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))

# Package preferences
select <- dplyr::select
options(scipen = 999)

# Directory paths
wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')


processing_dir <- file.path(wdir, "2_processing")
data_subdir    <- if (USE_SAMPLE_DATA) "smpl" else "full"
chunks_dir     <- file.path(processing_dir, data_subdir, "chunks")
results_dir    <- file.path(processing_dir, data_subdir, "results")
output_dir     <- file.path(wdir, "3_output", data_subdir)


# MEMORY-AWARE WORKER COUNT (mirrors 02_run_simulations.R).
# RAM — not cores — is the binding constraint: too many workers exhausts memory and the OS kills
# one, surfacing as "Future (...) MultisessionFuture interrupted". We size workers so
# workers x gb_per_worker leaves ~12 GB headroom for the OS + the main process (which also holds
# the combined aggregated results). gb_per_worker is the measured per-worker peak (incl.
# R/package overhead): aggregate_chunk peaks ~6 GB at 400 chunks (~3,625 px/chunk), slightly
# above 02's worker, so we use a slightly larger value here. Re-measure if N_CHUNKS changes.
gb_per_worker <- 7
mem_gb <- suppressWarnings(tryCatch(
  as.numeric(system("sysctl -n hw.memsize", intern = TRUE)) / 1024^3,
  error = function(e) NA_real_))
N_WORKERS <- if (is.na(mem_gb)) 2L else max(1L, floor((mem_gb - 12) / gb_per_worker))
N_WORKERS <- min(N_WORKERS, parallel::detectCores() - 1L)
# N_WORKERS <- 5   # uncomment to override manually

cat("Data mode:", if (USE_SAMPLE_DATA) "SAMPLE (200 pixels)" else "FULL (~1.5M pixels)", "\n")
cat("Using", N_WORKERS, "parallel workers",
    if (!is.na(mem_gb)) sprintf("(detected %.0f GB RAM)", mem_gb) else "(RAM undetected; conservative default)", "\n")
cat("Results directory:", results_dir, "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load shared parameters from model_params.rds -----
# model_params.rds is written by 01_prepare_data.R and holds all parameters that
# must be consistent across all three scripts: iota, scc, nyears, cp_list, years,
# and model coefficients. Reading from this file (rather than hardcoding values here)
# ensures 03 always uses the same settings as the run that produced the chunks.

model_params_path <- file.path(chunks_dir, "model_params.rds")
if (!file.exists(model_params_path)) {
  stop("model_params.rds not found at: ", model_params_path,
       "\nPlease run 01_prepare_data.R first.")
}
model_params <- readRDS(model_params_path)

iota    <- model_params$iota
scc     <- model_params$scc
scc_dly <- model_params$scc_dly
nyears  <- model_params$nyears

cat("Parameters loaded from model_params.rds: iota =", iota,
    "| scc =", scc, "| nyears =", nyears, "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Load results manifest -----
# The manifest produced by 02_run_simulations.R records the exact absolute paths
# of every result chunk. Using it (rather than list.files()) ensures we only process
# files from the current run — no accidental inclusion of stale chunks.

manifest_path <- file.path(results_dir, "results_manifest.rds")
if (!file.exists(manifest_path)) {
  stop("results_manifest.rds not found at: ", manifest_path,
       "\nPlease run 02_run_simulations.R first.")
}
results_manifest <- readRDS(manifest_path)
result_files <- results_manifest$file_path

# Verify every file listed in the manifest actually exists on disk
missing_files <- result_files[!file.exists(result_files)]
if (length(missing_files) > 0) {
  stop("Manifest lists files that do not exist:\n", paste(missing_files, collapse = "\n"))
}

cat("Found", length(result_files), "result chunks via manifest\n")
cat("Total input size:", round(sum(results_manifest$file_size) / 1024^2, 1), "MB\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Define per-chunk aggregation worker -----
#
# aggregate_chunk() runs the full aggregation pipeline for a single chunk file.
# It receives all required parameters explicitly — workers run in isolated R sessions
# (future::multisession) and cannot access the parent environment's globals.
#
# Input:  one result chunk parquet (crp_price, cprice, pixel_id, year, info, biomass)
# Output: aggregated data frame, one row per (pixel_id, cprice), as a list element

aggregate_chunk <- function(chunk_file, iota, scc_dly, nyears, run_checks = FALSE,
                            max_reads = 3L) {
  # Disable arrow's internal thread pool inside this worker. Nested parallelism -- arrow's own
  # threads running inside a future::multisession worker -- is the trigger for silent,
  # row-group-shaped read corruption on macOS: a worker's read_parquet returns NA / mismatched
  # rows for a contiguous slice WITHOUT erroring, which later surfaces as NA in a joined baseline
  # or a many-to-many join expansion. Single-threaded arrow reads avoid the race; the per-chunk
  # read is small, so cross-chunk parallelism still dominates throughput.
  arrow::set_cpu_count(1L)
  options(arrow.use_threads = FALSE)

  tryCatch({

    # ---- READ + JOIN (one attempt) ----
    # Encapsulated so we can RETRY on a corrupt read (see retry loop below). Returns the joined
    # abate_df (still holding all years, incl. the 2020 seed row) plus any many-to-many join_issues.
    read_and_join <- function() {
      # ---- READ ----
      df <- arrow::read_parquet(chunk_file)

      # ---- SCENARIO LABEL + ANNUAL EMISSIONS ----
      # emissions = annual biomass loss (positive when forest declines year-over-year)
      # Combined into one pipeline to avoid an intermediate copy of df.
      df <- df %>%
        dplyr::group_by(cprice, info, crp_price, pixel_id) %>%
        dplyr::arrange(year) %>%
        dplyr::mutate(emissions = dplyr::lag(biomass) - biomass) %>%
        dplyr::ungroup()

      # ---- EXTRACT BASELINES ----
      # Three counterfactuals are extracted (see plan / header notes):
      #   bl_fi    = D(0, V(0))    actual baseline  -> emissions impact & social benefits (keyed on price-independent pixel/year)
      #   bl_fi_vp = D(0, V(P))    FI paid-for baseline -> payments & opportunity cost     (keyed per carbon price)
      #   bl_ai_vp = D_ai(0, V(P)) AI paid-for baseline -> offers & non-additionality      (keyed per carbon price)
      bl_fi <- df %>%
        dplyr::filter(cprice == 0 & info == "fi") %>%
        dplyr::select(pixel_id, year, crp_price, biomass_bl = biomass, emissions_bl = emissions)

      bl_fi_vp <- df %>%
        dplyr::filter(info == "fi_bl_vp") %>%
        dplyr::select(pixel_id, year, crp_price, cprice,
                      biomass_bl_vp = biomass, emissions_bl_vp = emissions)

      bl_ai_vp <- df %>%
        dplyr::filter(info == "ai_bl_vp") %>%
        dplyr::select(pixel_id, year, crp_price, cprice,
                      biomass_bl_ai_vp = biomass, emissions_bl_ai_vp = emissions)

      # ---- BUILD ABATEMENT DATA FRAME ----
      # Join baselines onto the policy scenarios (cprice != 0, info == "fi").
      # The actual baseline bl_fi is price-independent (joined on pixel/year/crp_price);
      # the two paid-for baselines are price-specific (joined on pixel/year/crp_price/cprice).
      # Track row counts before/after each join: a many-to-many would expand them.
      vp_keys    <- c("pixel_id", "year", "crp_price", "cprice")
      left_df    <- df %>% dplyr::filter(cprice != 0 & info == "fi")
      n_before   <- nrow(left_df)
      left_df    <- left_df %>% dplyr::left_join(bl_fi,    by = c("pixel_id", "year", "crp_price"))
      n_after_fi <- nrow(left_df)
      left_df    <- left_df %>% dplyr::left_join(bl_fi_vp, by = vp_keys)
      n_after_fivp <- nrow(left_df)
      left_df    <- left_df %>% dplyr::left_join(bl_ai_vp, by = vp_keys)
      n_after_aivp <- nrow(left_df)
      abate_df   <- left_df %>% dplyr::select(-info) %>% dplyr::distinct()
      rm(left_df)

      join_issues <- list()
      if (n_after_fi != n_before)
        join_issues$bl_fi <- list(
          n_before = n_before, n_after = n_after_fi,
          bl_dupes = nrow(bl_fi %>% dplyr::count(pixel_id, year, crp_price) %>% dplyr::filter(n > 1))
        )
      if (n_after_fivp != n_after_fi)
        join_issues$bl_fi_vp <- list(
          n_before = n_after_fi, n_after = n_after_fivp,
          bl_dupes = nrow(bl_fi_vp %>% dplyr::count(pixel_id, year, crp_price, cprice) %>% dplyr::filter(n > 1))
        )
      if (n_after_aivp != n_after_fivp)
        join_issues$bl_ai_vp <- list(
          n_before = n_after_fivp, n_after = n_after_aivp,
          bl_dupes = nrow(bl_ai_vp %>% dplyr::count(pixel_id, year, crp_price, cprice) %>% dplyr::filter(n > 1))
        )

      # Free large intermediates before returning.
      rm(df, bl_fi, bl_fi_vp, bl_ai_vp)
      gc()
      list(abate_df = abate_df, join_issues = join_issues)
    }

    # ---- RETRY ON CORRUPT READ ----
    # A transient corrupt read shows up as EITHER many-to-many join expansion (corrupt baseline
    # keys) OR NA in a joined baseline biomass column. Both are impossible on a clean read -- the
    # raw parquet has no NA biomass and unique baseline keys -- so either is a reliable signal to
    # re-read. With arrow threads now off, a re-read almost always returns clean data.
    corruption_signals <- function(rj) {
      s <- character(0)
      if (length(rj$join_issues) > 0)
        s <- c(s, paste0("join expansion (", paste(names(rj$join_issues), collapse = ", "), ")"))
      if (anyNA(rj$abate_df$biomass_bl))       s <- c(s, "NA biomass_bl")
      if (anyNA(rj$abate_df$biomass_bl_vp))    s <- c(s, "NA biomass_bl_vp")
      if (anyNA(rj$abate_df$biomass_bl_ai_vp)) s <- c(s, "NA biomass_bl_ai_vp")
      s
    }

    read_attempts <- 0L
    repeat {
      read_attempts <- read_attempts + 1L
      rj  <- read_and_join()
      bad <- corruption_signals(rj)
      if (length(bad) == 0L || read_attempts >= max_reads) break
      rm(rj); gc(FALSE)   # discard the corrupt read and try again
    }

    # If corruption persisted across all attempts, fail this chunk LOUDLY so Section 6 halts the
    # run with the chunk id -- never emit silently-wrong (NA or row-inflated) results downstream.
    if (length(bad) > 0L)
      stop(sprintf("read corruption persisted after %d attempt(s): %s",
                   read_attempts, paste(bad, collapse = "; ")))

    abate_df    <- rj$abate_df
    join_issues <- rj$join_issues
    rm(rj)

    # ---- FILTER ----
    # Drop extra years before all mutations so every subsequent operation runs on the
    # smaller final subset. Both crop-price regimes (crp_price = "endog" and "exog") flow
    # through here; the group_by(cprice, info, crp_price, pixel_id) above and the
    # crp_price-keyed baseline joins keep the two regimes separate.
    stopifnot("abate_df does not extend through 2020 + nyears" =
      max(abate_df$year) >= 2021 + nyears)
    abate_df <- abate_df %>%
      dplyr::filter(year > 2020, year < 2021 + nyears)

    # ---- ALL METRICS IN ONE PASS ----
    # Two distinct counterfactuals are used (see plan / header):
    #   ACTUAL (vs D(0,V(0)), the bl_fi baseline) drives emission impacts & social benefits.
    #   PAID-FOR (vs D(0,V(P))/D_ai(0,V(P))) drives payments, offers, and opportunity cost,
    #     because the policymaker anticipates the carbon price's crop-price shift V(0)->V(P)
    #     and a price-taking land user's no-payment behavior is D(0,V(P)).
    #
    # dly_co2:   additional forest stock vs ACTUAL baseline (cumulative delayed emissions)
    # abate_co2: annual avoided emissions vs ACTUAL baseline
    # *_paid / *_ai: counterparts vs the PAID-FOR baselines D(0,V(P)) / D_ai(0,V(P))
    # pv_*: discounted present-value equivalents; all computed here to avoid a
    # second intermediate copy of abate_df.
    #
    # NOTE: abate_co2 (and hence pv_co2 below) can be NEGATIVE for a small number of
    # extreme pixels. This is expected, not a bug -- it is the leakage / general-equilibrium
    # effect of the endogenous crop-price uplift exceeding the carbon opportunity cost for
    # low-carbon-density pixels (example: pixel 1269571). The ACTUAL avoided emissions
    # (pv_co2) is deliberately left unclamped so this genuine effect is preserved. Note that
    # the PAID-FOR avoided emissions (abate_co2_paid, vs D(0,V(P))) stays positive for such
    # pixels, since the policymaker now pays against the price-shifted baseline.
    discount_factor <- 1 / (1 + iota)^(abate_df$year - 2021)
    cp_dly          <- (iota / (1 + iota)) * abate_df$cprice

    abate_df <- abate_df %>%
      dplyr::mutate(
        # --- ACTUAL counterfactual: D(0,V(0)) -> emission impacts & social benefits ---
        dly_co2      = biomass - biomass_bl,
        abate_co2    = emissions_bl - emissions,
        pv_emissions_bl = emissions_bl * discount_factor,
        pv_dly_co2   = dly_co2      / (1 + iota)^(year - 2021) * iota / (1 + iota),
        pv_co2       = abate_co2    / (1 + iota)^(year - 2021),
        # --- PAID-FOR counterfactual (FI): D(0,V(P)) -> payments & opportunity cost ---
        dly_co2_paid   = biomass - biomass_bl_vp,
        abate_co2_paid = emissions_bl_vp - emissions,
        pv_pymnt       = dly_co2_paid * cp_dly * discount_factor,
        pv_co2_paid    = abate_co2_paid / (1 + iota)^(year - 2021),
        # --- PAID-FOR counterfactual (AI): D_ai(0,V(P)) -> offers & non-additionality ---
        dly_co2_ai          = biomass - biomass_bl_ai_vp,
        abate_co2_ai_expctd = emissions_bl_ai_vp - emissions,
        pv_offer_ai         = dly_co2_ai * cp_dly * discount_factor,
        pv_emissions_bl_ai  = emissions_bl_ai_vp * discount_factor,
        pv_co2_ai_exptd     = abate_co2_ai_expctd / (1 + iota)^(year - 2021),
        # --- baseline-independent financial flows (unchanged) ---
        pv_pymnt_all = biomass     * cp_dly * discount_factor,
        pv_tax       = cprice * emissions   * discount_factor
      )

    # ---- TELESCOPING IDENTITY CHECK (sample runs only) ----
    # Cumulative avoided emissions must equal the delayed emissions stock at each year.
    # Skipped on full runs to avoid creating a 2+ GB sorted copy of abate_df.
    # Note: in the future we can remove this - just wanted to double check
    if (run_checks) {
      check_df <- abate_df %>%
        dplyr::arrange(pixel_id, cprice, crp_price, year) %>%
        dplyr::group_by(pixel_id, cprice, crp_price) %>%
        dplyr::mutate(
          cum_avoided_em      = cumsum(dplyr::coalesce(emissions_bl - emissions, 0)),
          cum_avoided_em_paid = cumsum(dplyr::coalesce(emissions_bl_vp - emissions, 0))
        ) %>%
        dplyr::ungroup()
      stopifnot("dly_co2 != cumulative avoided emissions" =
        all(abs(check_df$dly_co2 - check_df$cum_avoided_em) < 1e-6, na.rm = TRUE))
      stopifnot("dly_co2_paid != cumulative paid-for avoided emissions" =
        all(abs(check_df$dly_co2_paid - check_df$cum_avoided_em_paid) < 1e-6, na.rm = TRUE))
      rm(check_df)
    }

    # ---- AGGREGATE across 100 years ----
    # TODO: I'm realizing that I'm currently calculating opp_cost and determining participation based 
    # on the payments over the life of the program, which is somewhat inconsistent with our theory.
    # Probably want to tweak the AI scenario calculations to bring this into alignment with the theory.
    agg_df <- abate_df %>%
      dplyr::group_by(pixel_id, cprice, crp_price) %>%
      dplyr::summarise(
        pv_dly_co2   = sum(pv_dly_co2),
        pv_co2       = sum(pv_co2),
        pv_co2_paid  = sum(pv_co2_paid),                     # paid-for avoided emissions (vs D(0,V(P))), drives opportunity cost
        pv_co2_ai_exptd_raw = sum(pv_co2_ai_exptd),          # AI expected avoided emissions, BEFORE flooring (see below)
        pv_tax       = sum(pv_tax),
        pv_pymnt     = sum(pv_pymnt),
        pv_pymnt_all = sum(pv_pymnt_all),
        pv_offer_ai  = sum(pv_offer_ai),
        pv_emissions_bl = sum(pv_emissions_bl),
        pv_emissions_bl_ai = sum(pv_emissions_bl_ai),
        .groups = "drop"
      )

    # ---- FLOOR AI EXPECTED ABATEMENT AT ZERO (deliberate modeling choice, made transparent) ----
    # pv_co2_ai_exptd_raw CAN be negative, and this is EXPECTED, not a bug. The AI baseline
    # D_ai(0, V(P)) uses the regional-MEDIAN fixed effect, so for a pixel whose true FE exceeds
    # its region's median (pixel_fe > ai_pixel_fe), the median baseline under-predicts that
    # pixel's deforestation and the policy run can out-deforest it -> negative "expected"
    # abatement. We floor at 0 because the policymaker, computing offers from regional medians,
    # never credits negative expected abatement. We do NOT assert non-negativity (that would
    # halt on correct behavior); instead we track how often/how much the floor binds so it is
    # visible every run and a sudden change would flag genuinely unanticipated behavior.
    ai_floored <- agg_df %>% dplyr::filter(pv_co2_ai_exptd_raw < -1e-6)
    ai_floor_info <- list(
      n_rows        = nrow(ai_floored),
      n_pixels      = dplyr::n_distinct(ai_floored$pixel_id),
      total_floored = sum(pmin(agg_df$pv_co2_ai_exptd_raw, 0)),   # negative PV mass removed by the floor
      most_negative = if (nrow(ai_floored) > 0) min(ai_floored$pv_co2_ai_exptd_raw) else 0
    )
    agg_df <- agg_df %>%
      dplyr::mutate(pv_co2_ai_exptd = pmax(pv_co2_ai_exptd_raw, 0)) %>%
      dplyr::select(-pv_co2_ai_exptd_raw)

    list(success = TRUE, data = agg_df, chunk = basename(chunk_file),
         n_pixels = dplyr::n_distinct(agg_df$pixel_id),
         join_issues = join_issues, ai_floor_info = ai_floor_info,
         read_attempts = read_attempts)

  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e), chunk = basename(chunk_file))
  })
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Run aggregation in parallel -----

cat("Starting parallel aggregation...\n")
tictoc::tic("Total aggregation time")

future::plan(future::multisession, workers = N_WORKERS)
worker_results <- furrr::future_map(
  result_files,
  ~ aggregate_chunk(.x, iota = iota, scc_dly = scc_dly, nyears = nyears,
                    run_checks = USE_SAMPLE_DATA),
  .options  = furrr::furrr_options(seed = 93106L),
  .progress = TRUE
)
future::plan(future::sequential)
tictoc::toc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Check for worker failures -----

failed <- Filter(function(r) !isTRUE(r$success), worker_results)
if (length(failed) > 0) {
  msgs <- sapply(failed, function(r) paste0("  ", r$chunk, ": ", r$error))
  stop("Chunks failed:\n", paste(msgs, collapse = "\n"))
}

cat("All", length(worker_results), "chunks completed successfully.\n")
cat("Total pixels:", sum(sapply(worker_results, `[[`, "n_pixels")), "\n")

# ---- Transient-read retry report ----
# read_attempts > 1 means a worker hit corrupt-read symptoms (NA baseline / join expansion) and
# auto-recovered on a re-read. Zero retries is the expected steady state with arrow threads off;
# a rising count is an early warning that the macOS arrow-in-worker instability is resurfacing.
retries <- sapply(worker_results, function(r) (if (is.null(r$read_attempts)) 1L else r$read_attempts) - 1L)
if (sum(retries) > 0) {
  cat(sprintf("Transient-read retries: %d chunk(s) needed a re-read (%d extra read(s) total).\n",
              sum(retries > 0), sum(retries)))
  for (r in worker_results[retries > 0])
    cat("  ", r$chunk, ": ", r$read_attempts, " reads\n", sep = "")
} else {
  cat("Transient-read retries: none (all chunks read cleanly on first attempt).\n")
}

flagged <- Filter(function(r) length(r$join_issues) > 0, worker_results)
if (length(flagged) > 0) {
  warning("*** Many-to-many join expansion detected in ", length(flagged), " chunk(s):")
  for (r in flagged) {
    cat("  Chunk:", r$chunk, "\n")
    for (jname in names(r$join_issues)) {
      ji <- r$join_issues[[jname]]
      cat("    ", jname, " join: rows", ji$n_before, "->", ji$n_after,
          "| duplicate keys:", ji$bl_dupes, "\n", sep = "")
    }
  }
} else {
  cat("Join expansion check: no many-to-many issues detected in baseline joins.\n")
}

# ---- AI expected-abatement flooring report ----
# Summarize how often/how much the pmax(.,0) floor on pv_co2_ai_exptd bound across all chunks.
# Negative raw values are EXPECTED (regional-median FE under-predicts high-propensity pixels);
# this report makes the floor transparent rather than silent, so an unexpected jump is visible.
ai_floor_n_rows    <- sum(sapply(worker_results, function(r) r$ai_floor_info$n_rows))
ai_floor_n_pixels  <- sum(sapply(worker_results, function(r) r$ai_floor_info$n_pixels))
ai_floor_total     <- sum(sapply(worker_results, function(r) r$ai_floor_info$total_floored))
ai_floor_most_neg  <- min(sapply(worker_results, function(r) r$ai_floor_info$most_negative))
if (ai_floor_n_rows > 0) {
  cat(sprintf(paste0(
    "AI expected-abatement floor: %d (pixel x cprice) rows across %d pixel(s) had negative raw\n",
    "  pv_co2_ai_exptd and were floored to 0 (expected: regional-median FE under-predicts\n",
    "  high-propensity pixels). Negative PV mass removed: %.1f | most negative row: %.1f\n"),
    ai_floor_n_rows, ai_floor_n_pixels, ai_floor_total, ai_floor_most_neg))
} else {
  cat("AI expected-abatement floor: never bound (all pv_co2_ai_exptd >= 0).\n")
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 7: Combine chunk results -----

agg_df <- dplyr::bind_rows(lapply(worker_results, `[[`, "data"))
cat("Combined aggregated data:", nrow(agg_df), "rows,", ncol(agg_df), "cols\n")

# ---- NA GUARD ----
# aggregate_chunk() returns success = TRUE only when it ran to completion, so a HARD worker
# failure is already caught in Section 6. This guard catches a subtler, SILENT failure mode:
# a transient arrow::read_parquet glitch in a worker (macOS future::multisession + arrow C++
# instability) can decode a single Parquet row group as NA without erroring. That surfaces as
# NA in a contiguous slice of one chunk's output (e.g. one scenario x cprice for a block of
# pixels) and otherwise flows silently into sim_results.csv, where it poisons un-na.rm'd sums
# in downstream scripts (06_figures_tables.R). No column here should ever be NA on a clean run,
# so we assert that and, if it is violated, report exactly which columns/pixels/cprices are
# affected before stopping. Re-running the aggregation regenerates the bad chunk; if a worker
# keeps tripping on the same chunk, switch its result I/O from arrow to saveRDS.
na_counts <- vapply(agg_df, function(x) sum(is.na(x)), integer(1))
if (any(na_counts > 0)) {
  bad_cols <- names(na_counts)[na_counts > 0]
  cat("*** NA GUARD FAILED: NA values found in combined aggregated data.\n")
  cat("    NA counts by column:\n")
  for (col in bad_cols) cat("      ", col, ": ", na_counts[[col]], "\n", sep = "")

  affected <- agg_df %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(bad_cols), is.na)) %>%
    dplyr::distinct(pixel_id, cprice, crp_price)
  cat("    Affected (pixel_id x cprice x crp_price) rows: ", nrow(affected), "\n", sep = "")
  cat("    Affected cprice values: ",
      paste(sort(unique(affected$cprice)), collapse = ", "), "\n", sep = "")
  cat("    pixel_id range of affected rows: ",
      paste(range(affected$pixel_id), collapse = " - "), "\n", sep = "")
  cat("    Sample affected rows:\n")
  print(utils::head(affected, 10))

  stop("NA values in aggregated results (see report above). This usually indicates a transient ",
       "per-worker arrow read failure for one chunk, not a logic error -- re-run the aggregation. ",
       "If the same chunk keeps failing, switch the worker's result I/O from arrow to saveRDS.")
} else {
  cat("NA guard: no NA values in combined aggregated data.\n")
}

# # Convergence check: pv_dly_co2 and pv_co2 should be within 5% of each other.
# # They converge exactly as nyears → ∞; at 100 years a small discrepancy is expected.
# # Note: in the future we can remove this - just wanted to double check
# stopifnot("pv_dly_co2 and pv_co2 differ by more than 5%" =
#   all(abs(agg_df$pv_dly_co2 - agg_df$pv_co2) /
#       (abs(agg_df$pv_dly_co2) + 1e-10) < 0.1, na.rm = TRUE))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 8: Annualize abatement -----

# Convert PV of avoided emissions to annualized tonnes/year using Stavins (1999)
# annualization formula (converges to abatement × iota as nyears → ∞).
sim_results <- agg_df %>%
  dplyr::mutate(
    abate_co2_y      = pv_co2          * iota / (1 - (1 + iota)^(-nyears)),  # ACTUAL avoided (vs D(0,V(0)))
    abate_co2_y_paid = pv_co2_paid     * iota / (1 - (1 + iota)^(-nyears)),  # PAID-FOR avoided (vs D(0,V(P))) -> opp cost
    abate_co2_y_ai   = pv_co2_ai_exptd * iota / (1 - (1 + iota)^(-nyears)),
    bl_emit_co2_y    = pv_emissions_bl * iota / (1 - (1 + iota)^(-nyears))
  ) %>%
  dplyr::select(-pv_dly_co2)  # redundant with pv_co2; drop to keep output tidy



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Section 9: Calculate asymmetric information outcomes  --------------

# Opportunity cost is integrated from the PAID-FOR abatement series (measured vs D(0,V(P)))
# rather than the actual series. A price-taking land user faces crop prices V(P), so their
# no-payment behavior is D(0,V(P)); the cost of moving them to the policy outcome D(P,V(P))
# is the area to the left of that supply curve. This keeps opportunity cost, participation,
# and welfare consistent with the transfers offered.
#
# UNITS: we integrate against pv_co2_paid (the present-value avoided-emissions series), NOT
# the annualized abate_co2_y_paid. opp_cost must be a present value to match pv_pymnt,
# pv_bnft_fi, pv_tax (all PVs). Integrating against the annualized series instead made
# opp_cost an annual flow, ~A x too small (A = iota/(1-(1+iota)^-nyears) ~ iota), which is
# why opp_cost / pv_pymnt came out ~0.5*iota ~ 0.02 instead of the expected ~0.5.
oppcost_df <- lazy_dt(sim_results) %>%
  select(pixel_id, cprice, crp_price, pv_co2_paid) %>%
  arrange(pixel_id, crp_price, cprice) %>%
  group_by(pixel_id, crp_price) %>%
  mutate(
    cp_lag    = lag(cprice,      default = 0),
    abate_lag = lag(pv_co2_paid, default = 0),
    # Trapezoid area = (base1 + base2) * height / 2
    #   base1  = cp_lag                    (previous carbon price, left edge of price interval)
    #   base2  = cprice                    (current carbon price, right edge of price interval)
    #   height = pv_co2_paid - abate_lag   (increase in paid-for PV abatement over this price step)
    # Cumulative sum integrates piecewise areas into total opportunity cost (PV) at each cprice.
    opp_cost  = cumsum((cprice + cp_lag) * (pv_co2_paid - abate_lag) / 2)
  ) %>%
  ungroup() %>%
  select(pixel_id, cprice, crp_price, opp_cost) %>%
  as_tibble()

oppcost_keys  <- c("cprice", "crp_price", "pixel_id")
n_sim_before  <- nrow(sim_results)
oppcost_dupes <- oppcost_df %>% dplyr::count(!!!rlang::syms(oppcost_keys)) %>% dplyr::filter(n > 1)
sim_dupes     <- sim_results %>% dplyr::count(!!!rlang::syms(oppcost_keys)) %>% dplyr::filter(n > 1)

# Q for Catha: Let's make sure we've addressed the bug you found recently with treatment of opt out
sim_results <- sim_results %>%
  left_join(oppcost_df, by = oppcost_keys) %>%
  mutate(
    # participation & offer use the PAID-FOR (V(P)) quantities; benefits use ACTUAL (V(0)) emissions.
    participate_ai = if_else(pv_offer_ai >= opp_cost, 1, 0),
    pv_paid_ai = pv_offer_ai*participate_ai,
    pv_bnft_fi     = pv_co2 * scc,                          # social benefit of ACTUAL avoided emissions
    pv_bnft_ai     = participate_ai * pv_co2 * scc,         # social benefit of ACTUAL avoided emissions
    pv_nonadd_ai   = participate_ai * (pv_emissions_bl_ai - pv_emissions_bl)
    # Non-additionality = credited (paid-for) baseline emissions - actual baseline emissions.
    #   pv_emissions_bl_ai now = D_ai(0,V(P)) emissions (what the policymaker credits/pays for);
    #   pv_emissions_bl       = D(0,V(0))   emissions (the true environmental baseline).
    # So this captures BOTH the FE-misspecification gap (median vs actual FE) AND the
    # price-anticipation gap (V(P) vs V(0)): non-additional = D_ai(0,V(P)) - D(0,V(0)).
  )

if (nrow(sim_results) != n_sim_before) {
  warning("*** oppcost join expanded rows: ", n_sim_before, " -> ", nrow(sim_results),
          "\n    oppcost_df duplicate keys: ", nrow(oppcost_dupes),
          "\n    sim_results duplicate keys: ", nrow(sim_dupes))
} else {
  cat("oppcost join check: no row expansion detected.\n")
}

# Annualize AI results
sim_results <- sim_results %>%
  dplyr::mutate(
    nonadd_co2_y = pv_nonadd_ai * iota / (1 - (1 + iota)^(-nyears)),
    optout_co2_y = abate_co2_y * (1 - participate_ai)
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 10: Write output -----

output_file <- file.path(output_dir, "sim_results.csv")
readr::write_csv(sim_results, output_file)
cat("Wrote", nrow(sim_results), "rows to", output_file, "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 11: Validate against reference output -----
# Always runs. On the full dataset the current output is subset to reference pixel_ids
# before comparison, so this check is valid regardless of dataset size.
# Note: sim_results_ref.csv was generated with the *corrected* simulation logic
# (non-compounding carbon price). If validation fails after changing the revenue/cp
# calculation, regenerate the reference using simulation_workflow.R on sample data.
ref_file <- file.path(output_dir, "sim_results_ref.csv")
if (!file.exists(ref_file)) {
  cat("Reference file not found at", ref_file, "— skipping validation\n")
} else {
  cat("\nRunning validation against", basename(ref_file), "...\n")

  ref           <- readr::read_csv(ref_file, show_col_types = FALSE)
  ref_pixel_ids <- unique(ref$pixel_id)
  cat("  Reference pixels:", length(ref_pixel_ids),
      "| Current run pixels:", dplyr::n_distinct(sim_results$pixel_id), "\n")

  # sim_results now contains both crop-price regimes; the reference holds only endog rows.
  # Restrict to endog so the comparison aligns 1:1 (this also serves as the endog regression guard).
  current <- sim_results %>%
    dplyr::filter(crp_price == "endog", pixel_id %in% ref_pixel_ids) %>%
    dplyr::arrange(pixel_id, cprice)
  ref_sorted <- ref %>% dplyr::arrange(pixel_id, cprice)

  # Compare only non-AI numeric columns present in both data frames.
  # Columns ending in _ai depend on ai_pixel_fe, which differs between sample
  # and full runs by design — exclude them from validation.
  shared_numeric <- intersect(names(current), names(ref_sorted))
  shared_numeric <- shared_numeric[sapply(current[shared_numeric], is.numeric)]
  shared_numeric <- shared_numeric[!grepl("_ai$", shared_numeric)]

  max_diffs <- sapply(shared_numeric, function(col)
    max(abs(current[[col]] - ref_sorted[[col]]), na.rm = TRUE))

  cat("  Max absolute differences vs reference:\n")
  print(round(max_diffs, 10))

  if (all(max_diffs < 1e-6)) {
    cat("  VALIDATION PASSED — results match reference within 1e-6\n")
  } else {
    warning("Results differ from reference by more than 1e-6 in column(s): ",
            paste(names(max_diffs)[max_diffs >= 1e-6], collapse = ", "))
  }
}

cat("\n=== 03_aggregate_results.R COMPLETE ===\n")


# Pixel 1269571 shows negative avoided emissions in the sample data. This is an
# expected leakage outcome, not a bug -- see the note by the abate_co2 calculation
# in aggregate_chunk() above. To inspect it:
test <- sim_results %>% filter(pixel_id == 1269571)

# Potential tests:
sim_results %>% filter(pv_co2_paid < 0)
sim_results %>% filter(opp_cost < 0)
