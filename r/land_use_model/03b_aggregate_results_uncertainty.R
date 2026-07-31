#################################
# 03_aggregate_results_uncertainty.R
# "Distribution of emission-savings/PV metrics across coefficient uncertainty draws"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# Variant of 03_aggregate_results.R that aggregates the per-draw simulation output
# produced by 02_run_simulations_uncertainty.R (which now runs the FULL CP_LIST, not
# just cp=80) into a cross-draw distribution of the same metrics 03_aggregate_results.R
# computes -- pv_co2, pv_co2_paid, pv_co2_ai_exptd, pv_tax, pv_pymnt, pv_pymnt_all,
# pv_offer_ai, pv_emissions_bl(_ai), abate_co2_y(_paid/_ai), bl_emit_co2_y, opp_cost,
# participate_ai, pv_paid_ai, pv_bnft_fi/ai, pv_nonadd_ai, nonadd_co2_y, optout_co2_y --
# but summed to a TOTAL across all pixels (per cprice x crp_price) rather than kept at
# pixel resolution, since pixel-level detail across e.g. 200 draws is not tractable to
# store or compare. For each metric, the output reports the mean, sd, and 2.5th/97.5th
# percentile across draws (see AGGREGATION DESIGN below for why percentiles).
#
# NOTE: this covers everything 03_aggregate_results.R computes EXCEPT the manuscript
# Table 2 / MVPF / welfare comparisons, which are a further aggregation done downstream
# in 06_figures_tables.R, not in 03_aggregate_results.R itself.
#
# AGGREGATION DESIGN:
# Because chunking is by pixel_id (not by scenario), a single chunk file already
# contains every cprice for its own pixels. That means the opp_cost integral and
# AI-participation logic -- which 03_aggregate_results.R computes AFTER combining all
# chunks, because it's written as one combined-data-frame operation -- only ever needs
# data from the SAME pixel across cprices, which is already local to one chunk. So this
# script folds that logic into the per-chunk worker itself, and collapses each chunk
# immediately to one row per (cprice, crp_price) summed across that chunk's pixels,
# before returning -- workers hand back a tiny partial-sum table, never full per-pixel
# data. That matters here because this aggregation runs once per draw (potentially 200x
# on full data), so keeping each worker's return payload tiny is worth the small
# duplication of re-deriving the per-chunk sums each run (there's no caching across
# draws -- see 02_run_simulations_uncertainty.R's NO CHECKPOINTING note for why).
#
# Draws are a Krinsky-Robb / quasi-Bayesian parametric bootstrap sample (see
# 01b_prepare_uncertainty_draws.R), so the 2.5th/97.5th percentile of a metric's value
# ACROSS DRAWS is used directly as its 95% CI (the standard "percentile method" for this
# kind of bootstrap) -- this doesn't assume normality, which matters since several of
# these metrics are participation-gated (participate_ai) or run through a cumulative
# integral (opp_cost) and could plausibly be skewed. sd() across draws is reported
# alongside as a simple symmetric-uncertainty summary; note this is NOT divided by
# sqrt(N_DRAWS) -- each draw is already a full realization of the statistic (not a
# sample mean over repeated sampling of the same population), so sd() across draws IS
# the standard error of the estimate under this design.
#
# PREREQUISITES:
# Run 01_prepare_data.R, then 01b_prepare_uncertainty_draws.R, then
# 02_run_simulations_uncertainty.R first (matching USE_SAMPLE_DATA), so that
# results_dir/draw_summary.rds and each results_dir/draw_XX/results_manifest.rds exist.
#
# OUTPUT:
# - results_dir/results_uncertainty_by_draw.csv -- one row per (draw_id, cprice,
#   crp_price), with the raw summed total for every metric. Useful for inspecting the
#   draw distribution directly (e.g. a custom plot) rather than only its summary stats.
# - results_dir/results_uncertainty_summary.csv -- one row per (cprice, crp_price), with
#   <metric>_mean, <metric>_sd, <metric>_p2_5, <metric>_p97_5 for every metric.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

# Must match the USE_SAMPLE_DATA setting used in 01_prepare_data.R,
# 01b_prepare_uncertainty_draws.R, and 02_run_simulations_uncertainty.R -- this script
# reads their output directly.
USE_SAMPLE_DATA <- TRUE

# For quick testing, limit how many draws get aggregated. Set to NULL to aggregate every
# draw in draw_summary.rds.
MAX_DRAWS <- 10   # e.g. 3 for a quick smoke test; NULL = aggregate all available draws

source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))

select <- dplyr::select
options(scipen = 999)

# MEMORY-AWARE WORKER COUNT (mirrors 02_run_simulations_uncertainty.R).
# gb_per_worker is carried over from 03_aggregate_results.R's measured peak (~6-7 GB at
# 400 chunks); this worker does a bit more per-chunk work (opp_cost/participation folded
# in, see AGGREGATION DESIGN above) but returns far less data, so this is a reasonable
# starting estimate -- re-measure if it turns out to be off.
gb_per_worker <- 7
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
# N_WORKERS <- 5   # uncomment to override manually

# Directory paths
wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')

processing_dir <- file.path(wdir, "2_processing")
data_subdir    <- if (USE_SAMPLE_DATA) "smpl" else "full"
chunks_dir     <- file.path(processing_dir, data_subdir, "chunks")
results_dir    <- file.path(processing_dir, data_subdir, "results")

cat("Data mode:", if (USE_SAMPLE_DATA) "SAMPLE (200 pixels)" else "FULL (~1.5M pixels)", "\n")
cat("Using", N_WORKERS, "parallel workers",
    if (!is.na(mem_gb)) sprintf("(detected %.0f GB RAM)", mem_gb) else "(RAM undetected; conservative default)", "\n")
cat("Results directory:", results_dir, "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load shared parameters and the draw summary -----
# model_params.rds gives iota/scc/nyears, same as 03_aggregate_results.R.
# draw_summary.rds (written by 02_run_simulations_uncertainty.R) is all-or-nothing --
# that script stops immediately on any chunk failure -- so every draw listed here is
# guaranteed complete.

model_params_path <- file.path(chunks_dir, "model_params.rds")
if (!file.exists(model_params_path)) {
  stop("model_params.rds not found at: ", model_params_path,
       "\nPlease run 01_prepare_data.R first.")
}
model_params <- readRDS(model_params_path)

iota   <- model_params$iota
scc    <- model_params$scc
nyears <- model_params$nyears

cat("Parameters loaded from model_params.rds: iota =", iota,
    "| scc =", scc, "| nyears =", nyears, "\n")

draw_summary_path <- file.path(results_dir, "draw_summary.rds")
if (!file.exists(draw_summary_path)) {
  stop("draw_summary.rds not found at: ", draw_summary_path,
       "\nPlease run 02_run_simulations_uncertainty.R first.")
}
draw_summary <- readRDS(draw_summary_path)
cat("Loaded draw_summary.rds:", nrow(draw_summary), "draws\n")

if (!is.null(MAX_DRAWS)) {
  draw_summary <- draw_summary[seq_len(min(MAX_DRAWS, nrow(draw_summary))), ]
  cat("MAX_DRAWS =", MAX_DRAWS, "-- limiting this run to", nrow(draw_summary), "draw(s).\n")
}
print(draw_summary)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Metric columns -----
# Every metric 03_aggregate_results.R's final sim_results carries (post-annualization,
# post-opp_cost/participation), excluding key columns (pixel_id/cprice/crp_price) and
# pv_dly_co2 (redundant with pv_co2, dropped there too). Defined once here and reused by
# both the per-chunk worker (Section 4) and the cross-draw summary (Section 6).

METRIC_COLS <- c(
  "pv_co2", "pv_co2_paid", "pv_co2_ai_exptd", "pv_tax", "pv_pymnt", "pv_pymnt_all",
  "pv_offer_ai", "pv_emissions_bl", "pv_emissions_bl_ai", "abate_co2_y",
  "abate_co2_y_paid", "abate_co2_y_ai", "bl_emit_co2_y", "opp_cost", "participate_ai",
  "pv_paid_ai", "pv_bnft_fi", "pv_bnft_ai", "pv_nonadd_ai", "nonadd_co2_y", "optout_co2_y"
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Define per-chunk aggregation worker -----
#
# Mirrors aggregate_chunk() in 03_aggregate_results.R (Sections 4, 8, 9 there) so the
# emissions/PV/opp_cost/participation logic is guaranteed to match the point-estimate
# pipeline, but additionally collapses to chunk-level (cprice x crp_price) sums before
# returning -- see AGGREGATION DESIGN in the header for why that's valid to do per chunk.

aggregate_chunk <- function(chunk_file, iota, scc, nyears, metric_cols,
                            run_checks = FALSE, max_reads = 3L) {
  arrow::set_cpu_count(1L)
  options(arrow.use_threads = FALSE)

  tryCatch({

    # ---- READ + JOIN (one attempt) ----
    read_and_join <- function() {
      df <- arrow::read_parquet(chunk_file)

      df <- df %>%
        dplyr::group_by(cprice, info, crp_price, pixel_id) %>%
        dplyr::arrange(year) %>%
        dplyr::mutate(emissions = dplyr::lag(biomass) - biomass) %>%
        dplyr::ungroup()

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
        join_issues$bl_fi <- list(n_before = n_before, n_after = n_after_fi)
      if (n_after_fivp != n_after_fi)
        join_issues$bl_fi_vp <- list(n_before = n_after_fi, n_after = n_after_fivp)
      if (n_after_aivp != n_after_fivp)
        join_issues$bl_ai_vp <- list(n_before = n_after_fivp, n_after = n_after_aivp)

      rm(df, bl_fi, bl_fi_vp, bl_ai_vp)
      gc()
      list(abate_df = abate_df, join_issues = join_issues)
    }

    # ---- RETRY ON CORRUPT READ (mirrors 03_aggregate_results.R) ----
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
      rm(rj); gc(FALSE)
    }

    if (length(bad) > 0L)
      stop(sprintf("read corruption persisted after %d attempt(s): %s",
                   read_attempts, paste(bad, collapse = "; ")))

    abate_df    <- rj$abate_df
    join_issues <- rj$join_issues
    rm(rj)

    stopifnot("abate_df does not extend through 2020 + nyears" =
      max(abate_df$year) >= 2021 + nyears)
    abate_df <- abate_df %>%
      dplyr::filter(year > 2020, year < 2021 + nyears)

    # ---- ANNUAL / PV METRICS (mirrors 03_aggregate_results.R Section 4) ----
    discount_factor <- 1 / (1 + iota)^(abate_df$year - 2021)
    cp_dly          <- (iota / (1 + iota)) * abate_df$cprice

    abate_df <- abate_df %>%
      dplyr::mutate(
        dly_co2      = biomass - biomass_bl,
        abate_co2    = emissions_bl - emissions,
        pv_emissions_bl = emissions_bl * discount_factor,
        pv_dly_co2   = dly_co2      / (1 + iota)^(year - 2021) * iota / (1 + iota),
        pv_co2       = abate_co2    / (1 + iota)^(year - 2021),
        dly_co2_paid   = biomass - biomass_bl_vp,
        abate_co2_paid = emissions_bl_vp - emissions,
        pv_pymnt       = dly_co2_paid * cp_dly * discount_factor,
        pv_co2_paid    = abate_co2_paid / (1 + iota)^(year - 2021),
        dly_co2_ai          = biomass - biomass_bl_ai_vp,
        abate_co2_ai_expctd = emissions_bl_ai_vp - emissions,
        pv_offer_ai         = dly_co2_ai * cp_dly * discount_factor,
        pv_emissions_bl_ai  = emissions_bl_ai_vp * discount_factor,
        pv_co2_ai_exptd     = abate_co2_ai_expctd / (1 + iota)^(year - 2021),
        pv_pymnt_all = biomass     * cp_dly * discount_factor,
        pv_tax       = cprice * emissions   * discount_factor
      )

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

    agg_df <- abate_df %>%
      dplyr::group_by(pixel_id, cprice, crp_price) %>%
      dplyr::summarise(
        pv_co2       = sum(pv_co2),
        pv_co2_paid  = sum(pv_co2_paid),
        pv_co2_ai_exptd_raw = sum(pv_co2_ai_exptd),
        pv_tax       = sum(pv_tax),
        pv_pymnt     = sum(pv_pymnt),
        pv_pymnt_all = sum(pv_pymnt_all),
        pv_offer_ai  = sum(pv_offer_ai),
        pv_emissions_bl = sum(pv_emissions_bl),
        pv_emissions_bl_ai = sum(pv_emissions_bl_ai),
        .groups = "drop"
      )

    # ---- FLOOR AI EXPECTED ABATEMENT AT ZERO (mirrors 03_aggregate_results.R) ----
    ai_floored <- agg_df %>% dplyr::filter(pv_co2_ai_exptd_raw < -1e-6)
    ai_floor_info <- list(
      n_rows        = nrow(ai_floored),
      n_pixels      = dplyr::n_distinct(ai_floored$pixel_id),
      total_floored = sum(pmin(agg_df$pv_co2_ai_exptd_raw, 0)),
      most_negative = if (nrow(ai_floored) > 0) min(ai_floored$pv_co2_ai_exptd_raw) else 0
    )
    agg_df <- agg_df %>%
      dplyr::mutate(pv_co2_ai_exptd = pmax(pv_co2_ai_exptd_raw, 0)) %>%
      dplyr::select(-pv_co2_ai_exptd_raw)

    # ---- ANNUALIZE (mirrors 03_aggregate_results.R Section 8) ----
    sim_results_chunk <- agg_df %>%
      dplyr::mutate(
        abate_co2_y      = pv_co2          * iota / (1 - (1 + iota)^(-nyears)),
        abate_co2_y_paid = pv_co2_paid     * iota / (1 - (1 + iota)^(-nyears)),
        abate_co2_y_ai   = pv_co2_ai_exptd * iota / (1 - (1 + iota)^(-nyears)),
        bl_emit_co2_y    = pv_emissions_bl * iota / (1 - (1 + iota)^(-nyears))
      )

    # ---- OPPORTUNITY COST + AI PARTICIPATION (mirrors 03_aggregate_results.R Section 9) ----
    # A single chunk holds ALL cprice values for its own pixels (chunking splits by
    # pixel_id only), so this cross-cprice-within-pixel integral is valid per chunk.
    # NOTE: explicitly namespaced throughout (dplyr::/dtplyr::), unlike the point-estimate
    # script's equivalent block -- that one runs in the main process (packages attached via
    # sourced files), but this runs inside a future::multisession worker, which is not
    # guaranteed to have packages attached (see how every other call in this function is
    # already dplyr::-prefixed for the same reason).
    oppcost_df <- dtplyr::lazy_dt(sim_results_chunk) %>%
      dplyr::select(pixel_id, cprice, crp_price, pv_co2_paid) %>%
      dplyr::arrange(pixel_id, crp_price, cprice) %>%
      dplyr::group_by(pixel_id, crp_price) %>%
      dplyr::mutate(
        cp_lag    = dplyr::lag(cprice,      default = 0),
        abate_lag = dplyr::lag(pv_co2_paid, default = 0),
        opp_cost  = cumsum((cprice + cp_lag) * (pv_co2_paid - abate_lag) / 2)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(pixel_id, cprice, crp_price, opp_cost) %>%
      dplyr::as_tibble()

    oppcost_keys <- c("cprice", "crp_price", "pixel_id")
    n_before_oc  <- nrow(sim_results_chunk)

    sim_results_chunk <- sim_results_chunk %>%
      dplyr::left_join(oppcost_df, by = oppcost_keys) %>%
      dplyr::mutate(
        participate_ai = dplyr::if_else(pv_offer_ai >= opp_cost, 1, 0),
        pv_paid_ai     = pv_offer_ai * participate_ai,
        pv_bnft_fi     = pv_co2 * scc,
        pv_bnft_ai     = participate_ai * pv_co2 * scc,
        pv_nonadd_ai   = participate_ai * (pv_emissions_bl_ai - pv_emissions_bl)
      )

    if (nrow(sim_results_chunk) != n_before_oc) {
      stop(sprintf("opp_cost join expanded rows in %s: %d -> %d",
                   basename(chunk_file), n_before_oc, nrow(sim_results_chunk)))
    }

    sim_results_chunk <- sim_results_chunk %>%
      dplyr::mutate(
        nonadd_co2_y = pv_nonadd_ai * iota / (1 - (1 + iota)^(-nyears)),
        optout_co2_y = abate_co2_y * (1 - participate_ai)
      )

    # ---- NA GUARD (mirrors 03_aggregate_results.R Section 7) ----
    na_counts <- vapply(sim_results_chunk[metric_cols], function(x) sum(is.na(x)), integer(1))
    if (any(na_counts > 0)) {
      stop(sprintf("NA values in chunk %s (columns: %s)",
                   basename(chunk_file), paste(names(na_counts)[na_counts > 0], collapse = ", ")))
    }

    # ---- COLLAPSE TO CHUNK-LEVEL PARTIAL SUMS (per cprice x crp_price) ----
    # Returning only this small table -- not the full per-pixel data -- keeps the socket
    # transfer back to the main process tiny, which matters when this runs once per draw.
    chunk_totals <- sim_results_chunk %>%
      dplyr::group_by(cprice, crp_price) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(metric_cols), sum),
        n_pixels = dplyr::n_distinct(pixel_id),
        .groups = "drop"
      )

    list(success = TRUE, data = chunk_totals, chunk = basename(chunk_file),
         n_pixels = dplyr::n_distinct(sim_results_chunk$pixel_id),
         join_issues = join_issues, ai_floor_info = ai_floor_info,
         read_attempts = read_attempts)

  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e), chunk = basename(chunk_file))
  })
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Aggregate each draw, once per row of draw_summary -----
#
# The worker pool is started once and reused across all draws (mirrors
# 02_run_simulations_uncertainty.R). For each draw: aggregate all chunks in parallel
# (each returning its own chunk-level partial sums, Section 4), stop immediately on any
# chunk failure (no checkpointing -- matches 02_run_simulations_uncertainty.R), then sum
# the chunk-level partial sums into that draw's grand total per (cprice, crp_price).

cat("\nStarting per-draw aggregation for", nrow(draw_summary), "draws...\n")
tictoc::tic("Total aggregation time (all draws)")

future::plan(future::multisession, workers = N_WORKERS)

draw_totals <- vector("list", nrow(draw_summary))

for (i in seq_len(nrow(draw_summary))) {

  draw_t0    <- Sys.time()
  draw_id    <- draw_summary$draw_id[i]
  draw_alpha <- draw_summary$alpha[i]
  draw_beta  <- draw_summary$beta[i]
  draw_dir   <- draw_summary$results_dir[i]

  cat(sprintf("\n--- Draw %d/%d (id=%d): alpha = %.6f, beta = %.6f ---\n",
              i, nrow(draw_summary), draw_id, draw_alpha, draw_beta))

  manifest_path_d <- file.path(draw_dir, "results_manifest.rds")
  if (!file.exists(manifest_path_d)) {
    stop("Draw ", draw_id, ": results_manifest.rds not found at: ", manifest_path_d)
  }
  results_manifest_d <- readRDS(manifest_path_d)
  result_files_d <- results_manifest_d$file_path

  missing_files <- result_files_d[!file.exists(result_files_d)]
  if (length(missing_files) > 0) {
    stop("Draw ", draw_id, ": manifest lists files that do not exist:\n",
         paste(missing_files, collapse = "\n"))
  }

  # ---- Aggregate all chunks for this draw in parallel ----
  worker_results_d <- furrr::future_map(
    result_files_d,
    ~ aggregate_chunk(.x, iota = iota, scc = scc, nyears = nyears, metric_cols = METRIC_COLS,
                      run_checks = USE_SAMPLE_DATA),
    .options  = furrr::furrr_options(seed = 93106L),
    .progress = TRUE
  )

  # ---- Stop immediately on any chunk failure ----
  failed_d <- Filter(function(r) !isTRUE(r$success), worker_results_d)
  if (length(failed_d) > 0) {
    msgs <- sapply(failed_d, function(r) paste0("  ", r$chunk, ": ", r$error))
    stop(sprintf("Draw %d: chunks failed:\n%s", draw_id, paste(msgs, collapse = "\n")))
  }

  flagged_d <- Filter(function(r) length(r$join_issues) > 0, worker_results_d)
  if (length(flagged_d) > 0) {
    warning(sprintf("Draw %d: many-to-many join expansion detected in %d chunk(s)",
                    draw_id, length(flagged_d)))
  }

  ai_floor_n_rows_d <- sum(sapply(worker_results_d, function(r) r$ai_floor_info$n_rows))
  if (ai_floor_n_rows_d > 0) {
    ai_floor_total_d <- sum(sapply(worker_results_d, function(r) r$ai_floor_info$total_floored))
    cat(sprintf("  AI floor bound on %d (pixel x cprice) rows this draw (PV mass floored: %.1f)\n",
                ai_floor_n_rows_d, ai_floor_total_d))
  }

  # ---- Combine this draw's chunk-level partial sums into the draw's grand total ----
  chunk_totals_d <- dplyr::bind_rows(lapply(worker_results_d, `[[`, "data"))
  draw_total_d <- chunk_totals_d %>%
    dplyr::group_by(cprice, crp_price) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(METRIC_COLS), sum),
      n_pixels = sum(n_pixels),
      .groups = "drop"
    ) %>%
    dplyr::mutate(draw_id = draw_id, alpha = draw_alpha, beta = draw_beta, .before = 1)

  draw_secs <- as.numeric(difftime(Sys.time(), draw_t0, units = "secs"))
  cat(sprintf("  Draw %d: aggregated %d chunks (%d pixels) in %.1f s\n",
              draw_id, length(worker_results_d), draw_total_d$n_pixels[1], draw_secs))

  draw_totals[[i]] <- draw_total_d
}

future::plan(future::sequential)
tictoc::toc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Combine across draws and write outputs -----

draws_all <- dplyr::bind_rows(draw_totals) %>%
  dplyr::arrange(cprice, crp_price, draw_id)

by_draw_file <- file.path(results_dir, "results_uncertainty_by_draw.csv")
readr::write_csv(draws_all, by_draw_file)
cat("\nWrote", nrow(draws_all), "rows to", by_draw_file, "\n")

# ---- Cross-draw distribution: mean, sd, and 95% percentile CI for every metric -----
# See AGGREGATION DESIGN in the header for why percentiles (not a normal-approximation
# CI) are the primary uncertainty measure here.
summary_stats <- draws_all %>%
  dplyr::group_by(cprice, crp_price) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(METRIC_COLS),
      list(
        mean  = ~mean(.x),
        sd    = ~stats::sd(.x),
        p2_5  = ~stats::quantile(.x, 0.025),
        p97_5 = ~stats::quantile(.x, 0.975)
      ),
      .names = "{.col}_{.fn}"
    ),
    n_draws = dplyr::n(),
    .groups = "drop"
  )

summary_file <- file.path(results_dir, "results_uncertainty_summary.csv")
readr::write_csv(summary_stats, summary_file)
cat("Wrote", nrow(summary_stats), "rows to", summary_file, "\n")
print(summary_stats)

cat("\n=== 03_aggregate_results_uncertainty.R COMPLETE ===\n")
