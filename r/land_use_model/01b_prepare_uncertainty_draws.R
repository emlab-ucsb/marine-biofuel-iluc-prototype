#################################
# 01b_prepare_uncertainty_draws.R
# "Draw coefficient uncertainty and re-profile fixed effects per draw"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# Prep stage for the uncertainty-propagation workflow (02_run_simulations_uncertainty.R).
# Draws N_DRAWS (alpha, beta) pairs jointly from the multivariate normal implied by
# vcov(model) (Krinsky-Robb / quasi-Bayesian parametric bootstrap), then re-profiles the
# pixel_id and year^country fixed effects for EACH draw (see refit_fes() below for why
# the FEs must be re-profiled per draw rather than held fixed at the point estimate).
#
# This is split out from 02_run_simulations_uncertainty.R into its own stage because it
# has a very different resource profile than the chunk simulation stage: it needs the
# full historical estimation panel (fe_panel, ~10M rows / ~1 GB on the full dataset) but
# NOT the chunk manifest, and each draw's refit is independent of every other draw's --
# so this stage's ~N_DRAWS feglm() fits are themselves embarrassingly parallel across a
# worker pool, exactly like the chunk simulation is. Keeping the two stages separate
# means each gets a worker pool sized for its own memory profile, and a chunk-sim rerun
# never has to redo the (comparatively few, but individually expensive) FE refits.
#
# HOW IT WORKS:
# 1. Draw N_DRAWS (alpha, beta) pairs from N(c(alpha, beta), vcov(model)).
# 2. A parallel worker re-solves the FE structure for EACH draw's (alpha, beta) via
#    refit_fes() and writes the result (gamma_df/gamma_df_ai/delta_df -- small, tens of
#    MB) to its own file, overwriting any prior file for that draw_id.
# 3. Writes draws.rds and fe_draws_manifest.rds (both covering all N_DRAWS draws) --
#    02_run_simulations_uncertainty.R reads the manifest to build its per-draw runs.
#
# NO CHECKPOINTING:
# Every run re-fits and overwrites FE tables for ALL N_DRAWS draws from scratch -- there
# is no skip-if-exists logic. If any draw's refit fails, the script stops immediately
# rather than continuing past the failure. An earlier version of this script skipped
# draws whose FE-table file already existed, but that's a deliberate simplification
# removed for now (see 02_run_simulations_uncertainty.R's header note for the same
# reasoning: skip-if-exists silently trusts stale files without checking whether they
# still match the current input data).
#
# PREREQUISITES:
# Run 01_prepare_data.R first (needs model.rds and K_data_past in data_directory).


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

# Toggle between sample (200 pixels) and full (~1.5M pixels) run.
# Must match the USE_SAMPLE_DATA setting used in 01_prepare_data.R and
# 02_run_simulations_uncertainty.R (this stage's outputs are read directly by that script).
USE_SAMPLE_DATA <- TRUE

# Number of (alpha, beta) draws to propagate through the simulation.
N_DRAWS <- 200

source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))

select <- dplyr::select
options(scipen = 999)
set.seed(93106)

# MEMORY-AWARE WORKER COUNT.
# This stage's per-worker footprint is fe_panel (~1 GB on full data) + fixest::feglm()
# working memory for a ~10M-row / ~1.5M-FE-level fit -- lighter than the chunk-simulation
# workers in 02_run_simulations_uncertainty.R (~6 GB/worker), so gb_per_worker below is a
# conservative reused estimate. Re-measure and lower it if you want to push N_WORKERS
# higher on a machine where this stage is the binding constraint.
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
# N_WORKERS <- 8   # uncomment to override manually
cat("Using", N_WORKERS, "parallel workers",
    if (!is.na(mem_gb)) sprintf("(detected %.0f GB RAM)", mem_gb) else "(RAM undetected; conservative default)", "\n")

# Directory paths
wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')

data_directory <- if (USE_SAMPLE_DATA) file.path(wdir, "1_input/smpl") else file.path(wdir, "1_input/full")

processing_dir <- file.path(wdir, "2_processing")
data_subdir    <- if (USE_SAMPLE_DATA) "smpl" else "full"
chunks_dir     <- file.path(processing_dir, data_subdir, "chunks")
results_dir    <- file.path(processing_dir, data_subdir, "results")
fe_draws_dir   <- file.path(results_dir, "fe_draws")
dir.create(fe_draws_dir, recursive = TRUE, showWarnings = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load point-estimate alpha/beta -----
# model_params.rds gives the point estimates used as the mean of the draw distribution.

model_params_path <- file.path(chunks_dir, "model_params.rds")
if (!file.exists(model_params_path)) {
  stop("model_params.rds not found at: ", model_params_path,
       "\nPlease run 01_prepare_data.R first.")
}
model_params <- readRDS(model_params_path)
alpha_hat <- model_params$alpha
beta_hat  <- model_params$beta


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Draw coefficient uncertainty -----
#
# model_params.rds only stores point estimates for alpha/beta, not their variance-
# covariance matrix, so we load the fitted model directly here to get vcov(model) and
# draw joint (alpha, beta) pairs from the implied multivariate normal (Krinsky-Robb /
# quasi-Bayesian parametric bootstrap). Each draw is a full, internally-consistent
# (alpha, beta) pair -- NOT independent univariate draws -- which preserves their
# estimated covariance. Relies on set.seed(93106) (Section 1) for reproducibility; no
# other randomness is consumed before this point.

cat("Loading model for vcov()...\n")
model <- readRDS(glue::glue("{data_directory}/model.rds"))
model_family <- model$family   # needed by refit_fes() (Section 5) to re-profile FEs per draw

vcov_sub <- vcov(model)[c("revenue", "remaining_treecover_share"),
                         c("revenue", "remaining_treecover_share")]

draws <- MASS::mvrnorm(N_DRAWS, mu = c(alpha_hat, beta_hat), Sigma = vcov_sub)
colnames(draws) <- c("alpha", "beta")
draws <- as.data.frame(draws)
draws$draw_id <- seq_len(N_DRAWS)

cat("Drew", N_DRAWS, "(alpha, beta) pairs from N(c(alpha, beta), vcov(model)):\n")
print(utils::head(draws))

saveRDS(draws, file.path(results_dir, "draws.rds"))
cat("Saved draws to:", file.path(results_dir, "draws.rds"), "\n")

rm(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Load historical estimation panel for FE re-profiling -----
#
# refit_fes() (Section 5) needs the SAME data `model` was originally fit on, to re-solve
# the FE structure for each draw's (alpha, beta). Ka_simulations_data_preparation.R fits
# `model` on data_past (year < 2021) AFTER zeroing out def_shr for fully-deforested
# pixel-years and dropping those rows -- K_data_past.{Rdata,rds} was saved BEFORE that
# filter, so we reapply it here identically to reconstruct the exact estimation sample.

cat("Loading historical panel for FE re-profiling...\n")

if (USE_SAMPLE_DATA) {
  fe_panel <- readRDS(glue::glue("{data_directory}/K_data_past.rds"))
} else {
  load(glue::glue("{data_directory}/K_data_past.Rdata"))   # loads `data_past`
  fe_panel <- data_past
  rm(data_past)
}

fe_panel <- fe_panel %>%
  dplyr::mutate(def_shr = dplyr::if_else(remaining_treecover_share == 0, NA_real_, def_shr)) %>%
  tidyr::drop_na(def_shr)

# pixel_id -> region mapping for the AI (regional-median) fixed-effect redraw. region is
# a time-invariant pixel attribute, so distinct() over the multi-year panel collapses to
# one row per pixel_id.
group_idx <- fe_panel %>%
  dplyr::select(pixel_id, region) %>%
  dplyr::distinct() %>%
  dplyr::arrange(pixel_id)

cat("Loaded FE re-profiling panel:", nrow(fe_panel), "pixel-years,",
    dplyr::n_distinct(fe_panel$pixel_id), "pixels\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Define FE re-profiling -----
#
# refit_fes() re-profiles pixel_id and year^country fixed effects conditional on a drawn
# (alpha, beta) pair. It holds the slope contribution fixed via fixest's `offset` argument
# (offset = alpha_d*revenue + beta_d*remaining_treecover_share) and fits an intercept-only
# model with the SAME FE structure `model` was originally estimated with, so fixest solves
# only for the FEs -- restoring the internal consistency (FE moment conditions) that would
# otherwise be broken by pairing a new (alpha, beta) with the point-estimate model's FEs.
# vcov = "iid" is the cheapest valid option (fixest has no "skip VCOV entirely" setting)
# since only the point FE values are needed here, not inference.
refit_fes <- function(alpha_d, beta_d, panel, model_family, group_idx, fe_window = 2016:2020) {

  # This stage parallelizes ACROSS draws (N_WORKERS simultaneous feglm() fits), so each
  # individual fit must not also try to multithread internally via OpenMP -- otherwise
  # N_WORKERS processes each spawning multiple threads oversubscribes the machine's cores.
  fixest::setFixest_nthreads(1)

  offset_vec <- alpha_d * panel$revenue + beta_d * panel$remaining_treecover_share

  refit <- fixest::feglm(
    def_shr ~ 1 | pixel_id + year^country,
    data     = panel,
    offset   = offset_vec,
    family   = model_family,
    fixef.rm = "none",
    vcov     = "iid"
  )

  fixedeffects_d <- fixef(refit, na.rm = FALSE)

  gamma_df_d <- fe_to_tibble(fixedeffects_d$pixel_id) %>%
    dplyr::rename(pixel_id = fe_id, pixel_fe = fe_val) %>%
    dplyr::mutate(pixel_id = as.numeric(pixel_id))

  # year^country FE -> country-level yc_fe: recent-window (fe_window) average, falling
  # back to the full-sample mean.
  delta_long <- data.frame(fe_val = as.numeric(fixedeffects_d$`year^country`))
  delta_long$year_country <- names(fixedeffects_d$`year^country`)
  delta_long <- delta_long %>%
    tidyr::separate(year_country, into = c("year", "country"), sep = "_", extra = "merge") %>%
    dplyr::mutate(year = as.integer(year))

  delta_full <- delta_long %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(yc_fe_full = mean(fe_val, na.rm = TRUE), .groups = "drop")
  delta_window <- delta_long %>%
    dplyr::filter(year %in% fe_window) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(yc_fe_win = mean(fe_val, na.rm = TRUE), .groups = "drop")

  delta_df_d <- delta_full %>%
    dplyr::left_join(delta_window, by = "country") %>%
    dplyr::mutate(yc_fe = dplyr::coalesce(yc_fe_win, yc_fe_full)) %>%
    dplyr::select(country, yc_fe)

  gamma_df_ai_d <- redraw_fes(gamma_df_d, group_idx, "median") %>%
    dplyr::rename(ai_pixel_fe = pixel_fe)

  list(gamma_df = gamma_df_d, gamma_df_ai = gamma_df_ai_d, delta_df = delta_df_d)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Re-profile FEs for every draw, in parallel -----
#
# Each draw's refit is independent of every other draw's, so all N_DRAWS refits are
# dispatched across the worker pool at once instead of one-at-a-time in the main process
# (which is what 02_run_simulations_uncertainty.R did before this stage was split out --
# every draw's refit left all chunk-simulation workers idle for its full duration).
# Every run recomputes and overwrites all N_DRAWS draws' FE tables from scratch (see the
# NO CHECKPOINTING header note); the script stops immediately if any draw's refit fails.

fe_draw_path <- function(draw_id) file.path(fe_draws_dir, sprintf("fe_draw_%03d.rds", draw_id))
draws$fe_draw_path <- vapply(draws$draw_id, fe_draw_path, character(1))

cat("\nRe-profiling FEs for all", nrow(draws), "draws...\n")

tictoc::tic("FE re-profiling (all draws)")
future::plan(future::multisession, workers = N_WORKERS)

refit_results <- furrr::future_map(
  seq_len(nrow(draws)),
  function(i) {
    tryCatch({
      fe <- refit_fes(draws$alpha[i], draws$beta[i], fe_panel, model_family, group_idx)
      saveRDS(fe, draws$fe_draw_path[i])
      list(draw_id = draws$draw_id[i], success = TRUE)
    }, error = function(e) {
      list(draw_id = draws$draw_id[i], error = conditionMessage(e), success = FALSE)
    })
  },
  .options  = furrr::furrr_options(seed = 93106L),
  .progress = TRUE
)

future::plan(future::sequential)
tictoc::toc()

failed <- Filter(function(r) !isTRUE(r$success), refit_results)
if (length(failed) > 0) {
  msgs <- sapply(failed, function(r) sprintf("  draw %d: %s", r$draw_id, r$error))
  stop(sprintf("%d draw(s) failed FE re-profiling:\n%s", length(failed), paste(msgs, collapse = "\n")))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 7: Write the FE-draws manifest -----
#
# All N_DRAWS draws succeed by this point -- Section 6 stops the script immediately on
# any refit failure -- so the manifest covers every draw.

fe_draws_manifest <- data.frame(
  draw_id      = draws$draw_id,
  alpha        = draws$alpha,
  beta         = draws$beta,
  fe_draw_path = draws$fe_draw_path,
  file_size    = file.size(draws$fe_draw_path)
)
saveRDS(fe_draws_manifest, file.path(fe_draws_dir, "fe_draws_manifest.rds"))

cat("\nSaved fe_draws_manifest.rds:", nrow(fe_draws_manifest), "draws\n")

cat("\n=== 01b_prepare_uncertainty_draws.R COMPLETE ===\n")
