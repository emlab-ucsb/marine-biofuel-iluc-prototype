#################################
# 00c_run_cobweb.R
# "Cobweb price iteration: endogenous crop price equilibrium under carbon pricing,
#  150-year horizon, discounted PV supply shock, chunk-parallel within each cp"
# author: "Robert Heilmayr (emLab)"
#################################
#
# PURPOSE:
# Solve for a vector of equilibrium crop prices for each carbon price scenario
# under a 150-year deforestation simulation horizon. Replaces 00b_run_cobweb.R,
# which held an entire pixel set in each worker and used an undiscounted single-
# snapshot supply shock.
#
# KEY DIFFERENCES FROM 00b_run_cobweb.R:
#   1. Outer loop over carbon prices is SERIAL; the inner loop parallelises over
#      pixel chunks per cobweb iteration. Each worker only ever holds ONE chunk
#      in memory (≈ 80 MB peak), so the script runs on a laptop.
#   2. Supply shock per crop is computed as a DISCOUNTED PV ratio over the full
#      150-year trajectory:
#         supply_shock[c] = (PV_counter[c] - PV_baseline[c]) / PV_baseline[c]
#      where each PV sums annual production / (1+iota)^(year-2021).
#   3. Output is a single CSV (plus matching .Rdata) of equilibrium prices by
#      crop and CPrice; no per-iteration diagnostic files are persisted.
#
# DATA FLOW:
#   This script produces K_final_prices_allcp_{NYEARS}y.{csv,Rdata}, which is
#   consumed by 01_prepare_data.R to bake equilibrium revenues into the 120
#   full-pipeline chunks. Therefore this script CANNOT consume those chunks
#   (circular dependency) — it does its own up-front chunking from raw inputs.
#
# PREREQUISITES:
#   - Raw inputs in data_directory (K_data_past, K_endo_input_data, etc.)
#   - K_elas_pivot_full.Rdata in data_directory
#   - model_params.rds (written by 01_prepare_data.R) is OPTIONAL; if absent,
#     NYEARS and iota fall back to documented defaults.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

USE_SAMPLE_DATA <- FALSE #only use FALSE here - SMPL will be based on this one

source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))

select <- dplyr::select
options(scipen = 999)
set.seed(93106)

N_WORKERS <- max(1L, parallel::detectCores() - 2L)
#N_WORKERS<-8
cat("Using", N_WORKERS, "parallel workers\n")

MAX_ITERS      <- 20L
CONV_TOLERANCE <- 1e-4
BASE_YEAR      <- 2021L
N_CHUNKS       <- 120L

CP_LIST <- c(0, 1, 10, 20, 40, 60, 80, 100, 120, 140, 160, 180, 193, 200, 220)

wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
source(here::here("r/directories.R"))
wdir <- glue::glue("{data_directory}/parallelized")
wdir <- file.path('/Users/clatka/github/data/parallelized')



data_subdir    <- if (USE_SAMPLE_DATA) "smpl" else "full"
data_directory <- file.path(wdir, "1_input", data_subdir)
processing_dir <- file.path(wdir, "2_processing")
chunks_dir     <- file.path(processing_dir, data_subdir, "chunks")

cobweb_worker_dir <- file.path(processing_dir, data_subdir, "cobweb")
dir.create(cobweb_worker_dir, recursive = TRUE, showWarnings = FALSE)

# NYEARS and iota: prefer model_params.rds (keeps consistency with 01-03).
# Fall back to documented defaults if 01_prepare_data.R has not yet been run.
model_params_path <- file.path(chunks_dir, "model_params.rds")
if (file.exists(model_params_path)) {
  mp <- readRDS(model_params_path)
  NYEARS <- mp$nyears
  iota   <- mp$iota
  cat(sprintf("Loaded from model_params.rds: NYEARS = %d, iota = %g\n", NYEARS, iota))
} else {
  NYEARS <- 150L
  iota   <- 0.05
  warning("model_params.rds not found at: ", model_params_path,
          "\nFalling back to NYEARS = ", NYEARS, ", iota = ", iota,
          ". Re-run after 01_prepare_data.R for consistency.")
}
YEARS <- seq(2020L, 2020L + NYEARS - 1L)

cat("Data mode:      ", if (USE_SAMPLE_DATA) "SAMPLE" else "FULL", "\n")
cat("Data directory: ", data_directory, "\n")
cat("Cobweb workdir: ", cobweb_worker_dir, "\n")
cat("NYEARS:         ", NYEARS, " | Years:", min(YEARS), "to", max(YEARS), "\n")
cat("iota:           ", iota, " | BASE_YEAR:", BASE_YEAR, "\n")
cat("N_CHUNKS:       ", N_CHUNKS, "\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load raw data -----

cat("Loading raw data...\n")

if (USE_SAMPLE_DATA) {
  data_past            <- readRDS(file.path(data_directory, "K_data_past.rds"))
  endo_input_data      <- readRDS(file.path(data_directory, "endo_input_data.rds"))
  prod_oil             <- readRDS(file.path(data_directory, "prod_oil.rds"))
  suit_harvest_country <- readRDS(file.path(data_directory, "suit_harvest_country.rds"))
  model                <- readRDS(file.path(data_directory, "model.rds"))
  load(file.path(data_directory, "K_elas_pivot_full.Rdata"))
} else {
  load(file.path(data_directory, "K_data_past.Rdata"))
  load(file.path(data_directory, "K_endo_input_data.Rdata"))
  load(file.path(data_directory, "K_prod_oil_full.Rdata"))
  load(file.path(data_directory, "B_suit_harvest_country_full.Rdata"))
  model <- readRDS(file.path(data_directory, "model.rds"))
  load(file.path(data_directory, "K_elas_pivot_full.Rdata"))
}

cat("  data_past:           ", nrow(data_past), "rows\n")
cat("  endo_input_data:     ", nrow(endo_input_data), "rows\n")
cat("  prod_oil:            ", nrow(prod_oil), "rows\n")
cat("  suit_harvest_country:", nrow(suit_harvest_country), "rows\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Extract model components -----

cat("Extracting model components...\n")

model_family <- model$family
coefs        <- broom::tidy(model) %>% select(term, estimate)

alpha   <- (coefs %>% filter(term == "revenue")                   %>% pull(estimate))[1]
beta    <- (coefs %>% filter(term == "remaining_treecover_share") %>% pull(estimate))[1]
linkinv <- model_family$linkinv

stopifnot("alpha coefficient not found" = !is.na(alpha),
          "beta coefficient not found"  = !is.na(beta))
cat("  alpha:", alpha, "| beta:", beta, "\n")

fixedeffects <- fixef(model, na.rm = FALSE)

gamma_df <- fe_to_tibble(fixedeffects$pixel_id) %>%
  rename(pixel_id = fe_id, pixel_fe = fe_val) %>%
  mutate(pixel_id = as.numeric(pixel_id))

delta_df <- data.frame(fixedeffects$`year^country`)
delta_df$year_country <- as.character(rownames(delta_df))
rownames(delta_df) <- NULL
delta_df <- delta_df %>%
  rename(fe_val = fixedeffects..year.country.) %>%
  separate(year_country, into = c("year", "country"), sep = "_", extra = "merge") %>%
  mutate(year = as.integer(year)) %>%
  group_by(country) %>%
  summarise(yc_fe = mean(fe_val, na.rm = TRUE))

cat("  gamma_df:", nrow(gamma_df), "pixels | delta_df:", nrow(delta_df), "countries\n")

rm(model, fixedeffects, coefs, model_family)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Preprocess data_past -----

if ("year" %in% names(data_past) && n_distinct(data_past$year) > 1) {
  data_past <- data_past %>% filter(year == 2020L)
  cat("Filtered data_past to year 2020:", nrow(data_past), "rows\n")
}

data_past <- data_past %>%
  mutate(
    pixel_id_group = pixel_id,
    def_shr        = if_else(remaining_treecover_share == 0, NA_real_, def_shr)
  ) %>%
  drop_na(def_shr, CF) %>%
  left_join(delta_df, by = "country") %>%
  left_join(gamma_df, by = "pixel_id")

stopifnot("NA values in pixel_fe" = !anyNA(data_past$pixel_fe),
          "NA values in yc_fe"    = !anyNA(data_past$yc_fe))
cat("data_past after preprocessing:", nrow(data_past), "pixels\n")

rm(gamma_df, delta_df)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Prepare endo_input_data -----

oil_price_2020 <- prod_oil %>%
  filter(year == 2020) %>%
  summarise(crudeoil_real = first(crudeoil_real)) %>%
  pull(crudeoil_real)

cat("2020 crude oil price:", oil_price_2020, "\n")

n_endo_before <- nrow(endo_input_data)
endo_input_data <- endo_input_data %>%
  left_join(
    suit_harvest_country %>%
      select(pixel_id, crop, gaez_attainable_yield_2000,
             minimum_travel_time_to_large_or_medium_port_minutes),
    by = c("pixel_id", "crop")
  ) %>%
  mutate(crudeoil_real = oil_price_2020,
         yield         = gaez_attainable_yield_2000)

stopifnot("Join changed endo_input_data row count" = nrow(endo_input_data) == n_endo_before)

rm(suit_harvest_country, prod_oil)
gc()

# Baseline price vector (price_2021 column in endo_input_data is the observed 2020
# crop price; we treat it as the cp=0 equilibrium by assumption).
baseline_prices <- endo_input_data %>%
  group_by(crop) %>%
  summarise(price_end = first(price_2021), .groups = "drop") %>%
  arrange(crop)
cat("Baseline prices: ", nrow(baseline_prices), "crops\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Chunk pixels and write cobweb input parquets -----
#
# Each chunk holds (a) sim columns for a slice of pixels, (b) the pixel×crop
# endo_input_data subset for the same pixels. Chunks are matched by chunk_id
# so a worker reads both files for the same pixel slice.

# Adapt N_CHUNKS for small inputs (sample mode): require ≥ ~10 pixels per chunk.
N_CHUNKS <- min(N_CHUNKS, max(1L, nrow(data_past) %/% 10L))
cat("Chunking", nrow(data_past), "pixels into", N_CHUNKS, "chunks...\n")

# Deterministic chunk assignment by pixel_id rank → balanced sizes.
# ungroup() defensively: if upstream data carries a grouping (one group per row
# e.g.), dplyr::row_number() would return 1 for every row and dump everything
# into chunk 1.
pixel_chunk_map <- data_past %>%
  ungroup() %>%
  select(pixel_id) %>%
  arrange(pixel_id) %>%
  mutate(chunk_id = ((dplyr::row_number() - 1L) %% N_CHUNKS) + 1L)

stopifnot(
  "Chunk assignment collapsed: pixel_chunk_map has only one chunk_id value" =
    dplyr::n_distinct(pixel_chunk_map$chunk_id) == N_CHUNKS
)

# Process sim chunks first, free, then endo chunks — keeps peak memory ≈ one slim
# table rather than two.
data_sim_full <- data_past %>%
  select(pixel_id, lag_remaining_treecover, remaining_treecover_share,
         pixel_count, CF, pixel_fe, yc_fe, revenue) %>%
  inner_join(pixel_chunk_map, by = "pixel_id")
rm(data_past); gc()

sim_chunks <- split(data_sim_full, data_sim_full$chunk_id)
rm(data_sim_full)
for (nm in names(sim_chunks)) {
  k     <- as.integer(nm)
  sim_k <- sim_chunks[[nm]] %>% select(-chunk_id)
  arrow::write_parquet(sim_k,
    file.path(cobweb_worker_dir, sprintf("sim_chunk_%03d.parquet", k)))
  if (k == 1L || k %% 20L == 0L || k == N_CHUNKS) {
    cat(sprintf("  sim chunk  %3d: %d rows\n", k, nrow(sim_k)))
  }
}
rm(sim_chunks); gc()

endo_slim_full <- endo_input_data %>%
  select(pixel_id, crop, pixel_count, ha_pixel, def_for_ag_share,
         harvested_share_gaez, m3_yield_ton_per_ha, price_2021,
         crudeoil_real, minimum_travel_time_to_large_or_medium_port_minutes, yield) %>%
  inner_join(pixel_chunk_map, by = "pixel_id")
rm(endo_input_data); gc()

endo_chunks <- split(endo_slim_full, endo_slim_full$chunk_id)
rm(endo_slim_full)
for (nm in names(endo_chunks)) {
  k      <- as.integer(nm)
  endo_k <- endo_chunks[[nm]] %>% select(-chunk_id)
  arrow::write_parquet(endo_k,
    file.path(cobweb_worker_dir, sprintf("endo_chunk_%03d.parquet", k)))
  if (k == 1L || k %% 20L == 0L || k == N_CHUNKS) {
    cat(sprintf("  endo chunk %3d: %d rows\n", k, nrow(endo_k)))
  }
}
rm(endo_chunks, pixel_chunk_map); gc()

# Shared params bundle (small) — workers read this from disk.
cobweb_params <- list(
  alpha          = alpha,
  beta           = beta,
  linkinv        = linkinv,
  elas_pivot     = elas_pivot,
  years          = YEARS,
  iota           = iota,
  base_year      = BASE_YEAR,
  max_iters      = MAX_ITERS,
  conv_tolerance = CONV_TOLERANCE
)
saveRDS(cobweb_params, file.path(cobweb_worker_dir, "cobweb_params.rds"))
cat("Wrote cobweb_params.rds\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 7: Helper functions (defined in main; furrr exports to workers) -----

# Per chunk, compute PV(production) per crop given a 150-year forest-cover trajectory.
# Loops over years to bound memory: each year's join is roughly chunk_pixels × crops.
compute_chunk_pv_production <- function(for_shr_chunk, endo_chunk, iota, base_year) {
  for_shr_dt <- data.table::as.data.table(for_shr_chunk)
  endo_dt    <- data.table::as.data.table(endo_chunk)
  data.table::setkey(for_shr_dt, pixel_id)
  data.table::setkey(endo_dt,    pixel_id)

  years <- sort(unique(for_shr_dt$year))
  pv_acc <- data.table::data.table(crop = character(0), pv_production = numeric(0))

  for (t in years) {
    disc <- 1 / ((1 + iota) ^ (t - base_year))

    year_slice <- for_shr_dt[year == t, .(pixel_id, remaining_treecover_share)]
    joined     <- endo_dt[year_slice, on = "pixel_id", nomatch = 0]

    annual <- joined[, .(
      annual_prod = sum(
        pixel_count * (1 - remaining_treecover_share) *
          ha_pixel * def_for_ag_share * harvested_share_gaez *
          m3_yield_ton_per_ha,
        na.rm = TRUE
      )
    ), by = crop]

    annual[, pv_production := annual_prod * disc]
    pv_acc <- data.table::rbindlist(list(
      pv_acc,
      annual[, .(crop, pv_production)]
    ))
  }

  pv_acc[, .(pv_production = sum(pv_production, na.rm = TRUE)), by = crop]
}

# Worker entry point: runs ONE chunk for ONE (cp, current_prices).
# Returns a small per-crop PV tibble. No big object crosses the socket.
run_chunk <- function(chunk_id, cp, current_prices, worker_data_dir, params) {
  sim_path  <- file.path(worker_data_dir, sprintf("sim_chunk_%03d.parquet",  chunk_id))
  endo_path <- file.path(worker_data_dir, sprintf("endo_chunk_%03d.parquet", chunk_id))

  sim_chunk  <- arrow::read_parquet(sim_path)
  endo_chunk <- arrow::read_parquet(endo_path)

  # update_revenues() expects new_prices with columns (crop, price_end).
  new_revs <- update_revenues(endo_chunk, current_prices)

  starting <- sim_chunk %>%
    dplyr::select(-revenue) %>%
    dplyr::inner_join(new_revs, by = "pixel_id")

  for_shr <- simulate_10_dt(
    starting_data = starting,
    cp            = cp,
    revenue_col   = "revenue",
    fe_col        = "pixel_fe",
    years         = params$years,
    alpha         = params$alpha,
    beta          = params$beta,
    linkinv       = params$linkinv
  )

  compute_chunk_pv_production(for_shr, endo_chunk, params$iota, params$base_year)
}

# Apply elasticity matrix: supply_shock (per crop) → price shocks → new prices.
# Mirrors calc_price_changes() in 00b_run_cobweb.R lines 271–283.
apply_elasticity <- function(supply_shock, elas_pivot, baseline_prices) {
  
  elas_mat <- elas_pivot %>%
    tibble::column_to_rownames("crop") %>%
    as.matrix()
  elas_mat  <- elas_mat[, rownames(elas_mat)]
  off_diag  <- elas_mat[row(elas_mat) != col(elas_mat)]
  if (!all(off_diag == 0, na.rm = TRUE)) {
    stop("elas_pivot is not diagonal: non-zero off-diagonal elements detected.")
  }
  message("Check passed: elas_pivot is diagonal.")

  price_shocks <- supply_shock %>%
    left_join(elas_pivot, by = "crop") %>%
    mutate(across(-c(crop, supply_shock), ~ if_else(. == 0, 0, 1/. * supply_shock)))%>% #change to 1/ elas - to use the inverted elasticity from Roberts and Schlenker
    select(-supply_shock, -crop) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    pivot_longer(names_to = "crop", values_to = "shock", cols = everything())

  baseline_prices %>%
    rename(price_baseline = price_end) %>%
    left_join(price_shocks, by = "crop") %>%
    mutate(price_end = price_baseline * (1 + shock)) %>%
    select(crop, price_end)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 8: Baseline PV production (cp = 0, parallel over chunks, once) -----

cat("\n=== Computing baseline PV production (cp = 0) ===\n")
tictoc::tic("Baseline PV")

future::plan(future::multisession, workers = N_WORKERS)

baseline_chunk_results <- furrr::future_map(
  seq_len(N_CHUNKS),
  function(k) {
    run_chunk(
      chunk_id        = k,
      cp              = 0,
      current_prices  = baseline_prices,
      worker_data_dir = cobweb_worker_dir,
      params          = cobweb_params
    )
  },
  .options  = furrr::furrr_options(seed = 93106L),
  .progress = TRUE
)

pv_baseline <- dplyr::bind_rows(baseline_chunk_results) %>%
  group_by(crop) %>%
  summarise(pv_baseline = sum(pv_production, na.rm = TRUE), .groups = "drop") %>%
  arrange(crop)

tictoc::toc()
cat("Baseline PV per crop:\n")
print(pv_baseline)

stopifnot("Some baseline PVs are zero or negative — check chunks" =
            all(pv_baseline$pv_baseline > 0))

saveRDS(pv_baseline, file.path(cobweb_worker_dir, "pv_baseline.rds"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 9: Cobweb loop — outer over CP_LIST, inner over iterations -----

cat("\n=== Cobweb iteration ===\n")
cat("CP_LIST:", paste(CP_LIST, collapse = ", "), "\n")

final_prices_list <- vector("list", length(CP_LIST))

for (j in seq_along(CP_LIST)) {
  cp <- CP_LIST[j]
  cat(sprintf("\n--- cp = %g (%d / %d) ---\n", cp, j, length(CP_LIST)))
  tictoc::tic(sprintf("cp = %g", cp))

  current_prices <- baseline_prices  # always start from baseline
  converged      <- FALSE
  last_prices    <- NULL

  for (i in seq_len(MAX_ITERS)) {

    chunk_pvs <- furrr::future_map(
      seq_len(N_CHUNKS),
      function(k) {
        run_chunk(
          chunk_id        = k,
          cp              = cp,
          current_prices  = current_prices,
          worker_data_dir = cobweb_worker_dir,
          params          = cobweb_params
        )
      },
      .options  = furrr::furrr_options(seed = 93106L + i)
    )

    pv_counter <- dplyr::bind_rows(chunk_pvs) %>%
      group_by(crop) %>%
      summarise(pv_counter = sum(pv_production, na.rm = TRUE), .groups = "drop")

    supply_shock <- pv_baseline %>%
      inner_join(pv_counter, by = "crop") %>%
      mutate(supply_shock = (pv_counter - pv_baseline) / pv_baseline) %>%
      select(crop, supply_shock)

    # Sign check: under cp > 0, less deforestation → lower production → shock ≤ 0.
    if (cp > 0) {
      violations <- supply_shock %>% filter(supply_shock > 1e-8)
      if (nrow(violations) > 0) {
        warning(sprintf(
          "cp=%g iter=%d: positive supply shock for %d crop(s): %s",
          cp, i, nrow(violations),
          paste(head(violations$crop, 5), collapse = ", ")
        ))
      }
    }

    new_prices <- apply_elasticity(supply_shock, elas_pivot, baseline_prices)

    # Convergence: max relative price change vs. previous iteration's prices.
    if (i > 1L) {
      diffs <- new_prices %>%
        rename(price_new = price_end) %>%
        inner_join(current_prices %>% rename(price_old = price_end), by = "crop") %>%
        mutate(rel_change = abs(price_new - price_old) / price_old)
      max_change <- max(diffs$rel_change, na.rm = TRUE)
      cat(sprintf("  iter %2d | max rel price change: %.6e\n", i, max_change))
      if (max_change < CONV_TOLERANCE) {
        cat(sprintf("  cp=%g converged after %d iterations\n", cp, i))
        converged   <- TRUE
        last_prices <- new_prices
        break
      }
    } else {
      cat(sprintf("  iter %2d | initial step\n", i))
    }

    current_prices <- new_prices
    last_prices    <- new_prices
  }

  tictoc::toc()

  if (!converged && cp > 0) {
    stop(sprintf("No convergence for cp = %g after %d iterations", cp, MAX_ITERS))
  }

  # cp = 0 sanity: by construction, equilibrium prices must equal the baseline.
  if (cp == 0) {
    max_dev <- max(abs(last_prices$price_end - baseline_prices$price_end) /
                     baseline_prices$price_end)
    if (max_dev > CONV_TOLERANCE) {
      warning(sprintf("cp=0 equilibrium prices deviate from baseline by %.6e — check pipeline",
                      max_dev))
    }
  }

  final_prices_list[[j]] <- last_prices %>% mutate(CPrice = cp)
}

future::plan(future::sequential)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 10: Combine and export -----

final_prices_allcp <- dplyr::bind_rows(final_prices_list) %>%
  select(crop, CPrice, price_end) %>%
  arrange(CPrice, crop)

csv_path  <- file.path(data_directory, sprintf("K_final_prices_allcp_%dy.csv",   NYEARS))
rdata_path <- file.path(data_directory, sprintf("K_final_prices_allcp_%dy.Rdata", NYEARS))

readr::write_csv(final_prices_allcp, csv_path)
save(final_prices_allcp, file = rdata_path)

cat("\n=== 00c_run_cobweb.R COMPLETE ===\n")
cat(sprintf("Wrote %d rows to:\n  %s\n  %s\n",
            nrow(final_prices_allcp), csv_path, rdata_path))
cat("Next step: copy/symlink to K_final_prices_allcp.Rdata and run 00_generate_sample_data.R\n")

cat("Afterwards: copy/symlink to K_final_prices_allcp.Rdata and run 01_prepare_data.R\n")
