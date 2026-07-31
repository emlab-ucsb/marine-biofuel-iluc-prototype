#################################
# 01_prepare_data.R
# "Preparing chunked input data for parallelized deforestation simulation"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# This script is the first of two scripts in a parallelized simulation workflow.
# It loads all raw input data, preprocesses it, and writes memory-efficient chunk
# files that can be independently loaded by parallel workers in 02_run_simulations.R.
#
# WHY CHUNKING?
# The full dataset (~1.5M grid cells) is too large to hold in memory on a single
# machine during the simulation. Since every grid cell is simulated independently
# (no cross-pixel dependencies), we can split the work into small chunks. Each
# parallel worker loads only its own chunk, runs all simulation scenarios for those
# pixels, and writes its results to disk. This keeps per-worker memory usage low.
#
# WHAT THIS SCRIPT PRODUCES:
# - N_CHUNKS parquet files in <processing_dir>/lbcs/chunks/
#   Each file contains one row per pixel with:
#   - Pixel identifiers and forest cover state variables
#   - Pixel and country fixed effects (pre-extracted from model)
#   - Asymmetric-info fixed effects (pre-computed regional medians)
#   - Pre-computed revenues for ALL carbon price scenarios (one column each)
#     This avoids workers needing to load the large endo_input_data table.
# - chunk_manifest.rds: metadata for 02_run_simulations.R (file paths, row counts)
# - model_params.rds: model coefficients and configuration for workers
#
# IMPORTANT: One parquet file per chunk (NOT a partitioned dataset directory).
# Using write_dataset() with partitioning was the cause of past "chunk bleed" where
# workers accidentally loaded the full dataset. One file per chunk guarantees
# physical isolation: a worker calling read_parquet("chunk_001.parquet") cannot
# read any other chunk's data.
#
# NOTE TO CATHA: In a future code refactoring, I think it might make sense to 
# place a lot of this code into an initial data prep workflow.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

# Toggle between sample data (200 pixels, for testing) and full dataset (~1.5M pixels).
# Set to FALSE when ready to run the full analysis.
USE_SAMPLE_DATA <- FALSE

source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))

select <- dplyr::select
options(scipen = 999)
set.seed(93106)

# Number of chunks to split the data into.
# Can increase if you run out of memory during chunk writing or downstream processing.
N_CHUNKS <- 400

# Carbon prices to simulate (must match 02_run_simulations.R)
CP_LIST <- c(1, 10, 20, 40, 60, 80, 120, 160, 193, 200)

# Discount rate and social cost of carbon — shared across all three scripts via model_params.rds
iota    <- 0.05
scc     <- 193
scc_dly <- scc * iota / (1 + iota)

# Simulation years: 2020 is the baseline; we simulate 2021 through 2020+NYEARS
NYEARS <- 150
YEARS  <- seq(2020L, 2020L + NYEARS)

# Directory paths 
wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')



if (USE_SAMPLE_DATA) {
  data_directory <- file.path(wdir, "1_input/smpl")
} else {
  data_directory <- file.path(wdir, "1_input/full")
}

processing_dir  <- file.path(wdir, "2_processing") 
data_subdir     <- if (USE_SAMPLE_DATA) "smpl" else "full"
chunks_dir      <- file.path(processing_dir, data_subdir, "chunks")
dir.create(chunks_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- file.path(wdir, "3_output", data_subdir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Data mode:", if (USE_SAMPLE_DATA) "SAMPLE (200 pixels)" else "FULL (~1.5M pixels)", "\n")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load all raw data -----
# All raw files are loaded here. After preprocessing and chunk writing, the large
# objects (endo_input_data, suit_harvest_country) will be freed from RAM.

cat("Loading raw data...\n")

if (USE_SAMPLE_DATA) {
  data_past            <- readRDS(glue::glue("{data_directory}/K_data_past.rds"))
  endo_input_data      <- readRDS(glue::glue("{data_directory}/endo_input_data.rds"))
  prod_oil             <- readRDS(glue::glue("{data_directory}/prod_oil.rds"))
  suit_harvest_country <- readRDS(glue::glue("{data_directory}/suit_harvest_country.rds"))
  model                <- readRDS(glue::glue("{data_directory}/model.rds"))
  load(file = glue::glue("{data_directory}/K_final_prices_allcp_150y.Rdata"))
} else {
  load(glue::glue("{data_directory}/K_data_past.Rdata"))
  load(glue::glue("{data_directory}/K_endo_input_data.Rdata"))
  load(glue::glue("{data_directory}/K_prod_oil_full.Rdata"))
  load(glue::glue("{data_directory}/B_suit_harvest_country_full.Rdata"))
  model <- readRDS(glue::glue("{data_directory}/model.rds"))
  load(file = glue::glue("{data_directory}/K_final_prices_allcp_150y.Rdata"))
}

cat("  data_past loaded:", nrow(data_past), "rows,", ncol(data_past), "cols\n")
cat("  endo_input_data loaded:", nrow(endo_input_data), "rows\n")
cat("  prod_oil loaded:", nrow(prod_oil), "rows\n")
cat("  suit_harvest_country loaded:", nrow(suit_harvest_country), "rows\n")
cat("  model loaded\n")
cat("  final_prices_allcp loaded:", nrow(final_prices_allcp), "rows\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Preprocess data_past -----
# Replicate the preprocessing steps from simulation_workflow.R exactly.

# The full data_past spans 2001–2020 (one row per pixel per year).
# The simulation uses 2020 as the baseline starting year, so filter to that year.
# The sample data was already pre-filtered to 2020, so this is a no-op for the sample.
baseline_year <- 2020L
if ("year" %in% names(data_past) && n_distinct(data_past$year) > 1) {
  data_past <- data_past %>% filter(year == baseline_year)
  cat("Filtered data_past to year", baseline_year, ":", nrow(data_past), "rows\n")
}
stopifnot(
  "data_past contains duplicate pixel_ids after year filter — check baseline_year" =
    n_distinct(data_past$pixel_id) == nrow(data_past)
)

# Add pixel grouping variable (currently equal to pixel_id)
data_past <- data_past %>%
  mutate(pixel_id_group = pixel_id)

# Drop completely deforested pixels (remaining_treecover_share == 0 → def_shr set to NA).
# These pixels have no forest left and cannot be included in the simulation.
n_deforested <- sum(data_past$remaining_treecover_share == 0, na.rm = TRUE)
if (n_deforested > 0) cat("  Dropping", n_deforested, "fully deforested pixels\n")
data_past <- data_past %>%
  mutate(def_shr = if_else(remaining_treecover_share == 0, NA_real_, def_shr)) %>%
  drop_na(def_shr)

# Drop pixels without carbon density data (CF/CF_pix). These cannot produce a
# biomass estimate. Filter here so all downstream processing skips them entirely.
n_missing_cf <- sum(is.na(data_past$CF) | is.na(data_past$CF_pix))
if (n_missing_cf > 0) {
  warning(sprintf(
    "%d pixels (%.1f%%) have NA carbon density (CF/CF_pix) and will be excluded from simulation.",
    n_missing_cf, 100 * n_missing_cf / nrow(data_past)
  ))
  data_past <- data_past %>% filter(!is.na(CF), !is.na(CF_pix))
}

n_pixels <- nrow(data_past)
cat("After filtering:", n_pixels, "pixels remain for simulation\n")
stopifnot("data_past must have at least 1 pixel" = n_pixels > 0)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Prepare endo_input_data for revenue calculations -----
# Join yield and travel time from suit_harvest_country, and add 2020 crude oil price.
# This is the same preprocessing done in simulation_workflow.R.

# Extract 2020 oil price (single scalar used for all pixels)
oil_price_2020 <- prod_oil %>%
  filter(year == 2020) %>%
  summarise(crudeoil_real = first(crudeoil_real)) %>%
  pull(crudeoil_real)

cat("2020 crude oil price:", oil_price_2020, "\n")

n_endo_before_join <- nrow(endo_input_data)

endo_input_data <- endo_input_data %>%
  left_join(
    suit_harvest_country %>%
      select(pixel_id, crop, gaez_attainable_yield_2000,
             minimum_travel_time_to_large_or_medium_port_minutes),
    by = c("pixel_id", "crop")
  ) %>%
  mutate(
    crudeoil_real = oil_price_2020,
    yield = gaez_attainable_yield_2000
  )

# Verify the join did not add or drop rows (a mismatch would silently corrupt revenue calcs)
stopifnot(
  "Join with suit_harvest_country changed row count of endo_input_data" =
    nrow(endo_input_data) == n_endo_before_join
)
cat("endo_input_data after join:", nrow(endo_input_data), "rows (expected", n_endo_before_join, ")\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Extract model components -----
# Extract coefficients and fixed effects once here. Workers receive these as
# small scalars/functions (not the full model object).

cat("Extracting model components...\n")

model_family <- model$family
coefs        <- broom::tidy(model) %>% select(term, estimate)

# Revenue and tree cover coefficients from the logit model
alpha <- (coefs %>% filter(term == "revenue")                      %>% pull(estimate))[1]
beta  <- (coefs %>% filter(term == "remaining_treecover_share")    %>% pull(estimate))[1]

stopifnot("alpha coefficient not found in model" = !is.na(alpha))
stopifnot("beta coefficient not found in model"  = !is.na(beta))
cat("  alpha (revenue coef):", alpha, "\n")
cat("  beta  (treecover coef):", beta, "\n")

# Pixel-level fixed effects (gamma_df): one row per pixel_id
fixedeffects <- fixef(model, na.rm = FALSE)
gamma_df <- fe_to_tibble(fixedeffects$pixel_id) %>%
  rename(pixel_id = fe_id, pixel_fe = fe_val) %>%
  mutate(pixel_id = as.numeric(pixel_id))
cat("  gamma_df (pixel FEs):", nrow(gamma_df), "rows\n")

# Country-by-year fixed effects, averaged over a RECENT window to get a single per-country FE.
# Averaging over the FULL 2001-2020 sample reverted the forward baseline toward mid-sample
# deforestation rates, roughly halving baseline emissions relative to the 2020 jump-off (the forward
# baseline's first year predicted ~2.6 Gt CO2 vs ~6.0 Gt observed in 2020). Averaging instead over
# the recent high-deforestation window 2016-2020 keeps the baseline near recent observed rates
# (first-year prediction ~4.2 Gt). Countries with no year^country FE inside the window fall back to
# their full-sample mean so no pixel is left with an NA yc_fe (asserted below).
FE_WINDOW <- 2016:2020
delta_long <- data.frame(fixedeffects$`year^country`)
delta_long$year_country <- as.character(rownames(delta_long))
rownames(delta_long) <- NULL
delta_long <- delta_long %>%
  rename(fe_val = fixedeffects..year.country.) %>%
  separate(year_country, into = c("year", "country"), sep = "_", extra = "merge") %>%
  mutate(year = as.integer(year))

delta_full <- delta_long %>%                       # full-sample mean (fallback)
  group_by(country) %>%
  summarise(yc_fe_full = mean(fe_val, na.rm = TRUE), .groups = "drop")
delta_window <- delta_long %>%                     # recent-window mean (preferred)
  filter(year %in% FE_WINDOW) %>%
  group_by(country) %>%
  summarise(yc_fe_win = mean(fe_val, na.rm = TRUE), .groups = "drop")

delta_df <- delta_full %>%
  left_join(delta_window, by = "country") %>%
  mutate(yc_fe = dplyr::coalesce(yc_fe_win, yc_fe_full))
n_fallback <- sum(is.na(delta_df$yc_fe_win))
delta_df <- delta_df %>% select(country, yc_fe)
cat(sprintf("  delta_df (country FEs): %d countries; FE window %d-%d (%d countries fell back to full-sample mean)\n",
            nrow(delta_df), min(FE_WINDOW), max(FE_WINDOW), n_fallback))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Pre-compute revenues for all carbon price scenarios -----
#
# WHY HERE AND NOT IN THE WORKERS?
# update_revenues() requires endo_input_data, which at full scale is ~2.5 GB.
# Loading this into every parallel worker would crash the machine. By computing
# revenues centrally and storing one column per scenario in the chunk files,
# workers only need the small per-pixel revenue value — not the full crop data.
#
# We compute revenue for the baseline (cp=0, from data_past) and for each cp in CP_LIST.

cat("Pre-computing revenues for all carbon price scenarios...\n")

# Baseline revenue (cp=0) comes directly from data_past
n_pixels_with_rev <- n_distinct(data_past$pixel_id)
cat("  cp=0 (baseline): using revenue column from data_past,", n_pixels_with_rev, "pixels\n")

# Assert that final_prices_allcp contains equilibrium prices for every cp in CP_LIST.
# If this fails, the input data is missing price scenarios — update K_final_prices_allcp.Rdata.
missing_cprices <- setdiff(CP_LIST, unique(final_prices_allcp$CPrice))
if (length(missing_cprices) > 0) {
  stop(sprintf(
    "final_prices_allcp is missing endogenous crop prices for %d carbon price level(s): %s\n",
    length(missing_cprices), paste(missing_cprices, collapse = ", ")
  ))
}
cat("  All", length(CP_LIST), "carbon price levels present in final_prices_allcp\n")

# Revenue under each endogenous carbon price scenario
revenue_list <- list()
for (cp in CP_LIST) {
  crop_prices <- final_prices_allcp %>% filter(CPrice == cp)

  rev_cp <- update_revenues(endo_input_data, crop_prices) %>%
    rename(!!paste0("revenue_cp", cp) := revenue)

  # update_revenues covers all pixels in endo_input_data, which may include pixels
  # filtered out of data_past (e.g. fully deforested). The key check is that every
  # pixel remaining in data_past has a revenue value — missing pixels would produce
  # NA revenues and silently corrupt the simulation.
  missing_pixels <- setdiff(data_past$pixel_id, rev_cp$pixel_id)
  if (length(missing_pixels) > 0) {
    stop(sprintf("Revenue calc for cp=%d is missing %d pixels that are present in data_past",
                 cp, length(missing_pixels)))
  }

  revenue_list[[paste0("cp", cp)]] <- rev_cp
  cat("  cp =", cp, ": revenue computed for", nrow(rev_cp), "pixels\n")
}

# Join all revenue columns into a single wide table (one row per pixel_id).
# Then filter to data_past pixels: endo_input_data covers ~2M pixels while data_past
# retains only ~1.45M after dropping fully deforested pixels. The extra rows are not
# needed and would bloat the chunk files.
revenue_wide <- Reduce(function(x, y) left_join(x, y, by = "pixel_id"), revenue_list) %>%
  filter(pixel_id %in% data_past$pixel_id)

stopifnot(
  "Revenue wide table has wrong number of rows after join" =
    nrow(revenue_wide) == n_pixels
)
cat("revenue_wide assembled:", nrow(revenue_wide), "pixels x", ncol(revenue_wide), "cols\n")

# Free the two largest objects now that revenues are computed
rm(endo_input_data, suit_harvest_country, prod_oil)
gc()
cat("Freed endo_input_data, suit_harvest_country, prod_oil from RAM\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 7: Pre-compute asymmetric-information fixed effects -----
#
# The "asymmetric information" (ai) baseline scenario models a policymaker who
# doesn't know individual pixel fixed effects and instead uses the regional median FE.
#
# WHY BEFORE CHUNKING?
# redraw_fes() with "median" computes the median pixel_fe WITHIN each region.
# This requires ALL pixels across the full dataset to be present — if we chunked
# first and computed medians within chunks, each chunk would get a different (wrong)
# median. So we must compute ai_pixel_fe on the full data before splitting.

cat("Computing asymmetric-info fixed effects (regional medians)...\n")

group_idx <- data_past %>%
  select(pixel_id, region) %>%
  distinct() %>%
  arrange(pixel_id)

gamma_df_ai <- redraw_fes(gamma_df, group_idx, "median") %>%
  rename(ai_pixel_fe = pixel_fe)

# gamma_df_ai covers all pixels in the model (same as gamma_df), not just the sample.
# The left_join in Section 8 will filter to sample pixels. Verify:
# (a) All sample pixels are present in gamma_df_ai (so the join produces no NAs)
# (b) Within the sample pixels, every region gets a single ai_pixel_fe (median) value
sample_ai <- gamma_df_ai %>%
  filter(pixel_id %in% data_past$pixel_id)

stopifnot(
  "Not all sample pixels found in gamma_df_ai (model FEs may be incomplete)" =
    nrow(sample_ai) == n_pixels
)
stopifnot(
  "NA values in ai_pixel_fe for sample pixels" =
    !anyNA(sample_ai$ai_pixel_fe)
)
# Verify each region in the sample gets a uniform median FE
region_fe_check <- sample_ai %>%
  left_join(data_past %>% select(pixel_id, region), by = "pixel_id") %>%
  group_by(region) %>%
  summarise(n_unique_fe = n_distinct(ai_pixel_fe), .groups = "drop")
stopifnot(
  "Pixels within a region must all share one ai_pixel_fe value (median)" =
    all(region_fe_check$n_unique_fe == 1L)
)
n_unique_regions <- n_distinct(data_past$region)
cat("  ai_pixel_fe computed:", n_distinct(sample_ai$ai_pixel_fe),
    "unique values across", n_unique_regions, "regions in sample\n") #CL: note, it seems some regions justhappen to have the same FE


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 8: Assemble the full simulation input table -----
# Merge all preprocessed components into one wide table: one row per pixel.
# Columns: pixel state variables, FEs (fi and ai), country FE, and one revenue
# column per carbon price scenario. This is the complete input for workers.

cat("Assembling simulation input table...\n")

# Join country FE to data_past via country column
country_fe_lookup <- data_past %>%
  select(pixel_id, country) %>%
  distinct() %>%
  left_join(delta_df, by = "country")

sim_input <- data_past %>%
  select(pixel_id, lag_remaining_treecover, remaining_treecover_share,
         pixel_count, CF, CF_pix, revenue) %>%
  rename(revenue_cp0 = revenue) %>%
  left_join(gamma_df,         by = "pixel_id") %>%  # adds pixel_fe  (fi scenario)
  left_join(gamma_df_ai,      by = "pixel_id") %>%  # adds ai_pixel_fe (ai scenario)
  left_join(country_fe_lookup, by = "pixel_id") %>%  # adds yc_fe
  left_join(revenue_wide,     by = "pixel_id")       # adds revenue_cpXX columns

# --- Critical integrity assertions before chunking ---
stopifnot("sim_input has wrong number of rows"    = nrow(sim_input) == n_pixels)
stopifnot("NA values in pixel_fe"                 = !anyNA(sim_input$pixel_fe))
stopifnot("NA values in ai_pixel_fe"              = !anyNA(sim_input$ai_pixel_fe))
stopifnot("NA values in yc_fe"                    = !anyNA(sim_input$yc_fe))
stopifnot("NA values in lag_remaining_treecover"  = !anyNA(sim_input$lag_remaining_treecover))
stopifnot("NA values in pixel_count"              = !anyNA(sim_input$pixel_count))
stopifnot("NA values in CF"                       = !anyNA(sim_input$CF))
stopifnot("NA values in CF_pix"                   = !anyNA(sim_input$CF_pix))

# Verify all revenue columns are present
expected_rev_cols <- c("revenue_cp0", paste0("revenue_cp", CP_LIST))
missing_rev_cols  <- setdiff(expected_rev_cols, names(sim_input))
stopifnot("Missing revenue columns in sim_input" = length(missing_rev_cols) == 0)

cat("sim_input assembled:", nrow(sim_input), "rows,", ncol(sim_input), "cols\n")
cat("  Columns:", paste(names(sim_input), collapse = ", "), "\n")

# --- Pixel-level static attributes (not time-varying, not scenario-indexed) ---
pixel_attrs <- data_past %>%
  mutate(biomass_2020 = remaining_treecover_share * CF_pix * pixel_count) %>%
  select(pixel_id, country, biomass_2020)

readr::write_csv(pixel_attrs, file.path(output_dir, "grid_smry.csv"))
cat("pixel_attributes.csv saved:", nrow(pixel_attrs), "pixels\n")

# Free fixed effect tables and revenue wide (now merged into sim_input)
rm(gamma_df, gamma_df_ai, revenue_wide, delta_df, country_fe_lookup, data_past)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 9: Split into chunks and write parquet files -----
#
# Strategy: sort by pixel_id for reproducibility, then assign sequential chunk IDs.
# Write each chunk as a SEPARATE parquet file — do not use write_dataset() with
# partitioning, as that creates a directory structure where workers could
# accidentally open the whole dataset and load all data into memory.

# Delete any existing chunk files and manifest before writing new ones.
# This prevents stale chunks from a prior run (different dataset, N_CHUNKS, or pixel
# filters) from being silently mixed with fresh chunks in 02_run_simulations.R.
old_files <- list.files(chunks_dir, pattern = "\\.(parquet|rds)$", full.names = TRUE)
if (length(old_files) > 0) {
  file.remove(old_files)
  cat("Removed", length(old_files), "files from prior run in", chunks_dir, "\n")
}

cat("Splitting into", N_CHUNKS, "chunks and writing parquet files...\n")
cat("  Output directory:", chunks_dir, "\n")

# Assign chunk IDs (pixels per chunk = ceiling(n_pixels / N_CHUNKS))
chunk_size <- ceiling(n_pixels / N_CHUNKS)
sim_input <- sim_input %>%
  ungroup() %>%
  arrange(pixel_id) %>%
  mutate(chunk_id = ceiling(row_number() / chunk_size))

actual_n_chunks <- n_distinct(sim_input$chunk_id)
cat("  Target chunks:", N_CHUNKS, "| Actual chunks:", actual_n_chunks,
    "| Rows per chunk: ~", chunk_size, "\n")

# Write each chunk as its own parquet file
manifest <- list()

for (ch in sort(unique(sim_input$chunk_id))) {
  chunk_data <- sim_input %>%
    filter(chunk_id == ch) %>%
    select(-chunk_id)  # chunk_id not needed by workers

  file_path <- file.path(chunks_dir, sprintf("chunk_%03d.parquet", ch))
  arrow::write_parquet(chunk_data, file_path)

  manifest[[ch]] <- list(
    chunk_id  = ch,
    file_path = file_path,
    n_rows    = nrow(chunk_data),
    min_pixel = min(chunk_data$pixel_id),
    max_pixel = max(chunk_data$pixel_id),
    file_size = file.size(file_path)
  )

  cat("  Wrote chunk", ch, "/", actual_n_chunks, "—",
      nrow(chunk_data), "rows,",
      round(file.size(file_path) / 1024^2, 1), "MB",
      "(", file_path, ")\n")
}

total_size_mb <- sum(sapply(manifest, `[[`, "file_size")) / 1024^2
cat("  All chunks written. Total size:", round(total_size_mb, 1), "MB across",
    actual_n_chunks, "files\n")

rm(sim_input)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 10: Validate chunk integrity -----
#
# This is the critical check that was missing in previous attempts. We re-read
# ONLY the pixel_id column from every chunk file and verify:
#   (a) No pixel_id appears in more than one file (no chunk overlap)
#   (b) Total pixels across all files equals the original n_pixels (no data loss)
#   (c) Row counts match the manifest (no file corruption)
#
# This runs quickly since we only read the small pixel_id column, not the full data.

cat("Validating chunk integrity (re-reading pixel_id columns)...\n")

all_pixel_ids <- lapply(manifest, function(m) {
  arrow::read_parquet(m$file_path, col_select = "pixel_id")$pixel_id
})

flat_ids <- unlist(all_pixel_ids)

stopifnot(
  "CHUNK OVERLAP DETECTED: same pixel_id appears in multiple chunk files" =
    length(flat_ids) == length(unique(flat_ids))
)
stopifnot(
  "MISSING PIXELS: total pixels across chunk files does not match original n_pixels" =
    length(flat_ids) == n_pixels
)

for (i in seq_along(manifest)) {
  actual_rows <- length(all_pixel_ids[[i]])
  if (actual_rows != manifest[[i]]$n_rows) {
    stop(sprintf("Chunk %d: file has %d rows but manifest records %d",
                 i, actual_rows, manifest[[i]]$n_rows))
  }
}

cat("CHUNK VALIDATION PASSED: ", actual_n_chunks, "chunks, ",
    length(flat_ids), "total pixels, no overlaps\n", sep = "")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 11: Save manifest and model parameters -----
# Workers need the manifest to find their chunk file, and model_params for the simulation.

manifest_df <- dplyr::bind_rows(manifest)
saveRDS(manifest_df, file.path(chunks_dir, "chunk_manifest.rds"))
cat("Manifest saved:", nrow(manifest_df), "chunks\n")

# Save model parameters as a small object (~1 KB) that workers can read without
# loading the full model. This avoids serializing the model object to each worker.
model_params <- list(
  alpha   = alpha,
  beta    = beta,
  linkinv = model_family$linkinv,
  cp_list = CP_LIST,
  years   = YEARS,
  nyears  = NYEARS,
  iota    = iota,
  scc     = scc,
  scc_dly = scc_dly
)
saveRDS(model_params, file.path(chunks_dir, "model_params.rds"))
cat("Model params saved (alpha =", alpha, ", beta =", beta, ")\n")

cat("\n=== 01_prepare_data.R COMPLETE ===\n")
cat("Chunks directory:", chunks_dir, "\n")
cat("Chunks written:  ", actual_n_chunks, "\n")
cat("Total pixels:    ", n_pixels, "\n")
cat("Next step: run 02_run_simulations.R\n")

