#################################
# 00_generate_sample_data.R
# "Generate sample dataset as an exact subset of the full data"
# author: "Robert Heilmayr"
#################################
#
# PURPOSE:
# Creates a sample dataset by subsetting the full input files to N_SAMPLE pixels.
# Because the sample is a strict subset of the full data (same pixel values, same
# prices, same model), running the pipeline on the sample should produce results
# that match the full run for those pixels exactly — making cross-run comparisons
# a reliable test of code correctness rather than a confound of data differences.
#
# Run this script once (or whenever the full input data changes) to refresh the
# sample directory used by 01_prepare_data.R when USE_SAMPLE_DATA = TRUE.
#
# PREREQUISITES:
# Full input data must exist at full_data_dir below.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

source(here::here("r/land_use_model/packages.R"))
options(scipen = 999)
set.seed(93106)

N_SAMPLE <- 200  # number of pixels to include in the sample

# Extra grid cells to force into the sample beyond the random N_SAMPLE draw.
# These exist in the full data but may not be picked by the random sample; listing
# them here guarantees inclusion (e.g. cells that break downstream scripts).
ADDITIONAL_SAMPLE_PIXELS <- c(1269571)

# Define working directories
wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#CL
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')

full_data_dir   <- file.path(wdir, "1_input/full")
sample_data_dir <- file.path(wdir, "1_input/smpl")

dir.create(sample_data_dir, recursive = TRUE, showWarnings = FALSE)
cat("Full data directory:  ", full_data_dir, "\n")
cat("Sample data directory:", sample_data_dir, "\n")
cat("Target sample size:   ", N_SAMPLE, "pixels\n\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load full data_past and select sample pixels -----

cat("Loading K_data_past.Rdata...\n")
load(file.path(full_data_dir, "K_data_past.Rdata")) ##CL: K_data_past.Rdata is not synced - I added it April 22, 2026

# Filter to 2020 baseline year before sampling so we operate on one row per pixel
if ("year" %in% names(data_past) && dplyr::n_distinct(data_past$year) > 1) {
  data_past <- data_past %>% dplyr::filter(year == 2020L)
  cat("  Filtered to year 2020:", nrow(data_past), "pixels\n")
}

# Apply the same pre-filters as 01_prepare_data.R so every sampled pixel is valid
data_past <- data_past %>%
  dplyr::mutate(def_shr = dplyr::if_else(remaining_treecover_share == 0, NA_real_, def_shr)) %>%
  tidyr::drop_na(def_shr) %>%
  dplyr::filter(!is.na(CF), !is.na(CF_pix))

cat("  Valid pixels after pre-filters:", nrow(data_past), "\n")
stopifnot("Fewer valid pixels than N_SAMPLE" = nrow(data_past) >= N_SAMPLE)

# Sample pixel_ids
sample_pixel_ids <- sample(data_past$pixel_id, N_SAMPLE)
cat("  Selected", length(sample_pixel_ids), "pixels\n")

# Force-include the additional cells. Warn if any aren't valid pixels in
# data_past (post-filter) — those can't be added and likely need investigation.
missing_extra <- setdiff(ADDITIONAL_SAMPLE_PIXELS, data_past$pixel_id)
if (length(missing_extra) > 0) {
  warning(sprintf(
    "ADDITIONAL_SAMPLE_PIXELS not present in valid data_past (dropped by year/def_shr/CF filters): %s",
    paste(missing_extra, collapse = ", ")
  ))
}
sample_pixel_ids <- union(sample_pixel_ids, ADDITIONAL_SAMPLE_PIXELS)
cat("  Final sample size:", length(sample_pixel_ids), "pixels (",
    length(setdiff(sample_pixel_ids, ADDITIONAL_SAMPLE_PIXELS)), "random +",
    length(ADDITIONAL_SAMPLE_PIXELS), "forced )\n\n")

# Save filtered data_past
data_past_sample <- data_past %>% dplyr::filter(pixel_id %in% sample_pixel_ids)
saveRDS(data_past_sample, file.path(sample_data_dir, "K_data_past.rds"))
cat("Saved K_data_past.rds:", nrow(data_past_sample), "rows\n")
rm(data_past, data_past_sample)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Subset and save endo_input_data -----

cat("Loading K_endo_input_data.Rdata...\n")
load(file.path(full_data_dir, "K_endo_input_data.Rdata")) ##CL: K_endo_input_data.Rdata is not synced - I added it April 22, 2026
cat("  Loaded:", nrow(endo_input_data), "rows\n")

endo_sample <- endo_input_data %>% dplyr::filter(pixel_id %in% sample_pixel_ids)
saveRDS(endo_sample, file.path(sample_data_dir, "endo_input_data.rds"))
cat("Saved endo_input_data.rds:", nrow(endo_sample), "rows\n") #CL: TODO: double check that saved version from Robert also has 3400 rows
rm(endo_input_data, endo_sample)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Subset and save suit_harvest_country -----

cat("Loading B_suit_harvest_country_full.Rdata...\n")
load(file.path(full_data_dir, "B_suit_harvest_country_full.Rdata"))
cat("  Loaded:", nrow(suit_harvest_country), "rows\n")

suit_sample <- suit_harvest_country %>% dplyr::filter(pixel_id %in% sample_pixel_ids)
saveRDS(suit_sample, file.path(sample_data_dir, "suit_harvest_country.rds"))
cat("Saved suit_harvest_country.rds:", nrow(suit_sample), "rows\n")
rm(suit_harvest_country, suit_sample)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Copy pixel-independent files as-is -----
# prod_oil, model, and final_prices_allcp are not pixel-specific — copy directly.

cat("Loading and saving prod_oil...\n")
load(file.path(full_data_dir, "K_prod_oil_full.Rdata"))
saveRDS(prod_oil, file.path(sample_data_dir, "prod_oil.rds"))
cat("Saved prod_oil.rds:", nrow(prod_oil), "rows\n")
rm(prod_oil)

cat("Copying model.rds...\n")
file.copy(
  file.path(full_data_dir, "model.rds"), #CL April 2026: not in these folders - added from models folder
  file.path(sample_data_dir, "model.rds"),
  overwrite = TRUE
)
cat("Saved model.rds\n")

cat("Copying K_final_prices_allcp_150y.Rdata...\n")
file.copy(
  file.path(full_data_dir, "K_final_prices_allcp_150y.Rdata"),
  file.path(sample_data_dir, "K_final_prices_allcp_150y.Rdata"),
  overwrite = TRUE
)
cat("Saved K_final_prices_allcp_150y.Rdata\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 6: Save sample pixel_ids for reference -----

saveRDS(sample_pixel_ids, file.path(sample_data_dir, "sample_pixel_ids.rds"))
cat("\nSample pixel_ids saved to sample_pixel_ids.rds\n")

cat("\n=== 00_generate_sample_data.R COMPLETE ===\n")
cat("Sample directory:", sample_data_dir, "\n")
cat("Files written: K_data_past.rds, endo_input_data.rds, suit_harvest_country.rds,\n")
cat("               prod_oil.rds, model.rds, K_final_prices_allcp_150y.Rdata,\n")
cat("               sample_pixel_ids.rds\n")
cat("Next step: run 01_prepare_data.R with USE_SAMPLE_DATA = TRUE\n")
  