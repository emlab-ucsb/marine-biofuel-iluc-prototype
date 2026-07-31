#################################
# 05_validate_results.R
# "Diagnostic checks on raw simulation output chunks"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE: Validate results from parallelized workflow to ensure results match a series of logic checks.
#
# TODO: Currently worthless - no checks run. Needs to be fleshed out.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

USE_SAMPLE_DATA <- FALSE

source(here::here("r/land_use_model/packages.R"))
select <- dplyr::select
options(scipen = 999)

wdir        <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
processing_dir <- file.path(wdir, "2_processing")
data_subdir    <- if (USE_SAMPLE_DATA) "smpl" else "full"
results_dir    <- file.path(processing_dir, data_subdir, "results")

# Only check this many chunks (NULL = all chunks)
MAX_CHUNKS <- NULL

N_WORKERS <- max(1L, min(parallel::detectCores() - 2L, 10L))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load manifest -----

manifest_path <- file.path(results_dir, "results_manifest.rds")
if (!file.exists(manifest_path)) {
  stop("results_manifest.rds not found at: ", manifest_path,
       "\nPlease run 02_run_simulations.R first.")
}
results_manifest <- readRDS(manifest_path)
result_files <- results_manifest$file_path

missing_files <- result_files[!file.exists(result_files)]
if (length(missing_files) > 0) {
  stop("Manifest lists files that do not exist:\n", paste(missing_files, collapse = "\n"))
}

if (!is.null(MAX_CHUNKS)) result_files <- head(result_files, MAX_CHUNKS)
cat("Checking", length(result_files), "chunk(s)...\n\n")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Per-chunk diagnostic function -----

check_results <- function(chunk_file) {
  # Read only baseline rows and needed columns via Arrow pushdown
  ds <- arrow::open_dataset(chunk_file)


  }


### These are some of the checks we've had in the past. Need to:
### 1. Update to run these on the batched data
### 2. Expand to build 100% confidence in final results.
### e.g. possible checks to add: 
# potential revenues are constant across years within a given pixel_id and cprice
# avoided emissions if FI scenario are strictly positive
# emissions delays and permanent abatement converge over 150 year time span


# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Run checks -----

# # NOTE: Probably should develop additional checks

# # Test: When cprice==scc, benefits should equal payments under full information
# violations_benefit_payment <- impacts_df %>%
#   filter(cprice == scc & abs(pv_bnft_dly - pv_pymnt) > 1e-6)
# if (nrow(violations_benefit_payment) > 0) {
#   sample_violations_benefit_payment <- violations_benefit_payment %>%
#     select(scn, pixel_id, pv_bnft_dly, pv_pymnt) %>%
#     slice_head(n = 10)
#   stop(sprintf(
#     "Assertion failed: %d rows have benefits not equal to payments when cprice == scc. Example rows:\n%s",
#     nrow(violations_benefit_payment),
#     paste(capture.output(print(sample_violations_benefit_payment)), collapse = "\n")
#   ))
# }

# # Test: emissions delays must be non-negative
# violations <- abate_df %>%
#   filter(dly_co2 < 0)
# if (nrow(violations) > 0) {
#   sample_violations <- violations %>%
#     select(scn, pixel_id, year, dly_co2, biomass, biomass_bl) %>%
#     slice_head(n = 10)
#   stop(sprintf(
#     "Assertion failed: %d rows have negative emissions. Example rows:\n%s",
#     nrow(violations),
#     paste(capture.output(print(sample_violations)), collapse = "\n")
#   ))
# }

# # Test: opportunity costs must be less than full information payment offer
# violations_oppcost <- impacts_df %>%
#   filter(opp_cost > pv_pymnt)
# if (nrow(violations_oppcost) > 0) {
#   sample_violations_oppcost <- violations_oppcost %>%
#     select(scn, pixel_id, opp_cost, pv_pymnt) %>%
#     slice_head(n = 10)
#   stop(sprintf(
#     "Assertion failed: %d rows have opportunity cost greater than payment offer. Example rows:\n%s",
#     nrow(violations_oppcost),
#     paste(capture.output(print(sample_violations_oppcost)), collapse = "\n")
#   ))
# }

# # Test: Confirm that endogenous prices reduce emissions delays relative to exogenous prices - Currently fails, is this due to negative revenues?
# violations_endog_exog <- impacts_df %>%
#   filter(crp_price == "endog") %>%
#   left_join(
#     impacts_df %>%
#       filter(crp_price == "exog") %>%
#       select(pixel_id, cprice, dly_co2_exog = dly_co2),
#     by = c("pixel_id","cprice")
#   ) %>%
#   filter(dly_co2 > dly_co2_exog)
# if (nrow(violations_endog_exog) > 0) {
#   sample_violations_endog_exog <- violations_endog_exog %>%
#     select(scn, pixel_id, dly_co2, dly_co2_exog) %>%
#     slice_head(n = 10)
#   stop(sprintf(
#     "Assertion failed: %d grid cells have higher emissions delays under endogenous prices than exogenous prices. Example rows:\n%s",
#     nrow(violations_endog_exog),
#     paste(capture.output(print(sample_violations_endog_exog)), collapse = "\n")
#   ))
# }

# print("All checks passed.")