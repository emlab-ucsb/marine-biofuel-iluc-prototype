#################################
# 03_summarise_results.R
# "Country and gridded summaries of forest carbon by price scenario" (not part of upstream)
#################################
#
# PURPOSE:
# Reduces the per-pixel results of 02_run_simulations.R to two small files for report.qmd:
# - data/derived/land_use_country_results.csv.gz: country x scenario x year totals of forest
#   carbon (t CO2) and forest area (ha).
# - data/derived/land_use_grid_loss.csv.gz: for mapping, each MAP_RES_DEG grid square's 2020
#   forest carbon and its additional forest carbon loss relative to BAU in the final model year,
#   one column per scenario (t CO2).
#
# UNITS (upstream conventions):
# - biomass (02 output) = remaining_treecover_share x CF_pix x pixel_count, in t CO2. Upstream
#   sums it to Gt CO2 (08_manuscript_values.R) and charges a $/t CO2 carbon price against CF.
# - CF is t CO2 per hectare of tree cover: upstream's ha_per_pixel = CF_pix / CF
#   (simulation_functions.R). So forest area (ha) = biomass / CF.
#
# Each result file is paired with the chunk file it was simulated from, which supplies each
# pixel's country and CF, so no large table is ever held in memory. Pixel coordinates come from
# grid_smry.csv, written by 01_prepare_data.R.

USE_SAMPLE_DATA <- FALSE

# Grid-square size (degrees) for the map file. Model cells are 1/12 degree.
MAP_RES_DEG <- 0.5

source(here::here("r/land_use_model/paths.R"))

data_subdir <- if (USE_SAMPLE_DATA) "smpl" else "full"
chunks_dir  <- file.path(wdir, "2_processing", data_subdir, "chunks")
results_dir <- file.path(wdir, "2_processing", data_subdir, "results")
output_dir  <- file.path(wdir, "3_output", data_subdir)
suffix      <- if (USE_SAMPLE_DATA) "_smpl" else ""
country_file <- here::here("data/derived", paste0("land_use_country_results", suffix, ".csv.gz"))
grid_file    <- here::here("data/derived", paste0("land_use_grid_loss", suffix, ".csv.gz"))

chunk_manifest   <- readRDS(file.path(chunks_dir, "chunk_manifest.rds"))
results_manifest <- readRDS(file.path(results_dir, "results_manifest.rds"))

# Pair each result file with its chunk file (results_chunk_001.parquet <- chunk_001.parquet).
pairs <- data.frame(result_file = results_manifest$file_path)
pairs$chunk_file <- file.path(chunks_dir, sub("^results_", "", basename(pairs$result_file)))
stopifnot(
  "Every result file needs its chunk file" = all(file.exists(pairs$chunk_file)),
  "Result and chunk file counts differ"   = nrow(pairs) == nrow(chunk_manifest)
)

summarise_pair <- function(result_file, chunk_file) {
  # Single-threaded arrow reads inside workers: upstream (03_aggregate_results.R) found
  # multi-threaded reads in multisession workers can silently corrupt rows on macOS.
  arrow::set_cpu_count(1L)
  options(arrow.use_threads = FALSE)

  px  <- data.table::as.data.table(
    arrow::read_parquet(chunk_file, col_select = c("pixel_id", "country", "CF"))
  )
  res <- data.table::as.data.table(arrow::read_parquet(result_file))
  n_res <- nrow(res)

  res <- px[res, on = "pixel_id"]
  if (nrow(res) != n_res || anyNA(res$country) || anyNA(res$CF)) {
    stop("Join of ", basename(result_file), " with ", basename(chunk_file),
         " changed row count or left pixels without country/CF")
  }

  country <- res[, .(forest_carbon_tco2 = sum(biomass), forest_area_ha = sum(biomass / CF)),
                 by = .(country, scenario = scn, year)]

  # Per-pixel additional loss vs BAU in the final year (positive = more loss than BAU).
  final <- res[year == max(year), .(pixel_id, scenario = scn, biomass)]
  bau   <- final[scenario == "bau", .(pixel_id, biomass_bau = biomass)]
  pixel_loss <- bau[final[scenario != "bau"], on = "pixel_id"][
    , .(pixel_id, scenario, loss_tco2 = biomass_bau - biomass)]

  list(country = country, pixel_loss = pixel_loss)
}

n_workers <- max(1L, min(8L, parallel::detectCores() - 1L))
cat("Summarising", nrow(pairs), "result files with", n_workers, "workers...\n")
future::plan(future::multisession, workers = n_workers)
partials <- furrr::future_map2(
  pairs$result_file, pairs$chunk_file, summarise_pair,
  .options = furrr::furrr_options(seed = NULL)
)
future::plan(future::sequential)

# --- Country totals: countries span chunks, so sum the per-chunk partial totals ---
country_results <- data.table::rbindlist(lapply(partials, `[[`, "country"))[
  , .(forest_carbon_tco2 = sum(forest_carbon_tco2), forest_area_ha = sum(forest_area_ha)),
  by = .(country, scenario, year)
][order(country, scenario, year)]

# Check: BAU 2020 forest carbon must equal 01's independently written per-pixel 2020 biomass.
grid_smry <- data.table::as.data.table(
  readr::read_csv(file.path(output_dir, "grid_smry.csv"), show_col_types = FALSE)
)
bau_2020 <- sum(country_results[scenario == "bau" & year == 2020L, forest_carbon_tco2])
stopifnot(
  "BAU 2020 forest carbon does not match grid_smry.csv biomass_2020" =
    isTRUE(all.equal(bau_2020, sum(grid_smry$biomass_2020), tolerance = 1e-10)),
  "Every country x scenario needs every year" =
    nrow(country_results) == data.table::uniqueN(country_results$country) *
      data.table::uniqueN(country_results$scenario) * data.table::uniqueN(country_results$year)
)

# --- Grid totals for mapping ---
stopifnot("grid_smry.csv needs lat/lon (re-run 01_prepare_data.R)" =
            all(c("lat", "lon") %in% names(grid_smry)))
pixel_loss <- data.table::rbindlist(lapply(partials, `[[`, "pixel_loss"))
grid_smry[, `:=`(
  lon_sq = floor(lon / MAP_RES_DEG) * MAP_RES_DEG + MAP_RES_DEG / 2,  # square centre
  lat_sq = floor(lat / MAP_RES_DEG) * MAP_RES_DEG + MAP_RES_DEG / 2
)]
pixel_loss <- grid_smry[, .(pixel_id, lon_sq, lat_sq)][pixel_loss, on = "pixel_id"]
stopifnot("Pixels in results without coordinates" = !anyNA(pixel_loss$lon_sq))

grid_loss <- data.table::dcast(
  pixel_loss[, .(loss_tco2 = sum(loss_tco2)), by = .(lon_sq, lat_sq, scenario)],
  lon_sq + lat_sq ~ paste0("loss_", scenario, "_tco2"), value.var = "loss_tco2"
)
grid_forest <- grid_smry[, .(forest_carbon_2020_tco2 = sum(biomass_2020)), by = .(lon_sq, lat_sq)]
grid_loss <- grid_loss[grid_forest, on = c("lon_sq", "lat_sq")][order(lat_sq, lon_sq)]
data.table::setnames(grid_loss, c("lon_sq", "lat_sq"), c("lon", "lat"))

# Check: each scenario's gridded loss sums to its country-level loss in the final year.
final_year <- max(country_results$year)
for (s in setdiff(unique(country_results$scenario), "bau")) {
  country_loss <- sum(country_results[scenario == "bau" & year == final_year, forest_carbon_tco2]) -
    sum(country_results[scenario == s & year == final_year, forest_carbon_tco2])
  stopifnot(isTRUE(all.equal(sum(grid_loss[[paste0("loss_", s, "_tco2")]]), country_loss,
                             tolerance = 1e-8)))
}

readr::write_csv(country_results, country_file)
readr::write_csv(grid_loss, grid_file)
cat("Wrote", nrow(country_results), "rows (",
    data.table::uniqueN(country_results$country), "countries x",
    data.table::uniqueN(country_results$scenario), "scenarios x",
    data.table::uniqueN(country_results$year), "years ) to", country_file, "\n")
cat("Wrote", nrow(grid_loss), MAP_RES_DEG, "degree grid squares to", grid_file, "\n")
