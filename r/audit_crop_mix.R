#################################
# audit_crop_mix.R
# Evidence for report.qmd Appendix C: the land-use model's crop mix (harvested_share_gaez)
#################################
#
# Reads the land-use model's inputs (read-only, via r/land_use_model/paths.R) and this repo's model
# run (land_use_model_runs/, from r/land_use_model/run_pipeline.R), and writes four small files to
# data/derived/ for the report:
# - crop_mix_grid.csv.gz: per 0.5-degree square, the palm and soybean shares of the crop mix the model
#   uses (harvested_share_gaez, set per GAEZ zone) and of M3 harvested area in the same cells.
# - crop_mix_countries.csv: per country, cells, distinct palm shares, maximum palm share, and cells
#   where palm is more than 5% of the crop mix.
# - crop_mix_regions.csv: palm and soybean shares (model and M3) and the cropland share of forest
#   loss in four well-known palm and soy frontiers.
# - crop_mix_numbers.csv: headline counts, and the model's first-year response to a +1% palm price in
#   Indonesia and Malaysia next to the response implied by Chen et al. (2026).
#
# Nothing here changes the model; it only reads its inputs and outputs. Peak memory is about that of
# 01_prepare_data.R.

source(here::here("r/land_use_model/paths.R"))
input_dir  <- file.path(input_wdir, "1_input/full")
chunks_dir <- file.path(wdir, "2_processing/full/chunks")
output_dir <- file.path(wdir, "3_output/full")
derived    <- here::here("data/derived")

MAP_RES_DEG <- 0.5

# Chen et al. (2026), Indonesia and Malaysia, 2002-2018 (pp. 5 and S5): +$1,000/ha of annual
# attainable palm revenue (price x GAEZ attainable yield) adds 9.3 ha of forest-to-palm conversion a
# year per 9 km cell; mean attainable revenue $1,933/ha; 32,394 cells.
CHEN_HA_PER_1000 <- 9.3
CHEN_MEAN_REVENUE <- 1933
CHEN_CELLS <- 32394

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Crop mix per simulated cell: the zone-level shares the model uses, and M3 harvested areas -----

# Each simulated cell's country and coordinates (01_prepare_data.R).
grid <- data.table::fread(file.path(output_dir, "grid_smry.csv"), select = c("pixel_id", "country", "lat", "lon"))

# suit_harvest_country: each cell's GAEZ zone (gaez, e.g. "54_Europe"), the zone-level crop share
# (harvested_share_gaez) and the cell's own M3 harvested area by crop (m3_harvested_area_ha).
load(file.path(input_dir, "B_suit_harvest_country_full.Rdata"))
shc <- data.table::as.data.table(suit_harvest_country[
  suit_harvest_country$pixel_id %in% grid$pixel_id,
  c("pixel_id", "crop", "gaez", "harvested_share_gaez", "m3_harvested_area_ha")
])
rm(suit_harvest_country)
gc()
m3 <- shc[, .(
  gaez = gaez[1],
  n_gaez = data.table::uniqueN(gaez),
  m3_area_all = sum(m3_harvested_area_ha, na.rm = TRUE),
  m3_area_palm = sum(m3_harvested_area_ha[crop == "Oil, palm"], na.rm = TRUE),
  m3_area_soy = sum(m3_harvested_area_ha[crop == "Soybeans"], na.rm = TRUE),
  shc_palm = harvested_share_gaez[crop == "Oil, palm"],
  shc_soy = harvested_share_gaez[crop == "Soybeans"]
), by = pixel_id]
stopifnot("A cell has more than one GAEZ zone" = all(m3$n_gaez == 1))
rm(shc)

# endo_input_data: the crop shares, yields and cropland share 01_prepare_data.R and update_revenues() use.
load(file.path(input_dir, "K_endo_input_data.Rdata"))
endo <- data.table::as.data.table(endo_input_data[endo_input_data$crop %in% c("Oil, palm", "Soybeans"), ])
rm(endo_input_data)
gc()
cells <- data.table::dcast(
  endo,
  pixel_id + ha_pixel + def_for_ag_share + cumulative_forest_loss_pixel_count ~ crop,
  value.var = c("harvested_share_gaez", "m3_yield_ton_per_ha")
)
data.table::setnames(cells, c("pixel_id", "ha_pixel", "ag", "loss_px", "palm", "soy", "y_palm", "y_soy"))
supply_base <- endo[, .(supply_t = supply_crop_2020[1]), by = crop] # one value per crop
rm(endo)

cells <- cells[grid, on = "pixel_id", nomatch = NULL]
cells <- m3[cells, on = "pixel_id"]
stopifnot(
  "Simulated cells missing from the crop data" = nrow(cells) == nrow(grid),
  "Duplicate cells after joins" = !anyDuplicated(cells$pixel_id),
  "endo_input_data and suit_harvest_country disagree on harvested_share_gaez" =
    isTRUE(all.equal(cells$palm, cells$shc_palm)) && isTRUE(all.equal(cells$soy, cells$shc_soy))
)
cells[, loss_ha := loss_px * ha_pixel]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Map, country and region summaries -----

cells[, `:=`(
  lon_sq = floor(lon / MAP_RES_DEG) * MAP_RES_DEG + MAP_RES_DEG / 2,
  lat_sq = floor(lat / MAP_RES_DEG) * MAP_RES_DEG + MAP_RES_DEG / 2
)]
# Model: mean zone-level share. M3: the square's palm (soy) harvested area over all 17 crops' harvested
# area, NA where M3 has no cropland in the square's simulated cells.
crop_grid <- cells[, .(
  palm_share_model = mean(palm), soy_share_model = mean(soy),
  palm_share_m3 = if (sum(m3_area_all) > 0) sum(m3_area_palm) / sum(m3_area_all) else NA_real_,
  soy_share_m3 = if (sum(m3_area_all) > 0) sum(m3_area_soy) / sum(m3_area_all) else NA_real_
), by = .(lon = lon_sq, lat = lat_sq)]
data.table::fwrite(crop_grid[order(lat, lon)], file.path(derived, "crop_mix_grid.csv.gz"))

countries <- cells[, .(
  cells = .N,
  distinct_palm_shares = data.table::uniqueN(palm),
  palm_share_max = max(palm),
  cells_palm_over_5pct = sum(palm > 0.05)
), by = country][order(-cells_palm_over_5pct, -palm_share_max)]
data.table::fwrite(countries, file.path(derived, "crop_mix_countries.csv"))

box <- function(name, lat1, lat2, lon1, lon2) {
  b <- cells[lat >= lat1 & lat <= lat2 & lon >= lon1 & lon <= lon2]
  b[, .(
    region = name, cells = .N,
    distinct_palm_shares = data.table::uniqueN(palm),
    palm_share_median = stats::median(palm), palm_share_max = max(palm),
    soy_share_median = stats::median(soy), soy_share_max = max(soy),
    palm_share_m3 = sum(m3_area_palm) / sum(m3_area_all),
    soy_share_m3 = sum(m3_area_soy) / sum(m3_area_all),
    forest_loss_2000_2020_mha = sum(loss_ha, na.rm = TRUE) / 1e6,
    cropland_share_of_forest_loss = sum(loss_ha * ag, na.rm = TRUE) / sum(loss_ha[!is.na(ag)], na.rm = TRUE)
  )]
}
regions <- data.table::rbindlist(list(
  box("Riau, Sumatra (1.5°S–2.5°N, 100–104°E)", -1.5, 2.5, 100, 104),
  box("Sabah and Sarawak, Borneo (1–7°N, 110–119°E)", 1, 7, 110, 119),
  box("Mato Grosso, Brazil (16–9°S, 60–51°W)", -16, -9, -60, -51),
  box("Paraguayan Chaco (24–20°S, 62–58°W)", -24, -20, -62, -58)
))
data.table::fwrite(regions, file.path(derived, "crop_mix_regions.csv"))

# Is each crop's share constant within a GAEZ zone?
by_zone <- cells[, .(distinct_palm = data.table::uniqueN(palm), distinct_soy = data.table::uniqueN(soy)), by = gaez]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# First-year response to +1% palm price in Indonesia and Malaysia -----
# Uses the chunk inputs of this repo's model run (01_prepare_data.R) and the scenario that shocks
# palm alone. Revenue is linear in each price, so +1% palm revenue is BAU revenue plus 1/shock of
# that scenario's revenue change. pred() is simulate_10_dt()'s first-year deforestation rate.

mp <- readRDS(file.path(chunks_dir, "model_params.rds"))
scen <- data.table::fread(file.path(output_dir, "price_scenarios.csv"))
shocked <- scen[pct_change != 0, .(crops = paste(sort(unique(crop)), collapse = "+"), pct = pct_change[1]), by = scenario]
palm_only <- shocked[crops == "Oil, palm"][1]
stopifnot("No palm-only scenario in price_scenarios.csv" = nrow(palm_only) == 1 && !is.na(palm_only$scenario))

chunk_files <- sort(list.files(chunks_dir, "^chunk_\\d+\\.parquet$", full.names = TRUE))
im <- data.table::rbindlist(lapply(chunk_files, function(f) {
  ch <- data.table::as.data.table(arrow::read_parquet(f))[country %in% c("Indonesia", "Malaysia")]
  if (nrow(ch) == 0) return(NULL)
  rev_palm <- ch[[paste0("revenue_", palm_only$scenario)]]
  rev_1pct <- ch$revenue_bau + (rev_palm - ch$revenue_bau) / palm_only$pct
  pred <- function(rev) mp$linkinv(rev * mp$alpha + ch$remaining_treecover_share * mp$beta + ch$pixel_fe + ch$yc_fe)
  ch[, .(
    pixel_id,
    extra_px = lag_remaining_treecover * (pred(rev_1pct) - pred(revenue_bau)),
    d_rev_annual = (rev_1pct - revenue_bau) * 0.05 # update_revenues() capitalizes at 5%
  )]
}))
im <- cells[, .(pixel_id, ha_pixel, ag, palm, y_palm)][im, on = "pixel_id"]
im[, `:=`(extra_ha = extra_px * ha_pixel, ag = data.table::fcoalesce(ag, 0))]

model_ha      <- sum(im$extra_ha)
model_palm_t  <- sum(im$extra_ha * im$ag * im$palm * im$y_palm, na.rm = TRUE)
all_palm_t    <- sum(im$extra_ha * im$y_palm, na.rm = TRUE)
yield_im      <- all_palm_t / model_ha # model's palm yield, weighted by where the extra clearing is
# Change in a cell's annual revenue from +1% palm price, as the model sees it (diluted by the palm
# share of the crop mix) and per hectare of palm (undiluted).
d_rev_mix  <- mean(im$d_rev_annual)
d_rev_palm <- mean(im$d_rev_annual[im$palm > 0] / im$palm[im$palm > 0])
chen_ha    <- CHEN_HA_PER_1000 * 0.01 * CHEN_MEAN_REVENUE / 1000 * CHEN_CELLS
world_palm_t <- 78.938e6 # USDA PSD 2024/25, as in report.qmd

numbers <- data.table::data.table(metric = c(
  "cells", "distinct_palm_shares", "distinct_soy_shares", "palm_share_max",
  "gaez_zones", "gaez_zones_with_more_than_one_palm_or_soy_share",
  "im_palm_only_scenario_pct",
  "im_cells", "im_model_extra_clearing_ha", "im_model_extra_palm_t", "im_model_palm_t_if_all_cleared_land_were_palm",
  "im_model_palm_yield_t_per_ha", "im_model_d_rev_annual_crop_mix", "im_model_d_rev_annual_per_ha_palm",
  "im_chen_extra_clearing_ha", "im_chen_d_rev_annual_per_ha_palm",
  "im_model_elasticity", "im_chen_elasticity",
  "supply_crop_2020_palm_t", "supply_crop_2020_soy_t"
), value = c(
  nrow(cells), data.table::uniqueN(cells$palm), data.table::uniqueN(cells$soy), max(cells$palm),
  nrow(by_zone), sum(by_zone$distinct_palm > 1 | by_zone$distinct_soy > 1),
  palm_only$pct,
  nrow(im), model_ha, model_palm_t, all_palm_t,
  yield_im, d_rev_mix, d_rev_palm,
  chen_ha, 0.01 * CHEN_MEAN_REVENUE,
  model_palm_t / world_palm_t / 0.01, chen_ha * yield_im / world_palm_t / 0.01,
  supply_base[crop == "Oil, palm", supply_t], supply_base[crop == "Soybeans", supply_t]
))
data.table::fwrite(numbers, file.path(derived, "crop_mix_numbers.csv"))
print(numbers)
print(regions)
print(countries[1:12])
