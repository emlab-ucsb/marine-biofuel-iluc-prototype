#################################
# audit_crop_mix.R
# Evidence for report.qmd Appendix C: the land-use model's crop mix (harvested_share_gaez)
#################################
#
# Reads the land-use model's inputs (read-only, via r/land_use_model/paths.R) and this repo's model
# run (land_use_model_runs/, from r/land_use_model/run_pipeline.R), and writes five small files to
# data/derived/ for the report:
# - crop_mix_grid.csv.gz: per 0.5-degree square, the palm and soybean shares of the crop mix the model
#   uses (harvested_share_gaez, set per GAEZ zone) and of M3 harvested area in the same cells.
# - crop_mix_countries.csv: per country, cells, distinct palm shares, maximum palm share, and cells
#   where palm is more than 5% of the crop mix.
# - crop_mix_regions.csv: palm and soybean shares (model and M3) and the cropland share of forest
#   loss in four well-known palm and soy frontiers.
# - crop_mix_numbers.csv: headline counts.
# - crop_mix_cropland_share.csv: the share of forest loss the model counts as cropland
#   (def_for_ag_share), weighted by 2000-2020 forest loss and by the primary scenario's extra clearing.
#
# Nothing here changes the model; it only reads its inputs and outputs. Peak memory is about that of
# 01_prepare_data.R.

source(here::here("r/land_use_model/paths.R"))
input_dir  <- file.path(input_wdir, "1_input/full")
chunks_dir <- file.path(wdir, "2_processing/full/chunks")
output_dir <- file.path(wdir, "3_output/full")
derived    <- here::here("data/derived")

MAP_RES_DEG <- 0.5

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

# endo_input_data: the crop shares and cropland share 01_prepare_data.R and update_revenues() use.
load(file.path(input_dir, "K_endo_input_data.Rdata"))
endo <- data.table::as.data.table(endo_input_data[endo_input_data$crop %in% c("Oil, palm", "Soybeans"), ])
rm(endo_input_data)
gc()
cells <- data.table::dcast(
  endo,
  pixel_id + ha_pixel + def_for_ag_share + cumulative_forest_loss_pixel_count ~ crop,
  value.var = "harvested_share_gaez"
)
data.table::setnames(
  cells,
  c("pixel_id", "ha_pixel", "def_for_ag_share", "cumulative_forest_loss_pixel_count", "Oil, palm", "Soybeans"),
  c("pixel_id", "ha_pixel", "ag", "loss_px", "palm", "soy")
)
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
# Headline counts -----

numbers <- data.table::data.table(metric = c(
  "cells", "distinct_palm_shares", "distinct_soy_shares", "palm_share_max",
  "gaez_zones", "gaez_zones_with_more_than_one_palm_or_soy_share"
), value = c(
  nrow(cells), data.table::uniqueN(cells$palm), data.table::uniqueN(cells$soy), max(cells$palm),
  nrow(by_zone), sum(by_zone$distinct_palm > 1 | by_zone$distinct_soy > 1)
))
data.table::fwrite(numbers, file.path(derived, "crop_mix_numbers.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Share of forest loss the model counts as cropland -----
# def_for_ag_share (ag) is a cell's 2000-2020 forest loss that became cropland or palm, over its total
# forest loss (upstream r/Kb_endogenous_price_preparation.R); endog_prices() counts only that share of
# newly cleared forest as cropland. Weighted by each cell's 2000-2020 forest loss, and by the extra forest
# the primary scenario (vo_rs) clears 1, 15 and 25 years after the shock (02's per-pixel results, with
# area = biomass / CF as in 03_summarise_results.R).

results_dir <- file.path(wdir, "2_processing/full/results")
extra <- data.table::rbindlist(lapply(sort(list.files(results_dir, "^results_chunk_\\d+\\.parquet$", full.names = TRUE)), function(f) {
  r <- data.table::as.data.table(arrow::read_parquet(f))[scn %in% c("bau", "vo_rs")]
  base <- min(r$year)
  r <- r[year %in% (base + c(1, 15, 25))]
  ch <- data.table::as.data.table(arrow::read_parquet(
    file.path(chunks_dir, sub("^results_", "", basename(f))), col_select = c("pixel_id", "CF")
  ))
  w <- data.table::dcast(r, pixel_id + year ~ scn, value.var = "biomass")[ch, on = "pixel_id", nomatch = NULL]
  w[, .(pixel_id, years = year - base, extra_ha = (bau - vo_rs) / CF)]
}))
extra <- cells[, .(pixel_id, country, ag = data.table::fcoalesce(ag, 0))][extra, on = "pixel_id"]
stopifnot("Result pixels missing from the crop data" = !anyNA(extra$country))

scopes <- list(
  "All countries" = unique(cells$country), Indonesia = "Indonesia", Malaysia = "Malaysia",
  "Democratic Republic of the Congo" = "Democratic Republic of the Congo", Brazil = "Brazil"
)
cropland_share <- data.table::rbindlist(lapply(names(scopes), function(s) {
  h <- cells[country %in% scopes[[s]] & !is.na(ag) & !is.na(loss_ha)]
  x <- extra[country %in% scopes[[s]]]
  data.table::rbindlist(list(
    data.table::data.table(
      scope = s, weight = "2000-2020 forest loss", years = NA_integer_,
      forest_mha = sum(h$loss_ha) / 1e6, cropland_share = sum(h$loss_ha * h$ag) / sum(h$loss_ha)
    ),
    x[, .(scope = s, weight = "vo_rs extra clearing", forest_mha = sum(extra_ha) / 1e6,
      cropland_share = sum(extra_ha * ag) / sum(extra_ha)), by = years]
  ), use.names = TRUE)
}))
data.table::fwrite(cropland_share, file.path(derived, "crop_mix_cropland_share.csv"))

print(numbers)
print(cropland_share)
print(regions)
print(countries[1:12])
