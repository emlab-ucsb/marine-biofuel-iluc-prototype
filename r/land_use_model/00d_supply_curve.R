#################################
# 00d_supply_curve.R
# "The land-use model's own supply curves for palm oil and soybeans" (not part of upstream)
#################################
#
# PURPOSE:
# Traces out the crop supply response implied by the land-use model, so report.qmd can compare it
# with the outside supply elasticity used in the price shock (~0.12). For a grid of price changes,
# it raises the palm oil and/or soybean price, simulates forest cover forward from 2020 at a zero
# carbon price, and converts the extra forest cleared relative to BAU into extra crop production.
# Run it manually, like upstream's 00c_run_cobweb.R; it does not depend on the price-shock
# scenarios in 01_prepare_data.R.
#
# EXPERIMENTS (each at every price change in PCT_CHANGES):
# - palm:          "Oil, palm" price changes; every other crop at baseline.
# - soy:           "Soybeans" price changes; every other crop at baseline.
# - vegetable_oil: a vegetable-oil price change mapped onto the model's crops as in
#                  01_prepare_data.R: palm takes the full change, soybeans SOY_OIL_VALUE_SHARE of it.
#
# SUPPLY uses upstream's conversion from forest cleared to crop production (endog_prices() in
# simulation_functions.R; compute_chunk_pv_production() in 00c_run_cobweb.R):
#   extra production of crop c = sum over pixels of extra forest cleared (30 m pixels)
#     x ha_pixel x def_for_ag_share x harvested_share_gaez[c] x m3_yield_ton_per_ha[c]
# def_for_ag_share is the share of the cell's 2000-2020 forest loss that became cropland, and
# harvested_share_gaez is the cell's crop mix (it sums to 1 over the 17 crops). Palm yields are in
# t of palm oil, soybean yields in t of beans. Each experiment also reports the same sum without
# def_for_ag_share (every extra cleared hectare becomes cropland), as an upper bound.
#
# REVENUE is linear in each crop price, so revenue at any (palm, soy) price change is BAU revenue
# plus a multiple of the change from a +100% palm and a +100% soy step, each computed with
# upstream's update_revenues(). Section 3 checks this against a direct update_revenues() call.
#
# OUTPUT: data/derived/land_use_supply_curve.csv, one row per experiment x price change x
# horizon (years after the price change), with the extra forest cleared and the extra palm oil
# and soybean production relative to BAU.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 1: Configuration -----

source(here::here("r/land_use_model/packages.R"))
source(here::here("r/land_use_model/simulation_functions.R"))
source(here::here("r/land_use_model/paths.R"))

select <- dplyr::select
options(scipen = 999)

# Price changes (%) to trace the curve. The upper end brackets the price shocks in report.qmd.
PCT_CHANGES <- c(-50, -25, -10, -5, -1, 1, 5, 10, 25, 50, 100, 150, 200, 300, 400, 600, 800, 1000, 1500)

# Years after the price change at which supply is recorded. 1 is the short-run response.
HORIZONS <- c(1L, 5L, 10L, 25L)

# Soybean oil's share of soybean crush value. Must match 01_prepare_data.R and report.qmd.
SOY_OIL_VALUE_SHARE <- 4.72 / (4.72 + 8.66)

data_directory <- file.path(input_wdir, "1_input/full")
supply_file    <- here::here("data/derived/land_use_supply_curve.csv")
check_file     <- here::here("data/derived/land_use_country_results.csv.gz")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 2: Load and prepare inputs, as in 01_prepare_data.R Sections 2-5 -----

cat("Loading raw data...\n")
load(file.path(data_directory, "K_data_past.Rdata"))
load(file.path(data_directory, "K_endo_input_data.Rdata"))
load(file.path(data_directory, "K_prod_oil_full.Rdata"))
load(file.path(data_directory, "B_suit_harvest_country_full.Rdata"))
model <- readRDS(file.path(data_directory, "model.rds"))
load(file.path(data_directory, "K_final_prices_allcp_150y.Rdata"))

# Pixels: 2020 state, dropping fully deforested pixels and pixels without carbon density.
data_past <- data.table::as.data.table(data_past[data_past$year == 2020L, ])
data_past[is.na(remaining_treecover_share) | remaining_treecover_share == 0, def_shr := NA_real_]
data_past <- data_past[!is.na(def_shr) & !is.na(CF) & !is.na(CF_pix)]
stopifnot("Duplicate pixel_ids in 2020 data" = !anyDuplicated(data_past$pixel_id))
cat("  pixels:", nrow(data_past), "\n")

# Model coefficients, pixel FEs, and country FEs averaged over 2016-2020 (full-sample fallback).
alpha   <- stats::coef(model)[["revenue"]]
beta    <- stats::coef(model)[["remaining_treecover_share"]]
linkinv <- model$family$linkinv
fixedeffects <- fixest::fixef(model, na.rm = FALSE)
gamma_df <- data.table::data.table(
  pixel_id = as.numeric(names(fixedeffects$pixel_id)),
  pixel_fe = unname(fixedeffects$pixel_id)
)
FE_WINDOW <- 2016:2020
delta_long <- data.table::data.table(
  year    = as.integer(sub("_.*", "", names(fixedeffects$`year^country`))),
  country = sub("^[^_]*_", "", names(fixedeffects$`year^country`)),
  fe_val  = unname(fixedeffects$`year^country`)
)
delta_df <- delta_long[, .(
  yc_fe_win  = mean(fe_val[year %in% FE_WINDOW], na.rm = TRUE),
  yc_fe_full = mean(fe_val, na.rm = TRUE)
), by = country]
delta_df[, yc_fe := data.table::fifelse(is.na(yc_fe_win), yc_fe_full, yc_fe_win)]
delta_df <- delta_df[, .(country, yc_fe)]
rm(model, fixedeffects, delta_long)

# Crop data: yields and travel times for revenue, 2020 crude oil price. Only simulated pixels are
# kept; each pixel's revenue depends only on its own rows, so this doesn't change any value.
oil_price_2020 <- prod_oil$crudeoil_real[prod_oil$year == 2020][1]
endo <- data.table::as.data.table(endo_input_data[endo_input_data$pixel_id %in% data_past$pixel_id, ])
rm(endo_input_data, prod_oil)
shc_cols <- c("pixel_id", "crop", "gaez_attainable_yield_2000",
              "minimum_travel_time_to_large_or_medium_port_minutes")
shc <- data.table::as.data.table(
  suit_harvest_country[suit_harvest_country$pixel_id %in% data_past$pixel_id, shc_cols]
)
rm(suit_harvest_country)
n_endo <- nrow(endo)
endo <- shc[endo, on = c("pixel_id", "crop")]
stopifnot("Join with suit_harvest_country changed row count" = nrow(endo) == n_endo)
endo[, `:=`(crudeoil_real = oil_price_2020, yield = gaez_attainable_yield_2000)]
rm(shc)
gc()

baseline_prices <- final_prices_allcp[final_prices_allcp$CPrice == 0, ]
baseline_price_vec <- stats::setNames(baseline_prices$price_end, baseline_prices$crop)
stopifnot("No baseline price for palm or soybeans" =
            all(c("Oil, palm", "Soybeans") %in% names(baseline_price_vec)))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 3: Per-pixel revenue and production coefficients -----

revenue_at <- function(prices) {
  r <- update_revenues(endo, tibble::tibble(crop = names(prices), price_end = unname(prices)))
  data.table::as.data.table(r)[, .(pixel_id, revenue)]
}
step_prices <- function(crop) {
  p <- baseline_price_vec
  p[crop] <- 2 * p[crop]
  p
}

cat("Computing revenues...\n")
rev <- revenue_at(baseline_price_vec)[, .(pixel_id, revenue_bau = revenue)]
rev <- rev[revenue_at(step_prices("Oil, palm"))[, .(pixel_id, d_rev_palm = revenue)], on = "pixel_id"]
rev <- rev[revenue_at(step_prices("Soybeans"))[, .(pixel_id, d_rev_soy = revenue)], on = "pixel_id"]
rev[, `:=`(d_rev_palm = d_rev_palm - revenue_bau, d_rev_soy = d_rev_soy - revenue_bau)]

# Check the linear shortcut against a direct update_revenues() call at palm +150%, soy +50%.
direct <- local({
  p <- baseline_price_vec
  p[c("Oil, palm", "Soybeans")] <- p[c("Oil, palm", "Soybeans")] * c(2.5, 1.5)
  revenue_at(p)
})
direct <- rev[direct, on = "pixel_id"]
stopifnot("Revenue shortcut does not match update_revenues()" = isTRUE(all.equal(
  direct$revenue, direct$revenue_bau + 1.5 * direct$d_rev_palm + 0.5 * direct$d_rev_soy,
  tolerance = 1e-10
)))
rm(direct)

# t of crop per 30 m pixel of extra forest cleared (NA inputs count as zero, as in upstream's
# na.rm sums). def_for_ag_share and ha_pixel are per pixel, so dcast gives one row per pixel.
coefs <- data.table::dcast(
  endo[crop %in% c("Oil, palm", "Soybeans"), .(
    pixel_id, ha_pixel,
    ag   = data.table::fcoalesce(def_for_ag_share, 0),
    crop = data.table::fifelse(crop == "Oil, palm", "palm", "soy"),
    hy   = data.table::fcoalesce(harvested_share_gaez * m3_yield_ton_per_ha, 0)
  )],
  pixel_id + ha_pixel + ag ~ crop, value.var = "hy"
)
coefs[, `:=`(
  t_per_px_palm = ha_pixel * ag * palm, t_per_px_all_palm = ha_pixel * palm,
  t_per_px_soy  = ha_pixel * ag * soy,  t_per_px_all_soy  = ha_pixel * soy
)]
coefs[, c("ag", "palm", "soy") := NULL]

sim_input <- data_past[, .(pixel_id, country, lag_remaining_treecover, remaining_treecover_share,
                           pixel_count, CF, CF_pix)]
sim_input <- gamma_df[sim_input, on = "pixel_id"]
sim_input <- delta_df[sim_input, on = "country"]
sim_input <- rev[sim_input, on = "pixel_id"]
sim_input <- coefs[sim_input, on = "pixel_id"]
stopifnot(
  "Pixels lost or duplicated in joins" = nrow(sim_input) == nrow(data_past),
  "NA in simulation inputs" = !anyNA(sim_input[, .(pixel_fe, yc_fe, revenue_bau, d_rev_palm, d_rev_soy,
                                                   t_per_px_palm, t_per_px_soy,
                                                   t_per_px_all_palm, t_per_px_all_soy)]),
  "Non-finite ha_pixel" = all(is.finite(sim_input$ha_pixel))
)
cat(sprintf("  ha_pixel vs upstream's CF_pix / CF: max relative difference %.2g\n",
            max(abs(sim_input$ha_pixel / (sim_input$CF_pix / sim_input$CF) - 1))))
rm(data_past, endo, rev, coefs, gamma_df, delta_df)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 4: Simulate each price change -----

sim_years <- seq(2020L, 2020L + max(HORIZONS) - 1L)

# Forest cover (30 m pixels) at each horizon for palm and soy price changes of pct_palm, pct_soy %.
forest_at <- function(pct_palm, pct_soy) {
  sim_input[, revenue := revenue_bau + pct_palm / 100 * d_rev_palm + pct_soy / 100 * d_rev_soy]
  out <- simulate_10_dt(
    starting_data = sim_input[, .(pixel_id, lag_remaining_treecover, remaining_treecover_share,
                                  pixel_count, CF, yc_fe, pixel_fe, revenue)],
    cp = 0, revenue_col = "revenue", fe_col = "pixel_fe",
    years = sim_years, alpha = alpha, beta = beta, linkinv = linkinv
  )
  out <- out[year %in% (2020L + HORIZONS)]
  out[, .(pixel_id, year, forest_px = remaining_treecover_share * pixel_count)]
}

cat("Simulating BAU...\n")
bau <- forest_at(0, 0)
data.table::setnames(bau, "forest_px", "forest_px_bau")

# Check: BAU and the +20% smoke test reproduce 02_run_simulations.R's global forest carbon.
if (file.exists(check_file)) {
  lum <- data.table::as.data.table(readr::read_csv(check_file, show_col_types = FALSE))
  carbon <- function(forest) {
    x <- sim_input[, .(pixel_id, CF_pix)][forest, on = "pixel_id"]
    x[, .(carbon = sum(forest_px * CF_pix)), by = year]
  }
  smoke <- forest_at(20, 20)
  for (chk in list(list("bau", carbon(bau[, .(pixel_id, year, forest_px = forest_px_bau)])),
                   list("vo_20pct", carbon(smoke)))) {
    ref <- lum[scenario == chk[[1]], .(ref = sum(forest_carbon_tco2)), by = year][chk[[2]], on = "year"]
    if (!isTRUE(all.equal(ref$carbon, ref$ref, tolerance = 1e-9))) {
      print(ref)
      stop("Scenario '", chk[[1]], "' does not reproduce 02_run_simulations.R's global forest carbon")
    }
    cat("  ", chk[[1]], "matches 02's global forest carbon at years", paste(ref$year, collapse = ", "), "\n")
  }
  rm(lum, smoke)
} else {
  cat("  ", check_file, "not found; skipping the check against 02's results\n")
}

experiments <- data.table::rbindlist(list(
  data.table::data.table(experiment = "palm", pct_change = PCT_CHANGES,
                         pct_change_palm = PCT_CHANGES, pct_change_soy = 0),
  data.table::data.table(experiment = "soy", pct_change = PCT_CHANGES,
                         pct_change_palm = 0, pct_change_soy = PCT_CHANGES),
  data.table::data.table(experiment = "vegetable_oil", pct_change = PCT_CHANGES,
                         pct_change_palm = PCT_CHANGES,
                         pct_change_soy = SOY_OIL_VALUE_SHARE * PCT_CHANGES)
))

tictoc::tic("Simulations")
supply <- data.table::rbindlist(lapply(seq_len(nrow(experiments)), function(i) {
  e <- experiments[i]
  cat(sprintf("  %-13s %+6g%% (palm %+.1f%%, soy %+.1f%%)\n",
              e$experiment, e$pct_change, e$pct_change_palm, e$pct_change_soy))
  f <- forest_at(e$pct_change_palm, e$pct_change_soy)[bau, on = c("pixel_id", "year")]
  f <- sim_input[, .(pixel_id, ha_pixel, t_per_px_palm, t_per_px_soy,
                     t_per_px_all_palm, t_per_px_all_soy)][f, on = "pixel_id"]
  f[, extra_px := forest_px_bau - forest_px]  # extra 30 m pixels cleared vs BAU
  cbind(e, f[, .(
    years                    = year - 2020L,
    extra_forest_loss_ha     = sum(extra_px * ha_pixel),
    extra_palm_oil_t         = sum(extra_px * t_per_px_palm),
    extra_soybeans_t         = sum(extra_px * t_per_px_soy),
    extra_palm_oil_t_upper   = sum(extra_px * t_per_px_all_palm),
    extra_soybeans_t_upper   = sum(extra_px * t_per_px_all_soy)
  ), by = year][, -"year"])
}))
tictoc::toc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SECTION 5: Write -----

data.table::setorder(supply, experiment, years, pct_change)
data.table::fwrite(supply, supply_file)
cat("Wrote", nrow(supply), "rows to", supply_file, "\n")
print(supply[years == 1L & pct_change %in% c(-1, 1, 10, 100),
             .(experiment, pct_change, extra_forest_loss_ha, extra_palm_oil_t, extra_soybeans_t)])
