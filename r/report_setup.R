#################################
# report_setup.R
# Data, headline numbers, formatting helpers and figure theme for report.qmd and
# technical-notes.qmd
#################################
#
# Each document sources this file in its setup chunk, into knitr's environment. Package versions
# are pinned via renv (see renv.lock); functions are called with explicit pkg:: prefixes. Paths are
# relative to the project root, where Quarto renders both documents.

# Headline numbers. The price shock is written by report.qmd's export-price-shock chunk; the
# land-use results by r/land_use_model/03_summarise_results.R.
shock_2035 <- utils::read.csv(file.path("data", "derived", "vegetable_oil_price_shock_2035.csv"))
lum <- utils::read.csv(file.path("data", "derived", "land_use_country_results.csv.gz"))

# Soybean oil's share of soybean crush value (Janzen & Wang 2025; see Methods). Must match
# SOY_OIL_VALUE_SHARE in r/land_use_model/01_prepare_data.R.
soy_oil_value_share <- 4.72 / (4.72 + 8.66)

# Roberts & Schlenker (2013), Table 1 (p. 2279): supply and demand elasticities of world caloric
# production in six specifications, instrumental variables (IV) and three-stage least squares (3SLS),
# each with 3, 4 and 5 spline knots. The primary scenario uses the central values, 0.10 and -0.05,
# about the average of the six. Each specification estimates supply and demand together, so the
# sensitivity runs use matched pairs: the specifications with the smallest and largest multiplier,
# not the extremes of each range.
eps_s_rs <- 0.10
eps_d_rs <- -0.05
rs_specs <- data.frame(
  spec = c("IV, 3 knots", "IV, 4 knots", "IV, 5 knots", "3SLS, 3 knots", "3SLS, 4 knots", "3SLS, 5 knots"),
  eps_s = c(0.102, 0.096, 0.087, 0.116, 0.112, 0.097),
  eps_d = c(-0.028, -0.055, -0.054, -0.034, -0.062, -0.066)
)
stopifnot(
  "The central values should be about the average of the six specifications" =
    abs(mean(rs_specs$eps_s) - eps_s_rs) < 0.005 && abs(mean(rs_specs$eps_d) - eps_d_rs) < 0.005
)
# Price multiplier M = 1 / (eps_s - eps_d); eps_d is negative, so the denominator is eps_s + |eps_d|.
multiplier <- function(eps_s, eps_d) 1 / (eps_s - eps_d)
m_rs <- multiplier(eps_s_rs, eps_d_rs) # ~6.7
rs_specs$m <- multiplier(rs_specs$eps_s, rs_specs$eps_d)
rs_low <- rs_specs[which.min(rs_specs$m), ]
rs_high <- rs_specs[which.max(rs_specs$m), ]

# The model runs: the primary shock; the same shock on palm and on soybeans alone, to split the loss
# by crop; and the shocks at the low and high ends of Roberts & Schlenker's estimates.
scenarios <- c("vo_rs", "vo_rs_palm", "vo_rs_soy", "vo_rs_low", "vo_rs_high")

# Additional forest loss relative to BAU: BAU stock minus scenario stock, so positive = more loss.
# lum has one row per country x scenario x year (checked in 03_summarise_results.R).
bau <- lum |>
  dplyr::filter(scenario == "bau") |>
  dplyr::select(country, year, carbon_bau = forest_carbon_tco2, area_bau = forest_area_ha)
loss <- lum |>
  dplyr::filter(scenario %in% scenarios) |>
  dplyr::inner_join(bau, by = c("country", "year")) |>
  dplyr::mutate(
    carbon_loss = carbon_bau - forest_carbon_tco2, # t CO2
    area_loss = area_bau - forest_area_ha # ha
  )
stopifnot(
  "Every run needs every country and year" =
    nrow(loss) == length(scenarios) * nrow(bau) && !anyDuplicated(loss[c("country", "scenario", "year")])
)
global_loss <- loss |>
  dplyr::summarise(
    dplyr::across(c(carbon_loss, area_loss, carbon_bau, area_bau), sum),
    .by = c(scenario, year)
  )
gl <- function(scn, yr, var) global_loss[[var]][global_loss$scenario == scn & global_loss$year == yr]

final_year <- max(lum$year)
n_years <- final_year - min(lum$year)
country_final <- loss |>
  dplyr::filter(year == final_year) |>
  dplyr::mutate(share = carbon_loss / sum(carbon_loss), .by = scenario) |>
  dplyr::arrange(scenario, dplyr::desc(carbon_loss))
rs_final <- country_final |> dplyr::filter(scenario == "vo_rs")
# Countries whose additional loss peaks and then narrows by >5% (BAU later clears some of the
# same forest), among those with more than 1 Mt CO2 of additional loss.
gap_peaks <- loss |>
  dplyr::filter(scenario == "vo_rs") |>
  dplyr::summarise(
    peak = max(carbon_loss),
    peak_year = year[which.max(carbon_loss)],
    final = carbon_loss[year == final_year],
    .by = country
  )
n_narrowing <- sum(gap_peaks$peak > 1e6 & gap_peaks$final < 0.95 * gap_peaks$peak)
mys <- gap_peaks[gap_peaks$country == "Malaysia", ]

# ILUC carbon intensity: additional forest carbon loss after T years of fuel use, divided by T
# years of VO-RD energy at the 2035 level. Annual VO-RD energy is ICCT's 2035 VO-RD share of
# shipping energy (Table 1), saved by report.qmd's tbl-icct-table-1 chunk.
icct_saved <- utils::read.csv(file.path("data", "derived", "icct_table_1.csv"))
icct_2035 <- icct_saved[icct_saved$year == 2035, ]
vord_mj_2035 <- icct_2035$vord_pct_energy_demand / 100 * icct_2035$shipping_energy_demand_ej * 1e12
vord_direct_ci <- 42.3 # g CO2e/MJ, soy VO-RD (Seber & Pavlenko 2025, p. 6)
fossil_baseline_ci <- 93.3 # g CO2e/MJ, 2008 baseline (Seber & Pavlenko 2025, p. 1)
accounting_years <- c(20, 25, 30)
headline_years <- 25
iluc <- expand.grid(
  scenario = c("vo_rs", "vo_rs_low", "vo_rs_high"),
  years = accounting_years,
  stringsAsFactors = FALSE
)
iluc$carbon_loss <- mapply(
  function(scn, yrs) gl(scn, min(lum$year) + yrs, "carbon_loss"),
  iluc$scenario, iluc$years
)
iluc$g_per_mj <- iluc$carbon_loss * 1e6 / (iluc$years * vord_mj_2035)
iluc <- iluc[order(iluc$scenario, iluc$years), ]
il <- function(scn, yrs) iluc$g_per_mj[iluc$scenario == scn & iluc$years == yrs]
# Where an ILUC value sits relative to ICCT's 62 and Chen et al.'s 84.1 g CO2e/MJ, for the text.
vs_benchmarks <- function(x) if (x < 62) "below both" else if (x > 84.1) "above both" else "between them"

# Closest benchmark: Chen et al.'s ILUC from forest biomass carbon alone, the only pool we count. They
# study forest-to-palm conversion in Indonesia and Malaysia only, so we also give those countries' part
# of our headline value.
chen_biomass_iluc <- 58.7 # g CO2e/MJ, biomass carbon (AGB + BGB) (Chen et al. 2026, Table 1, p. 13)
iluc_of <- function(countries, scn = "vo_rs", yrs = headline_years) {
  rows <- loss$scenario == scn & loss$year == min(lum$year) + yrs & loss$country %in% countries
  sum(loss$carbon_loss[rows]) * 1e6 / (yrs * vord_mj_2035)
}
im_iluc <- iluc_of(c("Indonesia", "Malaysia"))
drc_iluc <- iluc_of("Democratic Republic of the Congo")

# Crop attribution: palm-only and soy-only runs of the Roberts & Schlenker shock. They sum to the joint run
# to within the interaction term reported here.
crop_split <- c(
  palm = gl("vo_rs_palm", final_year, "carbon_loss"),
  soy = gl("vo_rs_soy", final_year, "carbon_loss")
)
interaction_pct <- 100 * (gl("vo_rs", final_year, "carbon_loss") - sum(crop_split)) /
  gl("vo_rs", final_year, "carbon_loss")

fmt1 <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
fmt0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
# Elasticities at their source's precision, two or three decimals (0.10, −0.028), with a true minus sign.
fmt_el <- function(x) {
  digits <- ifelse(abs(round(x, 2) - x) < 1e-9, 2, 3)
  sub("^-", "−", mapply(formatC, x, format = "f", digits = digits))
}
# Country names as they read mid-sentence ("the Democratic Republic of the Congo").
the_country <- function(x) {
  ifelse(grepl("^(Democratic Republic|Republic of|Central African Republic|United States|United Kingdom|United Republic|Philippines|Netherlands|Gambia|Ivory Coast|Solomon Islands)", x), paste("the", x), x)
}

# Figures: one ggplot2 theme and one set of colors throughout. The primary scenario and crops keep
# the same colors in every figure; magnitude on maps uses viridisLite's magma, reversed so darker
# means more loss (the ends are trimmed so the lightest step stays visible on the gray land).
col_surface <- "#fcfcfb"
col_land <- "#ecebe6"
col_primary <- "#2a78d6"
col_range <- "#9cc3ee" # low-high range of Roberts & Schlenker's estimates
col_crop <- c("Oil palm" = "#1baf7a", "Soybeans" = "#eda100")
map_ramp <- function(n) viridisLite::magma(n, direction = -1, begin = 0.1, end = 0.95)

theme_report <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = col_surface, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#e1e0d9", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(colour = "#52514e"),
      axis.text = ggplot2::element_text(colour = "#52514e"),
      plot.title = ggplot2::element_text(colour = "#0b0b0b", face = "bold"),
      plot.title.position = "plot",
      legend.position = "top",
      legend.justification = "left"
    )
}
theme_map <- function() {
  theme_report() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.justification = "center"
    )
}

# Country outlines (Natural Earth 1:110m), as vertices; see r/make_basemap.R.
basemap <- utils::read.csv(file.path("data", "natural_earth", "ne_110m_admin_0_countries.csv.gz"))
