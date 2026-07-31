#################################
# 07_leakage_decomposition.R
# "Leakage decomposition supply curve (endogenous prices; SI figure)"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# Builds the leakage-decomposition supply curve, which makes explicit how a carbon price's
# gross conservation effect (a) and the price-induced leakage (b) combine into the actual
# avoided emissions (c). This is an ENDOGENOUS-price phenomenon: under the exogenous primary
# results leakage is identically zero, so this figure lives in its own script and reads the
# endog rows of sim_results.csv. The primary exog figures/tables are in 04 and 06.
#
# PREREQUISITES:
# Run 01_prepare_data.R, 02_run_simulations.R and 03_aggregate_results.R first (same
# USE_SAMPLE_DATA setting). sim_results.csv must contain crp_price == "endog" rows.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Configuration --------------

USE_SAMPLE_DATA <- FALSE

# Load packages and functions
source(here::here("r/land_use_model/packages.R"))


wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')
figure_dir <- glue::glue("{wdir}/4_results/figures")
# figure_dir <- file.path('/Users/clatka/github/lbcs/figures')


data_subdir     <- if (USE_SAMPLE_DATA) "smpl" else "full"
output_dir <- file.path(wdir, "3_output", data_subdir)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Load data (endogenous-price rows only) --------------

agg_df <- read_csv(file.path(output_dir, "sim_results.csv")) %>%
  filter(crp_price == "endog")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Build decomposition components --------------
# Per cprice, in gigatonnes CO2 / y:
#   (a) gross avoided  = sum(abate_co2_y_paid)  = D(0,V(P)) - D(P,V(P))
#   (c) net avoided    = sum(abate_co2_y)       = D(0,V(0)) - D(P,V(P))   (the headline s_curve)
#   (b) leakage        = (a) - (c)              = D(0,V(P)) - D(0,V(0))

total_df <- agg_df %>%
  group_by(cprice, crp_price) %>%
  summarize(abate_co2_y = sum(abate_co2_y) / 1e9, # converting to gigatonnes
            gross_avoided_co2_y = sum(abate_co2_y_paid) / 1e9, # (a) carbon-cost effect, holding prices at V(P)
            .groups = "drop") %>%
  mutate(leakage_co2_y = gross_avoided_co2_y - abate_co2_y) # (b) price-induced added emissions; (c)=abate_co2_y


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Decomposition supply curve: gross avoided, leakage, net avoided -----
# Carbon prices drive two opposing forces, which this figure makes explicit:
#   (a) gross avoided  = gross_avoided_co2_y = D(0,V(P)) - D(P,V(P)): emissions avoided by the
#       carbon cost, holding crop prices at their policy equilibrium V(P)  [dotted line].
#   (b) leakage        = leakage_co2_y       = D(0,V(P)) - D(0,V(0)): extra deforestation emissions
#       from the carbon price pushing crop prices up from V(0) to V(P)     [shaded region].
#   (c) net avoided    = abate_co2_y         = D(0,V(0)) - D(P,V(P)): the actual avoided emissions
#       (the headline s_curve)                                             [solid line].
# By construction c = a - b: leakage is the shaded gap between the gross (dotted) and net (solid) lines.
# NOTE: the annotate() x/y positions below are hand-tuned for the full-data scale (like the sibling
# p_actual_avoided_after_optout figure); adjust them to taste.

s_curve_decomp <- total_df %>%
  filter(crp_price == "endog") %>%
  ggplot(aes(y = cprice)) +
  geom_hline(yintercept = 0, color = "grey") + geom_vline(xintercept = 0, color = "grey") +
  geom_ribbon(aes(xmin = abate_co2_y, xmax = gross_avoided_co2_y), fill = pal_leakage, alpha = 0.8) +
  geom_line(aes(x = gross_avoided_co2_y, linetype = "Gross avoided")) +
  geom_line(aes(x = abate_co2_y,         linetype = "Net avoided")) +
  theme_bw(base_size = 12) +
  labs(x = expression(paste("Avoided emmissions (GtCO"[2]," / y)")),
       y = expression(paste("Carbon price ($/tonne CO"[2],")"))) +
  theme(legend.position = "none") +
  scale_linetype_manual(values = c("Gross avoided" = "dotted", "Net avoided" = "solid")) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  annotate("text", x = 1.28, y = 100, size = 4, hjust = "right",
           label = "Supply curve net\nof endogenous\nprice effects") +
  annotate("text", x = 1.57, y = 100,  size = 4, hjust = "left",
           label = "Supply curve under\nexogenous crop prices")
  # annotate("text", x = 1.12, y = 95,  size = 4, hjust = "left", color = pal_leakage,
  #          label = "Emissions induced by\ncrop price changes")
s_curve_decomp

ggsave(glue::glue("{figure_dir}/A13_leakage_decomp.png"),
       s_curve_decomp, width = 8, height = 5)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. In-text statistics for the leakage paragraph --------------
# Fills the values quoted in the text at the baseline carbon price: (i) the average crop-price
# increase the carbon price induces, and (ii) the potential -> realized avoided deforestation
# (the two curves of Figure 13).

BASELINE_CP <- 80   # baseline carbon price (USD / tCO2)

# (i) Average crop-price increase induced by the carbon price, relative to the no-carbon baseline
# V(0). final_prices_allcp (crop, CPrice, price_end) holds the endogenous crop-price equilibrium at
# each carbon price; CPrice == 0 is V(0). We report the simple cross-crop mean of the % change.
load(file.path(wdir, "1_input", data_subdir, "K_final_prices_allcp_150y.Rdata"))
v0 <- final_prices_allcp %>% filter(CPrice == 0)           %>% select(crop, p0 = price_end)
vP <- final_prices_allcp %>% filter(CPrice == BASELINE_CP) %>% select(crop, pP = price_end)
crop_price_chg          <- inner_join(v0, vP, by = "crop") %>% mutate(pct = (pP / p0 - 1) * 100)
avg_crop_price_increase <- mean(crop_price_chg$pct)

# (i-b) Price increases for selected high-deforestation-risk commodities. The cross-crop average masks
# substantial heterogeneity: some commodities rise by well more than the average (notably oil palm),
# while others rise by less. Labels follow the FAO naming used in final_prices_allcp$crop.
focal_crops   <- c("Oil, palm", "Cocoa, beans", "Coffee, green", "Soybeans")
missing_focal <- setdiff(focal_crops, crop_price_chg$crop)
if (length(missing_focal) > 0) {
  warning("Focal crops not found in final_prices_allcp (check crop naming): ",
          paste(missing_focal, collapse = ", "))
}
focal_price_chg <- crop_price_chg %>%
  filter(crop %in% focal_crops) %>%
  arrange(match(crop, focal_crops))

# (ii) Potential vs realized avoided deforestation at the baseline carbon price (endogenous prices;
# the two curves of Figure 13). Potential = gross avoided = the carbon cost alone holding prices at
# V(P), D(0,V(P)) - D(P,V(P)) [dotted line]; realized = net avoided after price-induced leakage
# subtracts part of it, D(0,V(0)) - D(P,V(P)) [solid line].
leak_bcp         <- total_df %>% filter(cprice == BASELINE_CP)
potential_supply <- leak_bcp$gross_avoided_co2_y
realized_supply  <- leak_bcp$abate_co2_y

cat(sprintf("\n===== Leakage paragraph statistics (carbon price = $%d / tCO2) =====\n", BASELINE_CP))
cat(sprintf("Average crop-price increase: %.1f%%  (median %.1f%%, range %.1f to %.1f%% across %d crops)\n",
            avg_crop_price_increase, median(crop_price_chg$pct),
            min(crop_price_chg$pct), max(crop_price_chg$pct), nrow(crop_price_chg)))
cat("Selected commodity price increases:\n")
for (k in seq_len(nrow(focal_price_chg))) {
  cat(sprintf("  %-16s %+.1f%%\n", focal_price_chg$crop[k], focal_price_chg$pct[k]))
}
cat(sprintf("Potential supply of avoided deforestation reduced from %.2f to %.2f GtCO2/y (leakage = %.2f)\n",
            potential_supply, realized_supply, potential_supply - realized_supply))
