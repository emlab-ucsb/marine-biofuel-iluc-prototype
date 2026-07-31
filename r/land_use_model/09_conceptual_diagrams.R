#################################
# 09_conceptual_diagrams.R
# "Paper figures: conceptual diagrams" (minimal reproducible version)
# author: "Catharina Latka, Robert Heilmayr (emLab)"
#################################
#
# PURPOSE:
# Minimal, self-contained code to reproduce the two conceptual figures:
#   (1) figures/conceptual_figures_over_under_PES_B_CP_inverse2.png
#         4-panel over-/under-estimation of the offer baseline (PM guess vs truth) and the
#         resulting non-additional / opportunity-cost areas.
#   (2) figures/conceptual_figures_baseline_mr_plot_B_CP_<l>_threepanles_alt.png
#         3-panel baseline: marginal returns ecdf -> remaining-forest supply -> delayed emissions.
#
# MODEL (conceptual): one landowner L whose land units have net returns r drawn from a logistic
# distribution centered on mu_L. A carbon price CP shifts the distribution left (mu_L - CP). The
# share of units with r > 0 is deforested; ecdf(0) gives remaining forest at each CP.
#
# NOTE ON RANDOMNESS: the figures are illustrative draws of a 1000-unit logistic distribution.
# We seed once below so the script is deterministic on its own. (In the original messy script the
# RNG state was inherited from earlier blocks; with 1000 samples the ecdf curves are stable, so the
# figures are visually equivalent. Change SEED if you want to match a specific earlier rendering.)

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(forcats)   # fct_reorder

wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')
fig_dir <- glue::glue("{wdir}/4_results/figures")

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Shared 11-colour carbon-price palette (one colour per CP level, from -20 to 80 by 10).
cp_pal <- c("#8073ac", "#b2abd2", "steelblue", "#fee0d2", "#fcbba1", "#fc9272",
            "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d")

# Accent colours. pal_nonadd mirrors the study palette (packages.R); the PM / asymmetric-information
# estimate uses dark goldenrod -- more legible on white than the bright study gold.
pal_pm     <- "goldenrod3"  # PM / asymmetric-information estimate
pal_nonadd <- "cyan4"       # non-additional emissions


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Generate the conceptual data ------------------------------------------------
# One landowner L (id = 303); net returns r ~ logistic(mu_L, s = 10). Each carbon price CP shifts
# the location to mu_L - CP. dist_dat holds the return draws; zerovalue holds ecdf(0) (= remaining
# forest share) at each CP.

n <- 1000
SEED <- 3004
set.seed(SEED)
mu_j <- runif(1000, -30, 100)
dat  <- data.frame(j = 1:n, mu_j = mu_j)

l    <- 303
mu_L <- dat$mu_j[l]

# Draw the landowner's fixed population of net returns ONCE. A carbon price then shifts every
# unit's return by -CP (common random numbers). Re-drawing an independent sample at each price
# (the previous approach) left the remaining-forest / avoided-emissions supply curves non-
# monotonic near saturation -- e.g. ecdf(0) hitting exactly 1.000 at one price and dipping at the
# next purely from sampling noise. Shifting one fixed sample is both the correct model (the same
# land units face the price) and monotonic by construction.
mu_L_dist <- rlogis(n = 1000, location = mu_L, s = 10)
dist_dat  <- data.frame(mu_L_dist)
zerovalue <- data.frame(zero_BL = ecdf(mu_L_dist)(0))

for (CP in seq(-20, 80, by = 10)) {
  mu_CP_dist <- mu_L_dist - CP   # same units, returns shifted down by the carbon price
  dist_dat   <- cbind(dist_dat, mu_CP_dist)
  colnames(dist_dat)[ncol(dist_dat)] <- paste0("CP_", CP)
  zerovalue$zero <- ecdf(mu_CP_dist)(0)
  colnames(zerovalue)[ncol(zerovalue)] <- paste0("zero_CP_", CP)
}

# Long form of the return draws: one row per (unit, carbon price).
dist_datlong <- dist_dat %>%
  pivot_longer(cols = starts_with("CP_"), names_to = "pricelevel",
               values_to = "mu_CP", names_prefix = "CP_") %>%
  mutate(val = as.numeric(pricelevel),
         pricelevel = fct_reorder(pricelevel, val))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. mb_plot_inv -- remaining-forest supply curve --------------------------------
# Uses zerovaluelong in its FIRST state: def_shr = remaining forest share. (Built before the second
# state below, which redefines def_shr = 1 - remaining forest for the avoided-emissions panels.)

zerovaluelong <- zerovalue %>%
  pivot_longer(cols = starts_with("zero_CP_"), names_to = "CP",
               values_to = "remainingforest", names_prefix = "zero_CP_") %>%
  mutate(def_shr = remainingforest, CP = as.numeric(CP))

mb_plot_inv <- ggplot(zerovaluelong) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey50") +
  geom_line(aes(y = CP, x = def_shr)) +
  geom_point(aes(y = CP, x = def_shr), color = cp_pal) +
  scale_y_continuous(breaks = seq(-20, 100, 10)) +
  labs(y = "Carbon price", x = bquote("Fraction of land remaining in forest (" * italic(kappa[jt]) * ")")) +
  theme_bw()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. zerovaluelong SECOND state + avoided-emissions levels -----------------------
# def_shr = 1 - remaining forest; colorfill flags the [0,20] opportunity-cost band; av_emis is
# avoided emissions relative to the CP = 0 baseline. These feed the over/under panels and delays.

zerovaluelong <- zerovalue %>%
  pivot_longer(cols = starts_with("zero_CP_"), names_to = "CP",
               values_to = "remainingforest", names_prefix = "zero_CP_") %>%
  mutate(def_shr = 1 - remainingforest, CP = as.numeric(CP)) %>%
  mutate(colorfill = if_else(CP <= 20 & CP >= 0, "OC", "empty")) %>%
  mutate(remfor0 = remainingforest[CP == 0],
         av_emis = remainingforest - remfor0)

ae0     <- as.double(zerovaluelong$av_emis[zerovaluelong$CP == 0])
ae10    <- as.double(zerovaluelong$av_emis[zerovaluelong$CP == 10])
ae10min <- as.double(zerovaluelong$av_emis[zerovaluelong$CP == -20])   # over-estimation case
ae20    <- as.double(zerovaluelong$av_emis[zerovaluelong$CP == 20])
ae60    <- as.double(zerovaluelong$av_emis[zerovaluelong$CP == 60])


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Panels for FIGURE 1 (over-/under-estimation) --------------------------------

# (a) PM under-estimates the baseline: conservative guess of r under p*
pm_under_plot_inv <- ggplot(dist_datlong %>%
    filter(pricelevel == 0 | pricelevel == 10 | pricelevel == 20) %>%
    mutate(scenario = if_else(pricelevel == 0, "True r under P=0",
                      if_else(pricelevel == 10, "Conservative estimate of r under P=0", "True r under P"))) %>%
    mutate(scenario = factor(scenario, levels = c("Conservative estimate of r under P=0",
                                                  "True r under P=0", "True r under P")))) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey50") +
  geom_line(aes(y = mu_CP, x = ..x.., color = scenario), stat = 'ecdf') +
  scale_color_manual(values = c(pal_pm, "steelblue", "grey20")) +
  labs(y = bquote(r[ijt]), x = bquote(italic(kappa[jt])), color = "") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.5, 0.17),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), line = element_blank(),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key        = element_rect(fill = "transparent", colour = NA))

# (c) PM over-estimates the baseline: generous guess of r under p*
pm_over_plot_inv <- ggplot(dist_datlong %>%
    filter(pricelevel == 0 | pricelevel == -10 | pricelevel == 20) %>%
    mutate(scenario = if_else(pricelevel == 0, "True r under P=0",
                      if_else(pricelevel == -10, "Generous estimate of r under P=0", "True r under P"))) %>%
    mutate(scenario = factor(scenario, levels = c("Generous estimate of r under P=0",
                                                  "True r under P=0", "True r under P")))) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey50") +
  geom_line(aes(y = mu_CP, x = ..x.., color = scenario), stat = 'ecdf') +
  scale_color_manual(values = c(pal_pm, "steelblue", "grey20")) +
  labs(y = bquote(r[ijt]), x = bquote(italic(kappa[jt])), color = "") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.5, 0.17),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), line = element_blank(),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key        = element_rect(fill = "transparent", colour = NA))

# (b) Under-estimation -> offer area S^offer (zeta / beta / omicron labels)
underestim_plot_inv <- ggplot(zerovaluelong, mapping = aes(y = CP, x = av_emis)) +
  geom_area(data = filter(zerovaluelong, colorfill == 'OC'), mapping = aes(x = av_emis, y = CP), fill = "#fcbba1") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "grey50") +
  annotate("segment", x = 0, y = 20, xend = .85, yend = 20, linetype = "dashed", color = "grey50") +
  geom_line(color = "grey50") +
  geom_rect(aes(xmin = ae10, xmax = ae20, ymin = 0, ymax = 20), fill = "lightgrey", color = "grey40", alpha = .02) +
  scale_y_continuous(breaks = seq(-20, 100, 10)) +
  theme_bw() +
  xlim(ae10min-.03, ae60) +
  theme(line = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(x = "Avoided emissions", y = "Carbon price") +
  annotate("text", x = -.015, y = 23, label = "P") +
  annotate("text", x = ae20, y = -3, label = paste("E*\"'\"*(P)"), parse = TRUE, color = "grey20") +
  annotate("text", x = ae0 - 0.01, y = -3, label = paste("E*\"'\"*(P==0)"), parse = TRUE, color = "steelblue") +
  annotate("text", x = ae10, y = -3, label = "tilde(E)*\"'\"*(P==0)", parse = TRUE, color = pal_pm) +
  annotate("text", x = 0.3, y = 7, label = bquote("zeta"), color = "#67000d", parse = TRUE) +
  annotate("text", x = 0.29, y = 15.5, label = bquote("beta"), color = "grey40", parse = TRUE) +
  annotate("text", x = 0.14, y = 3, label = bquote("lambda"), color = "#67000d", parse = TRUE)

# (d) Over-estimation -> non-additional area S^nonadd (zeta' / beta' / omicron' labels)
overestim_plot_inv <- ggplot(zerovaluelong, mapping = aes(y = CP, x = av_emis)) +
  geom_area(data = filter(zerovaluelong, colorfill == 'OC'), mapping = aes(x = av_emis, y = CP), fill = "#fcbba1") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "grey50") +
  annotate("segment", x = 0, y = 20, xend = .85, yend = 20, linetype = "dashed", color = "grey50") +
  geom_line(color = "grey50") +
  geom_rect(aes(xmin = ae20, xmax = ae10min, ymin = 0, ymax = 20), fill = "lightgrey", color = "grey40", alpha = .02) +
  geom_rect(aes(xmin = ae0, xmax = ae10min, ymin = 0, ymax = 20), fill = pal_nonadd, alpha = .03) +
  scale_y_continuous(breaks = seq(-20, 100, 10)) +
  theme_bw() +
  xlim(ae10min-.03, ae60) +
  theme(line = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(x = "Avoided emissions", y = "Carbon price") +
  annotate("text", x = -.015, y = 23, label = "P") +
  annotate("text", x = ae20, y = -3, label = paste("E*\"'\"*(P)"), parse = TRUE, color = "grey20") +
  annotate("text", x = ae0 + 0.01, y = -3, label = paste("E*\"'\"*(P==0)"), parse = TRUE, color = "steelblue") +
  annotate("text", x = ae10min - 0.01, y = -3, label = "tilde(E)*\"'\"*(P==0)", parse = TRUE, color = pal_pm) +
  annotate("text", x = 0.3, y = 7, label = bquote(zeta*"'"), color = "#67000d") +
  annotate("text", x = 0.1, y = 11, label = bquote(beta*"'"), color = "grey40") +
  annotate("text", x = -0.05, y = 11, label = bquote(lambda*"'"), color = pal_nonadd)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. Panels for FIGURE 2 (baseline three-panel) ----------------------------------

# (a) Marginal returns: ecdf of net returns r at each carbon price.
mr_plot_inv <- ggplot(dist_datlong) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey40") +
  geom_line(aes(y = mu_CP, x = ..x.., color = pricelevel), stat = 'ecdf') +
  scale_color_manual(values = cp_pal) +
  labs(y = bquote(r[ijt]), x = "Land parcels, ordered by net returns", color = "Carbon \nprice") +
  theme_bw() + theme(legend.position = "left")

# (c) Delayed emissions accumulated over 10 years, summed per carbon price.
delay_df <- zerovaluelong %>%
  mutate(CF = 114) %>% dplyr::select(-colorfill, -zero_BL)
remforestdf <- delay_df %>% filter(CP == 0) %>% mutate(remfor0 = remainingforest) %>% dplyr::select(CF, remfor0)
delay_df <- delay_df %>% left_join(remforestdf)   # natural join (CF, remfor0); see note above

thisyear <- delay_df
for (year in 1:10) {
  thisyear <- thisyear %>%
    mutate(av_emis = remainingforest - remfor0,
           delay = av_emis * CF,
           pres_year = paste(year))
  delay_df <- full_join(delay_df, thisyear)
  thisyear <- thisyear %>%
    mutate(remainingforest = remainingforest * (1 - def_shr)) %>%
    dplyr::select(-remfor0)
  remforestdf <- thisyear %>% filter(CP == 0) %>% mutate(remfor0 = remainingforest) %>% dplyr::select(CF, remfor0)
  thisyear <- thisyear %>%
    left_join(remforestdf) %>%   # natural join by CF (remfor0 was dropped above)
    mutate(av_emis = remainingforest - remfor0)
}

delay_dfagg <- delay_df %>% group_by(CP) %>% summarise(sum_delay = sum(delay), .groups = "drop")

plotd <- ggplot(delay_dfagg, aes(y = CP, x = sum_delay)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey50") +
  geom_line() + geom_point(aes(color = factor(CP))) + theme_bw() +
  scale_color_manual(values = cp_pal, guide = "none") +
  labs(y = "Carbon price", x = "Avoided emissions") +
  scale_y_continuous(breaks = seq(-20, 80, by = 10))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. Assemble and save the two figures -------------------------------------------

# FIGURE 1 (was line 1119): over-/under-estimation, 4 panels.
fig1 <- ggpubr::ggarrange(pm_under_plot_inv, underestim_plot_inv, pm_over_plot_inv, overestim_plot_inv,
                          ncol = 2, nrow = 2, labels = c("a", "b", "c", "d"), widths = c(0.5, 0.8))
ggsave(file.path(fig_dir, "f3_asym_info_conceptual.png"),
       fig1, height = 8, width = 9)

# FIGURE 2 (was line 1212): baseline three-panel (alt = delayed-emissions panel d).
fig2 <- ggpubr::ggarrange(mr_plot_inv, mb_plot_inv, plotd,
                          ncol = 3, nrow = 1, labels = c("            a", "b", "c"), widths = c(0.7, 0.6, 0.6))
ggsave(file.path(fig_dir, paste0("f2_cprice_conceptual.png")),
       fig2, height = 5, width = 12)

cat("Wrote both conceptual figures to", normalizePath(fig_dir), "\n")
