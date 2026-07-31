#################################
# 04_analyze_results.R
# "Analysis of parallelized results"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# This is the fourth script in the parallelized simulation workflow.
# It reads the aggregated results created in 03_aggregate_results.R and then 
# translates these into the key results for the analysis/
#
# PREREQUISITES:
# Run 01_prepare_data.R, 02_run_simulations.R and 03_aggregate_results first, with the same USE_SAMPLE_DATA setting.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Configuration --------------

USE_SAMPLE_DATA <- FALSE

# Load packages and functions
source(here::here("r/land_use_model/packages.R"))


wdir <- file.path('/Users/rheilmayr/Nextcloud/emlab/projects/current-projects/land-based-solutions/data/parallelized')
#source(here::here("r/directories.R"))
#wdir<- glue::glue("{data_directory}/parallelized")
# wdir <- file.path('/Users/clatka/github/data/parallelized')


data_subdir     <- if (USE_SAMPLE_DATA) "smpl" else "full"
output_dir <- file.path(wdir, "3_output", data_subdir)
# output_dir     <- file.path(wdir, "3_output_cl", data_subdir)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Load data --------------

grid_smry <- read_csv(file.path(output_dir, "grid_smry.csv"))
agg_df <- read_csv(file.path(output_dir, "sim_results.csv"))

agg_df <- agg_df %>%
  left_join(grid_smry, by = "pixel_id") %>%
  filter(crp_price == "exog")   # primary results = exogenous crop prices (endog moved to SI)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Generate supply curves --------------

# Calculate baseline total emissions
agg_df %>%
  filter(cprice==1) %>%
  select(bl_emit_co2_y) %>%
  sum()

# Calculate total biomass remaining after annualized year of policy
agg_df <- agg_df %>%
  mutate(biomass_remaining = biomass_2020 - bl_emit_co2_y + abate_co2_y)


### Aggregate supply curve
total_df <- agg_df %>%
  filter(crp_price=="exog") %>%
  group_by(cprice, crp_price) %>%
  summarize(abate_co2_y = sum(abate_co2_y) / 1e9, # converting to gigatonnes
            gross_avoided_co2_y = sum(abate_co2_y_paid) / 1e9, # (a) carbon-cost effect, holding prices at V(P)
            biomass_remaining = sum(biomass_remaining) / 1e9,
            at_risk_biomass = sum(bl_emit_co2_y) / 1e9,
            optout_co2_y = sum(optout_co2_y) / 1e9,
            nonadd_co2_y = sum(nonadd_co2_y) / 1e9) %>%
  filter(crp_price == "exog") %>%
  mutate(leakage_co2_y = gross_avoided_co2_y - abate_co2_y) # (b) price-induced added emissions; (c)=abate_co2_y

s_curve <- total_df %>% 
  ggplot(aes(x=abate_co2_y, y=cprice)) +
  geom_hline(yintercept=0,color="grey")+geom_vline(xintercept=0,color="grey")+
  geom_line() +
  # geom_point() +
  theme_bw(base_size = 12)+
  labs(x=expression(paste("Avoided emissions (gigatonnes CO"[2]," / y)")),
       y=expression(paste("Carbon price (USD/ tonne CO"[2],")")))+
  theme(legend.position="none")+
  scale_linetype_manual(values=c("solid"))+
  scale_y_continuous(breaks = seq(0, 200, 20))+
  xlim(0,1.6)
s_curve


# NOTE: The leakage-decomposition figure (gross vs. net avoided + leakage ribbon) lives in
# 07_leakage_decomposition.R. It is an endogenous-price phenomenon (under the exogenous primary
# results leakage is identically zero), so it is kept separate for the SI.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Adding full biomass curve --------------
# Question for co-authors: I'm currently contrasting the annualized supply curve
# against an annualized version of baseline emissions. I think thats' what we want?

s_curve_shifted <- total_df %>% 
  ggplot(aes(x=biomass_remaining, y=cprice)) +
  geom_hline(yintercept=0,color="grey")+geom_vline(xintercept=0,color="grey")+
  geom_line() +
  # geom_point() +
  theme_bw(base_size = 12)+
  labs(x=expression(paste("Carbon remaining in forests (gigatonnes CO"[2],")")),
       y=expression(paste("Carbon price (USD/ tonne CO"[2],")")))+
  theme(legend.position="none")+
  scale_linetype_manual(values=c("solid"))+
  scale_y_continuous(breaks = seq(0, 200, 20))
s_curve_shifted


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. Illustrating impact of asymmetric information --------------

total_df <- total_df %>%
  mutate(abate_wo_optout = abate_co2_y - optout_co2_y,
         abate_nonadd = abate_wo_optout + nonadd_co2_y)


p_actual_avoided_after_optout <- total_df %>%
  ggplot(aes(y=cprice)) +
  geom_hline(yintercept=0,color="grey") +
  geom_vline(xintercept=0,color="grey") +
  geom_line(aes(x = abate_co2_y, linetype = "-")) +
  geom_line(aes(x = abate_wo_optout)) +
  geom_ribbon(aes(xmin=abate_co2_y, xmax=abate_wo_optout),fill=pal_optout, alpha=0.8) +
  theme_bw(base_size = 20)+
  labs(x=expression(paste("Avoided emissions (gigatonnes CO"[2]," / y)")),
       y=expression(paste("Carbon price (USD/ tonne CO"[2],")")))+
  theme(legend.position="none")+
  scale_linetype_manual(values=c("dotted","solid"))+
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  xlim(0, 1.6) +
  annotate("text", x=0.57, y=100, size = 4, label= "Additional", color="black", hjust="right") +
  annotate("text", x=1.05, y=90, size = 4, label= "Additional +\nopt-out", color="black", hjust="left")

p_actual_avoided_after_optout
# ggsave(glue::glue("{pres_figures_directory}/M_paper_optout_delayed_10years.png"),
#   p_actual_avoided_after_optout,width=9,height=6)


#add non-additional plot here
p_actual_avoided_and_unadditional <- total_df %>%
  ggplot(aes(y=cprice)) +
  geom_hline(yintercept=0,color="grey") +
  geom_vline(xintercept=0,color="grey") +
  geom_ribbon(aes(xmin=abate_wo_optout, xmax=abate_nonadd),fill=pal_nonadd, alpha=0.8) +
  geom_line(aes(x = abate_wo_optout)) +
  geom_line(aes(x = abate_nonadd, linetype = "-")) +
  theme_bw(base_size = 20)+
  labs(x=expression(paste("Avoided emissions (gigatonnes CO"[2]," / y)")),
       y=expression(paste("Carbon price (USD/ tonne CO"[2],")")))+
  theme(legend.position="none")+
  scale_linetype_manual(values=c("dotted","solid"))+
  annotate("text", x=0.57, y=100, size = 4, label= "Additional", color="black", hjust="right") +
  annotate("text", x=1.24, y=90, size = 4, label= "Additional + \nNonadditional", color="black", hjust="left") +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  xlim(0, 1.6)

p_actual_avoided_and_unadditional

#ggsave(glue::glue("{pres_figures_directory}/M_paper_nonadd_delayed_10years.png"),
#  p_actual_avoided_and_unadditional,width=9,height=6)

#side-by-side
p_sidebyside<-ggarrange(p_actual_avoided_after_optout+ rremove("xlab")+ rremove("ylab")
                        ,p_actual_avoided_and_unadditional+ rremove("xlab")+ rremove("ylab")
                        ,nrow=1,ncol=2,labels=c("a","b"))
p_sidebyside<-annotate_figure(p_sidebyside, left = textGrob(expression(paste("Carbon price ($/ tonne CO"[2],")")), rot = 90, vjust = 1, gp = gpar(cex = 1)),
                              bottom = textGrob(expression(paste("Delayed emissions over 10 yars (gigatonnes CO"[2],")")), gp = gpar(cex = 1)))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. Subset to specific countries --------------

# National supply curves
selected_countries = c("Indonesia", "Brazil", "India", "Mexico")

cntry_df <- agg_df %>%
  filter(country %in% selected_countries) %>%
  group_by(country, cprice) %>%
  summarize(abate_co2_y = sum(abate_co2_y) / 1000000000) %>%
  ungroup() %>%
  mutate(country = reorder(country, -abate_co2_y, max))

cntry_s_curve <- cntry_df %>%
  ggplot(aes(x=abate_co2_y, y=cprice)) +
  geom_hline(yintercept=0,color="grey")+geom_vline(xintercept=0,color="grey")+
  geom_line() +
  geom_point() +
  facet_wrap(~country) +
  theme_bw(base_size = 12)+
  labs(x=expression(paste("Avoided emissions (gigatonnes CO"[2]," / y)")),
       y=expression(paste("Carbon price (USD/ tonne CO"[2],")")))+
  theme(legend.position="none")+
  scale_linetype_manual(values=c("solid"))+
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  ggtitle("MAC by country")
cntry_s_curve
