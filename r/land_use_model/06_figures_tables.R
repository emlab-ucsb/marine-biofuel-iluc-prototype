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
figure_dir <- glue::glue("{wdir}/4_results/figures")
# figure_dir <- file.path('/Users/clatka/github/lbcs/figures')


data_subdir     <- if (USE_SAMPLE_DATA) "smpl" else "full"
output_dir <- file.path(wdir, "3_output", data_subdir)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Load data --------------

grid_smry <- read_csv(file.path(output_dir, "grid_smry.csv"))
agg_df <- read_csv(file.path(output_dir, "sim_results.csv"))

agg_df <- agg_df %>%
  left_join(grid_smry, by = "pixel_id") %>%
  filter(crp_price == "exog")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Aggregate indicators --------------

# Calculate baseline total emissions
agg_df %>%
  filter(cprice==1) %>%
  select(bl_emit_co2_y) %>%
  sum()

# Calculate total biomass remaining after annualized year of policy
agg_df <- agg_df %>%
  mutate(biomass_remaining = biomass_2020 - bl_emit_co2_y + abate_co2_y)

agg_df %>% 
  group_by(cprice) %>%
  select(opp_cost, pv_pymnt, bl_emit_co2_y) %>%
  summarize(opp_cost = sum(opp_cost), pv_pymnt = sum(pv_pymnt), bl_emit = sum(bl_emit_co2_y)) %>%
  mutate(ratio = opp_cost / pv_pymnt)


### Aggregate indicators
total_df <- agg_df %>%
  filter(crp_price=="exog") %>%
  mutate(welfare=pv_bnft_fi-opp_cost, #need to check: negative opp_cost cases!
         opp_cost_ai=opp_cost*participate_ai,
         add_co2_y_ai=abate_co2_y*participate_ai,#I think abate_co2_y_ai contains all nonadd abatement as well??
         pv_co2_ai=pv_co2*participate_ai, # PV (discounted-sum) abatement among AI participants -> AI cost-ratio denominator
         welfare_ai=pv_bnft_ai-opp_cost_ai,
         #this needs to be double checked - but for test remove negative values from non_add and optout
         #nonadd_co2_y=if_else(nonadd_co2_y<0,0,nonadd_co2_y),
         #optout_co2_y=if_else(optout_co2_y<0,0,optout_co2_y),
         emissions_y=bl_emit_co2_y - abate_co2_y,
         nonadd_co2_y_all=biomass_remaining-abate_co2_y
         )%>%
  group_by(cprice, crp_price) %>%
  summarize(abate_co2_y = sum(abate_co2_y) / 1e9, # converting to gigatonnes
            biomass_remaining = sum(biomass_remaining) / 1e9,
            at_risk_biomass = sum(bl_emit_co2_y) / 1e9,
            optout_co2_y = sum(optout_co2_y) / 1e9,
            nonadd_co2_y = sum(nonadd_co2_y) / 1e9,
            pv_pymnt=sum(pv_pymnt)/1e9,
            pv_bnft_fi=sum(pv_bnft_fi)/1e9,
            opp_cost=sum(opp_cost)/1e9,
            welfare=sum(welfare)/1e9,
            pv_bnft_ai=sum(pv_bnft_ai)/1e9,
            pv_paid_ai=sum(pv_paid_ai)/1e9,
            abate_co2_y_ai=sum(abate_co2_y_ai)/1e9,
            welfare_ai=sum(welfare_ai)/1e9,
            pv_pymnt_all=sum(pv_pymnt_all)/1e9,
            pv_tax=sum(pv_tax)/1e9,
            opp_cost_ai=sum(opp_cost_ai)/1e9,
            add_co2_y_ai=sum(add_co2_y_ai)/1e9,
            pv_co2=sum(pv_co2)/1e9,         # PV (discounted-sum) abatement; cost-ratio denominator (FI/ALL/Tax)
            pv_co2_ai=sum(pv_co2_ai)/1e9,   # PV abatement among AI participants; AI cost-ratio denominator
            emissions_y=sum(emissions_y)/1e9,
            bl_emit_co2_y=sum(bl_emit_co2_y)/1e9,
            nonadd_co2_y_all=sum(nonadd_co2_y_all)/1e9
            #biomass_bl=sum(biomass_bl)/1e9
            ) %>%
  filter(crp_price == "exog")


total_df <- total_df %>%
  mutate(abate_wo_optout = abate_co2_y - optout_co2_y,
         abate_nonadd = abate_wo_optout + nonadd_co2_y,
         abate_paid_ai=abate_wo_optout+ nonadd_co2_y)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Create Table 2 --------------


SCC=193
fisc_ext=0.5*0.255
CarbonPrice=80


FI_payment<-total_df %>%
  dplyr::select(abate_co2_y,pv_co2,pv_pymnt,welfare,pv_bnft_fi,opp_cost) %>%
  mutate(nonadd=0,
    optout=0,
    # cost/efficiency ratios divide PV-dollar quantities by PV (discounted-sum) abatement pv_co2,
    # NOT the Stavins-annualized abate_co2_y, so numerator and denominator share the same (PV) basis.
    effectiveness=pv_co2/pv_pymnt*1000,
    govcost=pv_pymnt/pv_co2) %>%
  rename(add=abate_co2_y,gov=pv_pymnt,socval=pv_bnft_fi)%>%
  mutate(scenario="FI",gov=-gov,
         #create all steps for MVPF
         net_env_benefit=socval*(1-fisc_ext),
         fiscal_externality=socval*fisc_ext,
         WTP=-gov-opp_cost+net_env_benefit,#transfer=pv_pymnt=-gov
         net_costs=-gov-fiscal_externality, #costs=pv_pymnt=-gov
         MVPF=WTP/net_costs,
         soccost=opp_cost / pv_co2)%>%
  dplyr::select(-pv_co2)


AI_payment<-total_df %>% 
  dplyr::select(pv_bnft_ai,add_co2_y_ai,pv_co2_ai,nonadd_co2_y,optout_co2_y,pv_paid_ai,welfare_ai,opp_cost_ai) %>%
  # ratios use PV abatement among participants (pv_co2_ai), the PV counterpart of add_co2_y_ai
  mutate(effectiveness=pv_co2_ai/pv_paid_ai*1000,
         govcost=pv_paid_ai/pv_co2_ai) %>%
  rename(add=add_co2_y_ai,gov=pv_paid_ai,nonadd=nonadd_co2_y,optout=optout_co2_y,welfare=welfare_ai,socval=pv_bnft_ai,opp_cost=opp_cost_ai)%>%
  mutate(scenario="AI",gov=-gov,
         #create all steps for MVPF
         net_env_benefit=socval*(1-fisc_ext),
         fiscal_externality=socval*fisc_ext,
         WTP=-gov-opp_cost+net_env_benefit,#transfer=pv_pymnt=-gov
         net_costs=-gov-fiscal_externality, #costs=pv_pymnt=-gov
         MVPF=WTP/net_costs,
         soccost=opp_cost / pv_co2_ai)%>%
  dplyr::select(-pv_co2_ai)


ALL_payment<-total_df %>%
  dplyr::select(abate_co2_y, pv_co2, pv_pymnt_all, welfare, pv_bnft_fi, opp_cost, nonadd_co2_y_all) %>%
  mutate(nonadd=nonadd_co2_y_all,
         optout=0,
         # ratios use PV (discounted-sum) abatement pv_co2, not annualized abate_co2_y
         effectiveness=pv_co2/pv_pymnt_all*1000,
         govcost=pv_pymnt_all/pv_co2) %>%
  rename(add=abate_co2_y,gov=pv_pymnt_all,socval=pv_bnft_fi)%>%
  mutate(scenario="ALL",gov=-gov,
         #create all steps for MVPF
         net_env_benefit=socval*(1-fisc_ext),
         fiscal_externality=socval*fisc_ext,
         WTP=-gov-opp_cost+net_env_benefit,#transfer=pv_pymnt=-gov
         net_costs=-gov-fiscal_externality, #costs=pv_pymnt=-gov
         MVPF=WTP/net_costs,
         soccost=opp_cost / pv_co2)%>%dplyr::select(-nonadd_co2_y_all,-pv_co2)

Tax<-total_df %>%
  dplyr::select(abate_co2_y,pv_co2,pv_tax,welfare,pv_bnft_fi,opp_cost) %>%
  mutate(nonadd=0,
         optout=0,
         # ratios use PV (discounted-sum) abatement pv_co2, not annualized abate_co2_y
         effectiveness=pv_co2/pv_tax*1000,
         govcost=-pv_tax/pv_co2) %>%
  rename(add=abate_co2_y,gov=pv_tax,socval=pv_bnft_fi)%>%
  mutate(scenario="Tax",
         #create all steps for MVPF
         net_env_benefit=socval*(1-fisc_ext),
         fiscal_externality=socval*fisc_ext,
         WTP=-gov-opp_cost+net_env_benefit,#transfer=pv_pymnt=-gov
         net_costs=-gov-fiscal_externality, #costs=pv_pymnt=-gov
         MVPF=WTP/net_costs,
         soccost=opp_cost / pv_co2)%>%
  dplyr::select(-pv_co2)

full_table<-full_join(Tax,FI_payment)
full_table<-full_join(full_table,ALL_payment)
full_table<-full_join(full_table,AI_payment)


full_table<-full_table%>%dplyr::select(-WTP,-net_costs,-net_env_benefit,-fiscal_externality)


full_table_long<-full_table%>%ungroup()%>%filter(cprice==CarbonPrice)%>%dplyr::select(-cprice)%>%
  pivot_longer(
    -scenario,
    names_to  = "variable",
    values_to = "value"
  ) %>%mutate(value=round(value,digits = 3))%>%
  pivot_wider(
    names_from  = scenario,
    values_from = value
  )%>%
  mutate(category=if_else(variable=="add"|variable=="nonadd"|variable=="optout","Emission impacts",
                          if_else(variable=="gov"|variable=="socval"|variable=="opp_cost","Financial impacts","Efficiency measures")))%>%
  arrange(factor(variable, levels = c("add", "nonadd", "optout","avoided_co2_2030","gov","socval","opp_cost","effectiveness","govcost","soccost","welfare","MVPF")))%>%
  filter(variable!="avoided_co2_2030")%>%
  mutate(variable=recode(variable,
                         "add"="Additional emission abatement (Gt / y)",
                         "nonadd"= "Nonadditional emission `abatement' (Gt / y)",
                         "optout"="Opted out emission abatement (Gt / y)",
                         #"avoided_co2_2030"="Change in stored carbon (Gt/final year)",
                         "gov"= "Government balance ($ bill., PV)",
                         "socval"= "Social benefits ($ bill., PV)",
                         "opp_cost"= "Opportunity costs ($ bill., PV)",
                         "effectiveness"="Average cost effectiveness (t / $ 1000)",
                         "govcost"="Government cost per ton avoided ($ / t)",
                         "soccost"="Social cost per ton avoided ($ / t)",
                         "welfare"= "Welfare change ($ bill., PV)",
                         "MVPF"="Marginal value of public funds"
  ))%>%
  rename("Asymmetric information payment" =AI,
         "Full information payment" =FI,
         "Payment to all" =ALL,
         " "=variable,"  "=category)



full_table_long <- full_table_long[c(1,2,3,4,5,6,8,9,10,11), c(6,1,2,3,4,5)]

library(knitr)
kable(full_table_long, "latex")



full_table_long2 <- full_table_long[,-1]

kable(full_table_long2)

kable(full_table_long2, "latex")





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Create Figure 4 --------------


####alternative figure 1, one panel each policy
#%%%%%%%%%%%%%%%%%%%%
##figure 193 CP - tax vs payment (full info, to all)
#%%%%%%%%%%%%%%%%%%%%
x_at193<-total_df%>%
  filter(cprice==CarbonPrice,crp_price=="exog")
x_at193<-x_at193$abate_co2_y

tax_at193<-total_df%>%
  filter(cprice==CarbonPrice,crp_price=="exog")
tax_at193<-tax_at193$emissions_y  

tax_at0<-total_df%>%
  filter(cprice==CarbonPrice,crp_price=="exog")
tax_at0<-tax_at0$bl_emit_co2_y  

xr_at193<-total_df%>%
  filter(cprice==CarbonPrice,crp_price=="exog")
xr_at193<-xr_at193$biomass_remaining

remforest0<-total_df%>%
  filter(cprice==1,crp_price=="exog")
remforest0<-remforest0$biomass_remaining ##need to check if this was meant with biomass_bl_last??

#fi payment for delays
brace1<-data.frame(x=c(0,x_at193),y=c(85,85),label=c("Full information payment","Full information payment"))
brace2<-data.frame(x=c(x_at193,tax_at0),y=c(85,85),label=c("Tax","Tax"))

p_abatement_plot_PES_tax<-ggplot(total_df,
                                 aes(x=abate_co2_y,y=cprice))+
  geom_hline(yintercept=0,color="grey")+geom_vline(xintercept=0,color="grey")+
  geom_rect(xmin=0,xmax=x_at193,ymin=0,ymax=CarbonPrice,
            fill = "lightgrey", alpha=0.1,
            color="black"
  )+
  geom_rect(xmin=x_at193,xmax=tax_at0,ymin=0,ymax=CarbonPrice,
            fill = "lightgrey", alpha=0.1,
            color="black"
  )+
  ggbrace::stat_brace(data=brace1,aes(x=x,y=y,group=label,label=label),width = 5)+
  ggbrace::stat_bracetext(data=brace1,aes(x=x,y=y,group=label,label=label),width = 5)+
  
  ggbrace::stat_brace(data=brace2,aes(x=x,y=y,group=label,label=label),width = 5)+
  ggbrace::stat_bracetext(data=brace2,aes(x=x,y=y,group=label,label=label),width = 5)+
  geom_line()+
  theme_bw()+
  labs(x=expression(paste("Avoided emissions (Gt CO"[2]," / y)")),
       y=expression(paste("Carbon price ($ / t CO"[2],")")))+
  scale_y_continuous(breaks = seq(0, 240, 20),
                     limits = c(-20, 240))+
  xlim(0,max(tax_at0))

ggsave(glue::glue("{figure_dir}/M_paper_PES_Tax_delayed_150.png"),p_abatement_plot_PES_tax,width=5,height=5)




#brace2<-data.frame(x=c(0,xr_at193/1000000000,0,remforest0/1000000000,remforest0/1000000000,xr_at193/1000000000),y=c(-20,-20,0,0,0,0),label=c("payment to all remaining forest","payment to all remaining forest","nonadditional","nonadditional","additional","additional"))
brace3<-data.frame(x=c(0,remforest0,remforest0,xr_at193,0,xr_at193),y=c(CarbonPrice,CarbonPrice,CarbonPrice,CarbonPrice,210,210),label=c("Nonadditional","Nonadditional","Additional","Additional","Payment to all remaining forest","Payment to all remaining forest"))

p_remaining_forest_plot<-ggplot(total_df,
                                aes(x=biomass_remaining,y=cprice))+
  geom_hline(yintercept=0,color="grey")+geom_vline(xintercept=0,color="grey")+
  geom_rect(xmin=0,xmax=remforest0,ymin=0.3,ymax=CarbonPrice-.3,
            fill = pal_nonadd, alpha=0.1,
            color="black")+
  geom_rect(xmin = remforest0, ymin = 0.3, xmax = xr_at193-1, ymax = 193-.3, fill= "lightgrey",
            color="black",alpha=0.1
  )+
  ggbrace::stat_brace(data=brace3,aes(x=x,y=y,group=label,label=label),width = 5,rotate = 0)+
  ggbrace::stat_bracetext(data=brace3,aes(x=x,y=y,group=label,label=label),width = 5,rotate = 0)+
  geom_line()+
  theme_bw()+
  labs(x=expression(paste("Remaining carbon stored (Gt CO"[2],")")),y=expression(paste("Carbon price ($ / t CO"[2],")")))+ 
  scale_y_continuous(breaks = seq(0, max(240), 20),
                     limits = c(-20, max(240)))+
  xlim(0,xr_at193+100)





ggsave(glue::glue("{figure_dir}//M_paper_PESALL_delayed_150.png"),p_remaining_forest_plot,width=5,height=5)

pestax_arrange<-ggarrange(p_abatement_plot_PES_tax+ rremove("ylab"),p_remaining_forest_plot+ rremove("ylab"), nrow=1,ncol=2,labels=c("a","b"))

pestax_arrange<-annotate_figure(pestax_arrange, left = textGrob(expression(paste("Carbon price ($ / tonne CO"[2],")")), rot = 90, vjust = 1, gp = gpar(cex = 1)))


ggsave(glue::glue("{figure_dir}/M_paper_PES_Tax_150years.png"),pestax_arrange,width=11,height=6)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Create Figure 5 --------------

p_actual_avoided_after_optout <- total_df %>%
  ggplot(aes(y=cprice)) +
  geom_hline(yintercept=0,color="grey") +
  geom_vline(xintercept=0,color="grey") +
  geom_line(aes(x = abate_co2_y, linetype = "-")) +
  geom_line(aes(x = abate_wo_optout)) +
  geom_ribbon(aes(xmin=abate_co2_y, xmax=abate_wo_optout),fill=pal_optout, alpha=0.8) +
  theme_bw(base_size = 20)+
  labs(x=expression(paste("Avoided emissions (Gt CO"[2]," / y)")),
       y=expression(paste("Carbon price ($ / t CO"[2],")")))+
  theme(legend.position="none")+
  scale_linetype_manual(values=c("dotted","solid"))+
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  xlim(0, 2.25) +
  annotate("text", x=0.57, y=100, size = 4, label= "Additional", color="black", hjust="right") +
  annotate("text", x=1.45, y=90, size = 4, label= "Additional +\nopt-out", color="black", hjust="left")

p_actual_avoided_after_optout

ggsave(glue::glue("{figure_dir}/M_paper_optout_150years.png"),p_actual_avoided_after_optout,width=6,height=6)



#add unadditional plot here
p_actual_avoided_and_unadditional <- total_df %>%
  ggplot(aes(y=cprice)) +
  geom_hline(yintercept=0,color="grey") +
  geom_vline(xintercept=0,color="grey") +
  geom_ribbon(aes(xmin=abate_wo_optout, xmax=abate_nonadd),fill=pal_nonadd, alpha=0.8) +
  geom_line(aes(x = abate_wo_optout)) +
  geom_line(aes(x = abate_nonadd, linetype = "-")) +
  theme_bw(base_size = 20)+
  labs(x=expression(paste("Avoided emissions (Gt CO"[2]," / y)")),
       y=expression(paste("Carbon price ($ / t CO"[2],")")))+
  theme(legend.position="none")+
  scale_linetype_manual(values=c("dotted","solid"))+
  annotate("text", x=0.57, y=100, size = 4, label= "Additional", color="black", hjust="right") +
  annotate("text", x=1.7, y=90, size = 4, label= "Additional + \nNonadditional", color="black", hjust="left") +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  xlim(0, 2.25)

p_actual_avoided_and_unadditional

ggsave(glue::glue("{figure_dir}/M_paper_nonadd_150years.png"),p_actual_avoided_and_unadditional,width=6,height=6)

#side-by-side
p_sidebyside<-ggarrange(p_actual_avoided_after_optout+ rremove("xlab")+ rremove("ylab")
                        ,p_actual_avoided_and_unadditional+ rremove("xlab")+ rremove("ylab")
                        ,nrow=1,ncol=2,labels=c("a","b"))
p_sidebyside<-annotate_figure(p_sidebyside, left = textGrob(expression(paste("Carbon price ($ / t CO"[2],")")), rot = 90, vjust = 1, gp = gpar(cex = 1)),
                              bottom = textGrob(expression(paste("Avoided emissions (Gt CO"[2]," / y)")), gp = gpar(cex = 1)))



ggsave(glue::glue("{figure_dir}/M_paper_sidebyside_150years.png"),p_sidebyside,width=10.5,height=5)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Create Figure 6 --------------

#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

cols <- c("FI_avoided" = "lightgrey", "AI_avoided" = "#bf812d", "AI_optout" = pal_optout, "AI_nonadditional" = pal_nonadd,
          "AI_paid"="#c7eae5","AI_avoided2" = "#bf812d")


stacked_data<-total_df%>%
  filter(cprice==10|cprice==40|cprice==80|cprice==120|cprice==193)%>%
  rename(FI_avoided=abate_co2_y,
         AI_avoided=add_co2_y_ai,
         AI_optout=optout_co2_y,
         AI_paid=abate_paid_ai,
         AI_nonadditional=nonadd_co2_y )%>%
  mutate(AI_avoided2=AI_avoided)%>%
  select(cprice,crp_price,FI_avoided,AI_avoided,AI_avoided2,AI_optout,AI_paid,AI_nonadditional)%>%
  filter(crp_price=="exog")%>%
  pivot_longer(cols=c(FI_avoided,AI_avoided,AI_avoided2,AI_paid,AI_nonadditional,AI_optout),names_to = "scenario",values_to = "agg_emis")%>%
  mutate(group=case_when(scenario=="FI_total"~"FI_total",
                         scenario=="AI_total"~"AI_total",
                         scenario=="FI_avoided"~"FI_avoided",
                         scenario=="AI_paid"~"AI_paid",
                         scenario=="AI_optout"|scenario=="AI_avoided"~"AI_optout_plus_avoided",
                         scenario=="AI_nonadditional"|scenario=="AI_avoided2"~"AI_nonadd_plus_avoided"))


p_agg_overtime_emissionsavings_stacked_optout<-ggplot(stacked_data%>%
                                                        mutate(scenario=factor(scenario, levels=c('FI_avoided',
                                                                                                  'AI_optout','AI_avoided',
                                                                                                  'AI_paid', 'AI_nonadditional', 'AI_avoided2')))%>%
                                                        mutate(group=factor(group, levels=c("FI_total","FI_avoided","AI_optout_plus_avoided","AI_paid","AI_nonadd_plus_avoided","AI_total"))),
                                                      aes(x=group,y=agg_emis,fill=scenario
                                                      ))+
  #geom_col_pattern(aes(pattern=scenario,pattern_colour=scenario,pattern_angle=scenario,pattern_fill=scenario),alpha=0.8,fill='white')+
  geom_col()+
  theme_bw()+
  labs(x=expression("Carbon price ($ / t CO"[2]*")"),y=expression(paste("Avoided emissions (Gt CO"[2]," / y)")),fill="",pattern="",pattern_colour="",pattern_angle="",pattern_fill="")+
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="bottom",
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.text = element_text(size = 7))+
  scale_fill_manual(values = cols,
                    breaks = c("FI_avoided", "AI_optout", "AI_avoided", "AI_paid", "AI_nonadditional"),
                    labels = c(
                      "Full information avoided additional",
                      "Asymmetric information optout",
                      "Asymmetric information avoided additional",
                      "Asymmetric information paid",
                      "Asymmetric information avoided nonadditional"
                    ))+
  facet_grid(~cprice,scales="free_x",space = "free", switch="both")+#ylim(0,100)+
  guides(fill = guide_legend(nrow=3,byrow=TRUE),
         #pattern = guide_legend(nrow=3,byrow=TRUE)
  )



#arrange two parts of panel
ggsave(glue::glue("{figure_dir}/M_paper_panel_agg_overtime_emis_savings_stacked_150years.png"),p_agg_overtime_emissionsavings_stacked_optout,width=9,height=6)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Create Figure 7 --------------
gdp<-read_csv(glue::glue("{wdir}/1_input/full/gdp_per_capita_2000.csv"))
gdp_quintiles<-gdp%>%
  tidyr::drop_na(gdp_per_capita_2000)%>%
  dplyr::mutate(income_quintile = findInterval(gdp_per_capita_2000, quantile(gdp_per_capita_2000, probs = seq(0, 1, 0.2)),
                                               rightmost.closed = TRUE))



equity_df<-agg_df%>%
  filter(cprice==CarbonPrice,crp_price=="exog")%>%
  #create additional, optout, nonadditional under asymmetric info,dly_co2_ai=co2 delay offered to pay for (contains neg values in opting out group)
  mutate(surplus=pv_pymnt- opp_cost,
         surplus_tax=-pv_tax-opp_cost, #this makes no sense... land users face both, they pay tax and have oc
         surplus_ai=(pv_paid_ai- opp_cost)*participate_ai #note, if do not participate, they deforest and have no opportunity costs (but actually get value of deforestation) - is it right that we leave this out here?
  )%>%
  left_join(gdp_quintiles)

equity_df_agg<-equity_df%>%drop_na(income_quintile)%>%group_by(income_quintile)%>%summarise(
  Full_information_payment_sum_oc=-sum(opp_cost)/1000000000,
  Tax_sum_oc=-sum(opp_cost)/1000000000,
  Asymmetric_information_payment_sum_oc=-sum(opp_cost*participate_ai)/1000000000,
  
  Full_information_payment_sum_pay=sum(pv_pymnt)/1000000000,
  Tax_sum_pay=-sum(pv_tax)/1000000000,
  Asymmetric_information_payment_sum_pay=sum(pv_paid_ai*participate_ai)/1000000000,
  
  Full_information_payment_sum_surplus=sum(surplus)/1000000000,
  Tax_sum_surplus=sum(surplus_tax)/1000000000,
  Asymmetric_information_payment_sum_surplus=sum(surplus_ai*participate_ai)/1000000000,
  
  
)%>%
  pivot_longer(cols=-c(income_quintile),names_to = c("scenario", "stat"),
               names_pattern = "(.*)_(sum_oc|sum_pay|sum_surplus)",values_to = "surplus")%>%
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "Full_information_payment",
        "Asymmetric_information_payment",
        "Tax"
      )
    )
  )




equity_df_agg <- equity_df_agg %>%
  mutate(stat = recode(
    stat,
    sum_oc = "Opportunity costs",
    sum_pay = "Payments",
    sum_surplus ="Surplus"
  ))



stack_surplus<-ggplot(equity_df_agg,aes(x=factor(income_quintile),y=surplus,fill=stat))+
  geom_col(position = "dodge")+facet_wrap(~scenario,labeller = labeller(
    scenario = function(x) gsub("_", " ", x)
  ))+theme_bw()+
  labs(x="Income quintile (by GDP per capita in 2000)",
       y="Present value of financial flow (Billion $)",fill="")+theme(legend.position = "bottom")+scale_fill_viridis_d()

ggsave(glue::glue("{figure_dir}/M_paper_equity_median_surplus_150years_quintilesdodge.png"),stack_surplus,width=10,height=6)


