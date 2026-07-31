#################################
# 08_manuscript_values.R
# "Extract values reported in manuscript"
# author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################
#
# PURPOSE:
# Extract values reported in overleaf manuscript
# gross conservation effect (a) and the price-induced leakage (b) combine into the actual
# avoided emissions (c). This is an ENDOGENOUS-price phenomenon: under the exogenous primary
# results leakage is identically zero, so this figure lives in its own script and reads the
# endog rows of sim_results.csv. The primary exog figures/tables are in 04 and 06.
#
# PREREQUISITES:
# at least 03_aggregate_results.R and previous pipeline to load grid_smry.csv and sim_results.csv

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
# 4. Abstract --------------


#We find that payments of $80/tCO2 would reduce global deforestation by XX%
total_df %>%
  filter(cprice==80) %>%
  mutate(abatement_share=abate_co2_y/bl_emit_co2_y)%>%
  select(abatement_share)


#Untargeted payments at the same cabon price cost $XX trillion....
total_df %>%
  filter(cprice==80) %>%
  select(pv_pymnt_all) 

#Note: 1e9 has converted value to billion not trillion - double check with final values


#alternative: payment per additional tonne abated
total_df %>%
  filter(cprice==80) %>%
  mutate(pay_all_per_tonne=pv_pymnt_all/abate_co2_y)%>%
  select(pay_all_per_tonne)



#...with only XX% additionality
#based on payments
total_df %>%
  filter(cprice==80) %>%
  mutate(share_all_additional_payment=pv_pymnt/pv_pymnt_all*100)%>%
  select(share_all_additional_payment)
#note: additionallity for AI results based on add_co2_y_ai+nonadd_co2_y not based on payments
#as there we have not computed payment-based values


#check if about the same as share in additional abatement (as additional and nonadditional abatement reported in table 2..)
total_df %>%
  filter(cprice==80) %>%
  mutate(share_all_additional=abate_co2_y/biomass_remaining*100)%>%
  select(share_all_additional)

#this is much smaller...

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. Introduction --------------

#For example, a carbon price of $80/tCO2 would reduce annual emissions from deforestation by XX GtCO2, an XX percent reduction relative to baseline deforestation
total_df %>%
  filter(cprice==80) %>%
  select(abate_co2_y)



total_df %>%
  filter(cprice==80) %>%
  mutate(abatement_share=abate_co2_y/bl_emit_co2_y*100)%>%
  select(abatement_share)



#At a price of $80/tCO2, undifferentiated payments to all stewards of forests would cost  $XX trillion.
total_df %>%
  filter(cprice==80) %>%
  select(pv_pymnt_all) 

#Note: 1e9 has converted value to billion not trillion - double check with final values



#Modeling a well-informed .....,we estimate that XX% of all payments at a carbon price of $80/ tCO2 will be non-additional (Note: asymmetric information scenario)
total_df %>%
  filter(cprice==80) %>%
  mutate(share_ai_nonadditional=nonadd_co2_y/(add_co2_y_ai+nonadd_co2_y)*100)%>%
  select(share_ai_nonadditional)
#Note: is this correct to just use emission-based share for this instead of payment share? it should be the same due to fixed per tonne value?
#On the other hand, payments have been calculated differently with all the discounting, I think..?
#Problem: we know how much paid, but not exactly which share goes to additional and nonadditional in terms of pv flows

#Relative to a carbon tax or perfectly targeted payments, information asymmetries reduce the welfare benefits of carbon pricing by XX%
total_df %>%
  filter(cprice==80) %>%
  mutate(reduced_welfare=(welfare-welfare_ai)/welfare*100)%>%
  select(reduced_welfare)


#We estimate that only XX% of payments yield additional changes in deforestation at current voluntary market prices of $10/tCO2,...
total_df %>%
  filter(cprice==10) %>%
  mutate(share_ai_additional=add_co2_y_ai/(add_co2_y_ai+nonadd_co2_y)*100)%>%
  select(share_ai_additional)

#...but this increases to XX% at $80/tCO2
total_df %>%
  filter(cprice==80) %>%
  mutate(share_ai_additional=add_co2_y_ai/(add_co2_y_ai+nonadd_co2_y)*100)%>%
  select(share_ai_additional)


#..and to XX% at $193/tCO2
total_df %>%
  filter(cprice==193) %>%
  mutate(share_ai_additional=add_co2_y_ai/(add_co2_y_ai+nonadd_co2_y)*100)%>%
  select(share_ai_additional)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 6. Empirical results --------------


#add later - unchanged from previous results version


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 7. Simulation results: Environmental impacts --------------

#In the absence of information asymmetries, we estimate that the introduction of a $80/tCO2 price on deforestation would reduce global emissions by XX%, from XX GtCO2 per year to XX GtCO2 per year
total_df %>%
  filter(cprice==80) %>%
  mutate(abatement_share=abate_co2_y/bl_emit_co2_y*100)%>%
  select(abatement_share)


total_df %>%
  filter(cprice==80) %>%
  select(bl_emit_co2_y) 


total_df %>%
  filter(cprice==80) %>%
  select(emissions_y) 


#check: should be same as
total_df %>%
  filter(cprice==80) %>%
  mutate(emis_80_check=bl_emit_co2_y-abate_co2_y)%>%
  select(emis_80_check) 

#Assuming this payment program was permanently implemented, the present value of all abatement benefits would reach  $XX  billion
total_df %>%
  filter(cprice==80) %>%
  select(pv_co2)




#However, when all carbon stored in remaining forest receives payments at $80/tCO2, only XX% of transfers compensate land users for (additionally) avoided emissions 
#sentence refers to PAYMENTS
total_df %>%
  filter(cprice==80) %>%
  mutate(share_all_additional_payment=pv_pymnt/(pv_pymnt+pv_pymnt_all)*100)%>%
  select(share_all_additional_payment)






#These land users account fo XX% of the predicted carbon delays under full information at a carbon price of $80/ ton CO2.
#optout abatement
total_df %>%
  filter(cprice==80) %>%
  mutate(share_ai_optout=optout_co2_y/(abate_co2_y)*100)%>%
  select(share_ai_optout)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 8. Simulation results: Welfare impacts --------------


#For example, the implementation of a $80/tCO2 carbon tax on deforestation of full information payments would achieve global welfare gains of $XX billion....
total_df %>%
  filter(cprice==80) %>%
  select(welfare)



#greater than the annual GDP of 87% (XX???) of countries




#Under asymmetric information, land users' decisions to opt out of payments erode welfare gains by XX%,....
total_df %>%
  filter(cprice==80) %>%
  mutate(share_welfare_loss_ai=(welfare-welfare_ai)/welfare*100)%>%
  select(share_welfare_loss_ai)



#but payments still improve global welfare by $XX billion
total_df %>%
  filter(cprice==80) %>%
  select(welfare_ai)



#and on to the undifferentiated payment to all, which would coast nearly $XX trillion
total_df %>%
  filter(cprice==80) %>%
  select(pv_pymnt_all) 


#In our context, we find that payments under full information achieve an MVPF of XX, meaning that each dollar spent by government on payments increases global welfare by $XX
SCC=193
fisc_ext=0.5*0.255
CarbonPrice=80


total_df %>%
  filter(cprice==80) %>%
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
  dplyr::select(MVPF)




#Although non-additionality and opt out reduce the MVPF, payments under asymmetric information still achieve an MVPF of XX..
total_df %>% 
  filter(cprice==80) %>%
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
  dplyr::select(MVPF)



#We find that a global $80/tCO2 tax on deforestation has an MVPF of -XX, indicating that each dollar raised by the tax actually improves welfare.
total_df %>%
  filter(cprice==80) %>%
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
  dplyr::select(MVPF)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 9. Discussion --------------

#a a global carbon price of $80/t CO2 could reduce emissions by XX%
total_df %>%
  filter(cprice==80) %>%
  mutate(abatement_share=abate_co2_y/bl_emit_co2_y*100)%>%
  select(abatement_share)

#However, we also highlight that the cost of paying the social cost of carbon for all of the carbon stored in the world's forests would be infeasible, costing $XX trillion
#Untargeted payments at the same cabon price cost $XX trillion....
total_df %>%
  filter(cprice==80) %>%
  select(pv_pymnt_all) 

#Note: 1e9 has converted value to billion not trillion - double check with final values


#Specifically, we find that at current voluntary carbon market prices of roughly $10/tCO2, additionality is a mere XX%.
total_df %>%
  filter(cprice==10) %>%
  mutate(share_ai_additional=add_co2_y_ai/(add_co2_y_ai+nonadd_co2_y)*100)%>%
  select(share_ai_additional)

