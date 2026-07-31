#################################
# "Functions for simulating carbon effects"
#author: "Robert Heilmayr, Catharina Latka (emLab)"
#################################



# Robert note: I'd suggest adding some tests to make sure operations are working the way you expect. I've added some examples using the testthat package
library(testthat)

# Robert note: I'd suggest running computationally intensive large operations in dt rather than dplyr. dtplyr package can help make this easier
library(dtplyr)

# Robert note: One thing you can do to make your life easier is to explicitly declare which select you'd like to be your default
select <- dplyr::select

options(scipen=999)

# Robert note: Need to set random seed
# Set seed
set.seed(93106)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Defining functions -----
# (might move to separate script if these are useful in other scripts)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
retrieve_yc_fes <- function(y){
  # Purpose:
  # - Load country by year fixed effects for a given year 2021-2030
  #
  # Parameters:
  # - y: integer of year desired
  #
  # Output:
  # - delta_df: tibble detailing fe for each country within the selected year
  
  modely<-readRDS(glue::glue("{data_directory}/models/model{y}.rds"))
  fixedeffectsy<-fixef(modely,na.rm=FALSE)
  delta_df <- fe_to_tibble(fixedeffectsy$`year^country`) %>%
    rename(year_country = fe_id, yc_fe = fe_val)
  delta_df <- delta_df %>%
    filter(str_starts(year_country, as.character(y)))
  return(nest(delta_df))
}


redraw_fes <- function(fe_df, group_idx,fedraw){
  # Purpose:
  # - Randomly draw fixed effects from within each region
  #
  # Parameters:
  # - fe_df: Tibble of pixel-level fixed effects; should have pixel_id and pixel_fe columns
  # - group_idx: Tibble with two columns, pixel_id, and a second one defining the grouping over which re-sampling should take place
  # - fedraw: "boot"="LU" or "median"
  #
  # Output:
  # - fe_draw: New dataframe with new fixed effects draw
  grouping_var = (group_idx %>% names())[2]
  
  fe_df <- fe_df %>% 
    left_join(group_idx, by = "pixel_id")
  
  if(fedraw=="boot"|fedraw=="LU"){
  fe_draw <- fe_df %>%
    group_by_at(grouping_var)%>%
    mutate(pixel_fe=sample(pixel_fe, replace=TRUE))%>%
    ungroup() %>% 
    select(pixel_id, pixel_fe)}
  
  
  
  if(fedraw=="median"){
  #for median calculation trial
  fe_draw <- fe_df %>%
    group_by_at(grouping_var)%>%
    mutate(pixel_fe=median(pixel_fe, na.rm=TRUE))%>%
    ungroup() %>% 
    select(pixel_id, pixel_fe)}
  
  
  
  
  return(fe_draw)
}



fe_to_tibble <- function(fixed_effects){
  # Purpose:
  # - Reformats fixed effects output from fixef call into clean tibble
  #
  # Parameters:
  # - fixed_effects: One set of fixed effects from fixef call
  #
  # Output:
  # - fe_df: Tibble with two columns - fe_id with FE label, and fe_val with FE
  
  fe_df <- data.frame(fixed_effects)
  fe_df$fe_id <- rownames(fe_df)
  fe_df <- tibble(fe_id = fe_df$fe_id, fe_val = fe_df$fixed_effects)
  return(fe_df)
}


endog_prices <- function(endo_extract,endo_input_data,elas_pivot,prod_oil,suit_harvest_country,ye,cp_id,mc){
  # Purpose:
  # - compute new prices after considering endogenous adjustment in previous year
  #
  # Parameters:
  # - endo_extract: from prediction
  # - endo_input_data: input data from preparation loaded above
  # - elas_pivot: inverted elasticity matrix
  # - prod_oil: production index and oil prices by year needed for revenue calc.
  # - suit_harvest_country: grid and crop specific attainable yield etc data needed for revenue calc.
  # - y: year
  # - cp_id: carbon price
  # - mc: monte carlo simulation round
  #
  # Output:
  # - endo_prices: new_data with new columns pixel_id,year,revenue
  # - def_new_cropsupply: supply needed for next year's calculation
  # - def_new_price: prices needed for next year's calculation
  # - endog_revenue: save endog revenues
  
  print(paste0("year in endog fct: ",ye))
  print(paste0("mc in endog fct: ",mc))
  print(paste0("cp in endog fct: ",cp_id))
  
  cp=cp_id
  #1. count of deforestation in previous year, merge with other data
  endo_extract2<-endo_extract%>%
    dplyr::left_join(endo_input_data)
  

  
  #2. transform this to crop supply shock
  
  supply_addition<-endo_extract2%>%
    dplyr::mutate(additional_crop_supply_grid=count_def_pred*ha_pixel*def_for_ag_share*harvested_share_gaez*m3_yield_ton_per_ha)%>%
    dplyr::group_by(crop)%>%
    dplyr::summarise(additional_crop_supply=sum(additional_crop_supply_grid,na.rm=TRUE))%>%
    dplyr::ungroup()
  
  #print(paste0("nrow def_new_cropsupply"))
  #print(nrow(def_new_cropsupply))
  
  
  def_prev_cropsupply<-def_new_cropsupply %>% 
    filter(year<=ye-1,cp_id==cp,mc_id==mc)%>%
    #cumulative additional area until this year
    group_by(crop)%>%
    mutate(cum_prev_def=cumsum(additional_crop_supply))%>%
    ungroup()%>%
    filter(year==ye-1)%>%
    dplyr::select(-year,-additional_crop_supply)%>%
    distinct()

  
  #print(paste0("def_prev_cropsupply"))
  #print(def_prev_cropsupply)
  
  #3. calculate global percentage change by crop
  supply_shocks<-endo_extract2%>%
    dplyr::select(crop,supply_crop_2020)%>%
    dplyr::distinct()%>%
    dplyr::left_join(supply_addition)%>%
    dplyr::left_join(def_prev_cropsupply)%>%
    dplyr::filter(cp_id==cp)%>%
    #dplyr::filter(mc_id==mc)%>%
    dplyr::mutate(supply_shock=(additional_crop_supply)/(supply_crop_2020+cum_prev_def)*100)
  
  print(paste0("supply shock"))
  print(supply_shocks)
  
  
  
  #4. multiply with elasticity matrix
  price_shocks<-supply_shocks%>% 
    dplyr::select(crop,supply_shock)%>%
    dplyr::left_join(elas_pivot)%>%
    #multiply supply-shock with all columns
    dplyr::mutate(across(-c(crop,supply_shock),~.*supply_shock
                         ,.names = "{.col}"))%>% 
    dplyr::select(-supply_shock,-crop)%>%
    dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))%>%
    pivot_longer(names_to = "crop",values_to = "shock",cols = everything())
  
  print(paste0("price shock"))
  print(price_shocks)
  
  
  #5. update revenues - save in endo_prices
  prices_subset<-def_new_price%>%
    filter(year==ye)%>%
    dplyr::mutate(cp_id=if_else(year==2021,cp,cp_id),
                  mc_id=if_else(year==2021,mc,mc_id)
                  )%>% #in the first year, overwrite starting values
    dplyr::filter(cp_id==cp,
                  mc_id==mc)%>%
    mutate(year=ye+1)%>%
    distinct()%>%
    dplyr::left_join(prod_oil)%>%
    dplyr::left_join(price_shocks)%>%
    dplyr::mutate(price_change=price_prev*shock/100,
                  price_end=price_prev+price_change
    )
  
  
  print(head(prices_subset))
  
  
  prices_subset<-prices_subset%>%
    dplyr::select(crop,year,price_end,price_prev,production_index,crudeoil_real)%>%
    distinct()
  
  #print(prices_subset)
  
  
  
  suit_harvest_country<-suit_harvest_country%>%
    filter(pixel_id %in% endo_extract$pixel_id)
  
  
  endo_prices<-inner_join(suit_harvest_country,prices_subset,by="crop")
  
  
  endo_prices_return<<-rbind(endo_prices_return,endo_prices)%>%filter(pixel_id==882956)
  
  endo_prices<-endo_prices%>%
    mutate(yield=gaez_attainable_yield_2000)%>%
    #revenues based on wb prices conv real
    mutate(#revenuewbr_portoilconvpi=(price_wb_real_conv-crudeoil_real/45 *0.4*40*2/20/60*minimum_travel_time_to_large_or_medium_port_minutes)*yield*production_index,
      #revenue=(price_end-crudeoil_real/45 *0.4*40*2/20/60*minimum_travel_time_to_large_or_medium_port_minutes)*yield*production_index)%>%
      revenue=(price_end-crudeoil_real/45 *0.4*40*2/20/60*minimum_travel_time_to_large_or_medium_port_minutes)*yield)%>%
    #remove all (keep yield as separate share that's weighted across crops below)
    dplyr::select(pixel_id,year,revenue,
                  crop,harvested_share_gaez)%>%
    #create price metrics per grid!
    group_by(pixel_id,year)%>%
    #now create weighted average 
    mutate(across(starts_with(c("revenue")), ~ sum(.x*harvested_share_gaez, na.rm = TRUE)))%>%
    #divide by discount rate 
    mutate(across(starts_with(c("revenue")),  ~ (.x/0.05)))%>%
    ungroup()%>%
    dplyr::select(-crop,-harvested_share_gaez)%>%
    distinct()
  
  #print(paste0("endo_prices"))
  #print(endo_prices)
 
  
  #6. save new price to be adjusted in the following year and new supply quantity
  supply_addition<-supply_addition%>%
    dplyr::mutate(year=ye,
                  cp_id=as.numeric(cp),
                  mc_id=as.numeric(mc))
  
  
  
  def_new_cropsupply<<-def_new_cropsupply%>%
    dplyr::mutate(additional_crop_supply=as.numeric(additional_crop_supply),
                  year=as.numeric(year))%>%
    dplyr::full_join(supply_addition)
  
  #print(paste0("def_new_cropsupply"))
  #print(def_new_cropsupply)
  
  
  price_addition<-prices_subset%>%
    dplyr::select(crop,price_end,year)%>%
    dplyr::rename(price_prev=price_end)%>%
    dplyr::mutate(cp_id=as.numeric(cp),
                  mc_id=as.numeric(mc))
  
  
  def_new_price<<-def_new_price%>%
    dplyr::full_join(price_addition)
  
  supply_shocks<-supply_shocks%>%mutate(year=ye)
  
  supply_shock_return<<-supply_shock_return%>%
    dplyr::full_join(supply_shocks)
  
  endo_prices2<-endo_prices%>%
    dplyr::mutate(cp_id=as.numeric(cp),
                  mc_id=as.numeric(mc))
  
 
  
  endog_revenue<<-endog_revenue%>%
    filter(pixel_id %in% endo_extract$pixel_id)%>%
    dplyr::full_join(endo_prices2)
  
  
  
  
  return(endo_prices)
  

}


predict_defor_singleP_countryFE <- function(coefs, gamma_df = NULL, delta_df = NULL, model_family, new_data, cp_id,
                                             alpha = NULL, beta = NULL){
  # Purpose:
  # - Computes predicted deforestation
  #
  # Parameters:
  # - coefs: Tidy (broom) coefficients from model
  # - gamma_df: Desired pixel-level fixed effects
  # - delta_df: Desired country fixed effects
  # - model_family: Family used in original model
  # - new_data: Data to be used for prediction
  # - cp_id: carbon price index (NOTE: the notation is somewhat misleading, as this is not an index, but a single price)
  # - alpha: pre-extracted revenue coefficient (optional; extracted from coefs if NULL)
  # - beta: pre-extracted treecover coefficient (optional; extracted from coefs if NULL)
  #
  # Output:
  # - pred_data: new_data with new columns for linear and non-linear predictions

  # Define coefs (use pre-extracted values if provided to avoid repeated filtering)
  if (is.null(alpha)) alpha <- (coefs %>% filter(term == "revenue") %>% pull(estimate))[1]
  if (is.null(beta))  beta  <- (coefs %>% filter(term == "remaining_treecover_share") %>% pull(estimate))[1]
  #lmbda = (coefs %>% filter(term == "remaining_treecover_share_sq") %>% pull(estimate))[1]
  
  # Join in fixed effects
  new_data <- new_data %>% 
    lazy_dt()
  # new_data <- new_data %>% 
  #   # mutate(year_country = paste(as.character(year), country, sep = "_")) %>% 
  #   left_join(delta_df, by = "country") %>% 
  #   left_join(gamma_df, by = "pixel_id")
  
  # Compute prediction.
  # The carbon opportunity cost (cp_id * CF) is subtracted inline and never written
  # back to the revenue column, so starting_data carries the original revenue into
  # the next simulation year — preventing spurious compounding of the subtraction.
  pred_data <- new_data %>%
    mutate(linear_pred = (revenue - cp_id * CF) * alpha + remaining_treecover_share * beta +
             pixel_fe + yc_fe,
           pred = model_family$linkinv(linear_pred))
  
  return(pred_data %>% as_tibble())
}


predict_defor <- function(coefs, gamma_df, delta_df, model_family, new_data,cp_id){
  # Purpose:
  # - Computes predicted deforestation
  #
  # Parameters:
  # - coefs: Tidy (broom) coefficients from model
  # - gamma_df: Desired pixel-level fixed effects
  # - delta_df: Desired country-by-year fixed effects
  # - model_family: Family used in original model 
  # - new_data: Data to be used for prediction
  # - cp_id: carbon price index
  #
  # Output:
  # - pred_data: new_data with new columns for linear and non-linear predictions
  
  # Define coefs
  alpha = (coefs %>% filter(term == "revenue") %>% pull(estimate))[1]
  beta = (coefs %>% filter(term == "remaining_treecover_share") %>% pull(estimate))[1]
  #lmbda = (coefs %>% filter(term == "remaining_treecover_share_sq") %>% pull(estimate))[1]
  
  # Join in fixed effects
  new_data <- new_data %>% 
    lazy_dt()
  new_data <- new_data %>% 
    mutate(year_country = paste(as.character(year), country, sep = "_")) %>% 
    left_join(delta_df, by = "year_country") %>% 
    left_join(gamma_df, by = "pixel_id")
  
  
  
  
  # # Prep function for inverse link
  # link_inv <- partial(.f = model_family$linkinv)
  
  # Compute prediction
  pred_data <- new_data %>% 
    #Suggestion for CPrice:
    mutate(cp_id=cp_id)%>%
    mutate(revenue0=revenue,
           revenue=revenue-cp_id*CF)%>%
    mutate(linear_pred = revenue * alpha + remaining_treecover_share * beta + 
             #remaining_treecover_share_sq * lmbda + 
             pixel_fe + yc_fe,
           pred = model_family$linkinv(linear_pred))%>%
    # NOTE: Seems inefficient to be running this for P=0 in all runs
    mutate(linear_pred0 = revenue0 * alpha + remaining_treecover_share * beta + 
             #remaining_treecover_share_sq * lmbda + 
             pixel_fe + yc_fe,
           pred0 = model_family$linkinv(linear_pred0))
  
  
  
  
  return(pred_data %>% as_tibble())
}


mc_predictions <- function(coefs, model_family, fe_randomizer, years_df, starting_data, mc_id,cp_id,price_scenario){
  # Purpose:
  # - Runs full monte carlo iteration
  #
  # Parameters:
  # - coefs: Tidy (broom) coefficients from model
  # - model_family: Family used in original model 
  # - fe_randomizer: Partialized instance of redraw_fes function for pixel level fixed effects
  # - years_df: Dataframe of nested dataframes of year by country fixed effects
  # - starting_data: Initial conditions to use to iterate simulation
  # - mc_id: index for this MC run
  # - cp_id: index for carbon price - maybe enough if above in new_data?
  # - price_scenario: "exogenous","endogenous", "fixed","fixedpi"
  
  # Output:
  # - defor_predictions: tibble describing total predicted deforestation in each pixel_id
  # - opp_cost_data: opportunity cost calculation input
  
  print(paste0("Running monte carlo iteration: ", as.character(mc_id)))
  print(paste0("Carbon price: ", as.character(cp_id)))
  
  # Prep data for first prediction
  starting_data = starting_data %>% 
    mutate(forest_2020 = lag_remaining_treecover,  # Save starting forest area to compute total forest loss after iteration
           lag_remaining_treecover = lag_remaining_treecover - def, # Take out deforestation occurring in 2020 to calculate deforestation at start of 2021
           #lag_remaining_treecover_sq = lag_remaining_treecover**2, 
           remaining_treecover_share=lag_remaining_treecover/pixel_count,
           #remaining_treecover_share_sq=remaining_treecover_share**2,
           year = 2021) 
  
  if(price_scenario=="exogenous"){
    print("exogenous price feedback")
    starting_data<-starting_data%>%
      dplyr::select(-revenue)%>%
      dplyr::left_join(exo_prices)
  }
  
  if(price_scenario=="fixedpi"){
    print("fixedpi price feedback")
    starting_data<-starting_data%>%
      dplyr::select(-revenue)%>%
      dplyr::left_join(fixed_prices_pi)
  }
  
  
  
  if(price_scenario=="fixed"){
    print("fixed price feedback")
    starting_data<-starting_data%>%
      dplyr::select(-revenue)%>%
      dplyr::left_join(fixed_prices)
  }
  
  
  # Make this iteration's draw of pixel-level fixed effects
  fe_draw = fe_randomizer()
  
  
  
  # Start iteration through years here
  for (y in seq(2021, 2030)){
    print(paste0("year ", y))
    # Add year's deltas to predicter
    delta_df = (years_df %>% 
                  filter(year == y) %>% 
                  pull(yc_fes))[[1]]
    defor_predicter = partial(predict_defor, coefs = coefs, model_family = model_family, gamma_df = fe_draw, delta_df = delta_df,cp_id=cp_id)
    
    # Make prediction
    predicted_data <- defor_predicter(new_data = starting_data)
    
    
    #print(paste0("predicted data"))
    #print(unique(predicted_data$year))
    #print(head(predicted_data))
    
    
    
    alpha_ori = (coefs %>% filter(term == "revenue") %>% pull(estimate))[1]
    beta = (coefs %>% filter(term == "remaining_treecover_share") %>% pull(estimate))[1]
   
    
    options(digits=16)
    input_opp<-predicted_data%>%
      dplyr::select(pixel_id, year, pred,pred0,lag_remaining_treecover,pixel_count,revenue,revenue0,pixel_fe,yc_fe,linear_pred,CF,CF_pix)%>%
      distinct()%>%
      ungroup()%>%
      rowwise()%>%
      mutate(FE=pixel_fe + yc_fe, 
             alpha_ori=alpha_ori,
             beta=beta,
             R=revenue,
             R0=revenue0,
             kappa=lag_remaining_treecover/pixel_count)%>%
      ungroup()%>%
      mutate(mult_gamma = pred0/(1-kappa*(1-pred0)))%>%
      rowwise()%>%
      mutate(alpha = alpha_ori*mean(mult_gamma))%>%
      mutate(z0=kappa*(1-pred0),
             z1=kappa*(1-pred),
             #opportunity_cost_old = ((z1*log((z0*(1-z1))/(z1*(1-z0)))+log(1-z1)+0.5*(3-4*z1)/(1-z1)^2)-(log(1-z0)+0.5*(3-4*z0)/(1-z0)^2))/(kappa*alpha),
             #Q_kappa_test=Q_kappa(FE,a,R0,kappa),
             #p0_test= pred_defor(FE,a,R0,kappa),
             #p1_test= pred_defor(FE,a,R,kappa),
             #opportunity_cost=OC(FE,a,R0,R,kappa),
             #opportunity_cost=-(pred*cp_id*CF+log((1-pred0)/(1-pred0))/alpha_ori),
             count_def_pred=pred*lag_remaining_treecover,
             count_def_pred0=pred0*lag_remaining_treecover,
             count_def_delayed=count_def_pred0-count_def_pred,
             count_forestcover_endofyear= lag_remaining_treecover * (1- pred),
             #new adjustments Nov 19, 2024
             ha_per_pixel = CF_pix/CF,
             opp_costs_triangle=0.5*cp_id*CF_pix*(count_def_delayed),  
             opp_cost_empirics=-ha_per_pixel*lag_remaining_treecover*(pred*cp_id*CF+log((1-pred0)/(1-pred))/alpha_ori),##note: here we do not multiply with interest rate, but do this in the aggregation, for AI version this must be done in each loop!
             opp_cost_theory = lag_remaining_treecover*((1-z1)*log((1-z1)/(1-z0))+z1*log(z1/z0))/(kappa*alpha)
             )%>%
      ungroup()
    

    #input_opp<-predicted_data%>%
    #  dplyr::select(pixel_id, year, pred,pred0,lag_remaining_treecover,pixel_count,revenue,revenue0,pixel_fe,yc_fe,linear_pred)%>%
    #  distinct()%>%
    #  ungroup()%>%
    #  mutate(kappa=lag_remaining_treecover/pixel_count,
    #         alpha=alpha,
    #         mu=revenue * alpha + pixel_fe + yc_fe, 
    #         mu0=revenue0 * alpha + pixel_fe + yc_fe,
    #         #p=pred,
    #         #p0=pred0,
    #         #based on equation 4
    #         p=1-(1/kappa)*(1/(1+exp(mu))),#Idea: calculate p based on equation 4 - problem: for small kappas p becomes negative - Q kappa does not solve then
    #         p0=1-(1/kappa)*(1/(1+exp(mu0))),
    #         x_ori=1-p,
    #         x_ori0=1-p0,
    #         #calculate quantile and assess if negative or 0
    #         #if negative or 0, set p to 0 --> x=1
    #         Q_kappa0=mu0+log(kappa/(1-kappa)), #p should be the cumulated probability... unclear what we use best for this? even if we collect probabilities from previous ts, this is not considering for the fact that this share is relative to remaining forest cover!
    #         #Q_kappa0=mu0+log(kappa/(1-kappa)), #kappa seems to be in line with the theory, unsure if it really fits with the model specification
    #         x=if_else(Q_kappa0>0 | lag_remaining_treecover==pixel_count,x_ori,1),#take original p, if 100% forest cover, then Qkappa should be theoretically infinity
    #         x0=if_else(Q_kappa0>0 | lag_remaining_treecover==pixel_count,x_ori0,1),
    #         kappa_x=kappa*x,
    #         kappa_x0=kappa*x0,
    #         opportunity_cost_calc = (1/alpha)*(x*(log((1-kappa_x)/kappa_x)-mu0)-(1/kappa)*log(1-kappa_x)),
    #         opportunity_cost_calc=if_else(lag_remaining_treecover==0,0,opportunity_cost_calc),#note, a true 0 is only the case, if has been 0 already in 2021
    #         opportunity_cost_calc0 = (1/alpha)*(x0*(log((1-kappa_x0)/kappa_x0)-mu0)-(1/kappa)*log(1-kappa_x0)),
    #         opportunity_cost_calc0=if_else(lag_remaining_treecover==0,0,opportunity_cost_calc0),#note, a true 0 is only the case, if has been 0 already in 2021
    #         opportunity_cost=opportunity_cost_calc-opportunity_cost_calc0,
    #         count_def_pred=pred*lag_remaining_treecover
    #         )
    ##note: with this calculation, the resulting OC are pixel_id specific, but the same value for every year
    
    
    # Create starting data for next iteration
    if(y==2021){
      opp_cost_data<<-input_opp
    }else{
    opp_cost_data <<- opp_cost_data%>%
      full_join(input_opp)}
    
    print(unique(opp_cost_data$year))
    

    
    
    
    if(price_scenario=="endogenous"){
      print("endogenous price feedback")
      endo_extract<-predicted_data%>%
        mutate(count_def_pred=pred*lag_remaining_treecover)
      
      endo_prices<-endog_prices(endo_extract,endo_input_data,elas_pivot,prod_oil,suit_harvest_country,ye=y,cp=cp_id,mc=mc_id)%>%
        dplyr::select(-year)
      
      
      #replace revenue for next year
      predicted_data <- predicted_data %>%
        dplyr::select(-revenue)%>%
        dplyr::left_join(endo_prices)
      

      
    }
    
    predicted_data <- predicted_data %>% 
             mutate(lag_remaining_treecover = lag_remaining_treecover * (1- pred), # Compute remaining forest cover after deforestation
             #lag_remaining_treecover_sq = lag_remaining_treecover**2,
             remaining_treecover_share=lag_remaining_treecover/pixel_count,
             #remaining_treecover_share_sq=remaining_treecover_share**2,
             revenue = revenue, 
             year = y + 1# Update year counter for next iteration
             ) 
    
    
    #for exogenous price feedback
    if(price_scenario=="exogenous"){
      print("exogenous price feedback")
      
      
      predicted_data <- predicted_data %>% 
        dplyr::select(-revenue)%>%
        dplyr::left_join(exo_prices)
      
    }
   
     #for fixedpi price feedback
    if(price_scenario=="fixedpi"){
      print("fixedpi price feedback")
      
      
      predicted_data <- predicted_data %>% 
        dplyr::select(-revenue)%>%
        dplyr::left_join(fixed_prices_pi)
      
    }
    
    if(price_scenario=="fixed"){
      print("fixed price feedback")
      
      predicted_data <- predicted_data %>% 
        dplyr::select(-revenue)%>%
        dplyr::left_join(fixed_prices)
      
    }
    
    starting_data = predicted_data %>% 
      select(-c(year_country, yc_fe, pixel_fe, linear_pred, linear_pred0,pred,pred0))
  }
  
  #nest opportunity costs
  opp_cost_data_nested<-opp_cost_data%>%
    group_by(pixel_id)%>%
    mutate(opp_costs_triangle=sum(opp_costs_triangle),
           opp_cost_empirics=sum(opp_cost_empirics),
           opp_cost_theory=sum(opp_cost_theory),
           count_def_pred=sum(count_def_pred),
           count_def_pred0=sum(count_def_pred0),
           count_def_delayed=sum(count_def_delayed),
           count_forestcover_endofyear=sum(count_forestcover_endofyear))%>%
    ungroup()%>%
    dplyr::select(pixel_id,opp_costs_triangle,opp_cost_empirics,opp_cost_theory,count_def_pred,count_def_pred0,count_def_delayed,count_forestcover_endofyear
                  )%>%
    distinct()

  paymentflows<-opp_cost_data%>%dplyr::select(pixel_id,year,count_def_pred,CF_pix,count_def_delayed,count_forestcover_endofyear,opp_cost_empirics)%>%
    mutate(i=0.05,
           taxloan=CF_pix*count_def_pred*cp_id*((1-(1/(1+i))^(2031-year))/((1+i)^(year-2021))),
           tax=CF_pix*count_def_pred*cp_id*(1/(1+i)^(year-2021)), 
           fi_pes=CF_pix*count_def_delayed*cp_id*(i/(1+i))*(1/(1+i)^(year-2021)), 
           all_pes=CF_pix*count_forestcover_endofyear*cp_id*(i/(1+i))*(1/(1+i)^(year-2021)),
           opp_cost_discounted=opp_cost_empirics*(i/(1+i))*(1/(1+i)^(year-2021)),
           social_benefits=CF_pix*count_def_delayed*193*(i/(1+i))*(1/(1+i)^(year-2021))
           )%>%
    group_by(pixel_id)%>%
    #now sum up over time
    summarise(TAXLOAN=sum(taxloan),
              TAX=sum(tax),
              FI_PES=sum(fi_pes),
              ALL_PES=sum(all_pes),
              OPP_COST_DISC=sum(opp_cost_discounted),
              SOCIAL_BENEFITS=sum(social_benefits)
                                   )
  
  opp_cost_data_nested<-opp_cost_data_nested%>%left_join(paymentflows)
  
  
  # Calculate grid-cell level tree cover loss
  defor_predictions <- predicted_data %>% 
    mutate(defor = forest_2020 -def - lag_remaining_treecover)%>% #should this be -def? to be consistent with above? and result in same as count_def_pred?
    select(pixel_id, defor)%>%
    left_join(opp_cost_data_nested)

  # return(defor_predictions)
  
  return(nest(defor_predictions))

}

#### Main functions for simulation workflow
simulate_10 <- function(defor_predicter, starting_data, cp, years) {
  n_years <- length(years)
  results_list <- vector("list", n_years)

  for (i in seq_along(years)) {
    y <- years[i]

    predicted_data <- defor_predicter(new_data = starting_data, cp_id = cp)

    predicted_data <- predicted_data %>%
      mutate(
        lag_remaining_treecover   = lag_remaining_treecover * (1 - pred),
        remaining_treecover_share = lag_remaining_treecover / pixel_count,
        year = y + 1
      )

    results_list[[i]] <- predicted_data %>%
      select(pixel_id, year, remaining_treecover_share, pixel_count)

    starting_data <- predicted_data %>%
      select(-c(linear_pred, pred))
  }

  dplyr::bind_rows(results_list) %>%
    arrange(pixel_id, year)
}



update_revenues <- function(endo_input_data, new_prices) {
  endo_prices <- endo_input_data %>%
    left_join(new_prices %>% dplyr::select(crop, price_end), by = "crop") %>%
    mutate(revenue=(price_end-crudeoil_real/45 *0.4*40*2/20/60*minimum_travel_time_to_large_or_medium_port_minutes)*yield) %>%
    #remove all (keep yield as separate share that's weighted across crops below)
    dplyr::select(pixel_id,revenue,
                  crop,harvested_share_gaez)%>%
    #create price metrics per grid!
    group_by(pixel_id)%>%
    #now create weighted average 
    mutate(across(starts_with(c("revenue")), ~ sum(.x*harvested_share_gaez, na.rm = TRUE)))%>%
    #divide by discount rate 
    mutate(across(starts_with(c("revenue")),  ~ (.x/0.05)))%>%
    ungroup()%>%
    dplyr::select(-crop,-harvested_share_gaez)%>%
    distinct()
  return(endo_prices)
}

calc_trap_area <- function(a,b,h) {
  trap_area = ((a + b) * h) / 2 
  return(trap_area)
}


fv_to_pv <- function(fv, year) {
  pv <- fv / ((1 + iota) ^ (year - 2021))  # NOTE: Should PV year be 2021 or 2020?
  return(pv)
}


simulate_10_dt <- function(starting_data, cp, revenue_col, fe_col, years, alpha, beta, linkinv) {
  # Purpose:
  # - Fast data.table-based forward simulation replacing simulate_10() for the parallelized workflow.
  #   Produces identical results to simulate_10() but avoids repeated tibble allocation inside the
  #   101-year loop by using data.table in-place mutation (`:=`). At 1.5M pixels this is ~2x faster
  #   than the dplyr/lazy_dt approach because no new data frame is allocated each year.
  #
  # Parameters:
  # - starting_data: data.frame with one row per pixel. Must contain lag_remaining_treecover,
  #     remaining_treecover_share, pixel_count, CF, yc_fe, and the columns named by revenue_col and fe_col.
  # - cp: numeric carbon price scalar for this scenario.
  # - revenue_col: name of the column holding pre-computed revenue for this scenario (e.g. "revenue_cp80").
  # - fe_col: name of the pixel fixed-effect column to use ("pixel_fe" for fi, "ai_pixel_fe" for ai).
  # - years: integer vector of simulation years (e.g. 2020:2120). Output year is year + 1.
  # - alpha, beta: pre-extracted model coefficient scalars (revenue and treecover terms).
  # - linkinv: inverse link function from model$family$linkinv (e.g. logistic function).
  #
  # Output:
  # - data.table with columns (pixel_id, year, remaining_treecover_share, pixel_count),
  #   one row per pixel per year (years[1]+1 through years[end]+1).

  # Copy to avoid modifying the caller's data; cast to data.table for in-place operations.
  dt <- data.table::copy(data.table::as.data.table(starting_data))

  # Pre-compute net revenue ONCE: agricultural return minus carbon opportunity cost.
  # This is constant for all simulation years within a given scenario — the endogenous
  # crop price (captured in revenue_col) and the carbon density (CF) do not change over time.
  # Computing it here rather than inside the loop prevents the spurious compounding that
  # would occur if we subtracted cp*CF on every iteration.
  dt[, revenue_adj := get(revenue_col) - cp * CF]

  results <- vector("list", length(years))

  for (i in seq_along(years)) {
    # Predict deforestation probability for each pixel using the logit model.
    dt[, pred := linkinv(revenue_adj * alpha + remaining_treecover_share * beta +
                           get(fe_col) + yc_fe)]

    # Update state for the next year BEFORE storing results (matches simulate_10 ordering).
    dt[, lag_remaining_treecover := lag_remaining_treecover * (1.0 - pred)]
    dt[, remaining_treecover_share := lag_remaining_treecover / pixel_count]
    dt[, pred := NULL]

    # Store this year's post-deforestation forest cover (output year = simulation year + 1)
    results[[i]] <- dt[, .(pixel_id, year = years[i] + 1L,
                            remaining_treecover_share, pixel_count)]
  }

  data.table::rbindlist(results)
}