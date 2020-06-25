# wealth_bayes_panelhet = brm(
#   bf(
#     wealth_eco ~ 
#       wealth_eco_lag +
#       trend + 
#       FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_wi + FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_bw +
#       (1|country_text_id)),
#   data = wealth_list[[1]], family = gaussian(),
#   prior = prior_unit_tscs,
#   warmup = 2000, iter = 7000,
#   chains = 6, 
#   control = list(adapt_delta = 0.95)
# )
# 
# summary(wealth_bayes_panelhet$data)
# newdata = data.frame(
#   scenario = "positive",
#   #wealth_eco_lag = 1 ,
#   country_text_id = "SWE",
#   FKM_5_mb_Fec.FKM_5_mb_tot_FecT_wi = -0.0152450  
# )
# 
# newdata2 = data.frame(
#   scenario = "negative",
#   #wealth_eco_lag = 1 ,
#   country_text_id = "GBR",
#   FKM_5_mb_Fec.FKM_5_mb_tot_FecT_wi = -0.0152450  
# )
# 
# 
# 
# dynamicsim(wealth_bayes_panelhet, newdata, newdata2, lagVar="wealth_eco_lag", timepoints = 30)

dynamicsim = function(brms_model, ..., lagVar, timepoints = 20, simulations = 1000) {
  
  make_plot_data = function(newdata, scenario) {
    mean_data = brms_model$data %>% 
      group_by(country_text_id) %>% 
      summarise_all(mean, na.rm=T) %>% 
      filter(country_text_id == newdata$country_text_id)
    
    newdata = newdata %>% 
      bind_cols(mean_data[colnames(mean_data) %!in% colnames(newdata)]) 
    
    print("Your Dataframe:")
    print(newdata)
    
    newdata_frame = newdata %>% 
      dplyr::slice(rep(1:n(), each = simulations))
    
    ppc_value = data.frame(array(NA, dim=c(simulations, timepoints)))

    for (i in 1:timepoints) {
      if(i %% 10==0) {
        # Print on the screen some message
        cat(paste0(i, ".."))
      }
      
      ppc_value[,i] = as.numeric(posterior_predict(brms_model, newdata = newdata_frame, nsamples=1))
      
      newdata_frame[lagVar] = ppc_value[,i]
    }
    
    print("")
    
    mean_values = ppc_value %>% 
      summarise_all(funs(mean = mean(., na.rm=T))) %>% 
      mutate(stat = "mean") %>% 
      pivot_longer(cols=starts_with("X"), values_to = "mean_y") %>% 
      mutate(time = 1:timepoints) %>% 
      select(-name)
    
    lower_values = ppc_value %>% 
      summarise_all(funs(lower = quantile(., prob=0.025, na.rm=T)))  %>% 
      mutate(stat = "lower") %>% 
      pivot_longer(cols=starts_with("X"), values_to = "lower") %>% 
      mutate(time = 1:timepoints) %>% 
      select(-name)
    upper_values = ppc_value %>% 
      summarise_all(funs(upper = quantile(., prob=0.975, na.rm=T))) %>% 
      mutate(stat = "upper") %>% 
      pivot_longer(cols=starts_with("X"), values_to = "upper") %>% 
      mutate(time = 1:timepoints) %>% 
      select(-name)
    
    final_data = mean_values %>% 
      bind_cols(lower_values) %>% 
      bind_cols(upper_values) %>% 
      mutate(scen = newdata$scenario[1])

    return(final_data)   
  }
  
  my_dfs = list(...)
  combined_data = data.frame()
  for (i in 1:length(my_dfs)) {
    combined_data = bind_rows(combined_data,
                              make_plot_data(my_dfs[[i]], i))
  }
  
  
  final_plot = combined_data %>% 
    mutate(scen = as.factor(scen)) %>% 
    ggplot(aes(x=time, y=mean_y, ymin=lower , ymax=upper, fill=scen)) +
    geom_line(size=1.1) +
    geom_ribbon(alpha=0.5) +
    theme_bw()
  
  return(final_plot)
}

