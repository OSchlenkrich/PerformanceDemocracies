# dnyamic simulation

# get means of all numeric variables
all_means_numeric = femod_between$model %>% 
  summarise_if(is.numeric, mean) %>% 
  melt() %>% 
  filter(variable != "Gini_lag")


quantile(femod_between$model$cso_lag , 0.95)
quantile(femod_between$model$cso_lag , 0.05)

# create scenario
high_scenario = data.frame(variable = names(coef(femod_between)), coef = coef(femod_between), row.names = NULL) %>% 
  left_join(all_means_numeric) %>% 
  mutate(value = if_else(variable=="family_name_short_lagLeft", 1, value),
         result = coef * value) %>% 
  summarise(all_coefs = mean(result, na.rm=T)) %>% 
  pull(all_coefs)

# create scenario
low_scenario = data.frame(variable = names(coef(femod_between)), coef = coef(femod_between), row.names = NULL) %>% 
  left_join(all_means_numeric) %>% 
  mutate(value = if_else(variable=="family_name_short_lagLeft", 0, value),
         result = coef * value) %>% 
  summarise(all_coefs = mean(result, na.rm=T)) %>% 
  pull(all_coefs)


gini_mean = mean(femod_between$model$Gini, na.rm=T)
Timep = max(as.numeric(GINI_plm$year_id))

get_timepoints = function(Scenario_type_data, name_scenario) {
  my_results = array(NA, dim=c(Timep, 1000))
  my_results[1,] = rnorm(1000, coef(femod_between)[1] + Scenario_type_data + coef(femod_between)[2]*gini_mean,sd(femod_between$residuals))
  
  for(i in 2:Timep) {
    my_results[i,] = rnorm(1000,
                           coef(femod_between)[1] + Scenario_type_data + coef(femod_between)[2]*mean(my_results[i-1,]),
                           sd(femod_between$residuals)  )
  }
  plot_data = data.frame( 
    y_mean = apply(my_results, 1, mean),
    y_min = apply(my_results, 1, FUN=function(x) quantile(x, 0.025)),
    y_max = apply(my_results, 1, FUN=function(x) quantile(x, 0.0975)),
    x = 1:Timep
  ) %>% 
    mutate(Scenario =name_scenario)
  
  return(plot_data)  
}





get_timepoints(high_scenario, "high") %>% 
  bind_rows(get_timepoints(low_scenario, "low")) %>% 
  ggplot(aes(y = y_mean, ymin=y_min, ymax=y_max, x=x, fill=Scenario)) + 
  geom_ribbon() +
  ylim(26,32)
