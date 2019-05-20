# dnyamic simulation

# get means of all numeric variables
all_means_numeric = femod_between$model %>% 
  summarise_if(is.numeric, mean) %>% 
  melt() 

quantile(femod_between$model$gdp_capita_lag2, 0.95)

# create scenario
high_scenario = data.frame(variable = names(coef(femod_between)), row.names = NULL) %>% 
  left_join(all_means_numeric) %>% 
  mutate(value = if_else(variable=="union_density", 39.7659706185, value),
         value = if_else(variable=="mod_cluster_1stFEC", 1, value),
         value = if_else(is.na(value)==T,0,value),
         value = if_else(variable=="(Intercept)", 1, value)) 

# create scenario
low_scenario = data.frame(variable = names(coef(femod_between)), row.names = NULL) %>% 
  left_join(all_means_numeric) %>% 
  mutate(value = if_else(variable=="union_density", 39.7659706185, value),
         value = if_else(variable=="mod_cluster_1stFec", 0, value),
         value = if_else(is.na(value)==T,0,value),
         value = if_else(variable=="(Intercept)", 1, value))  


gini_mean = mean(femod_between$model$Gini, na.rm=T)
Timep = max(as.numeric(GINI_plm$year_id))
library(mvtnorm)


get_timepoints = function(Scenario_type_data, name_scenario) {
  my_results = array(NA, dim=c(Timep, 1000))
  
  coef_mat = rmvnorm(1000, coef(femod_between), vcovBK(femod_between))
  
  my_results[1,] =  coef_mat %*% Scenario_type_data$value
  
  for(i in 2:Timep) {
    
    Scenario_type_data = Scenario_type_data %>% 
      mutate(value = if_else(variable=="Gini_lag", mean( my_results[i-1,]), value))
    
    my_results[i,] =  coef_mat %*% Scenario_type_data$value
  }
  
  
  plot_data = data.frame( 
    y_mean = apply(my_results, 1, FUN=function(x) quantile(x, 0.5)),
    y_min = apply(my_results, 1, FUN=function(x) quantile(x, 0.05)),
    y_max = apply(my_results, 1, FUN=function(x) quantile(x, 0.95)),
    x = 1:Timep
  ) %>% 
    mutate(Scenario =name_scenario)
  
  return(plot_data)  
}


get_timepoints(high_scenario, "Fec") %>% 
  bind_rows(get_timepoints(low_scenario, "fEc")) %>% 
  ggplot(aes(y = y_mean, ymin=y_min, ymax=y_max, x=x, fill=Scenario)) + 
  geom_line() +
  geom_ribbon(alpha=0.5) +
  ylab("Predict Value") +
  xlab("t")


