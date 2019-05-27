# Base Functions

# Function for Mode
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# unit root test
purtest_function = function(dataset, variable, lags, exo) {
  print(variable)
  for (i in 1:length(unique(dataset$country))) {
    dataset2check = dataset %>% 
      filter(country == unique(dataset$country)[i]) %>% 
      select(country, year_id, var2check = variable)
    
    dataset_plm <- pdata.frame(data.frame(dataset2check), index=c("country", "year_id"))
    
    parameter = round(purtest(dataset_plm$var2check, test="hadri", exo=exo, lags = lags)$statistic$p.value, 2)
    
    print(paste(unique(dataset$country)[i], 
                parameter)
    )
    
  }
}


# detrending function
detrending = function(dataset, year, variable){
  frame = data.frame(country="test", year =as.factor(year))
  
  if (length(na.omit(dataset))!=0 ) {
    dataset = data.frame(detrend_var = dataset,
                         year = year,
                         country = "test") 
    dataset = pdata.frame(dataset, index=c("country", "year"))
    
    detrend_result = plm(detrend_var ~ as.numeric(year) + I(as.numeric(year)^2) , 
                         dataset, model="pooling")
    
    results = data.frame(year=index(detrend_result)[,2], resid = detrend_result$residuals) %>% 
      full_join(frame, by="year")
    return(results$resid)
  } else {
    return(NA)
  }
  
}



# Summary Table to Excel


results_to_excel = function(reg_obj, filename, PCSE = F) {
  if (PCSE == F) {
    print("No PCSEs")
    results_df = data.frame(varnames = rownames(round(summary(reg_obj)$coefficients, 3)),
                            round(summary(reg_obj)$coefficients, 3))
  } else {
    coef_obj = data.frame(as.matrix.data.frame(round(coeftest(re_mod_cluster_1st, vcov=vcovBK),3))) %>% 
      rename("Estimate" = 1,
             "Std..Error"  = 2,
             "t.value"  = 3,
             "Pr...t.." = 4)
    
    
    results_df = data.frame(varnames = rownames(round(summary(reg_obj)$coefficients, 3)),
                            coef_obj)
                            
  }
  results_df = results_df %>% 
    rename(prb = 5) %>% 
    mutate(Std.Er = paste("(", Std..Error, ")", sep="")) %>%  # xx must be removed in Excel
    mutate(Sign = if_else(prb < 0.001, "***", 
                          if_else(prb < 0.01, "**", 
                                  if_else(prb < 0.05, "*", " ")))) %>%
    mutate(Estimate = paste(Estimate, Sign, sep="")) %>% 
    mutate(Estimate = paste(Estimate, Std.Er, sep=" ")) %>% 
    dplyr::select(varnames, Estimate) 
  
  file = paste("Results/", filename, ".csv", sep="")
  write.csv2(results_df, file=file, row.names = T)
}

