# Base Functions

# Function for Mode
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# unit root test
purtest_function = function(dataset, variable, lags) {
  print(variable)
  for (i in 1:length(unique(dataset$country))) {
    dataset2check = dataset %>% 
      filter(country == unique(dataset$country)[i]) %>% 
      select(country, year_id, var2check = variable)
    
    dataset_plm <- pdata.frame(data.frame(dataset2check), index=c("country", "year_id"))
    
    parameter = round(purtest(dataset_plm$var2check, test="hadri", exo="intercept", lags = lags)$statistic$p.value, 2)
    
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
