
generateTableBrms_GA = function(..., prob_interval = 0.95, modelnames = NULL) {
  options(scipen=999)
  digits = 2
  
  make_table_brms = function(brms_model, prob_interval) {
    
    raw_table = broom.mixed::tidy(brms_model, conf.level = prob_interval, conf.int = T, conf.method = c("HPDinterval"))
    
    mod_table = raw_table %>% 
      select(term, Estimates = estimate, HPD_lower = conf.low, HPD_upper = conf.high) %>% 
      mutate(term = if_else(term == "(Intercept)", "Intercept", term),
             term = if_else(term == "sd__Observation", "Residual", term))  %>% 
      mutate_if(is.numeric, funs(sprintf("%1.2f", round(.,digits)))) %>% 
      mutate(Estimates = paste(Estimates, "<br/>", "(", HPD_lower, ", ", HPD_upper,")", sep="")) %>% 
      select(Predictors = term, Estimates)
    
    # Create Final Dataset
    
    # Separator
    Separator_Residuals = data.frame(Predictors = " <strong><i>Residuals")
    Separator_Random = data.frame(Predictors = " <strong><i>Random Effects")
    Separator_Random = data.frame(Predictors = " <strong><i>Random Effects")
    
    # Number of Observations
    NumberObs = data.frame(Predictors = "Num. obs.",
                           Estimates =  as.character(dim(brms_model$data)[1]))
    
    
    
    mytable = mod_table %>% 
      bind_rows(NumberObs) 
    print(mytable)
    
    return(mytable) 
  }
  
  sum_obj = list(...)
  
  dust_table = lapply(sum_obj, FUN = function(X) make_table_brms(X, prob_interval = prob_interval))
  
  # First Column is model with most predictors
  max_predictors = which(unlist(lapply(dust_table, FUN = function(X) dim(X)[1])) == max(unlist(lapply(dust_table, FUN = function(X) dim(X)[1]))))
  dust_data = data.frame("Predictors" = dust_table[[max_predictors[1]]]$Predictors)  %>% 
    na.omit()
  
  for (i in 1:length(dust_table)) {
    if (is.null(modelnames) == T ) {
      colnames(dust_table[[i]])[2] = paste("M", i, sep="")
    } else {
      colnames(dust_table[[i]])[2] = modelnames[i]
    }
    dust_table[[i]] = na.omit(dust_table[[i]])
    dust_data = full_join(dust_data, dust_table[[i]], by="Predictors")
  }
  
  dust_data_bottom = dust_data %>% 
    filter(Predictors == "Residual" | Predictors == "Num. obs.")
  dust_data_FKM = dust_data %>% 
    filter(grepl("FKM", Predictors))
  dust_data_odempr = dust_data %>% 
    filter(grepl("odempr", Predictors)) 
  
  dust_data = dust_data %>% 
    filter(Predictors != "Residual") %>% 
    filter(Predictors != "Num. obs.") %>%  
    filter(grepl("FKM", Predictors) == F)  %>%  
    filter(grepl("odempr", Predictors) == F) %>%
    bind_rows(dust_data_FKM)  %>%
    bind_rows(dust_data_odempr) %>%
    bind_rows(dust_data_bottom)
  
  # NAs for the Separators
  for (i in 2:dim(dust_data)[2]) {
    dust_data[,i] = ifelse(grepl("Residuals", dust_data$Predictors), NA, dust_data[,i])
    dust_data[,i] = ifelse(grepl("Random Effects", dust_data$Predictors), NA, dust_data[,i])
    
  }
  
  # print(dust_data)
  
  dust_data = dust_data %>%
    #relabel
    mutate(Predictors = gsub("_vdem", "", Predictors),
           Predictors = gsub("_cpds", "", Predictors),
           Predictors = gsub("_cbi", "", Predictors),
           Predictors = gsub("_odempr", "", Predictors),
           Predictors = gsub("_wdi", "", Predictors),
           Predictors = gsub("_vi", "", Predictors),
           Predictors = gsub("_cat_ctl", "", Predictors),
           Predictors = gsub("_pr_ctl", "", Predictors),
           Predictors = gsub("_num_ctl", "", Predictors),
           Predictors = gsub("_ctl", "", Predictors),
           Predictors = gsub("_wi", "<sub>wi</sub>", Predictors),
           Predictors = gsub("_lag", "<sub>,t-1", Predictors),
           Predictors = gsub("_spatial", "<sub>spatial", Predictors),
           Predictors = gsub("_bw", "<sub>bw", Predictors),
           Predictors = gsub("FKM5_", "", Predictors),
           Predictors = gsub("FKM4", "Dim", Predictors))
  
  # Dust Table
  mytable_final = dust_data %>% 
    dust() %>% 
    
    sprinkle(border = c("bottom"), part=c("head")) %>%
    sprinkle(halign="center", part="head") %>%
    #sprinkle(halign="center", part="body") %>%
    
    sprinkle(rows = 1, border = "top",  border_thickness=2) %>%
    sprinkle(cols = 2:dim(dust_data)[2], border = "left",  border_thickness=2, part="head") %>%
    sprinkle(cols = 2:dim(dust_data)[2], border = "left",  border_thickness=2) %>%
    
    sprinkle(halign="center") %>%
    sprinkle(valign="middle") %>%
    
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, halign="center", font_size_units = "pt", part="body") %>% 
    sprinkle(cols = 1, halign="left", part="body") %>% 
    
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
  
  return(mytable_final)
  
}
