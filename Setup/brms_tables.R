# BRMS Table ####

#broom.mixed::tidyMCMC(wealth_control1_lag1, conf.level = 0.95, conf.int = T, conf.method = c("HPDinterval"))



generateTableBrms = function(..., prob_interval = 0.95, modelnames = NULL) {
  options(scipen=999)
  digits = 3
  
  make_table_brms = function(brms_model, prob_interval) {
    if (dim(broom::tidy(brms_model,
                        par_type = c("all"),
                        prob = prob_interval) %>%
            filter(term == "sigma"))[1] != 0) {
      
      residuals_brms = broom::tidy(brms_model,
                                   par_type = c("all"),
                                   prob = prob_interval) %>%
        filter(term == "sigma")  %>% 
        mutate_if(is.numeric, funs(sprintf("%1.3f", round(.,digits)))) %>% 
        mutate(CI_95 = paste(lower, "-", upper)) %>% 
        select(Predictors = term, Estimates = estimate, CI_95) %>% 
        mutate(Predictors = ifelse(grepl("sigma", Predictors), "&epsilon;", Predictors)) 
      
    } else  {
      residuals_brms = broom::tidy(brms_model,
                                   par_type = c("non-varying"),
                                   prob = prob_interval) %>% 
        mutate(estimate = sprintf("%1.3f", round(estimate, digits)),
               lower  = sprintf("%1.3f", round(lower, digits)),
               upper = sprintf("%1.3f", round(upper, digits)),
        ) %>% 
        mutate(CI_95 = paste(lower, "-", upper)) %>% 
        filter(grepl("sigma_", term))  %>% 
        select(Predictors = term, Estimates = estimate, CI_95) %>% 
        mutate(Predictors = ifelse(grepl("sigma_Intercept", Predictors), "&epsilon;", Predictors))%>% 
        mutate(Predictors = ifelse(grepl("sigma", Predictors), paste(gsub("sigma_", "", Predictors), "<sub>&epsilon;", sep=""), Predictors)) 
      
    }
    
    fix_effects = broom::tidy(brms_model,
                              par_type = c("non-varying"),
                              prob = prob_interval) %>% 
      mutate(estimate = sprintf("%1.3f", round(estimate, digits)),
             lower  = sprintf("%1.3f", round(lower, digits)),
             upper = sprintf("%1.3f", round(upper, digits)),
      ) %>% 
      mutate(CI_95 = paste(lower, "-", upper)) %>% 
      filter(!grepl("sigma_", term))  %>% 
      select(Predictors = term, Estimates = estimate, CI_95) %>% 
      # zzz_ for ordering
      mutate(Predictors = ifelse(grepl("sigma", Predictors), paste("zzz_", Predictors, sep=""), Predictors)) %>% 
      #arrange(Predictors) %>% 
      mutate(Predictors = gsub("zzz_", "", Predictors)) %>% 
      mutate(Predictors = ifelse(grepl("sigma", Predictors), paste(gsub("sigma_", "", Predictors), "<sub>&sigma;", sep=""), Predictors)) 
    
    # Check for Random Effects
    if (dim(broom::tidy(brms_model,
                        par_type = c("all"),
                        prob = prob_interval) %>% 
            select(term) %>% 
            filter(grepl("sd", term)))[1] != 0 ) {
      random_effects = broom::tidy(brms_model,
                                   par_type = c("hierarchical"),
                                   prob = prob_interval) %>% 
        mutate(estimate = sprintf("%1.3f", round(estimate, digits)),
               lower  = sprintf("%1.3f", round(lower, digits)),
               upper = sprintf("%1.3f", round(upper, digits)),
        ) %>% 
        filter(!grepl("cor_", term)) %>% 
        mutate(CI_95 = paste(lower, "-", upper)) %>% 
        select(Predictors = term, Estimates = estimate, CI_95)
      
      random_effects_ctry = random_effects %>% 
        filter(grepl("country_text_id", Predictors)) %>% 
        filter(!grepl("sigma", Predictors)) 
      names_sd = strsplit(random_effects_ctry$Predictors, "__")
      if (length(names_sd) != 0) {
        for (i in 1:length(names_sd)) {
          random_effects_ctry$Predictors[i] = paste("&sigma;", "<sub>country: ", names_sd[[i]][2], sep="")
        }
        
      }
      
      random_effects_year = random_effects %>% 
        filter(grepl("year_0", Predictors))  %>% 
        filter(!grepl("sigma", Predictors)) %>% 
        filter(!grepl("country", Predictors))
      
      names_sd = strsplit(random_effects_year$Predictors, "__")
      if (length(names_sd) != 0) {
        for (i in 1:length(names_sd)) {
          random_effects_year$Predictors[i] = paste("&sigma;", "<sub>year: ", names_sd[[i]][2], sep="")
        }
      }
      
      
      random_effects_sigma = random_effects %>% 
        filter(grepl("sigma", Predictors))
      
      names_sd = strsplit(random_effects_sigma$Predictors, "__")
      if (length(names_sd) != 0) {
        for (i in 1:length(names_sd)) {
          random_effects_sigma$Predictors[i] = paste("&sigma;", "<sub>country: ", names_sd[[i]][2], sep="")
        }
      }
    } else {
      random_effects_ctry = data.frame("Predictors" = NA, "Estimates"= NA, "CI_95"= NA)
      random_effects_year = data.frame("Predictors"= NA, "Estimates"= NA, "CI_95"= NA)
      random_effects_sigma = data.frame("Predictors"= NA, "Estimates"= NA, "CI_95"= NA)
      
    }
    
    # Create Final Dataset
    
    # Separator
    Separator_Residuals = data.frame(Predictors = " <strong><i>Residuals")
    Separator_Random = data.frame(Predictors = " <strong><i>Random Effects")
    Separator_Random = data.frame(Predictors = " <strong><i>Random Effects")
    
    # Number of Observations
    NumberObs = data.frame(Predictors = "Num. obs.",
                           Estimates =  as.character(dim(brms_model$data)[1]))
    
    
    
    mytable = fix_effects  %>% 
      bind_rows(Separator_Residuals) %>% 
      bind_rows(residuals_brms) %>% 
      bind_rows(Separator_Random) %>% 
      bind_rows(random_effects_ctry) %>% 
      bind_rows(random_effects_year) %>% 
      bind_rows(random_effects_sigma) %>% 
      mutate(Estimates = paste(Estimates, " (", CI_95, ")", sep="")) %>% 
      select(-CI_95) %>% 
      bind_rows(NumberObs) 
    print(mytable)

   return(mytable) 
  }
  
  
  
  sum_obj = list(...)
  
  dust_table = lapply(sum_obj, FUN = function(X) make_table_brms(X, prob_interval = prob_interval))

  # First Column is model with most predictors
  max_predictors = which(unlist(lapply(dust_table, FUN = function(X) dim(X)[1])) == max(unlist(lapply(dust_table, FUN = function(X) dim(X)[1]))))
  dust_data = data.frame("Predictors" = dust_table[[max_predictors]]$Predictors)  %>% 
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
           Predictors = gsub("_bw", "<sub>bw", Predictors))

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

# LOO Table ####

loo_table = function(loo_compare_object) {
  loo_table = loo_compare_object %>% 
    as.data.frame() %>% 
    mutate(LOO =  paste(round(elpd_diff, 2), " (",round(se_diff, 2), ")", sep="")) %>%
    mutate(rownames = rownames(.)) %>% 
    select("Model" = rownames, LOO) 
  
  
  loo_table$Model = loo_compare_object %>% 
    as.data.frame() %>% 
    rownames()

  loo_table %>% 
    dust()  %>% 
    
    sprinkle(border = c("bottom"), part=c("head")) %>%
    sprinkle(halign="center", part=c("head")) %>%
    sprinkle(halign="center", part=c("body")) %>%
    
    sprinkle(rows = 1, border = "top",  border_thickness=2) %>%
    sprinkle(cols = 2, border = "left",  border_thickness=2, part=c("head")) %>%
    sprinkle(cols = 2, border = "left",  border_thickness=2) %>%
    
    sprinkle(halign="center") %>%
    sprinkle(valign="middle") %>%
    
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
}


# Regression Table


make_brms_reg_table = function(..., prob_interval=0.95, digits=2) {
  make_single_table = function(brms_model_tid, prob_interval, digits) {
    Single_table = broom.mixed::tidyMCMC(brms_model_tid, conf.level = prob_interval, conf.int = T, conf.method = c("HPDinterval")) %>% 
      rename(Predictors = term) %>% 
      filter(grepl("b_", Predictors) | grepl("sd_", Predictors) ) %>% 
      mutate(estimate = sprintf("%1.3f", round(estimate, digits)),
             conf.low  = sprintf("%1.3f", round(conf.low, digits)),
             conf.high = sprintf("%1.3f", round(conf.high, digits)),
      ) %>% 
      mutate(estimate = paste(estimate, "<br/>(", conf.low, " - ", conf.high, ")", sep="")) %>% 
      select(-std.error, -conf.low, -conf.high) 
    
    fixef = Single_table %>% 
      filter(grepl("sd_", Predictors) == F & grepl("_sigma_", Predictors) == F)
    
    ranef = Single_table %>% 
      filter(grepl("sd_", Predictors)) %>% 
      filter(grepl("_sigma_", Predictors) == F)
    
    residualef = Single_table %>% 
      filter(grepl("_sigma_", Predictors) )
    
    
    
    # Number of Observations
    NumberObs = data.frame(Predictors = "Num. obs.",
                           estimate =  as.character(dim(brms_model_tid$data)[1]))
    
    
    
    
    return(list(fixef, ranef, residualef, NumberObs))
  }
  
  sum_obj = list(...)
  
  dust_table = lapply(sum_obj, FUN = function(X) make_single_table(X, prob_interval = prob_interval, digits))
  
  # Separator
  Separator_Residuals = data.frame(Predictors = " <strong><i>Residuals")
  Separator_Random = data.frame(Predictors = " <strong><i>Random Effects")
  
  
  comb_fixef = dust_table[[1]][[1]]
  comb_ranef = dust_table[[1]][[2]]
  comb_resid = dust_table[[1]][[3]]
  comb_nobs = dust_table[[1]][[4]]
  
  for (i in 2:length(dust_table)) {
    comb_fixef = comb_fixef %>% 
      full_join(dust_table[[i]][[1]], by="Predictors")
    comb_ranef = comb_ranef %>% 
      full_join(dust_table[[i]][[2]], by="Predictors")
    comb_resid = comb_resid %>% 
      full_join(dust_table[[i]][[3]], by="Predictors")
    comb_nobs = comb_nobs %>% 
      full_join(dust_table[[i]][[4]], by="Predictors")
  }
  
  dust_table_final = comb_fixef %>% 
    bind_rows(Separator_Random) %>% 
    bind_rows(comb_ranef) %>% 
    bind_rows(Separator_Residuals) %>% 
    bind_rows(comb_resid) %>% 
    bind_rows(comb_nobs)
  
  colnames(dust_table_final)[-1] = paste("M", 1:length(dust_table), sep="")
  
  
  
  dust_table_final = dust_table_final %>%
    #relabel
    mutate(Predictors = gsub("b_", "", Predictors),
           Predictors = gsub("FKM4_", "Dim ", Predictors),
           Predictors = gsub("FKM5_", "", Predictors),
           Predictors = gsub("_vdem", "", Predictors),
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
           Predictors = gsub("sd_", "sd<sub>", Predictors),
           Predictors = gsub("__Intercept", "", Predictors),
           Predictors = gsub("country_text_id__sigma", "", Predictors),
           Predictors = gsub("_df", "&Delta;", Predictors),
           
           Predictors = gsub("sd_Intercept", "sd<sub>Intercept", Predictors)
    )
  
  # Dust Table
  mytable_final = dust_table_final %>% 
    dust() %>% 
    
    sprinkle(border = c("bottom"), part=c("head")) %>%
    sprinkle(halign="center", part="head") %>%
    #sprinkle(halign="center", part="body") %>%
    
    sprinkle(rows = 1, border = "top",  border_thickness=2) %>%
    sprinkle(cols = 2:dim(dust_table_final)[2], border = "left",  border_thickness=2, part="head") %>%
    sprinkle(cols = 2:dim(dust_table_final)[2], border = "left",  border_thickness=2) %>%
    
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
