# BRMS Table ####

generateTableBrms = function(..., prob_interval = 0.95, modelnames = NULL) {
  
  make_table_brms = function(brms_model, prob_interval) {
    if (dim(broom::tidy(brms_model,
                        par_type = c("all"),
                        prob = prob_interval) %>%
            filter(term == "sigma"))[1] != 0) {
      
      residuals_brms = broom::tidy(brms_model,
                                   par_type = c("all"),
                                   prob = prob_interval) %>%
        filter(term == "sigma")  %>% 
        mutate_if(is.numeric, funs(round(.,3))) %>% 
        mutate(CI_95 = paste(lower, "-", upper)) %>% 
        select(Predictors = term, Estimates = estimate, CI_95) %>% 
        mutate(Predictors = ifelse(grepl("sigma", Predictors), "&epsilon;", Predictors)) 
      
    } else  {
      residuals_brms = broom::tidy(brms_model,
                                   par_type = c("non-varying"),
                                   prob = prob_interval) %>% 
        mutate(estimate = round(estimate,3),
               lower  = round(lower,2),
               upper = round(upper,2),
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
      mutate(estimate = round(estimate,3),
             lower  = round(lower,2),
             upper = round(upper,2),
      ) %>% 
      mutate(CI_95 = paste(lower, "-", upper)) %>% 
      filter(!grepl("sigma_", term))  %>% 
      select(Predictors = term, Estimates = estimate, CI_95) %>% 
      # zzz_ for ordering
      mutate(Predictors = ifelse(grepl("sigma", Predictors), paste("zzz_", Predictors, sep=""), Predictors)) %>% 
      arrange(Predictors) %>% 
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
        mutate(estimate = round(estimate,3),
               lower  = round(lower,2),
               upper = round(upper,2),
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
    
    
    
    mytable = fix_effects  %>% 
                     bind_rows(Separator_Residuals) %>% 
                     bind_rows(residuals_brms) %>% 
                     bind_rows(Separator_Random) %>% 
                     bind_rows(random_effects_ctry) %>% 
                     bind_rows(random_effects_year)%>% 
                     bind_rows(random_effects_sigma) %>% 
      mutate(Estimates = paste(Estimates, " (", CI_95, ")", sep="")) %>% 
      select(-CI_95)
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

  # Dust Table
  mytable_final = dust_data %>% 
    dust() %>% 
    
    sprinkle(border = c("bottom"), part=c("head")) %>%
    sprinkle(halign="center", part=c("head")) %>%
    
    sprinkle(rows = 1, border = "top",  border_thickness=2) %>%
    sprinkle(cols = 2:dim(dust_data)[2], border = "left",  border_thickness=2, part="head") %>%
    sprinkle(cols = 2:dim(dust_data)[2], border = "left",  border_thickness=2) %>%
    
    sprinkle(halign="center") %>%
    sprinkle(valign="middle") %>%
    
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
  
  return(mytable_final)
  
}

# LOO Table ####

loo_table = function(loo_compare_object) {
  loo_compare_object %>% 
    tidy() %>% 
    mutate(LOO =  paste(round(elpd_diff, 2), " (",round(se_diff, 2), ")", sep="")) %>% 
    select("Model" = .rownames, LOO) %>% 
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

