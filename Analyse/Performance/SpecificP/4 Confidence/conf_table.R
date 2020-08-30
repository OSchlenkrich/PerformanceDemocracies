# For GLMMTMB
make_glmm_tables = function(..., rsquared = F) {

  make_glmm_data = function(model) {
    library(broom.mixed)
    library(DataCombine)
    library(pixiedust)
    
    
    first_row = data.frame(term = "Fixed Part", Estimate = NA)
    tab1_FE = first_row %>%
      bind_rows(
        broom.mixed::tidy(model) %>% 
          filter(effect != "ran_pars") %>% 
          select(-component,-group) %>% 
          mutate_at(vars(estimate, std.error), funs(round(., 3))) %>% 
          mutate("sig" = ifelse(p.value < 0.001, "***", 
                                ifelse(p.value < 0.01, "**",
                                       ifelse(p.value < 0.05, "*",
                                              ifelse(p.value < 0.1, "+", "")))))  %>% 
          # mutate_if(is.numeric, funs(round(.,3))) %>% 
          mutate(std.error = paste("(", std.error, ")", sep="")) %>% 
          select(term, estimate, std.error, sig) %>% 
          unite("Estimate", estimate, sig, sep=" ") %>% 
          unite("Estimate", Estimate, std.error, sep="<br>")         
      )
    
    
    first_row = data.frame(term = "Random Part", Estimate = NA)
    tab1_RE = first_row %>% 
      bind_rows(
        broom.mixed::tidy(model) %>% 
          filter(effect != "fixed")  %>% 
          mutate_at(vars(estimate), funs(round(., 3))) %>%
          unite("term",group, term) %>% 
          select(term, Estimate = estimate)  
      )
    
    tab1 = tab1_FE %>% 
      rbind(tab1_RE)
    
    summary_model = summary(model)
    if (rsquared == T) {
      r_squared = r2(model)
      if (length(summary_model$ngrps$cond) == 2) {
        tab1_foot = glance(model) %>% 
          select(logLik, AIC , BIC, df.residual) %>% 
          mutate(
            'R2 (cond.)' = r_squared$R2_conditional,
            'R2 (marg.)' = r_squared$R2_marginal,
            N_Obs = summary_model$nobs,
            N_Group1 = summary_model$ngrps$cond[1],
            N_Group2 = summary_model$ngrps$cond[2]) %>% 
          pivot_longer(cols=everything()) %>% 
          mutate(value = round(value, 2))
      } else {
        tab1_foot = glance(model) %>% 
          select(logLik, AIC , BIC, df.residual) %>% 
          mutate(
            'R2 (cond.)' = r_squared$R2_conditional,
            'R2 (marg.)' = r_squared$R2_marginal,
            N_Obs = summary_model$nobs,
            N_Group = summary_model$ngrps$cond[1]) %>% 
          pivot_longer(cols=everything()) %>% 
          mutate(value = round(value, 2))
      }  
    } else {
      if (length(summary_model$ngrps$cond) == 2) {
        tab1_foot = glance(model) %>% 
          select(logLik, AIC , BIC, df.residual) %>% 
          mutate(
            # 'R2 (cond.)' = r_squared$R2_conditional,
            # 'R2 (marg.)' = r_squared$R2_marginal,
            N_Obs = summary_model$nobs,
            N_Country_text_id = summary_model$ngrps$cond[1]) %>% 
          pivot_longer(cols=everything()) %>% 
          mutate(value = round(value, 2))
      } else {
        tab1_foot = glance(model) %>% 
          select(logLik, AIC , BIC, df.residual) %>% 
          mutate(
            # 'R2 (cond.)' = r_squared$R2_conditional,
            # 'R2 (marg.)' = r_squared$R2_marginal,
            N_Obs = summary_model$nobs,
            N_Countries = summary_model$ngrps$cond[1]) %>% 
          pivot_longer(cols=everything()) %>% 
          mutate(value = round(value, 2))
      }
    }
    
    return(list(tab1, tab1_foot))
  }
  
  
  sum_obj = list(...)
  nr_models = length(sum_obj)
  
  dust_tables = lapply(sum_obj, make_glmm_data)
  
  
  tab_full = data.frame(term = dust_tables[[2]][[1]]$term)
  for (i in 1:nr_models) {
    tab_full = tab_full %>% 
      select_at(vars(-matches("xvar"))) %>% 
      group_by(term) %>% 
      full_join(data.frame(term = dust_tables[[i]][[1]]$term, xvar = dust_tables[[i]][[1]]$term) %>% 
                  mutate(term = gsub("I\\(", "", term),
                         term = gsub("\\^2)", "", term)), by="term") %>% 
      ungroup() %>% 
      mutate(term = ifelse(is.na(xvar) == F, as.character(xvar), term))
  }
  
  
  
  
  
  randommixed = tab_full[(which(grepl("Random",tab_full$term))):length(tab_full$term),]
  tab_full = tab_full[-((which(grepl("Random",tab_full$term))):length(tab_full$term)),]
  FKMs = randommixed[which(grepl("FKM",randommixed$term)),]
  GDPs = randommixed[which(grepl("gdppc",randommixed$term)),]
  
  odempr = randommixed[which(grepl("odempr",randommixed$term)),]
  
  randommixed = randommixed[-which(grepl("FKM",randommixed$term)),]
  randommixed = randommixed[-which(grepl("gdppc",randommixed$term)),]
  
  tab_full = tab_full %>% 
    bind_rows(GDPs) %>% 
    bind_rows(FKMs) %>% 
    bind_rows(odempr)  %>% 
    bind_rows(randommixed)
  
  tab_full = data.frame(term = tab_full$term)
  tab_full_foot = data.frame(name = dust_tables[[nr_models]][[2]]$name)
  for (i in 1:nr_models) {
    tab_full = tab_full %>% 
      left_join(dust_tables[[i]][[1]], "term") 
    tab_full_foot = tab_full_foot %>% 
      left_join(dust_tables[[i]][[2]], "name") 
    
  }
  
  # delete duplicate rows
  tab_full = tab_full %>% 
    ungroup() %>% 
    distinct()
  
  colnames(tab_full)[-1] = paste("M", 1:nr_models, sep="")
  tab_full = tab_full %>% 
    mutate(term = gsub("_caus", "", term),
           term = gsub("_num_ctl", "", term),
           term = gsub("FKM5_", "", term),
           term = gsub("FKM4_", "Dim_", term),
           term = gsub("__.*", "", term),
           term = gsub("_sd", "<sub>sd", term),
           term = gsub("_wdi", "<sub>wdi", term),
           term = gsub("_fh", "<sub>fh", term),
           term = gsub("_wb", "<sub>wb", term),
           term = gsub("_gle", "<sub>gle", term),
           term = gsub("_pt", "<sub>pt", term),
           term = gsub("(Intercept)", "Intercept", term),
           term = gsub("I\\(", "", term),
           term = gsub("\\^2)", "<sub>sq", term),
           term = gsub("_ivs_ctl", "", term),
           term = gsub("_odempr", "", term),
           term = gsub("_ord", "", term))
  
  
  
  tab_full_foot = tab_full_foot %>% 
    mutate(name = gsub("_", "<sub>", name))
  
  italic = c(which(tab_full$term == "Fixed Part"), which(tab_full$term == "Random Part"))
  
  
  dust_obj = dust(tab_full, glance_foot = F) %>% 
    redust(tab_full_foot, part="foot") %>% 
    #Colnames
    sprinkle_colnames(term = "Term") %>%
    
    sprinkle(rows = 1, border = "top") %>%
    sprinkle(rows = 1, border = "top", part = "foot") %>%
    
    # font size
    sprinkle(rows = italic, border = "top", border_color = "black", italic = TRUE) %>% 
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="foot") %>% 
    
    #NA values
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
  
  return(dust_obj)  
  
}
