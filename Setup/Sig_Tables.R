# Significance Tables with pixiedust
library(pixiedust)
library(DataCombine)
#########


# Create Summary Table with Significance
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
            N_Variables = summary_model$ngrps$cond[1],
            N_Country_text_id = summary_model$ngrps$cond[2]) %>% 
          pivot_longer(cols=everything()) %>% 
          mutate(value = round(value, 2))
      } else {
        tab1_foot = glance(model) %>% 
          select(logLik, AIC , BIC, df.residual) %>% 
          mutate(
            # 'R2 (cond.)' = r_squared$R2_conditional,
            # 'R2 (marg.)' = r_squared$R2_marginal,
            N_Obs = summary_model$nobs,
            N_Variables = summary_model$ngrps$cond[1],
            N_Countries = summary_model$ngrps$cond[2]) %>% 
          pivot_longer(cols=everything()) %>% 
          mutate(value = round(value, 2))
      }
    }
    
    return(list(tab1, tab1_foot))
  }
  
  
  sum_obj = list(...)
  nr_models = length(sum_obj)
  
  dust_tables = lapply(sum_obj, make_glmm_data)
  
  
  tab_full = data.frame(term = dust_tables[[nr_models]][[1]]$term)
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

# FOR DIRICHLET
make_table_diri = function(..., oddsRatios = F) {
  library(Compositional)
  insertRow_f <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }
  
  make_dust_data = function(model_obj) {
    sum_obj = summary(model_obj)

    vars_per_profile = ((sum_obj$npar-1)/(length(sum_obj$varnames)-1) )-1
    
    summary_table = data.frame(
      term = rownames(sum_obj$coef.mat),
      sum_obj$coef.mat,
      stringsAsFactors = F,
      row.names = NULL)

    if (oddsRatios == T){
      summary_table$Estimate[-length(summary_table$Estimate)] = exp( summary_table$Estimate[-length(summary_table$Estimate)] )
      
    }

    summary_table_gaze = summary_table %>% 
      bind_cols(
        "sig" = ifelse(summary_table$Pr...z.. < 0.001, "***", 
                       ifelse(summary_table$Pr...z.. < 0.01, "**",
                              ifelse(summary_table$Pr...z.. < 0.05, "*",
                                     ifelse(summary_table$Pr...z.. < 0.1, "+", ""))))
      )  %>% 
      mutate_if(is.numeric, funs(round(.,2))) %>% 
      mutate(Std..Error = paste("(", Std..Error, ")", sep="")) %>% 
      select(term, Estimate, Std..Error, sig) %>% 
      unite("Estimate", Estimate, sig, sep=" ") %>% 
      unite("Estimate", Estimate, Std..Error, sep="<br>")
    
    
    # Change Intercept to Intercept + DemocracyProfile
    rownames = rownames(sum_obj$coef.mat)
    border_intercept = which(rownames == "(Intercept)")
    
    y_names = sum_obj$varnames
    y_names = gsub("X_", "", y_names )

    summary_table_gaze$term[border_intercept[-which(border_intercept == max(border_intercept))]] = paste("Intercept", y_names[2:length(y_names)])
    summary_table_gaze$term[-border_intercept] = paste(summary_table_gaze$term[-border_intercept], rep(y_names[2:length(y_names)], each = vars_per_profile))

    # change last intercept to phi
    summary_table_gaze$term[max(border_intercept)] = "phi"
    
    # Profiles Separator  
    new = matrix( cbind(y_names[-1], ""), nrow=length(y_names[-1]), ncol=2)
    adder = 1
    
    summary_table_gaze = rbind(new[1,], summary_table_gaze)
    border_intercept_adapted = array(NA, (length(y_names[-1])-1))
    for (i in 2:length(y_names[-1])) {
      
      border_intercept_adapted[i-1] = border_intercept[i] + adder
      
      summary_table_gaze = InsertRow(summary_table_gaze, 
                                     NewRow = new[i,], 
                                     RowNum = border_intercept[i] + adder)
      adder = adder + 1
    }
    
 
    # Create Custom Foot with AIC, BIC, N etc.
    LL = round(sum_obj$logLik,2)
    AIC = round(sum_obj$aic,2)
    R2 = round(totvar(fitted(model_obj))/totvar(model_obj$Y),2)
    #BIC = round(sum_obj$bic,2) // works bad compared to AIC in dirichlet context (own simulation)
    N = sum_obj$nobs
    
    foot_matrix = data.frame(name = rbind("LL",
                                          "AIC",
                                          # "R2",
                                          #"BIC", 
                                          "N", 
                                          paste("Ref.:", y_names[1])), 
                             values= rbind(LL, 
                                           AIC,
                                           # R2,
                                           #BIC, 
                                           N,
                                           ""))
    
    
    return(list(summary_table_gaze, foot_matrix, border_intercept=border_intercept_adapted))
  }
  ####

  # Input into list
  sum_obj = list(...)
  
  # Apply make_dust_data to list with objects
  dust_data = lapply(sum_obj, make_dust_data)  
  
  
  # Create main DF
  dust_data_join = dust_data[[1]][[1]] 
  if (length(dust_data) > 1) {
    for (i in 2:length(dust_data)) {
      dust_data_join = dust_data_join %>% 
        full_join(dust_data[[i]][[1]], by="term")
    }
  }
  
  dust_data_join = dust_data_join %>% 
    separate(term, c("term", "profile"), sep=" ", fill="left") %>% 
    arrange(profile) %>% 
    unite(term, "term", "profile") %>% 
    mutate(term = gsub("NA_", "", term))
  
  #Rename Columns and Rows
  colnames(dust_data_join)[-1] = paste("M", 1:length(dust_data), sep="")

  # dust_data_join$term = gsub("_Fec", "", dust_data_join$term) 
  # dust_data_join$term = gsub("_fEc", "", dust_data_join$term) 
  # dust_data_join$term = gsub("_fEC", "", dust_data_join$term) 
  # dust_data_join$term = gsub("_FeC", "", dust_data_join$term) 
  dust_data_join$term = gsub("mp_Cluster4_", "", dust_data_join$term) 
  dust_data_join$term = gsub("mp_Cluster5_", "", dust_data_join$term) 
  dust_data_join$term = gsub("_caus_FEC", "", dust_data_join$term, ignore.case = T) 
  dust_data_join$term = gsub("Intercept_FEC", "Intercept", dust_data_join$term, ignore.case = T) 
  
  ####
  # Create foot DF
  foot_data_join = dust_data[[1]][[2]] 
  if (length(dust_data) > 1) {
    for (i in 2:length(dust_data)) {
      foot_data_join = foot_data_join %>% 
        full_join(dust_data[[i]][[2]], by="name")
    }  
  }
  foot_data_join$name = gsub("mp_Cluster4_", "", foot_data_join$name) 
  foot_data_join$name = gsub("mp_Cluster5_", "", foot_data_join$name) 
  
  border_intercept = dust_data_join %>% 
    select(term) %>% 
    filter(grepl("phi", term) == F) %>% 
    summarise(rows = n()/(length(sum_obj[[1]]$varnames) - 1))
  border_intercept = cumsum(rep(border_intercept$rows, length(sum_obj[[1]]$varnames) - 2)) +1
    
  

  ####
  # Create Table with pixiedust
  dust(dust_data_join, glance_foot = F)  %>% 
    redust(foot_data_join, part="foot") %>% 
    # rename
    sprinkle_colnames("term" = "Term") %>%
    
    sprinkle(halign = "center", part=c("head")) %>% 
    
    sprinkle(cols = 2:length(dust_data), halign = "center", valign = "middle", part=c("body")) %>%
    sprinkle(cols = 2:length(dust_data), halign = "center", part=c("foot")) %>% 
    
    sprinkle(rows = 1, italic = TRUE) %>%  
    sprinkle(rows = border_intercept, border = "top", border_color = "black", italic = TRUE) %>% 
    sprinkle(rows = dim(dust_data_join)[1], border = "top", border_color = "black") %>% 
    
    sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
    sprinkle(rows=1, border = "top", part = "foot", border_thickness=2)  %>% 
    sprinkle(rows=dim(foot_data_join)[1], border = "top", part = "foot", bold = TRUE, border_thickness=1)  %>% 
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="foot") %>% 
    # NAs
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
}


# For Rendering in knitr, see Setup/knitR_tab/table_word.Rmd
# my_table = make_table_diri(m1, m2, m3, m4, m5, m6, m7)

# Factor Table
fa_table = function(fa_model) {
  my_omega = omega(fa_model$r, nfactors=fa_model$factors, fm="mle", option="second", plot=F)
  print(my_omega)
  
  dust_df = data.frame(items = row.names(fa_model$loadings),
                       unclass(fa_model$loadings),
                       h2 = round(fa_model$communalities,3)) %>% 
    mutate(items = gsub("_num_eco", "", items)) %>%
    arrange(-.[,2]) %>% 
    mutate_if(is.numeric, funs(round(.,3)))  %>% 
    mutate(h2 = as.character(h2)) %>% 
    mutate_if(is.numeric, funs(ifelse(abs(.) > 0.3, paste("<b>", ., sep=""), .))) 
  
  if (fa_model$factors == 1) {
    dust_sub_df = data.frame(fitmeasure = "Proportion var:", value = fa_model$Vaccounted[2,1]) %>%
      bind_rows(data.frame(fitmeasure = "&omega;<sup>total", value = my_omega$omega.tot)) %>% 
      mutate_if(is.numeric, funs(round(.,3))) 
  } else {
    dust_sub_df = data.frame(fitmeasure = "Proportion Variance:", value = fa_model$Vaccounted[2,1]) %>%
      bind_rows(data.frame(fitmeasure = "RMSEA:", value = fa_model$RMSEA[1]) ) %>% 
      #bind_rows(data.frame(fitmeasure = "BIC:", value = fa_model$BIC)) %>% 
      #bind_rows(data.frame(fitmeasure = "TLI:", value = fa_model$TLI)) %>% 
      bind_rows(data.frame(fitmeasure = "&omega;<sup>total", value = my_omega$omega.tot)) %>% 
      bind_rows(data.frame(fitmeasure = "&omega;<sup>group", value = round(my_omega$omega.group[2,2],3))) %>% 
      mutate_if(is.numeric, funs(round(.,3))) 
    

    if (fa_model$factors > 1) {
      for (i in 2:fa_model$factors) {
        dust_sub_df = dust_sub_df %>% 
          mutate(!!paste(i) := c(round(fa_model$Vaccounted[2,i],3), NA, NA, round(my_omega$omega.group[2,i+1],3))) 
      }
    }

  }
  
  dust_sub_df = dust_sub_df %>%
    mutate(X = NA) %>% 
    data.frame()
  
  dust_df = dust_df %>% 
    mutate(items = gsub("_num_eco", "", items),
           items = gsub("_num_soc", "", items),
           items = gsub("_num_env", "", items),
           items = gsub("_num_ds", "", items))
  
  fa_table = dust(dust_df)  %>% 
    redust(dust_sub_df, part="foot")  %>%
    sprinkle_colnames(h2 = "h<sup>2") %>% 
    #borders
    sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
    sprinkle(rows=1, border = "top", part = "foot", border_thickness=2)  %>%
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="foot") %>% 
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_na_string(na_string = "", part="foot") %>% 
    
    sprinkle_print_method("html")
  return(fa_table)
}



# Factor Table UMX
fa_table_umx = function(fa_solution, RMSEA, TLI, my_omega) {
  
  loadings_umx = loadings(fa_solution)

  if (dim(loadings(fa_solution))[2] == 1) {
    dust_df = data.frame(varnames = row.names(loadings_umx),
                         F1 = unclass(loadings_umx[,1]), 
                         h2 = (unclass((loadings_umx[,1])^2)), row.names = NULL) %>% 
      mutate(varnames = gsub("_ord_ivs", "", varnames)) %>%
      arrange(-.[,2]) %>% 
      mutate_if(is.numeric, funs(round(.,3)))  %>% 
      mutate(h2 = as.character(h2)) %>% 
      mutate_if(is.numeric, funs(ifelse(abs(.) > 0.3, paste("<b>", ., sep=""), .)))
  }
  if (dim(loadings(fa_solution))[2] == 2) {
    dust_df = data.frame(varnames = row.names(loadings_umx),
                         F1 = unclass(loadings_umx[,1]),
                         F2 = unclass(loadings_umx[,2]), 
                         h2 = (unclass((loadings_umx[,1])^2)) + (unclass((loadings_umx[,2])^2)), row.names = NULL) %>% 
      mutate(varnames = gsub("_ord_ivs", "", varnames)) %>%
      arrange(-.[,2]) %>% 
      mutate_if(is.numeric, funs(round(.,3)))  %>% 
      mutate(h2 = as.character(h2)) %>% 
      mutate_if(is.numeric, funs(ifelse(abs(.) > 0.3, paste("<b>", ., sep=""), .)))
  }
  dust_sub_df = data.frame(fitmeasure = "Proportion var:", value = sum(loadings_umx^2)/length(loadings_umx)) %>%
    bind_rows(data.frame(fitmeasure = "RMSEA:", value = RMSEA) ) %>% 
    #bind_rows(data.frame(fitmeasure = "TLI:", value = TLI)) %>%
    #bind_rows(data.frame(fitmeasure = "alpha", value = alpha(fa_solution$data$observed, check.keys=TRUE, n.iter=10)$total$std.alpha)) %>% 
    bind_rows(data.frame(fitmeasure = "&omega;<sup>total", value = my_omega$omega.tot)) %>% 
    mutate_if(is.numeric, funs(round(.,3))) 
  
  for (i in 1:dim(loadings(fa_solution))[2]) {
      dust_sub_df = dust_sub_df %>% 
        mutate(!!paste(i) := NA) 
      }
  
  dust_sub_df = dust_sub_df %>%
    #mutate(X = NA) %>% 
    data.frame()
  
  print(dust_sub_df)
  
  fa_table = dust(dust_df)  %>% 
    redust(dust_sub_df, part="foot")  %>%
    sprinkle_colnames(h2 = "h<sup>2") %>% 
    #borders
    sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
    sprinkle(rows=1, border = "top", part = "foot", border_thickness=2)  %>%
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="foot") %>% 
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_na_string(na_string = "", part="foot") %>% 
    
    sprinkle_print_method("html")
  return(fa_table)
}



# Anova Table
make_anova_table = function(anova_output, truenames = T, diri = F) {
  if (diri == T) {
    anova_output = data.frame(Df = anova_output$df,
                              deviance = anova_output$Deviance,
                              "Chisq" = anova_output$Difference,
                              "Pr(>Chisq)" = anova_output$`Pr(>Chi)`,
                              logLik = NA, 
                              Chi.Df = NA) %>% 
      rename("Pr(>Chisq)" = Pr..Chisq.) 
    
  }
  
  
  if (truenames == T) {
    modelnames = rownames(anova_output)
  } else {
    nrmodels =   dim(anova_output)[1]
    modelnames = 1:nrmodels
  }
  
  anova_table = data.frame(models = modelnames, anova_output, row.names = NULL) %>% 
    bind_cols("sig" = ifelse(anova_output$`Pr(>Chisq)` < 0.001, "***", 
                             ifelse(anova_output$`Pr(>Chisq)` < 0.01, "**",
                                    ifelse(anova_output$`Pr(>Chisq)` < 0.05, "*",
                                           ifelse(anova_output$`Pr(>Chisq)` < 0.1, "+", ""))))
    ) %>% 
    mutate_if(is.numeric, funs(round(.,2))) %>%
    mutate(sig = ifelse(is.na(sig) == F, paste(Pr..Chisq., sig), NA)) %>% 
    select(-logLik, -Chi.Df, -Pr..Chisq.) 
  
  
  dusttable = anova_table %>% 
    dust() %>%
    #rename Columns
    sprinkle_colnames(models = "Model", Df = "df",  Chisq = "Chi-Square", sig = "Sig." ) %>% 
    sprinkle(border = c("right", "left", "bottom", "top"), part=c("head"))%>%   
    sprinkle(border = c("right", "left", "bottom","top")) %>% 
    sprinkle(rows = 1, border = "top",  border_thickness=2) %>%
    
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    # na values
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
  return(dusttable)  
} 
