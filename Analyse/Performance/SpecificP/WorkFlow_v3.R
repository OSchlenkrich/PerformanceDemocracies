library(performance)
library(bbmle)
library(glmmTMB)
library(broom)

# Profiles to Test

profiles_testing = list()
profiles_testing[[1]] = "FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw"
profiles_testing[[2]] = "FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw"
profiles_testing[[3]] = "FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw"
profiles_testing[[4]] = "FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw"
profiles_testing[[5]] = "FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw"
profiles_testing[[6]] = "FKM4_E_wi + FKM4_E_wi_lag + FKM4_E_bw"
profiles_testing[[7]] = "FKM4_c_wi + FKM4_c_wi_lag + FKM4_c_bw"
# profiles_testing[[8]] = "execpar_1981_odempr + feduni1981_odempr"
# profiles_testing[[9]] = "centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw"

profiles_testing_ecm = list()
profiles_testing_ecm[[1]] = "FKM5_Fec_df_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw"
profiles_testing_ecm[[2]] = "FKM5_FeC_df_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw"
profiles_testing_ecm[[3]] = "FKM5_fEC_df_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw"
profiles_testing_ecm[[4]] = "FKM5_fEc_df_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw"
profiles_testing_ecm[[5]] = "FKM5_FEC_df_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw"
profiles_testing_ecm[[6]] = "FKM4_E_df_wi + FKM4_E_wi_lag + FKM4_E_bw"
profiles_testing_ecm[[7]] = "FKM4_c_df_wi + FKM4_c_wi_lag + FKM4_c_bw"

# Compiling via Stan
compile_stan = function(brmsformula, brmsdata, priors, warmup, iter, chains, thin = thin) {
  scode <- make_stancode(brmsformula,
                         prior = priors,
                         data = brmsdata)
  
  stanmod <- stan_model(model_code = scode, verbose = F)
  
  print(paste("START", Sys.time()))
  print(brmsformula)
  
  brms_fit = list()
  for (i in 1:length(brmsdata)) {
    print(i)
    sdata <- make_standata(brmsformula,
                           data = brmsdata[i])
    
    
    stanfit <- sampling(stanmod, data = sdata,
                        warmup=warmup, iter=iter, chains=chains, thin=thin)
    
    brms_fit[[i]] <- brm(brmsformula,
                         prior = priors,
                         data = brmsdata, empty = TRUE)
    
    brms_fit[[i]]$fit <- stanfit
    brms_fit[[i]] <- rename_pars(brms_fit[[i]])
    
  }
  print(paste("END", Sys.time()))
  return(brms_fit)
}




test_modelparameters = function(dependent_var, lagvar, brmsdata,
                                warmup = 1000, iter = 2000, chains = 6, thin = 1) {
  
  LDV_form = paste(dependent_var, lagvar, sep= " ~ ")
  Unit_form = paste(LDV_form, "(1|country_text_id)", sep = " + ")
  Time_form = paste(Unit_form, "(1|year_0)", sep = " + ")
  
  PHet_form = bf(Time_form,
                 sigma =~ (1|country_text_id))
  
  prior_ldv <- c(set_prior("cauchy(0,5)", class = "sigma"),
                      set_prior("normal(0,2)", class = "Intercept"),
                      set_prior("normal(0,100)", class = "b"),
                      set_prior("normal(0.5, 1)", class = "b", coef=lagvar))
  prior_unit <- c(set_prior("cauchy(0,5)", class = "sigma"),
                  set_prior("cauchy(0,5)", class = "sd"),
                  set_prior("normal(0,2)", class = "Intercept"),
                  set_prior("normal(0,100)", class = "b"),
                  set_prior("normal(0.5, 1)", class = "b", coef=lagvar))
  
  prior_phet <- c(set_prior("cauchy(0,5)", class = "sd"),  
                  set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),                    
                  set_prior("normal(0,2)", class = "Intercept"),
                  set_prior("normal(0,100)", class = "b"),
                  set_prior("normal(0.5, 1)", class = "b", coef=lagvar))
  
  
  model_list = list()
  
  model_list[[1]] = compile_stan(LDV_form, brmsdata, priors = prior_ldv,
                                 warmup = warmup, iter = iter, chains = chains, thin = thin)

  model_list[[2]] = compile_stan(Unit_form, brmsdata, priors = prior_unit,
                                 warmup = warmup, iter = iter, chains = chains, thin = thin)
  model_list[[3]] = compile_stan(Time_form, brmsdata, priors = prior_unit,
                                 warmup = warmup, iter = iter, chains = chains, thin = thin)

  model_list[[4]] = compile_stan(PHet_form, brmsdata, priors = prior_phet,
                                 warmup = warmup, iter = iter, chains = chains, thin = thin)

  model_list[[1]][[1]] = add_criterion(model_list[[1]][[1]], "loo")
  model_list[[2]][[1]] = add_criterion(model_list[[2]][[1]], "loo")
  model_list[[3]][[1]] = add_criterion(model_list[[3]][[1]], "loo")
  model_list[[4]][[1]] = add_criterion(model_list[[4]][[1]], "loo")
  
  return(model_list)
}




# Adds Profiles to Formula
make_brms_model = function(base_formula, addprofile = NULL, data, prior_brms, iter = 2000, warmup=1000, chains = 4, thin = 1) {
  model_maker = function(base_formula, addprofile) {
    formula = paste(base_formula)
    formula[3] = gsub("year_0\\)", paste("year_0)", addprofile, sep="+"), formula[3])
    
    new_formula = as.formula(paste(formula[2], formula[3], sep = "~"))
    
    
    bf_base_formula = bf(new_formula,
                         sigma ~ (1|country_text_id))
    
    return(bf_base_formula)
  }
  
  if(is.null(addprofile) == T) {
    newformula = bf(base_formula,
                    sigma ~ (1|country_text_id))
  } else {
    newformula = model_maker(base_formula, addprofile)
    
  }
  
  print(paste("START", Sys.time()))
  print(newformula)
  
  bayesmodel = compile_stan(newformula, data, prior_brms, warmup = warmup, iter = iter, chains = chains, thin = thin)
  
  print(paste("ENDE", Sys.time()))
  return(bayesmodel)
}




# Priors BRMS ####
prior_unit_tscs <- c(set_prior("cauchy(0,5)", class = "sd"),
                     set_prior("normal(0,10)", class = "Intercept"))


prior_phet_tscs <- c(set_prior("cauchy(0,5)", class = "sd"),
                     set_prior("normal(0,10)", class = "Intercept"),
                     set_prior("cauchy(0,5)", class = "sd", dpar="sigma"),
                     set_prior("normal(0,10)", class = "Intercept", dpar="sigma"))

prior_full_tscs <- c(set_prior("normal(0,10)", class = "b"),
                     set_prior("cauchy(0,5)", class = "sd"),
                     set_prior("normal(0,10)", class = "Intercept"))

prior_full_phet_tscs <- c(set_prior("normal(0,10)", class = "b"),
                          set_prior("cauchy(0,5)", class = "sd"),
                          set_prior("normal(0,10)", class = "Intercept"),
                          set_prior("cauchy(0,5)", class = "sd", dpar="sigma"),
                          set_prior("normal(0,10)", class = "Intercept", dpar="sigma"))

# SAVE BRMS models
saveBRMS = function(modelname_label, ordner) {
  saveRDS(get(modelname_label), file=paste("Analyse/Performance/SpecificP/brmsModels/", ordner, "/", modelname_label, ".rds", sep=""))
}

loadBRMS = function(modelname_label, ordner) {
  readRDS(file=paste("Analyse/Performance/SpecificP/brmsModels/", ordner, "/", modelname_label, ".rds", sep=""))
}



# Multicollinearity Test ####
multicollinearity_test = function(vars, dependent_var, datalist, with_lag = T) {

  dependent_var_lag = paste(dependent_var, "_lag_wi", sep="")
  
    
  # get independent_vars
  independentvars = vars[grepl("num_ctl", vars) | grepl("FKM", vars)]
  
  independentvars_wi = paste(independentvars, "_wi", sep="")
  independentvars_wi_lag = paste(independentvars, "_wi_lag", sep="")
  independentvars_bw = paste(independentvars, "_bw", sep="")
  
  all_vifs = data.frame()
  for (i in 1:length(independentvars)) {
    my_frame = datalist[[1]] %>% 
      select(country_text_id, year_0, 
             depvar = dependent_var,
             depvar_lag = dependent_var_lag,
             indep = independentvars_wi[i],
             indep_wi_lag = independentvars_wi_lag[i],
             indep_bw = independentvars_bw[i]) %>%
      as.data.frame() 
          
    if (with_lag == T) {
      # lag included
      
      m1 = glmmTMB(depvar ~ depvar_lag +
                     indep + 
                     indep_wi_lag +
                     indep_bw +
                     (1|country_text_id),
                   my_frame)
      
      # Check Multicollinearity
      vif_stats = check_collinearity(m1)
      vif_stats$Parameter = c(dependent_var_lag, independentvars_wi[i],independentvars_wi_lag[i],independentvars_bw[i])
      
    } else {
      # without lag included
       
      m1 = glmmTMB(depvar ~ depvar_lag +
                     indep + 
                     indep_bw +
                     (1|country_text_id),
                   my_frame)
      
      # Check Multicollinearity
      vif_stats = check_collinearity(m1)
      vif_stats$Parameter = c(dependent_var_lag, independentvars_wi[i],independentvars_bw[i])
      
    }

    # Cosmetics
    vif_stats$Parameter = gsub("num_ctl_","",vif_stats$Parameter)
    vif_stats$Parameter = gsub("\\..*T", "", vif_stats$Parameter)
    
    # combine all estimates
    all_vifs = bind_rows(all_vifs, vif_stats)
  }
  
  # create dust table 
  # distinguish between low, <i>moderate and <b>high VIF correlation
  all_vifs_dust = all_vifs %>% 
    filter(Parameter != dependent_var_lag) %>% 
    mutate(strength_cor = if_else(VIF>5 & VIF<10, "moderate",
                                  if_else(VIF>=10, "high", "low")),
           VIF = round(VIF, 3),
           Parameter = if_else(strength_cor == "moderate", paste("<i>", Parameter, sep=""), Parameter),
           VIF = ifelse(strength_cor == "moderate", paste("<i>", VIF, sep=""), VIF),
           strength_cor = ifelse(strength_cor == "moderate", paste("<i>", strength_cor, sep=""), strength_cor),
           
           Parameter = if_else(strength_cor == "high", paste("<b>", Parameter, sep=""), Parameter),
           VIF = ifelse(strength_cor == "high", paste("<b>", VIF, sep=""), VIF),
           strength_cor = ifelse(strength_cor == "high", paste("<b>", strength_cor, sep=""), strength_cor)) %>% 
    select(-SE_factor)
  
  
  dust(all_vifs_dust)%>%
    sprinkle(font_size = 12, part="head") %>%
    sprinkle(font_size = 10) %>% 
    sprinkle_colnames("Variable", "VIF", "Correlation") %>% 

    sprinkle(row=1, border_color="black", border="top", border_thickness =2) %>% 
    sprinkle(col=1, border_color="black", border="right", border_thickness =2) %>% 
    sprinkle(col=1, border_color="black", border="right", border_thickness =2, part="head") %>% 
    sprinkle(col=2, border_color="black", border="right", border_thickness =1, part="body")%>% 
    sprinkle(col=2, border_color="black", border="right", border_thickness =1, part="head")%>% 
    
    sprinkle(halign="center", part="head") %>% 
    sprinkle(halign="center", part="body", cols=2:3) %>% 
    sprinkle_print_method("html")
}



# Unit Root Test ####

chech_stationarity_Beck = function(vars, datalist, model = "plm") {
  table_data = data.frame(matrix(NA, nrow=length(vars), ncol=4)) %>% 
    rename(variable = X1, estimate = X2, CI_l = X3, CI_u = X4)
  
  for (i in 1:length(vars)) {
    print(vars[i])
    
    vars_lag = paste(vars[i], "_wi_lag", sep="")
    
    # my_frame = datalist[[1]] %>% 
    #   select(country_text_id, year_0, variable = vars[i]) %>%
    #   group_by(country_text_id) %>% 
    #   tidyr::complete(country_text_id, year_0 = min(year_0):max(year_0), fill = list(NA)) %>% 
    #   mutate(lag = dplyr::lag(variable, 1)) %>% 
    #   as.data.frame() 
    
    my_frame = datalist[[1]] %>% 
      select(country_text_id, year_0, variable = vars[i], lag = vars_lag) %>% 
      as.data.frame()
    
    if (model == "plm") {
      m1 = plm(variable ~ lag,
               my_frame, index=c("country_text_id", "year_0"), model="within")
      
      parameter_estimate = tidy(m1, conf_level=.95, conf.int=T) %>% 
        filter(term == "lag")
      
      table_data$variable[i] = vars[i]
      table_data$estimate[i] = parameter_estimate$estimate
      table_data$CI_l[i] =   parameter_estimate$conf.low
      table_data$CI_u[i] =   parameter_estimate$conf.high
    } 
    if (model == "glmmTMB") {
      m1 = glmmTMB(variable ~ lag + (1 |country_text_id) + (1 |year_0),
                   my_frame,
                   control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) )
      parameter_estimate = data.frame(confint(m1, "lag", level = 0.95))
      print(summary(m1))
      table_data$variable[i] = vars[i]
      table_data$estimate[i] = parameter_estimate$Estimate
      table_data$CI_l[i] =   parameter_estimate$X2.5..
      table_data$CI_u[i] =   parameter_estimate$X97.5..
    }
  }
  
  table_data = table_data %>% 
    mutate_if(is.numeric, funs(round = round(.,4)))
  
  
  table_data$CI_u_round = ifelse(table_data$CI_u>=1, paste("<b>",table_data$CI_u_round), table_data$CI_u_round) 
  table_data$CI_l_round = ifelse(table_data$CI_l>=1, paste("<b>",table_data$CI_l_round), table_data$CI_l_round) 
  table_data$estimate_round = ifelse(table_data$estimate>=1, paste("<b>",table_data$estimate_round), table_data$estimate_round) 
  table_data$variable = ifelse(table_data$estimate>=1, paste("<b>",table_data$variable), table_data$variable) 
  
  table_data = table_data %>% 
    select(variable, estimate_round, CI_l_round, CI_u_round)
  
  table_data$variable = gsub("_eco", "",table_data$variable)
  table_data$variable = gsub("_num_ctl", "",table_data$variable)
  table_data$variable = gsub("_pr_ctl", "",table_data$variable)
  table_data$variable = gsub("_wi", "",table_data$variable)
  
  dust(table_data)  %>%
    sprinkle(font_size = 12, part="head") %>%
    sprinkle(font_size = 10) %>% 
    sprinkle_colnames("Variable", "Est.", "95% Conf. Interval", "95% Conf. Interval") %>% 
    sprinkle(merge = T, cols=3:4, part="head") %>% 
    
    sprinkle(row=1, border_color="black", border="top", border_thickness =2) %>% 
    sprinkle(col=1, border_color="black", border="right", border_thickness =2) %>% 
    sprinkle(col=1, border_color="black", border="right", border_thickness =2, part="head") %>% 
    sprinkle(col=2, border_color="black", border="right", border_thickness =1, part="body")%>% 
    sprinkle(col=2, border_color="black", border="right", border_thickness =1, part="head")%>% 
    
    sprinkle(halign="center", part="head") %>% 
    sprinkle(halign="center", part="body", cols=2:4) %>% 
    sprinkle_print_method("html")
}


chech_stationarity_Fisher = function(vars, datalist) {
  table_data_intercept = data.frame(matrix(NA, nrow=length(vars), ncol=5)) %>% 
    rename(test = X1, variable = X2, chisq = X3, df = X4, p_value = X5)
  table_data_trend = data.frame(matrix(NA, nrow=length(vars), ncol=5)) %>% 
    rename(test = X1, variable = X2, chisq = X3, df = X4, p_value = X5)
  
  for (i in 1:length(vars)) {
    print(vars[i])
    my_frame = datalist[[1]] %>% 
      select(country_text_id, year_0, variable = vars[i]) %>%
      group_by(year_0) %>% 
      mutate(variable = variable - mean(variable, na.rm=T)) %>% 
      ungroup() %>% 
      na.omit() 
    
    time_long = my_frame %>% 
      group_by(country_text_id) %>% 
      summarise(cnt = n()) %>% 
      filter(cnt > 10) %>% 
      distinct(country_text_id)
    
    my_frame = my_frame %>% 
      filter(country_text_id %in% time_long$country_text_id) %>% 
      pdata.frame(index = c("country_text_id", "year_0")) 
    
    result_intercept = purtest(my_frame$variable, pmax = 3, test = "madwu", exo="intercept", lags="AIC")
    result_trend = purtest(my_frame$variable, pmax = 3, test = "madwu", exo="trend", lags="AIC")
    
    print(result_trend)

    table_data_intercept$test[i] = "intercept"
    table_data_intercept$variable[i] = vars[i]
    table_data_intercept$chisq[i] = result_intercept$statistic$statistic
    table_data_intercept$df[i] = result_intercept$statistic$parameter
    table_data_intercept$p_value[i] = result_intercept$statistic$p.value
    
    table_data_trend$test[i] = "trend"
    table_data_trend$variable[i] = vars[i]
    table_data_trend$chisq[i] = result_trend$statistic$statistic
    table_data_trend$df[i] = result_trend$statistic$parameter
    table_data_trend$p_value[i] = result_trend$statistic$p.value

  }
  
  table_data = table_data_intercept %>% 
    bind_rows(table_data_trend) %>% 
    arrange(variable) %>%  
    mutate_if(is.numeric, funs(round(.,3)))
  
  
  table_data$test = ifelse(table_data$p_value > 0.05, paste("<b>",table_data$test), table_data$test) 
  table_data$variable = ifelse(table_data$p_value > 0.05, paste("<b>",table_data$variable), table_data$variable) 
  table_data$chisq = ifelse(table_data$p_value > 0.05, paste("<b>",table_data$chisq), table_data$chisq) 
  table_data$df = ifelse(table_data$p_value > 0.05, paste("<b>",table_data$df), table_data$df) 
  table_data$p_value = ifelse(table_data$p_value > 0.05, paste("<b>",table_data$p_value), table_data$p_value) 
  
  
  table_data$variable = gsub("_eco", "",table_data$variable)
  table_data$variable = gsub("_num_ctl", "",table_data$variable)
  
  dust(table_data)  %>%
    sprinkle(font_size = 12, part="head") %>%
    sprinkle(font_size = 10) %>% 
    sprinkle_colnames("Fisher Test", "Variable", "&chi;<sup>2", "df", "<i>p") %>% 
    # sprinkle(merge = T, cols=3:4, part="head") %>% 
    # 
    sprinkle(row=1, border_color="black", border="top", border_thickness =2) %>%
    sprinkle(col=1, border_color="black", border="right", border_thickness =2) %>%
    sprinkle(col=1, border_color="black", border="right", border_thickness =2, part="head") %>%
    sprinkle(col=2:5, border_color="black", border="right", border_thickness =1, part="body")%>%
    sprinkle(col=2:5, border_color="black", border="right", border_thickness =1, part="head")%>%
    
    sprinkle(halign="center", part="head") %>% 
    sprinkle(halign="center", part="body", cols=2:4) %>% 
    sprinkle_print_method("html")
}

# PPC:Unit Heterogeneity ####

ppc_unithet = function(mod_homogen, mod_unit, dataset, independent_var, unit = "country_text_id") {
  ppc_homogen = posterior_predict(mod_homogen)
  ppc_unit = posterior_predict(mod_unit)
  
  mod_hom_data = mod_homogen$data %>% 
    select_at(vars(-matches("country"), -matches("year"))) %>% 
    left_join(dataset, by = independent_var)  
  mod_unit_data = mod_unit$data %>% 
    select_at(vars(-matches("country"), -matches("year"))) %>% 
    left_join(dataset, by = independent_var)  
  
  plot_data_homogen = data.frame(mod_homogen$data, 
                                 country_text_id = mod_hom_data$country_text_id, 
                         year_0 = mod_hom_data$year_0,
                         posterior = t(ppc_homogen[sample(1:dim(ppc_homogen)[1], 1000),]))
  
  plot_data_unit = data.frame(mod_unit$data, 
                              country_text_id = mod_unit$data$country_text_id, 
                              year_0 = mod_unit_data$year_0,
                              posterior = t(ppc_unit[sample(1:dim(ppc_unit)[1], 1000),]))
  
  
  
  
  errorbar_plot_data_ctr = . %>%
    rename(variable = independent_var) %>% 
    pivot_longer(cols=c(starts_with("posterior"))) %>% 
    group_by(country_text_id, name) %>% 
    summarise(variable = mean(variable), 
              value = mean(value)) %>% 
    group_by(country_text_id) %>% 
    summarise(y_obs = mean(variable), 
              y_rep = mean(value),
              y_rep_lower = quantile(value, probs=0.025),
              y_rep_upper = quantile(value, probs=0.975)) %>% 
    ungroup() %>% 
    mutate(country_text_id = fct_reorder(country_text_id, y_obs))
  
    errorbar_plot_data_year = . %>%
      rename(variable = independent_var) %>% 
      pivot_longer(cols=c(starts_with("posterior"))) %>% 
      group_by(year_0, name) %>% 
      summarise(variable = mean(variable), 
                value = mean(value)) %>% 
      group_by(year_0) %>% 
      summarise(y_obs = mean(variable), 
                y_rep = mean(value),
                y_rep_lower = quantile(value, probs=0.025),
                y_rep_upper = quantile(value, probs=0.975)) %>% 
      ungroup() 
  
  
    p_value_data_ctr = . %>%
      rename(variable = independent_var) %>% 
      select_at(vars(country_text_id, variable, starts_with("posterior"))) %>% 
      filter(country_text_id %in% country_sample) %>% 
      group_by(country_text_id) %>% 
      summarise_all(funs(mean(.))) %>% 
      pivot_longer(cols=starts_with("posterior"))
    
    p_value_data_year = . %>%
      rename(variable = independent_var) %>% 
      select_at(vars(year_0, variable, starts_with("posterior"))) %>% 
      filter(year_0 %in% year_sample) %>% 
      group_by(year_0) %>% 
      summarise_all(funs(mean(.))) %>% 
      pivot_longer(cols=starts_with("posterior"))
    
    pvalues = . %>% 
      mutate(TStat = ifelse(value > variable, 1, 0)) %>% 
      group_by(country_text_id) %>% 
      summarise(TStat = mean(TStat)) %>% 
      mutate(TStat=round(TStat,2))
    
    pvalues_year = . %>% 
      mutate(TStat = ifelse(value > variable, 1, 0)) %>% 
      group_by(year_0) %>% 
      summarise(TStat = mean(TStat)) %>% 
      mutate(TStat=round(TStat,2))
    
  if (unit == "country_text_id") {
    
    # draw country sample
    
    country_sample = sample(unique(plot_data_homogen$country_text_id), 9)
    
    err_homogen_plot = errorbar_plot_data_ctr(plot_data_homogen)   %>% 
      ggplot(aes(x=country_text_id, y=y_obs, grp=country_text_id)) +
      geom_point(col="black", size=2) +
      geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
      ylab("y (observed)") +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none")   +
      ggtitle("Model: No Unit Heterogeneity")
    
    err_unit_plot = errorbar_plot_data_ctr(plot_data_unit)   %>% 
      ggplot(aes(x=country_text_id, y=y_obs, grp=country_text_id)) +
      geom_point(col="black", size=2) +
      geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
      ylab("y (observed)") + 
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle("Model: Unit Heterogeneity")   
    
    # P Value Plot

    p_unit_data_homogen = p_value_data_ctr(plot_data_homogen) 
    p_unit_values_homogen = pvalues(p_unit_data_homogen)
    
    p_unit_data_unit = p_value_data_ctr(plot_data_unit) 
    p_unit_values_unit = pvalues(p_unit_data_unit)
    
    
    p_value_plot_homogen = p_unit_data_homogen %>% 
      left_join(p_unit_values_homogen, by="country_text_id") %>% 
      ggplot(aes(x=value)) +
      geom_histogram(bins=40, alpha=0.8) +
      geom_vline(aes(xintercept =  variable), size=1.3) +
      geom_text(aes(x = -0.025, y = 250, label=TStat), size=3.5) +
      ylab("") +
      facet_wrap(country_text_id ~ .) +
      theme_bw() +
      theme(axis.text.y = element_blank())
    
    p_value_plot_unit = p_unit_data_unit %>% 
      left_join(p_unit_values_unit, by="country_text_id") %>% 
      ggplot(aes(x=value)) +
      geom_histogram(bins=40, alpha=0.8) +
      geom_vline(aes(xintercept =  variable), size=1.3) +
      geom_text(aes(x = -0.025, y = 250, label=TStat), size=3.5)  +
      ylab("") +
      facet_wrap(country_text_id ~ .) +
      theme_bw() +
      theme(axis.text.y = element_blank())
  } 

  if (unit == "year_0") {
    
    # draw sample year
    year_sample = sample(unique(plot_data_homogen$year_0), 9)
    
    
    err_homogen_plot = errorbar_plot_data_year(plot_data_homogen)   %>% 
      ggplot(aes(x=year_0, y=y_obs, grp=year_0)) +
      geom_point(col="black", size=2) +
      geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
      ylab("y (observed)") + 
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none")   +
      ggtitle("Model: No Time Heterogeneity")
      
    err_unit_plot = errorbar_plot_data_year(plot_data_unit)   %>% 
      ggplot(aes(x=year_0, y=y_obs, grp=year_0)) +
      geom_point(col="black", size=2) +
      geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
      scale_x_continuous(breaks=seq(0,100,5)) +
      ylab("y (observed)") + 
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle("Model: Time Heterogeneity")  
    
    p_time_data_homogen = p_value_data_year(plot_data_homogen) 
    p_time_values_homogen = pvalues_year(p_time_data_homogen)
    
    p_time_data_unit = p_value_data_year(plot_data_unit) 
    p_time_values_unit = pvalues_year(p_time_data_unit)
    
    
    p_value_plot_homogen = p_time_data_homogen %>% 
      left_join(p_time_values_homogen, by="year_0") %>% 
      ggplot(aes(x=value)) +
      geom_histogram(bins=40, alpha=0.8) +
      geom_vline(aes(xintercept =  variable), size=1.3) +
      geom_text(aes(x = -0.025, y = 250, label=TStat), size=3.5) +
      ylab("") +
      facet_wrap(year_0 ~ .) +
      theme_bw() +
      theme(axis.text.y = element_blank())
    
    p_value_plot_unit = p_time_data_unit %>% 
      left_join(p_time_values_unit, by="year_0") %>% 
      ggplot(aes(x=value)) +
      geom_histogram(bins=40, alpha=0.8) +
      geom_vline(aes(xintercept =  variable), size=1.3) +
      geom_text(aes(x = -0.025, y = 250, label=TStat), size=3.5) +
      ylab("") +
      facet_wrap(year_0 ~ .) +
      theme_bw() +
      theme(axis.text.y = element_blank())
  }
    
  # final_plot = ggarrange(err_homogen_plot, err_unit_plot, 
  #                        p_value_plot_homogen, p_value_plot_unit, nrow=2, ncol=2)
    final_plot = ggarrange(err_homogen_plot, err_unit_plot, nrow=1, ncol=2)
  return(final_plot)
}


# PPC: Panel heteroscedasticity ####
ppc_panelhet = function(mod_homogen, mod_unit, dataset, independent_var) {
  ppc_homogen = posterior_predict(mod_homogen)
  ppc_unit = posterior_predict(mod_unit)
  
  mod_hom_data = mod_homogen$data %>% 
    select_at(vars(-matches("country"), -matches("year"))) %>% 
    left_join(dataset, by = independent_var)  
  
  mod_unit_data = mod_unit$data %>% 
    select_at(vars(-matches("country"), -matches("year"))) %>% 
    left_join(dataset, by = independent_var)  
  
  
  
  plot_data_homogen = data.frame(mod_homogen$data, 
                                 country_text_id = mod_hom_data$country_text_id, 
                                 year_0 = mod_hom_data$year_0,
                                 posterior = t(ppc_homogen[sample(1:dim(ppc_homogen)[1], 1000),]))
  
  plot_data_unit = data.frame(mod_unit$data, 
                              country_text_id = mod_unit_data$country_text_id, 
                              year_0 = mod_unit_data$year_0,
                              posterior = t(ppc_unit[sample(1:dim(ppc_unit)[1], 1000),]))
  
  
  errorbar_plot_data_ctr = . %>%
    rename(variable = independent_var) %>% 
    pivot_longer(cols=c(starts_with("posterior"))) %>% 
    group_by(country_text_id, name) %>% 
    summarise(variable = sd(variable), 
              value = sd(value)) %>% 
    group_by(country_text_id) %>% 
    summarise(y_obs = mean(variable), 
              y_rep = mean(value),
              y_rep_lower = quantile(value, probs=0.025),
              y_rep_upper = quantile(value, probs=0.975)) %>% 
    ungroup() %>% 
    mutate(country_text_id = fct_reorder(country_text_id, y_obs))

  p_value_data_ctr = . %>%
    rename(variable = independent_var) %>% 
    select_at(vars(country_text_id, variable, starts_with("posterior"))) %>% 
    filter(country_text_id %in% country_sample) %>% 
    group_by(country_text_id) %>% 
    summarise_all(funs(sd(.))) %>% 
    pivot_longer(cols=starts_with("posterior"))
  

  pvalues = . %>% 
    mutate(TStat = ifelse(value > variable, 1, 0)) %>% 
    group_by(country_text_id) %>% 
    summarise(TStat = mean(TStat)) %>% 
    mutate(TStat=round(TStat,2))

  
  # draw country sample
  
  country_sample = sample(unique(plot_data_homogen$country_text_id), 9)
  
  err_homogen_plot = errorbar_plot_data_ctr(plot_data_homogen)   %>% 
    ggplot(aes(x=country_text_id, y=y_obs, grp=country_text_id)) +
    geom_point(col="black", size=2) +
    geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
    ylab("y sd (observed)") + 
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none")   +
    ggtitle("Model: No Heteroskedasticity")
  
  err_unit_plot = errorbar_plot_data_ctr(plot_data_unit)   %>% 
    ggplot(aes(x=country_text_id, y=y_obs, grp=country_text_id)) +
    geom_point(col="black", size=2) +
    geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
    ylab("y sd (observed)") + 
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Model: Panel heteroskedasticity")   
  
  # P Value Plot
  
  p_unit_data_homogen = p_value_data_ctr(plot_data_homogen) 
  p_unit_values_homogen = pvalues(p_unit_data_homogen)
  
  p_unit_data_unit = p_value_data_ctr(plot_data_unit) 
  p_unit_values_unit = pvalues(p_unit_data_unit)
  
  
  p_value_plot_homogen = p_unit_data_homogen %>% 
    left_join(p_unit_values_homogen, by="country_text_id") %>% 
    ggplot(aes(x=value)) +
    geom_histogram(bins=40, alpha=0.8) +
    geom_vline(aes(xintercept =  variable), size=1.3) +
    geom_text(aes(x = -0.025, y = 250, label=TStat), size=3.5) +
    ylab("") +
    facet_wrap(country_text_id ~ .) +
    theme_bw() +
    theme(axis.text.y = element_blank())
  
  p_value_plot_unit = p_unit_data_unit %>% 
    left_join(p_unit_values_unit, by="country_text_id") %>% 
    ggplot(aes(x=value)) +
    geom_histogram(bins=40, alpha=0.8) +
    geom_vline(aes(xintercept =  variable), size=1.3) +
    geom_text(aes(x = -0.025, y = 250, label=TStat), size=3.5) +
    ylab("") +
    facet_wrap(country_text_id ~ .) +
    theme_bw() +
    theme(axis.text.y = element_blank())
  

  # final_plot = ggarrange(err_homogen_plot, err_unit_plot, 
  #                        p_value_plot_homogen, p_value_plot_unit, nrow=2, ncol=2)
  final_plot = ggarrange(err_homogen_plot, err_unit_plot, nrow=1, ncol=2)
  
  return(final_plot)
}


# PPC: Autoregression ####

ppc_auto = function(mod_auto, dataset, independent_var, country_sample = NULL) {
  ppc_autoreg = posterior_predict(mod_auto)
  
  plot_data_auto = data.frame(mod_auto$data, 
                              country_text_id = dataset$country_text_id, 
                              year_0 = dataset$year_0,
                              posterior = t(ppc_autoreg[sample(1:dim(ppc_autoreg)[1], 1000),]))
  
  if (is.null(country_sample) == T ) {
    country_sample = sample(unique(plot_data_auto$country_text_id), 12)
  }
  
  auto_data_plot = plot_data_auto  %>%
    rename(variable = independent_var) %>% 
    filter(country_text_id %in% country_sample) %>% 
    select_at(vars(country_text_id, year_0, variable, starts_with("posterior")))  %>%
    pivot_longer(cols=starts_with("posterior")) %>% 
    group_by(country_text_id, year_0) %>% 
    summarise(y_obs = mean(variable),
              y_rep_lower = quantile(value, probs=c(0.025)),
              y_rep_mean  = mean(value),
              y_rep_upper = quantile(value, probs=c(0.975)))
  
  auto_data_plot %>% 
    ggplot(aes(x=year_0, y=y_obs, ymin=y_rep_lower, ymax=y_rep_upper)) +
    geom_line(color="red") +  
    geom_line(aes(y=y_rep_mean)) +
    geom_ribbon(alpha=0.3) +
    facet_wrap(country_text_id ~ .) +
    theme_bw()
}

# Residual Time Plot ####

ppc_resid_time = function(mod_auto, country_sample = NULL, title = NULL) {
  mod_resid = residuals(mod_auto, type="pearson", method="posterior_predict")
  

  plot_data_residuals = data.frame(mod_auto$data, 
                                   # country_text_id = dataset$country_text_id, 
                                   year_0 = mod_auto$data$year_0,
                                   residuals = mod_resid[,1])
  
  if (is.null(country_sample) == T ) {
    country_sample = sample(unique(plot_data_residuals$country_text_id), 9)
  }
  
  plot_data_residuals %>%
    filter(country_text_id %in% country_sample) %>%
    ggplot(aes(x=year_0, y=residuals))+
    geom_point(alpha=0.5) +
    geom_smooth(se=T, color="black", linetype = "longdash", span=0.8) +
    geom_hline(yintercept = 0) +
    facet_wrap(country_text_id ~ .) +
    theme_bw() +
    ggtitle(title) +
    xlab("")

}


ppc_resid_data = function(mod_auto) {
  mod_resid = residuals(mod_auto, summary=F, nsamples = 1)

  plot_data_residuals = mod_auto$data %>% 
    bind_cols(data.frame(residuals = mod_resid[1,]))
  

  return(plot_data_residuals)
}


# Fitted vs Residuals Plot ####

fitted_res_plot = function(model,  country_sample = NULL, title = NULL) {

  fitted_res_data = data.frame(model$data,
             fitted = fitted(model)[,1],
             residuals = residuals(model, type="pearson")[,1])
  
  # if (is.null(country_sample) == T ) {
  #   country_sample = sample(unique(fitted_res_data$country_text_id), 9)
  # }
  
  fitted_res_data %>% 
    #filter(country_text_id %in% country_sample) %>%
    ggplot(aes(x=fitted, y=residuals)) +
    geom_point() +
    geom_smooth(se=F, color="black", linetype = "longdash", span=0.8) +
    geom_hline(yintercept = c(0)) +
    geom_hline(yintercept = c(-2,2), linetype="longdash") +
    #facet_wrap(country_text_id ~ .) +
    theme_bw()  +
    ggtitle(title) +
    ylab("Standardized Residuals") +
    xlab("Fitted Values")
    #geom_text(data = fitted_res_data %>% filter(residuals > 4 | residuals < -4), aes(label=paste(country_text_id, year_0, sep = " ")))
}

fitted_var_plot = function(model, independent_var, country_sample = NULL, title = NULL) {
  
  fitted_res_data = data.frame(model$data,
                               residuals = residuals(model, type="pearson")[,1]) %>% 
    rename(variable = independent_var)
  
  if (is.null(country_sample) == T ) {
    country_sample = sample(unique(fitted_res_data$country_text_id), 9)
  }
  
  fitted_res_data %>% 
    filter(country_text_id %in% country_sample) %>%
    ggplot(aes(x=variable, y=residuals)) +
    geom_point() +
    geom_smooth(se=T, color="black", linetype = "longdash", span=0.8) +
    geom_hline(yintercept = c(0)) +
    geom_hline(yintercept = c(-2,2), linetype="longdash") +
    facet_wrap(country_text_id ~ .) +
    theme_bw()  +
    ggtitle(title) +
    xlab(independent_var)
}

fitted_var_alldata_plot = function(model, independent_var, title = NULL) {
  
  fitted_res_data = data.frame(model$data,
                               residuals = residuals(model, type="pearson")[,1]) %>% 
    rename(variable = independent_var)
  
  fitted_res_data %>% 
    ggplot(aes(x=variable, y=residuals)) +
    geom_point() +
    geom_smooth(se=T, color="black", linetype = "longdash", span=0.8) +
    geom_hline(yintercept = c(0)) +
    geom_hline(yintercept = c(-2,2), linetype="longdash") +
    theme_bw()  +
    ggtitle(title) +
    xlab(independent_var)
}

# Distribution Plot ####

distribution_plot = function(model, dep_var) {
  y = model$data %>% 
    pull(dep_var)
  yrep = posterior_predict(model)
  group = model$data$country_text_id
  
  
  violin_data = data.frame(y, 
                           ppc = t(yrep[sample(nrow(yrep), 100), ]), 
                           country_text_id =  model$data$country_text_id) %>% 
    pivot_longer(cols=starts_with("ppc"))
  
  violin_data %>%  
    ggplot(aes(x=1, y=value)) +
    geom_violin(fill="gray", color="gray", alpha=0.3, draw_quantiles= c(0.05, 0.5, 0.95)) + 
    geom_violin(data = violin_data, aes(x=1, y=y), color="black", alpha=0) +
    scale_x_continuous(breaks=NULL) +
    facet_wrap(country_text_id ~ .) +
    theme_bw()
} 

### Lag Distribution #####


lag_distribution_bayes = function(brms_model, LDV_label, IndV_label, dep_label, unit = 1, time_periods=4, ci=0.95, ecm = F) {
  
  posterior_coefs = posterior_samples(brms_model, pars = IndV_label) %>% 
    select_at(vars(-matches("_bw")))
  print(colnames(posterior_coefs))
  posterior_LDV = posterior_samples(brms_model, pars = LDV_label)
  print(colnames(posterior_LDV))
  
  if (ecm == T) {
    posterior_LDV = 1 + posterior_LDV
    posterior_coefs[,2] = posterior_coefs[,2] - posterior_coefs[,1]
  }
  
  
  shift <- function (x, shift) c(rep(0,times=shift), x[1:(length(x)-shift)])
  
  coefs = cbind(as.matrix(posterior_coefs),
                matrix(0, nrow=dim(as.matrix(posterior_coefs))[1], ncol = time_periods))
  LDV = as.matrix(posterior_LDV)
  
  # Calculate LRM
  LRM = rowSums(coefs)/(1 - rowSums(LDV))

  # Impulse Matrices X and Y
  impulse_x = c(unit, rep(0, dim(coefs)[2]-1))
  impulse_y = matrix(c(0, rep(1, dim(coefs)[2]-1)), nrow = 1)
  
  ldv_dim = 2
  while (ldv_dim  <= dim(LDV)[2]) {
    impulse_y = impulse_y %>% 
      rbind(shift(impulse_y,1))
    ldv_dim = ldv_dim + 1
  }
  impulse_y = t(impulse_y)
  
  time_array = array(NA, c(dim(coefs)[1], dim(coefs)[2]))
  
  # Calculation
  for (draw in 1:dim(coefs)[1]) {
    time_array[draw, 1] = sum(coefs[draw, ] * shift(impulse_x, 0))
    for (i in 2:dim(coefs)[2]) {
      y_lag = sum(time_array[draw, i-1] * LDV[draw,] * impulse_y[i,])
      
      time_array[draw, i] =  y_lag + sum(coefs[draw,] * shift(impulse_x, i-1))
    }
    
  }
  
  estimates = data.frame(array(NA, dim=c(dim(time_array)[2],3))) %>% 
    rename(est = X1, l = X2, u = X3) %>% 
    mutate(time = paste("t + ", 1:dim(time_array)[2], sep=""))
  
  for (i in 1:dim(estimates)[1]) {
    estimates$est[i] = median(time_array[,i])
    estimates$l[i] = hdi(time_array[,i], .width = ci)[1]
    estimates$u[i] = hdi(time_array[,i], .width = ci)[2]
  }
  
  print(paste("LRM:", round(median(LRM),3)))
  print(paste("HDI:", round(hdi(LRM, .width = ci), 3)))
  
  CI = round(hdi(LRM, .width = ci),3)
  
  label = paste("LRM: ", round(median(LRM),3), " (", CI[1,1], " - ",CI[1,2],")", sep="")
  title = gsub("b_","",colnames(posterior_coefs))
  title = gsub("_wi","",title)
  title = gsub("_pr_ctl","",title)
  title = gsub("_num_ctl","",title)
  title = gsub("_df","",title)
  

  p1 = ggplot(estimates, aes(x=time, y=est)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=l, ymax=u)) +
    geom_hline(yintercept = 0) +
    theme_bw()  +
    #annotate(geom = "text", label = label, x = Inf, y = Inf, hjust = 1, vjust = 1) +
    ggtitle(title, subtitle = label) +
    xlab("") +
    ylab(paste("Change in", dep_label, sep=" "))

  return(p1)
}


lag_distribution_bayes_ecm = function(brms_model_ecm, LDV_label, IndV_label, dep_label, unit = 1, time_periods=6, ci=0.95) {
  
  posterior_coefs = posterior_samples(brms_model_ecm, pars = IndV_label) %>% 
    select_at(vars(-matches("_bw")))  %>% 
    #sample_frac(0.25) %>% 
    as.matrix() 
  print(colnames(posterior_coefs))
  posterior_LDV = posterior_samples(brms_model_ecm, pars = LDV_label) %>% 
    as.matrix()
  print(colnames(posterior_LDV))
  
  

  time_array = array(NA, c(dim(posterior_coefs)[1], time_periods))
  LDV = as.matrix(abs(rowSums(posterior_LDV)))
  
  time_array[,1] = posterior_coefs[,1] * unit
  lag_coef = posterior_coefs[,2]
  
  LRM = lag_coef/LDV
  max(LRM)
  LRM_star = LRM - time_array[,1]
  
  for (t in 2:time_periods) {
    time_array[,t]  = (LRM_star * unit) * LDV
    LRM_star = LRM_star - time_array[,t]
    
  }
  
  estimates = data.frame(array(NA, dim=c(dim(time_array)[2],3))) %>% 
    rename(est = X1, l = X2, u = X3) %>% 
    mutate(time = paste("t + ", 1:dim(time_array)[2], sep=""))
  
  for (i in 1:dim(estimates)[1]) {
    estimates$est[i] = median(time_array[,i])
    estimates$l[i] = hdi(time_array[,i], .width = ci)[1]
    estimates$u[i] = hdi(time_array[,i], .width = ci)[2]
  }
  CI = round(hdi(LRM, .width = ci),3)
  
  print(paste("LRM:", round(median(LRM),3)))
  print(paste("HDI:", round(hdi(LRM, .width = ci), 3)))
  
  label = paste("LRM: ", round(median(LRM),3), " (", CI[1,1], " - ",CI[1,2],")", sep="")
  title = gsub("b_","",colnames(posterior_coefs))
  title = gsub("_wi","",title)
  
  
  ggplot(estimates, aes(x=time, y=est)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=l, ymax=u)) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    ggtitle(title, subtitle = label) +
    xlab("") +
    ylab("Effect") +
    ylab(paste("Change in", dep_label, sep=" "))
  
  
}

lag_distribution_both = function(brms_model, LDV_label, IndV_label, dep_label, unit = 1, time_periods=4, ci=0.95, ecm = F) {
  
  if (unit == "sd") {
    unit = brms_model$data %>% 
      select_at(vars(matches(IndV_label)))  %>% 
      select_at(vars(-matches("_bw"), -matches("_lag"))) %>% 
      summarise_all(funs(unit = sd(.))) %>% 
      pull(unit)
    
    print(unit)
  } else {
    unit = 1
  }
  
  posterior_coefs = posterior_samples(brms_model, pars = IndV_label) %>% 
    select_at(vars(-matches("_bw"))) 
  
  print(colnames(posterior_coefs))
  posterior_LDV = posterior_samples(brms_model, pars = LDV_label)
  print(colnames(posterior_LDV))
  
  
  X = unit
  
  a =  as.matrix(posterior_LDV)
  b0 = as.matrix(posterior_coefs[,1])
  b1 = as.matrix(posterior_coefs[,2])
  
  
  if (ecm == T) {
    ECrate = abs(rowSums(a))
    
    LRM = (b1/ECrate)
    LRM = LRM * X # scaling
  } else {
    ECrate = 1-abs(rowSums(a))
    
    LRM = ((b1 + b0)/ECrate) 
    LRM = LRM * X # scaling
  }
  
  time_periods = 6
  
  Y = array(NA, dim=c(dim(posterior_coefs)[1], time_periods))
  
  Y[,1] = b0 * X # T0
  LR_effect = LRM - Y[,1]
  
  for (i in 2:time_periods) {
    Y[,i] = LR_effect * ECrate
    LR_effect = LR_effect - Y[,i]
  }
  
  
  estimates = data.frame(array(NA, dim=c(time_periods,3))) %>% 
    rename(est = X1, l = X2, u = X3) %>% 
    mutate(time = paste("t + ", 0:(time_periods-1), sep=""))
  
  for (i in 1:dim(estimates)[1]) {
    estimates$est[i] = median(Y[,i])
    estimates$l[i] = hdi(Y[,i], .width = ci)[1]
    estimates$u[i] = hdi(Y[,i], .width = ci)[2]
  }
  
  CI = round(hdi(LRM, .width = ci),3)
  
  label = paste("LRM: ", round(median(LRM),3), " (", CI[1,1], " - ",CI[1,2],")", sep="")
  
  title = gsub("b_","",colnames(posterior_coefs))
  title = gsub("_wi","",title)
  title = gsub("_pr_ctl","",title)
  title = gsub("_num_ctl","",title)
  title = gsub("_df","",title)
  
  p1 = ggplot(estimates, aes(x=time, y=est)) + 
    geom_bar(stat="identity") +
    #geom_errorbar(aes(ymin=l, ymax=u)) +
    geom_hline(yintercept = 0) +
    theme_bw()  +
    ggtitle(title, subtitle = label) +
    xlab("") +
    ylab(paste("Change in", dep_label, sep=" "))
  return(p1)
}


# Check Autocorrelation of Residuals ####

check_autocorresiduals = function(brmsmodel, runs = 10) {
  
  results = data.frame(array(NA, dim = c(runs, 5)))
  colnames(results) = c("var", "lower", "upper", "est", "draw")
  results[,1] = "residuals_lag"
  
  for (i in 1:runs) {
    print(i)
    resid_Data = ppc_resid_data(brmsmodel)
    
    resid_Data_lag = resid_Data %>% 
      group_by(country_text_id) %>% 
      mutate(resid_lag = dplyr::lag(residuals, 1))
    
    
    formula1 = strsplit(paste(brmsmodel$formula)[1], split = "~")
    formula1[[1]][1] = "residuals ~ resid_lag +" 
    # glmmtmb cannot handle (1|year_0); it is too close to zero
    formula1[[1]][2] = gsub("\\+ \\(1 \\| year_0\\)","",formula1[[1]][2])
    new_formula = as.formula(paste(formula1[[1]][1], formula1[[1]][2], collapse = ""))
    
    #print(new_formula)
    m1 = glmmTMB(new_formula,
                 #dispformula =~ country_text_id,
                 resid_Data_lag,
                 control=glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))
    library(mvtnorm)
    draw = rmvnorm(1, mean = fixef(m1)$cond, sigma = vcov(m1)$cond)
    
    results[i,2:4] = as.numeric(confint(m1)["cond.resid_lag", ]  )  
    results[i,5] = draw[2]
  }
  return(results)

}



make_sig_plot = function(combined_brms, pars="bw", prob_interval = 0.95, dep_label) {
  
  conflevel = 1-prob_interval
  probs = c(0 + conflevel/2, 1 -( conflevel/2))
  

  FKM_list = list()
  for (i in 1:length(combined_brms)) {
    
    sd_ind = combined_brms[[i]]$data %>% 
      select_at(vars(matches("FKM"))) %>% 
      select_at(vars(matches(pars))) %>% 
      summarise_all(sd) %>% 
      pull()
    
    post_sample = posterior_samples(combined_brms[[i]], pars="FKM") %>% 
      as_tibble() %>% 
      select_at(vars(matches(pars))) %>% 
      mutate_all(funs(. * sd_ind))
    
    FKM_list[[i]]  = post_sample %>% 
      summarise_all(funs(Estimate = mean, 
                         lower = HDInterval::hdi(., credMass=prob_interval)[1], 
                         upper = HDInterval::hdi(., credMass=prob_interval)[2])) %>% 
      mutate(parameter = colnames(post_sample))
    
    # 
    # FKM_list[[i]] = posterior_summary(combined_brms[[i]], pars="FKM", probs = probs) %>% 
    #   data.frame() %>% 
    #   mutate(parameter = rownames(.))  %>% 
    #   select(parameter, Estimate, lower = 3, upper=4) %>% 
    #   filter(grepl(pars, parameter))
    
  }
  
  FKM_rows = bind_rows(FKM_list)
  
  
  titlelab = ifelse(pars == "bw", "Between-Effect", "Within-Effect")
  
  posterior_summary(combined_brms[[1]], pars=pars, probs = probs) %>% 
    data.frame() %>% 
    mutate(parameter = rownames(.)) %>% 
    select(parameter, Estimate, lower = 3, upper=4) %>% 
    filter(grepl("FKM", parameter) == F) %>% 
    bind_rows(FKM_rows) %>% 
    filter(grepl("_lag", parameter) == F) %>% 
    mutate(parameter = gsub("_num_ctl","",parameter),
           parameter = gsub("_pr_ctl","",parameter),
           parameter = gsub("b_","",parameter),
           parameter = gsub("_df_wi","",parameter),
           parameter = gsub("_bw","",parameter),
           parameter = gsub("_wi","",parameter),
           parameter = gsub("_df","",parameter)) %>%
    ggplot(aes(x=parameter, y=Estimate, ymin=lower, ymax=upper)) +
    geom_point(size=2) +
    geom_errorbar(size=1) +
    xlab("") +
    ylab(paste("Change in", dep_label, sep=" ")) +
    coord_flip() +
    theme_bw() +
    geom_hline(yintercept = 0) +
    ggtitle(titlelab) +
    scale_y_continuous(breaks=seq(-2,2,0.2))
}



# Simulation ####

get_dynsim = function(scenario, brms_model, LDV1, LDV2 = NULL, simulations = 100, length_preds = 20, ecm=F, minus_wi) {
  
  mysimulations = matrix(NA, nrow=length_preds, ncol=simulations) %>% 
    data.frame() 

  scenario_filler = scenario
  for (i in 2:length_preds) {
    
    if(i %% 10==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
    }
    
    p_sc1 = data.frame(predictions = fitted( brms_model, newdata=scenario_filler[i-1,], summary=F, nsamples=simulations, allow_new_levels=T)) %>% 
      pull(predictions) %>%
      as.numeric()
    
    
    if (is.null(LDV2) == F) {
      scenario_filler[i,LDV2] =  scenario_filler[i-1,LDV1]
    }
    
    
    if (ecm == T) {
      p_sc1 = p_sc1 + as.numeric(scenario_filler[i-1,LDV1]) + minus_wi
      scenario_filler[i,LDV1] = mean(p_sc1) - minus_wi
    
    } else {
      scenario_filler[i,LDV1] = mean(p_sc1) - minus_wi 

    }
    
    
    mysimulations[i,] = p_sc1
    # print(scenario_filler[i-1,] )
    #print(as.numeric(scenario_filler[i-1,]))
    
  }
  
  final_frame = data.frame(
    est = rowMeans(mysimulations, na.rm=T),
    lower = apply(mysimulations, 1, FUN = function(x) hdi(x)[1]),
    upper = apply(mysimulations, 1, FUN = function(x) hdi(x)[2]),
    year = 1:length_preds
  )
  return(final_frame)
} 







get_dynsimxx = function(scenario, brms_model, LDV1, LDV2 = NULL, simulations = 100, length_preds = 20, ecm=F) {
  
  mysimulations = matrix(NA, nrow=length_preds, ncol=simulations) %>% 
    data.frame() 
  

  p_sc1 = NULL
  for (i in 1:length_preds) {
    
    scenario_filler = scenario[i,] 
    
    if (is.null(p_sc1) == F) {
      if (ecm == T) {
        p_sc1 = p_sc1 + as.numeric(scenario_filler[LDV1])
      }
      
      if (is.null(LDV2) == F) {
        scenario_filler[LDV2] =  scenario_filler[LDV1]
        print(scenario_filler[LDV1])
      }
      
      scenario_filler[LDV1] = mean(p_sc1) 
      
      mysimulations[i,] = p_sc1
    }
    
    p_sc1 = data.frame(predictions = fitted( brms_model, newdata=scenario_filler, summary=F, nsamples=simulations)) %>% 
      pull(predictions) %>%
      as.numeric()
    
    mysimulations[i,] = p_sc1
    
  }

  final_frame = data.frame(preds = t(mysimulations))
  return(final_frame)
} 
