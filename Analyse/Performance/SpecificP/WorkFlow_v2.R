library(performance)
library(bbmle)
library(glmmTMB)
library(broom)

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
    vif_stats$Parameter = gsub("\\..*", "", vif_stats$Parameter)
    
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
    my_frame = datalist[[1]] %>% 
      select(country_text_id, year_0, variable = vars[i]) %>%
      group_by(country_text_id) %>% 
      mutate(lag = dplyr::lag(variable, 1)) %>% 
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
ppc_unithet = function(mod_homogen, mod_unit, dataset, independent_var, unit = "country") {
  ppc_homogen = posterior_predict(mod_homogen)
  ppc_unit = posterior_predict(mod_unit)
  
  
  plot_data_homogen = data.frame(mod_homogen$data, 
                                 country_text_id = dataset$country_text_id, 
                         year_0 = dataset$year_0,
                         posterior = t(ppc_homogen[sample(1:dim(ppc_homogen)[1], 1000),]))
  
  plot_data_unit = data.frame(mod_unit$data, 
                              country_text_id = dataset$country_text_id, 
                              year_0 = dataset$year_0,
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
    
  if (unit == "country") {
    
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
      ggtitle("No Unit Heterogeneity")
    
    err_unit_plot = errorbar_plot_data_ctr(plot_data_unit)   %>% 
      ggplot(aes(x=country_text_id, y=y_obs, grp=country_text_id)) +
      geom_point(col="black", size=2) +
      geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
      ylab("y (observed)") + 
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle("Unit Heterogeneity")   
    
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
      ggtitle("No Time Heterogeneity")
      
    err_unit_plot = errorbar_plot_data_year(plot_data_unit)   %>% 
      ggplot(aes(x=year_0, y=y_obs, grp=year_0)) +
      geom_point(col="black", size=2) +
      geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
      scale_x_continuous(breaks=seq(0,100,5)) +
      ylab("y (observed)") + 
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle("Time Heterogeneity")  
    
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
    
  final_plot = ggarrange(err_homogen_plot, err_unit_plot, 
                         p_value_plot_homogen, p_value_plot_unit, nrow=2, ncol=2)
  
  return(final_plot)
}


# PPC: Panel heteroscedasticity ####
ppc_panelhet = function(mod_homogen, mod_unit, dataset, independent_var) {
  ppc_homogen = posterior_predict(mod_homogen)
  ppc_unit = posterior_predict(mod_unit)
  
  
  plot_data_homogen = data.frame(mod_homogen$data, 
                                 country_text_id = dataset$country_text_id, 
                                 year_0 = dataset$year_0,
                                 posterior = t(ppc_homogen[sample(1:dim(ppc_homogen)[1], 1000),]))
  
  plot_data_unit = data.frame(mod_unit$data, 
                              country_text_id = dataset$country_text_id, 
                              year_0 = dataset$year_0,
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
    ggtitle("No Heteroskedasticity")
  
  err_unit_plot = errorbar_plot_data_ctr(plot_data_unit)   %>% 
    ggplot(aes(x=country_text_id, y=y_obs, grp=country_text_id)) +
    geom_point(col="black", size=2) +
    geom_errorbar(aes(ymin=y_rep_lower, ymax= y_rep_upper)) +
    ylab("y sd (observed)") + 
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Panel heteroskedasticity")   
  
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
  

  final_plot = ggarrange(err_homogen_plot, err_unit_plot, 
                         p_value_plot_homogen, p_value_plot_unit, nrow=2, ncol=2)
  
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

ppc_resid_time = function(mod_auto, dataset, country_sample = NULL, title = NULL) {
  mod_resid = residuals(mod_auto, type="pearson", method="posterior_predict")
  
  plot_data_residuals = data.frame(mod_auto$data, 
                                   # country_text_id = dataset$country_text_id, 
                                   year_0 = dataset$year_0,
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
    ggtitle(title) 
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

distribution_plot = function(model) {
  y = model$data$Y
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
lag_distribution_bayes = function(brms_model, LDV_label, IndV_label, unit = 1, time_periods=4, ci=0.95) {
  
  posterior_coefs = posterior_samples(brms_model, pars = IndV_label) %>% 
    select_at(vars(-matches("_bw")))
  print(colnames(posterior_coefs))
  posterior_LDV = posterior_samples(brms_model, pars = LDV_label)
  
  
  shift <- function (x, shift) c(rep(0,times=shift), x[1:(length(x)-shift)])
  
  coefs = cbind(as.matrix(posterior_coefs),
                matrix(0, nrow=dim(as.matrix(posterior_coefs))[1], ncol = time_periods))
  LDV = as.matrix(posterior_LDV)
  
  LRM = rowSums(coefs)/rowSums(1-LDV)
  
  # Impulse Matrices X and Y
  impulse_x = c(unit, rep(0, dim(coefs)[2]-1))
  impulse_y = c(0, rep(1, dim(coefs)[2]-1))
  
  for (ldv_dim in 2:dim(LDV)[2]) {
    impulse_y = impulse_y %>% 
      rbind(shift(impulse_y,1))
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
  ggplot(estimates, aes(x=time, y=est)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=l, ymax=u)) +
    geom_hline(yintercept = 0) +
    theme_bw()
  # barplot(colMeans(time_array), width = 1, space = 1, beside = TRUE, xlab = "Time Periods",
  #         ylab = "Change in Y", cex.main = 0.85, cex.lab=0.75, cex.axis=0.75 )
}

