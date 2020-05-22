library(performance)
library(bbmle)
library(glmmTMB)
library(broom)


# TEST FOR UNITROOT ####

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





# Test for Serial Correlation
check_LM_plm = function(model, lagorder = 1) {
  
  model_formula = formula(model)
  
  model_formula_resi = as.formula(gsub("\\+ \\(1 \\| country_text_id\\)", "", Reduce(paste, deparse(model_formula))))
  model_formula_resi = as.formula(gsub("\\+ \\(1 \\| year_0\\)", "", Reduce(paste, deparse(model_formula_resi))))
  model_formula_resi = as.formula(gsub("year_0", "as.numeric(year_0)", Reduce(paste, deparse(model_formula_resi))))
  
  plm_model = plm(model_formula_resi, wealth_list[[1]], index=c("country_text_id", "year_0"), method="within")
  print(summary(plm_model))
  
  table_data = data.frame('LM test' = rep(NA, lagorder), 
                          df = rep(NA, lagorder), 
                          p_value = rep(NA, lagorder))
  
  for (i in 1:lagorder) {
    bg_result = pbgtest(plm_model, order=i, fill=0)
    
    print(bg_result)
    
    
    # Create Table
    table_data$LM.test[i] = round(bg_result$statistic,3)
    table_data$df[i] = bg_result$parameter 
    table_data$p_value[i] = round(bg_result$p.value,4)
    
  }
  
  
  dust(table_data)  %>%
    sprinkle(font_size = 12, part="head") %>%
    sprinkle(font_size = 10) %>% 
    sprinkle(merge = T, cols=3, part="head") %>% 
    
    sprinkle(row=1, border_color="black", border="top", border_thickness =2) %>% 
    sprinkle(col=1, border_color="black", border="right", border_thickness =2) %>% 
    sprinkle(col=1, border_color="black", border="right", border_thickness =2, part="head") %>% 
    sprinkle(col=2, border_color="black", border="right", border_thickness =1, part="body")%>% 
    sprinkle(col=2, border_color="black", border="right", border_thickness =1, part="head")%>% 
    
    sprinkle(halign="center", part="head") %>% 
    sprinkle(halign="center", part="body", cols=2:3) %>% 
    sprinkle_print_method("html")
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

