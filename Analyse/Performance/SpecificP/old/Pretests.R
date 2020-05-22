library(performance)
library(bbmle)
library(glmmTMB)
library(broom)


# TEST FOR UNITROOT ####

chech_stationarity = function(vars, datalist, model = "plm") {
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
      m1 = glmmTMB(variable ~ lag + (1|country_text_id),
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


test = plm(wealth_eco ~ 1 + wealth_eco_lag + wealth_eco_lag2 + as.numeric(year_0) + fiscalcrisis_num_ctl,
           wealth_list[[1]], index=c("country_text_id", "year_0"), method="within"
)
summary(test)
pbgtest(test, order=1, fill=0)
test$residuals

new_data = data.frame(index(test), test$model, resi = test$residuals) %>% 
  ungroup() %>% 
  #group_by(country_text_id) %>% 
  mutate(resi_lag = dplyr::lag(resi, 1,  default = 0)) %>% 
  as.data.frame()

test2 = plm(resi ~ 1 + resi_lag + wealth_eco_lag + wealth_eco_lag2  + year_0 + fiscalcrisis_num_ctl,
            new_data, index=c("country_text_id", "year_0"), model="within")
summary(test2)


sum((predict(test2) - mean(new_data$resi))^2)/sum((new_data$resi- mean(new_data$resi))^2)
0.001549038*nrow(new_data)


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


check_LM_plm = function(model, lagorder) {
  
  model_formula = formula(model)

  model_formula_resi = as.formula(gsub(".* 1 ", "resi ~ resi_lag ", Reduce(paste, deparse(model_formula))))
  model_formula_resi = as.formula(gsub("\\+ \\(1 \\| country_text_id\\)", "", Reduce(paste, deparse(model_formula_resi))))
  model_formula_resi = as.formula(gsub("\\+ \\(1 \\| year_0\\)", "", Reduce(paste, deparse(model_formula_resi))))
  model_formula_resi = as.formula(gsub("year_0", "as.numeric(year_0)", Reduce(paste, deparse(model_formula_resi))))
  
  aux_data = data.frame(model$frame, resi = residuals(model)) %>% 
    group_by(country_text_id) %>% 
    mutate(resi_lag = dplyr::lag(resi, 1,  default = 0)) %>%
    as.data.frame() 
  
  print(model_formula_resi)
  
  m1 = plm(model_formula_resi,
           aux_data, index=c("country_text_id", "year_0"), model="within")

  rsquared = sum((predict(m1) - mean(aux_data$resi))^2)/sum((aux_data$resi- mean(aux_data$resi))^2)


  # BG Test  
  bg = rsquared * nrow(m1$model)
  sig = pchisq(bg, 1, lower.tail = FALSE)

  # Create Table
  table_data = data.frame('LM test' = round(bg,3), 
                          df = 1, 
                          p_value = sig)


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

m1 = plm(wealth_eco_df ~ wealth_eco_lag + wealth_eco_lag2 + as.numeric(year_0) ,
         wealth_list[[1]], index=c("country_text_id", "year_0"), model="within")
pbgtest(m1)




check_LM = function(model, LDV) {
  
  model_formula = formula(model)
  model_formula = as.formula(gsub("_df", "", Reduce(paste, deparse(formula(model)))))
  
  model_formula_resi = as.formula(gsub(".* 1 ", "resi ~ resi_lag ", Reduce(paste, deparse(model_formula))))
  model_formula_resi = as.formula(gsub("\\+ \\(1 \\| country_text_id\\)", "", Reduce(paste, deparse(model_formula_resi))))
  model_formula_resi = as.formula(gsub("\\+ \\(1 \\| year_0\\)", "", Reduce(paste, deparse(model_formula_resi))))
  
  aux_data = data.frame(model$frame, resi = residuals(model)) %>% 
    group_by(country_text_id) %>% 
    mutate(resi_lag = dplyr::lag(resi, 1,  default = 0)) %>% 
    as.data.frame()
  
  m1 = glmmTMB(model_formula_resi,
               aux_data,
               control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) )
  
  # Confidence Intervalls
  parameter_estimate_resi = round(data.frame(confint(m1, "resi_lag", level = 0.95)), 3)
  parameter_estimate_LDV = round(data.frame(confint(model, LDV, level = 0.95)), 3)
  
  
  # Create Table
  
  table_data = data.frame(matrix(NA, nrow=2, ncol=4)) %>% 
    rename(variable = X1, estimate = X2, CI_l = X3, CI_u = X4)
  
  table_data$variable[1] = LDV
  table_data$estimate[1] = parameter_estimate_LDV$Estimate
  table_data$CI_l[1] =   parameter_estimate_LDV$X2.5..
  table_data$CI_u[1] =   parameter_estimate_LDV$X97.5..
  
  table_data$variable[2] = "lagged Residuals"
  table_data$estimate[2] = parameter_estimate_resi$Estimate
  table_data$CI_l[2] =   parameter_estimate_resi$X2.5..
  table_data$CI_u[2] =   parameter_estimate_resi$X97.5..
  
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




table_data = data.frame(matrix(NA, nrow=length(vars), ncol=4)) %>% 
  rename(variable = X1, estimate = X2, CI_l = X3, CI_u = X4)

for (i in 1:length(vars)) {
  print(vars[i])
  my_frame = ds_list[[1]] %>% 
    select(country_text_id, year_0, variable = vars[i]) %>%
    group_by(country_text_id) %>% 
    mutate(lag = dplyr::lag(variable, 1)) %>% 
    as.data.frame() 
  
  
  # m1 = glmmTMB(variable ~ lag + year_0 + (1|country_text_id),
  #          my_frame,
  #          control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) )
  m1 = plm(variable ~ lag + as.numeric(year_0),
               my_frame, index=c("country_text_id", "year_0"), model="within")
  
  #parameter_estimate = confint(m1, "lag", level = 0.95)
  parameter_estimate = tidy(m1, conf_level=.95, conf.int=T) %>% 
    filter(term == "lag")
  
  table_data$variable[i] = vars[i]
  table_data$estimate[i] = parameter_estimate[,3]
  table_data$CI_l[i] =   parameter_estimate[,1]
  table_data$CI_u[i] =   parameter_estimate[,2]
}

dust(table_data)  %>%
  sprinkle(round = 3) %>% 
  sprinkle(row=1, border_color="black", border="top", border_thickness =2) %>% 
  sprinkle(col=1, border_color="black", border="right", border_thickness =2) %>% 
  sprinkle_print_method("html")

n_distinct(my_frame$year_0)

countries = data2length %>% 
  group_by(country_text_id) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  filter(n > 15)



my_frame_plm = pdata.frame(my_frame %>% filter(country_text_id %in%countries$country_text_id) %>% 
                             group_by(year_0) %>% 
                             mutate(variable = variable - mean(variable, narm=T)),  index=c("country_text_id", "year_0"))


asd = purtest(my_frame_plm$variable, exo="intercept", test="ips", lags="AIC", pmax=4)
asd
summary(asd)


# Economy
data2length = wealth_list[[1]] %>% 
  select_at(vars(
    country_text_id,
    year_0,
    wealth_eco,
    wealth_eco_df,
    wealth_eco_lag_wi,
    productivity_eco,
    productivity_eco_df,
    productivity_eco_lag_wi,
    matches("wi"),
    matches("bw")
    
    )) %>% 
  na.omit()

# Integration
data2length = soc_list[[1]] %>% 
  select_at(vars(
    country_text_id,
    year_0,
    eco_inequal_soc,
    eco_inequal_soc_df,
    eco_inequal_soc_lag_wi,
    soc_inequal_soc,
    soc_inequal_soc_df,
    soc_inequal_soc_lag_wi,
    matches("wi"),
    matches("bw")
    
  )) %>% 
  na.omit()

# PubSafe

data2length = ds_list[[1]] %>% 
  select_at(vars(
    country_text_id,
    year_0,
    fiscalcrisis_num_ctl,
    pubsafe_ds,
    pubsafe_ds_df,
    pubsafe_ds_lag_wi,
    pubsafe_ds_lag,
    pubsafe_ds_lag2,
    FKM_5_mb_fEc.FKM_5_mb_fEC,
    matches("wi"),
    matches("bw")
    
  )) %>% 
  na.omit()

n_distinct(data2length$country_text_id)
n_distinct(data2length$year_0)

general_formula = as.formula(
  pubsafe_ds_df ~ 1 + 
    
    (1|country_text_id) + (1|year_0) + 
    # path dependence
    pubsafe_ds_lag_wi +
    year_0 +
    fiscalcrisis_num_ctl +

    #economic modernization
    # pop_over65_wdi_num_ctl_wi_df + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi_df + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    # unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    #cbi_w_cbi_num_ctl_wi_df + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    #classification_core_num_ctl_wi_df + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi_df + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    # trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    eco_inequal_soc_num_ctl_wi_df + eco_inequal_soc_num_ctl_wi_lag +
    
     # main independent variables
    FKM_5_mb_fEc.FKM_5_mb_fEC_df_wi + FKM_5_mb_fEc.FKM_5_mb_fEC_wi_lag + FKM_5_mb_fEc.FKM_5_mb_fEC_bw +
    FKM_5_mb_fEc.FKM_5_mb_Fec_df_wi + FKM_5_mb_fEc.FKM_5_mb_Fec_wi_lag + FKM_5_mb_fEc.FKM_5_mb_Fec_bw +
    FKM_5_mb_fEc.FKM_5_mb_FeC_df_wi + FKM_5_mb_fEc.FKM_5_mb_FeC_wi_lag + FKM_5_mb_fEc.FKM_5_mb_FeC_bw +
    FKM_5_mb_FEC.FKM_5_mb_fEc_df_wi + FKM_5_mb_FEC.FKM_5_mb_fEc_wi_lag + FKM_5_mb_FEC.FKM_5_mb_fEc_bw
)

ggplot(ds_list[[1]], aes(year_0 , pubsafe_ds)) + 
  geom_point()


general_model = glmmTMB(
  general_formula,
  data2length,
  control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) 
)
summary(general_model)


var_names = colnames(general_model$frame)
laglength = 4

new_formula = general_formula

modellist = list()
modellist[[1]] = general_model
for (i in 2:laglength) {
  LIV = var_names[endsWith(var_names, "lag")]
  LIV_mod = paste(paste(LIV, i, sep=""), collapse = "+")
  
  new_formula = as.formula(paste(Reduce(paste, deparse(new_formula)), "+",LIV_mod))
  
  lag_model = glmmTMB(
    new_formula,
    data2length,
    control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) 
  )
  modellist[[i]]  = lag_model

}
names(data2length)
AICtab(modellist)
summary(modellist[[3]])


Adl_formula = as.formula(gsub("_df", "",Reduce(paste, deparse(modellist[[1]]$call$formula))))
Adl_formula = as.formula(gsub("_df", "",Reduce(paste, deparse(general_formula))))


names(data2length)

lag_model2 = glmmTMB(
  Adl_formula,
  ds_list[[1]] ,
  control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) 
)
summary(lag_model2)
performance::check_autocorrelation(lag_model2)




plmdata = wealth_list[[1]] %>% 
  #filter(country_text_id %in% "DEU" ) %>% 
  as.data.frame()

test = plm(wealth_eco ~ 1 + wealth_eco_lag+ fiscalcrisis_num_ctl,
           plmdata, index=c("country_text_id", "year_0"), method="within"
    )
summary(test)
pbgtest(test, order=1, fill=0)


new_data = data.frame(index(test), test$model, resi = test$residuals) %>% 
  ungroup() %>% 
  #group_by(country_text_id) %>% 
  mutate(resi_lag = dplyr::lag(resi, 1,  default = 0)) %>% 
  as.data.frame()

test2 = plm(resi ~ 1 + resi_lag + fiscalcrisis_num_ctl,
           new_data, index=c("country_text_id", "year_0"), model="within")
summary(test2)


sum((predict(test2) - mean(new_data$resi))^2)/sum((new_data$resi- mean(new_data$resi))^2)
0.01418825*nrow(new_data)

test3 = lm(resi ~ 1 + resi_lag + fiscalcrisis_num_ctl + country_text_id,
            new_data)
summary(test3)
summary(test3)$r.squared * nrow(new_data)
0.7032634 * (nrow(new_data)-1)


test33 = lm(wealth_eco ~ 1 + fiscalcrisis_num_ctl + country_text_id,
            plmdata)
bgtest(test33)


sum(test2$fitted.values^2)/sum(new_data$resi^2)
sum(predict(test2)^2)/sum(new_data$resi^2)

new_data = data.frame(plmdata, resi = test$residuals) %>% 
  na.omit() %>% 
  #group_by(country_text_id) %>% 
  mutate(resi_lag = dplyr::lag(resi, 1,  default = 0)) %>% 
  as.data.frame()

pchisq(2.274226, 1, lower.tail = FALSE)


r.squared(test2, "ess")



0.05942818*8
n_distinct(new_data$year_0)

lag_model = brm(
  Adl_formula,
  data2length, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 6000,
  chains=5 
)

library(bayesplot)
get_variables(lag_model)

summary(lag_model)
lag_distribution_bayes(lag_model, "pubsafe_ds_lag", "eco_inequa", ci=.95)


bayesplot::mcmc_intervals(lag_model, regex_pars="Fec_wi.*") +
  geom_vline(xintercept = 0)
  


  
lag_distribution2( 0.846431  ,  c(0.107624  , 0 , 0, 0  ), sd = 1, time_periods=5)
coefplot(fit_ml, 
         level = .8,
         r_intervals = TRUE, 
         r_col = "firebrick")

summary(lag_model)
check_collinearity(lag_model)

cor(data2length$corruption_vdem_num_ctl_bw, data2length$classification_core_num_ctl_bw)



lag_distribution2( 0.846431  ,  c(0.11  , -0.07   , -0.04, 0.01   ), sd = 1, time_periods=5)

AIC(m1)

lag_model= glmmTMB(
  productivity_eco ~ 1 + 
    # path dependence
    productivity_eco_lag_wi  +
    
    # Parties
    cabinet_cpds_num_ctl_wi + cabinet_cpds_num_ctl_wi_lag  + cabinet_cpds_num_ctl_bw +
    
    # formal institutions
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag   + cbi_w_cbi_num_ctl_bw +
    
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    
    # main independent variables
    FKM_5_mb_E.FKM_5_mb_e_wi + FKM_5_mb_E.FKM_5_mb_e_wi_lag + FKM_5_mb_E.FKM_5_mb_e_bw +
    
    (1|country_text_id) + (1|year_0),
  data2length,
  control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) 
)
summary(lag_model2)
AICtab(general_model, lag_model)



check_collinearity(lag_model)

lag_distribution2( 0.865611 ,  c(-0.112405, 0.118145), sd = 1, time_periods=5)



wealth_list[[1]]$productivity_eco

wealth_eco_lag_wi          0.9382209  0.0080106  117.12   <2e-16 ***
cbi_w_cbi_num_ctl_wi       0.0074691  0.0025421    2.94   0.0033 ** 
cbi_w_cbi_num_ctl_wi_lag  -0.0002082  0.0030225   -0.07   0.9451    
cbi_w_cbi_num_ctl_wi_lag2 -0.0061462  0.0026313   -2.34   0.0195 *  
cbi_w_cbi_num_ctl_bw       0.1098882  0.1257662    0.87   0.3823  


0.007557 -  0.007796 
-0.006479 -  0.007557  
