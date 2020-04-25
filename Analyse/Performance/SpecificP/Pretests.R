library(performance)
library(bbmle)
library(glmmTMB)


# TEST FOR UNITROOT ####
library(broom)
table_data = data.frame(matrix(NA, nrow=length(vars), ncol=4)) %>% 
  rename(variable = X1, estimate = X2, CI_l = X3, CI_u = X4)
for (i in 1:length(vars)) {
  print(vars[i])
  my_frame = env_list[[1]] %>% 
    select(country_text_id, year_0, variable = vars[i]) %>%
    group_by(country_text_id) %>% 
    mutate(lag = dplyr::lag(variable, 1)) %>% 
    as.data.frame()
  
  
  m1 = plm(variable ~ lag + as.numeric(year_0),
           my_frame, index = c("country_text_id","year_0"), model = "within")
  
  getconfidence = tidy(m1, conf.int = TRUE, conf.level = 0.95) %>% 
    filter(term=="lag") 
  table_data$variable[i] = vars[i]
  table_data$estimate[i] = getconfidence$estimate
  table_data$CI_l[i] = getconfidence$conf.low
  table_data$CI_u[i] = getconfidence$conf.high
}

dust(table_data)  %>%
  sprinkle(round = 3) %>% 
  sprinkle(row=1, border_color="black", border="top", border_thickness =2) %>% 
  sprinkle(col=1, border_color="black", border="right", border_thickness =2) %>% 
  sprinkle_print_method("html")



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


general_formula = as.formula(
  soc_inequal_soc_df ~ 1 + 
    
    (1|country_text_id) + (1|year_0) + 
    # path dependence
    soc_inequal_soc_lag_wi  +
    

    #economic modernization
    pop_over65_wdi_num_ctl_wi_df + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi_df + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    cbi_w_cbi_num_ctl_wi_df + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    classification_core_num_ctl_wi_df + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi_df + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
     # main independent variables
    FKM_5_mb_fEc.FKM_5_mb_fEC_df_wi + FKM_5_mb_fEc.FKM_5_mb_fEC_wi_lag + FKM_5_mb_fEc.FKM_5_mb_fEC_bw +
    FKM_5_mb_fEc.FKM_5_mb_Fec_df_wi + FKM_5_mb_fEc.FKM_5_mb_Fec_wi_lag + FKM_5_mb_fEc.FKM_5_mb_Fec_bw +
    FKM_5_mb_fEc.FKM_5_mb_FeC_df_wi + FKM_5_mb_fEc.FKM_5_mb_FeC_wi_lag + FKM_5_mb_fEc.FKM_5_mb_FeC_bw +
    FKM_5_mb_FEC.FKM_5_mb_fEc_df_wi + FKM_5_mb_FEC.FKM_5_mb_fEc_wi_lag + FKM_5_mb_FEC.FKM_5_mb_fEc_bw
)




general_model = glmmTMB(
  general_formula,
  data2length,
  control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) 
)



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


names(data2length)

lag_model2 = glmmTMB(
  Adl_formula,
  data2length,
  control = glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5)) 
)
summary(lag_model2)
performance::check_autocorrelation(lag_model2)


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
lag_distribution_bayes(lag_model, "soc_inequal_soc_lag_wi", "corruption_", ci=.95)


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
