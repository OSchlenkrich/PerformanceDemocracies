# Environment Regression AIR ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_env_v3.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_env.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/TSCS_imputation_Environment.R")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")

prior_base_env <- c(set_prior("cauchy(0,5)", class = "sd"),
                    set_prior("cauchy(0,5)", class = "sigma"),
                     set_prior("normal(0,100)", class = "Intercept"),
                     set_prior("normal(0,100)", class = "b"),
                     set_prior("normal(0, 1)", class = "b", coef="air_env_lag"))

prior_base2_env <- c(set_prior("cauchy(0,5)", class = "sd"),
                     set_prior("cauchy(0,5)", class = "sigma"),
                      set_prior("normal(0,100)", class = "Intercept"),
                      set_prior("normal(0,100)", class = "b"),
                      set_prior("normal(0,1)", class = "b", coef="air_env_lag"),
                      set_prior("normal(0,1)", class = "b", coef="air_env_lag2"))

# Create TSCS Data ####
air_list = make_reg_data(a.out_env, "GEP_env", 
                         naframe = mice_data_naframe_env, 
                         vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl", 
                                          "green_plt_cpds_notrans_ctl", "strength_green_notrans_ctl",
                                          "execpar_1981_odempr", "feduni1981_odempr",
                                          "centrip_odempr"))
# ENVIRONMENT: AIR ####

# Control Model 1 ####
air_list[[1]] %>% 
  select_at(vars(ends_with("wi"), ends_with("wi_lag"), -matches("FKM"), -ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")
air_list[[1]] %>% 
  select_at(vars(ends_with("wi_df"), ends_with("wi_lag"), -matches("FKM"), -ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

air_control1_lag1 = brm_multiple(
  air_env ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    (1|country_text_id) +  (1|year_0),
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_control1_lag1
#generateTableBrms(env_control1_lag1)
saveBRMS("air_control1_lag1")
#readRDS(paste("Analyse/Performance/SpecificP/brmsModels/", modelname, sep=""))


air_control1_lag2 = brm_multiple(
  air_env ~ air_env_lag + air_env_lag2 +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    (1|country_text_id) +  (1|year_0),
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base2_env,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_control1_lag2
saveBRMS("air_control1_lag2")


# generateTableBrms(air_control1_lag2)
# lag_distribution_bayes(air_control1_lag2, "air_env_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_control1_lag2, "air_env_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_control1_lag2, "air_env_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_control1_lag2, "air_env_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# 
# ppc_resid_time(air_control1_lag1)
# brmsmodel = air_control1_lag1
# 
# test1 = check_autocorresiduals(air_control1_lag1, runs=50)
# test2 = check_autocorresiduals(air_control1_lag2, runs=50)

# 1 Lag is enough 
# length(which(test1$lower > 0))/50
# length(which(test2$lower > 0))/50



# Control Model 2 ####

air_control2 = brm_multiple(
  air_env ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    strength_green_notrans_ctl_wi + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
    green_plt_cpds_notrans_ctl_wi + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    (1|country_text_id) +  (1|year_0),
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3500,
  control = list(adapt_delta = 0.95), 
  chains = 6
)
air_control2
saveBRMS("air_control2")

# test2 = check_autocorresiduals(air_control2, runs=50)
# length(which(test2$lower > 0))/50

air_control2_ecm = brm_multiple(
  air_env_df ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
    green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
    unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    (1|country_text_id) +  (1|year_0),
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3500,
  control = list(adapt_delta = 0.95), 
  chains = 6
)
generateTableBrms(air_control2_ecm)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "green_plt", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "green_plt", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "gdppc", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "cabinet", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "gdppc", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes_ecm(air_control2_ecm, "air_env_lag", "gdppc", unit = 1, time_periods=7, ci=0.95)


# Democracy Profiles ####
air_list[[1]] %>% 
  select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# 3 Cluster Solution ####
#FEc
air_FKM_13FEcc = brm_multiple(
  air_env_df ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    FKM3_FEc_wi_df + FKM3_FEc_wi_lag + FKM3_FEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
generateTableBrms(air_FKM_13FEcc)
lag_distribution_bayes_ecm(air_FKM_13FEcc, "air_env_lag", "trade", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes_ecm(air_FKM_13FEcc, "air_env_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes_ecm(air_FKM_13FEcc, "air_env_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes_ecm(air_FKM_13FEcc, "air_env_lag", "gdppc", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes_ecm(air_FKM_13FEcc, "air_env_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes_ecm(air_FKM_13FEcc, "air_env_lag", "FKM3_FEc", unit = 1, time_periods=7, ci=0.95)

fitted_var_alldata_plot(air_FKM_13FEcc, "trade_wdi_num_ctl_bw")
fitted_res_plot(air_FKM_13FEcc)

saveBRMS("air_FKM_13FEcc")


air_FKM_23FEcc = brm_multiple(
  air_env_df ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
    green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
    unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM3_FEc_wi_df + FKM3_FEc_wi_lag + FKM3_FEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3500,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_FKM_23FEcc
saveBRMS("air_FKM_23FEc")
# lag_distribution_bayes_ecm(air_FKM_23FEcc, "air_env_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)


readRDS("Analyse/Performance/SpecificP/brmsModels/air_FKM_23FEc.rds")

air_FKM_13FeeC = update(air_FKM_13FEcc, 
                        air_env_df ~ air_env_lag +
                          trend + 
                          fiscalcrisis_cat_ctl + 
                          gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                          
                          corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                          corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                          trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                          stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                          air_env_spatial_ctl +
                          
                          
                          FKM3_FeC_wi_df + FKM3_FeC_wi_lag + FKM3_FeC_bw + 
                          
                          (1|country_text_id) +  (1|year_0),
                        newdata = air_list[1], 
                        family = gaussian(),
                        prior = prior_base,
                        warmup = 2000, iter = 3000,
                        #control = list(adapt_delta = 0.95), 
                        chains = 6
)
saveBRMS("air_FKM_13FeeC")

air_FKM_23FeeC = update(air_FKM_23FEcc, 
                        air_env_df ~ air_env_lag +
                          trend + 
                          fiscalcrisis_cat_ctl + 
                          gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                          
                          corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                          corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                          trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                          stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                          air_env_spatial_ctl +
                          
                          strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                          green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                          unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                          cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                          
                          FKM3_FeC_wi_df + FKM3_FeC_wi_lag + FKM3_FeC_bw + 
                          
                          (1|country_text_id) +  (1|year_0),
                        newdata = air_list, 
                        family = gaussian(),
                        prior = prior_base,
                        warmup = 2000, iter = 3000,
                        #control = list(adapt_delta = 0.95), 
                        chains = 6
)
saveBRMS("air_FKM_23FeeC")

air_FKM_13ffEC = update(air_FKM_13FEcc, 
                        air_env_df ~ air_env_lag +
                          trend + 
                          fiscalcrisis_cat_ctl + 
                          gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                          
                          corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                          corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                          trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                          stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                          air_env_spatial_ctl +
                          
                          
                          FKM3_fEC_wi_df + FKM3_fEC_wi_lag + FKM3_fEC_bw + 
                          
                          (1|country_text_id) +  (1|year_0),
                        newdata = air_list, 
                        family = gaussian(),
                        prior = prior_base_env,
                        warmup = 2000, iter = 3000,
                        #control = list(adapt_delta = 0.95), 
                        chains = 6
)
saveBRMS("air_FKM_13ffEC")


air_FKM_23ffEC =  update(air_FKM_23FEcc, 
                         air_env_df ~ air_env_lag +
                           trend + 
                           fiscalcrisis_cat_ctl + 
                           gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                           
                           corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                           corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                           trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                           stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                           air_env_spatial_ctl +
                           
                           strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                           green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                           unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                           cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                           
                           FKM3_fEC_wi_df + FKM3_fEC_wi_lag + FKM3_fEC_bw + 
                           
                           (1|country_text_id) +  (1|year_0),
                         newdata = air_list, 
                         family = gaussian(),
                         prior = prior_base_env,
                         warmup = 2000, iter = 3000,
                         #control = list(adapt_delta = 0.95), 
                         chains = 6
)

saveBRMS("air_FKM_23ffEC")

# 5 Cluster Solution ####
air_FKM_15Feecc = update(air_FKM_13FEcc, 
                         air_env_df ~ air_env_lag +
                           trend + 
                           fiscalcrisis_cat_ctl + 
                           gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                           
                           corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                           corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                           trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                           stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                           air_env_spatial_ctl +
                           
                           FKM5_Fec_wi_df + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
                           
                           (1|country_text_id) +  (1|year_0),
                         newdata = air_list, 
                         family = gaussian(),
                         prior = prior_base_env,
                         warmup = 2000, iter = 3000,
                         #control = list(adapt_delta = 0.95), 
                         chains = 6
)

saveBRMS("air_FKM_15Feecc")

air_FKM_25Feecc = update(air_FKM_23FEcc, 
                         air_env_df ~ air_env_lag +
                           trend + 
                           fiscalcrisis_cat_ctl + 
                           gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                           
                           corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                           corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                           trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                           stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                           air_env_spatial_ctl +
                           
                           strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                           green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                           unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                           cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                           
                           FKM5_Fec_wi_df + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
                           
                           (1|country_text_id) +  (1|year_0),
                         newdata = air_list, 
                         family = gaussian(),
                         prior = prior_base_env,
                         warmup = 2000, iter = 3000,
                         #control = list(adapt_delta = 0.95), 
                         chains = 6
)
saveBRMS("air_FKM_25Feecc")


air_FKM_15FeeC = update(air_FKM_13FEcc, 
                        air_env_df ~ air_env_lag +
                          trend + 
                          fiscalcrisis_cat_ctl + 
                          gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                          
                          corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                          corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                          trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                          stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                          air_env_spatial_ctl +
                          
                          FKM5_FeC_wi_df + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
                          
                          (1|country_text_id) +  (1|year_0),
                        newdata = air_list, 
                        family = gaussian(),
                        prior = prior_base_env,
                        warmup = 2000, iter = 3000,
                        #control = list(adapt_delta = 0.95), 
                        chains = 6
)

saveBRMS("air_FKM_15FeeC")

air_FKM_25FeeC = update(air_FKM_23FEcc, 
                        air_env_df ~ air_env_lag +
                          trend + 
                          fiscalcrisis_cat_ctl + 
                          gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                          
                          corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                          corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                          trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                          stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                          air_env_spatial_ctl +
                          
                          strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                          green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                          unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                          cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                          
                          FKM5_FeC_wi_df + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
                          
                          (1|country_text_id) +  (1|year_0),
                        newdata = air_list, 
                        family = gaussian(),
                        prior = prior_base_env,
                        warmup = 2000, iter = 3000,
                        #control = list(adapt_delta = 0.95), 
                        chains = 6
)
saveBRMS("air_FKM_25FeeC")

air_FKM_15ffEC = update(air_FKM_13FEcc, 
                        air_env_df ~ air_env_lag +
                          trend + 
                          fiscalcrisis_cat_ctl + 
                          gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                          
                          corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                          corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                          trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                          stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                          air_env_spatial_ctl +
                          
                          FKM5_fEC_wi_df + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
                          
                          (1|country_text_id) +  (1|year_0),
                        newdata = air_list, 
                        family = gaussian(),
                        prior = prior_base_env,
                        warmup = 2000, iter = 3000,
                        #control = list(adapt_delta = 0.95), 
                        chains = 6
)
saveBRMS("air_FKM_15ffEC")


air_FKM_25ffEC = update(air_FKM_23FEcc, 
                        air_env_df ~ air_env_lag +
                          trend + 
                          fiscalcrisis_cat_ctl + 
                          gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                          
                          corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                          corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                          trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                          stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                          air_env_spatial_ctl +
                          
                          strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                          green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                          unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                          cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                          
                          FKM5_fEC_wi_df + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
                          
                          (1|country_text_id) +  (1|year_0),
                        newdata = air_list, 
                        family = gaussian(),
                        prior = prior_base_env,
                        warmup = 2000, iter = 3000,
                        #control = list(adapt_delta = 0.95), 
                        chains = 6
)
saveBRMS("air_FKM_25ffEC")

air_FKM_15ffEcc = update(air_FKM_13FEcc, 
                         air_env_df ~ air_env_lag +
                           trend + 
                           fiscalcrisis_cat_ctl + 
                           gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                           
                           corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                           corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                           trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                           stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                           air_env_spatial_ctl +
                           
                           FKM5_fEc_wi_df + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
                           
                           (1|country_text_id) +  (1|year_0),
                         newdata = air_list, 
                         family = gaussian(),
                         prior = prior_base_env,
                         warmup = 2000, iter = 3000,
                         #control = list(adapt_delta = 0.95), 
                         chains = 6
)
saveBRMS("air_FKM_15ffEcc")


air_FKM_25ffEcc = update(air_FKM_23FEcc, 
                         air_env_df ~ air_env_lag +
                           trend + 
                           fiscalcrisis_cat_ctl + 
                           gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                           
                           corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                           corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                           trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                           stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                           air_env_spatial_ctl +
                           
                           strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                           green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                           unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                           cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                           
                           FKM5_fEc_wi_df + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
                           
                           (1|country_text_id) +  (1|year_0),
                         newdata = air_list, 
                         family = gaussian(),
                         prior = prior_base_env,
                         warmup = 2000, iter = 3000,
                         #control = list(adapt_delta = 0.95), 
                         chains = 6
)
saveBRMS("air_FKM_25ffEcc")

air_FKM_15FEC = update(air_FKM_13FEcc, 
                       air_env_df ~ air_env_lag +
                         trend + 
                         fiscalcrisis_cat_ctl + 
                         gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                         
                         corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                         air_env_spatial_ctl +
                         
                         FKM5_FEC_wi_df + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
                         
                         (1|country_text_id) +  (1|year_0),
                       newdata = air_list, 
                       family = gaussian(),
                       prior = prior_base_env,
                       warmup = 2000, iter = 3000,
                       #control = list(adapt_delta = 0.95), 
                       chains = 6
)
air_FKM_15FEC
saveBRMS("air_FKM_15FEC")


air_FKM_25FEC = update(air_FKM_23FEcc, 
                       air_env_df ~ air_env_lag +
                         trend + 
                         fiscalcrisis_cat_ctl + 
                         gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                         
                         corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                         air_env_spatial_ctl +
                         
                         strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                         green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                         unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                         cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                         
                         FKM5_FEC_wi_df + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
                         
                         (1|country_text_id) +  (1|year_0),
                       newdata = air_list, 
                       family = gaussian(),
                       prior = prior_base_env,
                       warmup = 2000, iter = 3000,
                       #control = list(adapt_delta = 0.95), 
                       chains = 6
)
air_FKM_25FEC
saveBRMS("air_FKM_25FEC")

# Dimensional Profiles ####
air_FKM_1E = update(air_FKM_13FEcc, 
                   air_env_df ~ air_env_lag +
                     trend + 
                     fiscalcrisis_cat_ctl + 
                     gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                     
                     corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                     corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                     trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                     stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                     air_env_spatial_ctl +
                     
                     FKM5_E_wi_df + FKM5_E_wi_lag + FKM5_E_bw + 
                     
                     (1|country_text_id) +  (1|year_0),
                   newdata = air_list[1], 
                   family = gaussian(),
                   prior = prior_base_env,
                   warmup = 2000, iter = 3000,
                   #control = list(adapt_delta = 0.95), 
                   chains = 6
)
saveBRMS("air_FKM_1E")
lag_distribution_bayes_ecm(air_FKM_1E, "air_env_lag", "trade", unit = 1, time_periods=7, ci=0.95)

air_FKM_2E = update(air_FKM_23FEcc, 
                    air_env_df ~ air_env_lag +
                      trend + 
                      fiscalcrisis_cat_ctl + 
                      gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                      
                      corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                      corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                      trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                      stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                      air_env_spatial_ctl +
                      
                      strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                      green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                      unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                      cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                      
                      FKM5_E_wi_df + FKM5_E_wi_lag + FKM5_E_bw + 
                      
                      (1|country_text_id) +  (1|year_0),
                    newdata = air_list, 
                    family = gaussian(),
                    prior = prior_base_env,
                    warmup = 2000, iter = 3000,
                    #control = list(adapt_delta = 0.95), 
                    chains = 6
)
air_FKM_2E
saveBRMS("air_FKM_2E")
lag_distribution_bayes_ecm(air_FKM_2E, "air_env_lag", "FKM", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes_ecm(air_FKM_2E, "air_env_lag", "cabinet", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes_ecm(air_FKM_2E, "air_env_lag", "strength", unit = 1, time_periods=7, ci=0.95)

air_FKM_1c = update(air_FKM_13FEcc, 
                    air_env_df ~ air_env_lag +
                      trend + 
                      fiscalcrisis_cat_ctl + 
                      gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                      
                      corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                      corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                      trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                      stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                      air_env_spatial_ctl +
                      
                      FKM5_c_wi_df + FKM5_c_wi_lag + FKM5_c_bw + 
                      
                      (1|country_text_id) +  (1|year_0),
                    newdata = air_list, 
                    warmup = 2000, iter = 3000,
                    chains = 6
)
air_FKM_1c
saveBRMS("air_FKM_1c")
lag_distribution_bayes_ecm(air_FKM_1c, "air_env_lag", "FKM5", unit = 1, time_periods=7, ci=0.95)


air_FKM_2c = update(air_FKM_23FEcc, 
                    air_env_df ~ air_env_lag +
                      trend + 
                      fiscalcrisis_cat_ctl + 
                      gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                      
                      corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                      corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                      trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                      stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                      air_env_spatial_ctl +
                      
                      strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                      green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                      unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                      cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                      
                      FKM5_c_wi_df + FKM5_c_wi_lag + FKM5_c_bw + 
                      
                      (1|country_text_id) +  (1|year_0),
                    newdata = air_list, 
                    warmup = 2000, iter = 3000,
                    chains = 6
)
air_FKM_2c
saveBRMS("air_FKM_2c")




# Other Democracy Profiles ####

air_list[[1]] %>% 
  select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# Lijphart
air_FKM_1Lij = brm_multiple(
  air_env_df ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_FKM_1Lij
saveBRMS("air_FKM_1Lij")

air_FKM_2Lij = brm_multiple(
  air_env_df ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
    green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
    unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = air_list, 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_FKM_2Lij
saveBRMS("air_FKM_2Lij")


# Centripetalism
air_FKM_1Cent = brm_multiple(
  air_env_df ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    centrip_odempr_wi_df + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = air_list, 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_FKM_1Cent
saveBRMS("air_FKM_1Cent")

# lag_distribution_bayes_ecm(air_FKM_1Cent, "air_env_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(air_FKM_1Cent, "air_env_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# generateTableBrms(air_FKM_1Cent, air_FKM_2Cent)


air_FKM_2Cent = brm_multiple(
  air_env_df ~ air_env_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    air_env_spatial_ctl +
    
    strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
    green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
    unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    centrip_odempr_wi_df + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = air_list, 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_FKM_2Cent
saveBRMS("air_FKM_2Cent")

# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "cbi_w_cbi", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(air_FKM_2Cent, "air_env_lag", "trade_wdi", unit = 1, time_periods=7, ci=0.95)