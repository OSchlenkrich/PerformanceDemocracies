# Economy Regression ####
Sys.time()
# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/economy_out_v2.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_eco.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/TSCS_imputation_Economy.R")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")

prior_base <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                set_prior("cauchy(0,5)", class = "sigma"),
                set_prior("normal(0,100)", class = "Intercept"),
                set_prior("normal(0,100)", class = "b"),
                set_prior("normal(0, 1)", class = "b", coef="wealth_eco_lag"))

prior_base2 <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                 set_prior("cauchy(0,5)", class = "sigma"),
                set_prior("normal(0,100)", class = "Intercept"),
                set_prior("normal(0,100)", class = "b"),
                set_prior("normal(0,1)", class = "b", coef="wealth_eco_lag"),
                set_prior("normal(0,1)", class = "b", coef="wealth_eco_lag2"))

# Create TSCS Data ####
wealth_list = make_reg_data(economy_out, "wealth_eco", 
                            naframe = mice_data_naframe_eco, 
                            vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                             "execpar_1981_odempr", "feduni1981_odempr",
                                             "centrip_odempr"))

# WEALTH ####

# Control Model 1 ####
wealth_list[[1]] %>% 
  select_at(vars(ends_with("wi"), ends_with("wi_lag"), -matches("FKM"), -ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")
  

wealth_control1_lag1 = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_control1_lag1
#generateTableBrms(wealth_control1_lag1)
saveBRMS("wealth_control1_lag1")


#readRDS(paste("Analyse/Performance/SpecificP/brmsModels/", modelname, sep=""))

wealth_control1_lag2 = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    fiscalcrisis_cat_ctl + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
  
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_control1_lag2
saveBRMS("wealth_control1_lag2")


# generateTableBrms(wealth_control1_lag2)
# lag_distribution_bayes(wealth_control1_lag2, "wealth_eco_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_control1_lag2, "wealth_eco_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_control1_lag2, "wealth_eco_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_control1_lag2, "wealth_eco_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# 
# ppc_resid_time(wealth_control1_lag1)
# brmsmodel = wealth_control1_lag1
# 
# test1 = check_autocorresiduals(wealth_control1_lag1, runs=10)
# test2 = check_autocorresiduals(wealth_control1_lag2, runs=100)
# 
# length(which(test1$lower > 0))/10
# length(which(test2$upper < 0))/100


# Control Model 2 ####

wealth_control2 = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_control2
saveBRMS("wealth_control2")


# Democracy Profiles ####
wealth_list[[1]] %>% 
  select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# 3 Cluster Solution ####
#FEc
wealth_FKM_13FEcc = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 + 
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM3_FEc_wi + FKM3_FEc_wi_lag + FKM3_FEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_13FEcc")

wealth_FKM_23FEcc = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM3_FEc_wi + FKM3_FEc_wi_lag + FKM3_FEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_23FEcc
saveBRMS("wealth_FKM_23FEc")

wealth_FKM_13FeeC = update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 + 
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM3_FeC_wi + FKM3_FeC_wi_lag + FKM3_FeC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_13FeeC")

wealth_FKM_23FeeC = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag  + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM3_FeC_wi + FKM3_FeC_wi_lag + FKM3_FeC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_23FeeC")

wealth_FKM_13ffEC = update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM3_fEC_wi + FKM3_fEC_wi_lag + FKM3_fEC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_13ffEC")


wealth_FKM_23ffEC =  update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM3_fEC_wi + FKM3_fEC_wi_lag + FKM3_fEC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_23ffEC")

# 5 Cluster Solution ####
wealth_FKM_15Feecc = update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15Feecc")

wealth_FKM_25Feecc = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_25Feecc")


wealth_FKM_15FeeC = update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15FeeC")

wealth_FKM_25FeeC = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_25FeeC")

wealth_FKM_15ffEC = update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15ffEC")


wealth_FKM_25ffEC = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_25ffEC")

wealth_FKM_15ffEcc = update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15ffEcc")


wealth_FKM_25ffEcc = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_25ffEcc")

wealth_FKM_15FEC = update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15FEC")


wealth_FKM_25FEC = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_25FEC
saveBRMS("wealth_FKM_25FEC")

# Dimensional Profiles ####
wealth_FKM_1E =update(wealth_FKM_13FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +

    FKM5_E_wi + FKM5_E_wi_lag + FKM5_E_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_1E")

wealth_FKM_2E = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_E_wi + FKM5_E_wi_lag + FKM5_E_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_2E
saveBRMS("wealth_FKM_2E")

wealth_FKM_1c = update(wealth_FKM_13FEcc, 
                      wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
                        trend + 
                        pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                        corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                        cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                        corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                        trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                        fiscalcrisis_cat_ctl + 
                        stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                        wealth_eco_spatial_ctl +
                      
                        FKM5_c_wi + FKM5_c_wi_lag + FKM5_c_bw + 
                        
                        (1|country_text_id) +  (1|year_0),
                      newdata = wealth_list, 
                      warmup = 2000, iter = 3000,
                      chains = 6
)
wealth_FKM_1c
saveBRMS("wealth_FKM_1c")

wealth_FKM_2c = update(wealth_FKM_23FEcc, 
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_c_wi + FKM5_c_wi_lag + FKM5_c_bw + 
    
    (1|country_text_id) +  (1|year_0),
  newdata = wealth_list, 
  warmup = 2000, iter = 3000,
  chains = 6
)
wealth_FKM_2c
saveBRMS("wealth_FKM_2c")




# Other Democracy Profiles ####

wealth_list[[1]] %>% 
  select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# Lijphart
wealth_FKM_1Lij = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_1Lij
saveBRMS("wealth_FKM_1Lij")

wealth_FKM_2Lij = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_2Lij
saveBRMS("wealth_FKM_2Lij")


# Centripetalism
wealth_FKM_1Cent = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_1Cent
saveBRMS("wealth_FKM_1Cent")

# lag_distribution_bayes(wealth_FKM_1Cent, "wealth_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(wealth_FKM_1Cent, "wealth_eco_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# generateTableBrms(wealth_FKM_1Cent, wealth_FKM_2Cent)


wealth_FKM_2Cent = brm_multiple(
  wealth_eco ~ wealth_eco_lag + wealth_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    wealth_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_2Cent
saveBRMS("wealth_FKM_2Cent")

# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "cbi_w_cbi", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "trade_wdi", unit = 1, time_periods=7, ci=0.95)

# gestartet: 23:01
Sys.time()
