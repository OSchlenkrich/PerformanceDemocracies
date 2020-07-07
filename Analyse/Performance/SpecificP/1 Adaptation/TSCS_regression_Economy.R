# Economy Regression ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/economy_out_v2.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_eco.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/TSCS_imputation_Economy.R")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")

# Create TSCS Data ####
wealth_list = make_reg_data(economy_out, "wealth_eco", 
                            naframe = mice_data_naframe_eco, 
                            vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl"))
productivity_list = make_reg_data(economy_out, "productivity_eco", 
                                  naframe = mice_data_naframe_eco, 
                                  vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl"))


# WEALTH ####
# Base Model ####
prior_base <- c(set_prior("normal(0,10)", class = "Intercept"),
                set_prior("normal(0.5,0.2)", class = "b", coef = "wealth_eco_lag"),
                set_prior("normal(0,10)", class = "b"),
                set_prior("cauchy(0,5)", class = "sd"),
                set_prior("cauchy(0,5)", class = "sigma"))


wealth_base = brm_multiple(
  wealth_eco ~ wealth_eco_lag + 
    trend +
    (1|country_text_id) +  (1|year_0),
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

generateTableBrms(wealth_base)


# Control Model 1 ####
wealth_list[[1]] %>% 
  select_at(vars(ends_with("wi"), ends_with("wi_lag"), -matches("FKM"), -ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")
  


wealth_control1 = brm_multiple(
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_control1
generateTableBrms(wealth_control1)
test = ppc_resid_time(wealth_control1)
testsmall = test %>% 
  filter(country_text_id == "ESP") 

acf(testsmall$residuals, lag.max =20, plot=F)

test2 = test %>% 
  filter(country_text_id == "ESP") %>% 
  mutate(lag_res = dplyr::lag(residuals, 3))
cor(test2$residuals, test2$lag_res, use="pairwise")

# Control Model 2 ####

wealth_control2 = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_control2


# Democracy Profiles ####
wealth_list[[1]] %>% 
  select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# 3 Cluster Solution ####
wealth_FKM_13FEc = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_13FEc

wealth_FKM_23FEc = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_23FEc


wealth_FKM_13FeC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_13FeC

wealth_FKM_23FeC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_23FeC

wealth_FKM_13fEC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_13fEC


wealth_FKM_23fEC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_23fEC

# 5 Cluster Solution ####
wealth_FKM_15Fec = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_15Fec

wealth_FKM_25Fec = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_25Fec


wealth_FKM_15FeC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_15FeC

wealth_FKM_25FeC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_25FeC

wealth_FKM_15fEC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_15fEC


wealth_FKM_25fEC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_25fEC

wealth_FKM_15fEc = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_15fEc


wealth_FKM_25fEc = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_25fEc

wealth_FKM_15FEC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_15FEC


wealth_FKM_25FEC = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_25FEC

# Dimensional Profiles ####
wealth_FKM_1E = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

wealth_FKM_2E = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_2E

wealth_FKM_1c = update(wealth_FKM_1E, 
                      wealth_eco ~ wealth_eco_lag +
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
                      newdata = wealth_list[1], 
                      warmup = 2000, iter = 3000,
                      chains = 6
)
wealth_FKM_1c

wealth_FKM_2c = update(wealth_FKM_2E, 
  wealth_eco ~ wealth_eco_lag +
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
  newdata = wealth_list[1], 
  warmup = 2000, iter = 3000,
  chains = 6
)
wealth_FKM_2c




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
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_1Lij

wealth_FKM_2Lij = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_2Lij


# Centripetalism
wealth_FKM_1Cent = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_1Cent
lag_distribution_bayes(wealth_FKM_1Cent, "wealth_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_FKM_1Cent, "wealth_eco_lag", "trade", unit = 1, time_periods=7, ci=0.95)

generateTableBrms(wealth_FKM_1Cent, wealth_FKM_2Cent)


wealth_FKM_2Cent = brm_multiple(
  wealth_eco ~ wealth_eco_lag +
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_FKM_2Cent

lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "cbi_w_cbi", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "trade_wdi", unit = 1, time_periods=7, ci=0.95)
