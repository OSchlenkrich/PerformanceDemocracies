# Economy Regression ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/economy_out_v4.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_eco.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/TSCS_imputation_Economy.R")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")

prior_phet1_ecop <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                     set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                     set_prior("normal(0,2)", class = "Intercept"),
                     set_prior("normal(0,100)", class = "b"),
                     set_prior("normal(0, 1)", class = "b", coef="productivity_eco_wi_lag"))

prior_phet2_ecop <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                     set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                     set_prior("normal(0,2)", class = "Intercept"),
                     set_prior("normal(0,100)", class = "b"),
                     set_prior("normal(0,1)", class = "b", coef="productivity_eco_wi_lag"),
                     set_prior("normal(0,1)", class = "b", coef="productivity_eco_wi_lag2"))


# Create TSCS Data ####
productivity_list = make_reg_data(economy_out, "productivity_eco", 
                                  naframe = mice_data_naframe_eco, 
                                  vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                                   "execpar_1981_odempr", "feduni1981_odempr",
                                                   "centrip_odempr"),
                                  lag2 = T,
                                  nr_imputations = 5) 




# Test Model Structure ####
product_mpar = test_modelparameters("productivity_eco", "productivity_eco_wi_lag", productivity_list[1], chains=8)
saveBRMS("product_mpar", "Economy")
product_mpar = loadBRMS("product_mpar", "Economy")


loo_compare(product_mpar[[1]][[1]], product_mpar[[2]][[1]], product_mpar[[3]][[1]], product_mpar[[4]][[1]], model_names = c("Base", "Unit Heterogeneity", "Contemporaneous Correlation", "Panel Heteroscedasticity")) %>% 
  loo_table()

p1 = ppc_unithet(product_mpar[[1]][[1]], product_mpar[[2]][[1]], productivity_list[[1]], "productivity_eco",unit = "country_text_id")
p2 = ppc_unithet(product_mpar[[2]][[1]], product_mpar[[3]][[1]], productivity_list[[1]], "productivity_eco",unit = "year_0")
p3 = ppc_panelhet(product_mpar[[3]][[1]], product_mpar[[4]][[1]], productivity_list[[1]], "productivity_eco")
ggarrange(p1,p2,p3, nrow=3)


# Test Autocorrelation Structure ####
base_formula1_ecop = as.formula(productivity_eco ~ productivity_eco_wi_lag +
                                   trend + 
                                   fiscalcrisis_cat_ctl + 
                                   pop_over65_wdi_pr_ctl_bw +
                                   corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                   cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                                   corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                   trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                   stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                   productivity_eco_spatial_ctl +
                                   
                                   (1|country_text_id) +  (1|year_0))


base_formula2_ecop = as.formula(productivity_eco ~ productivity_eco_wi_lag + productivity_eco_wi_lag2 +
                                   trend + 
                                   fiscalcrisis_cat_ctl + 
                                   pop_over65_wdi_pr_ctl_bw +
                                   corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                   cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                                   corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                   trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                   stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                   productivity_eco_spatial_ctl +
                                   
                                   (1|country_text_id) +  (1|year_0))



PROD_lag1 = make_brms_model(base_formula1_ecop, addprofile = NULL, data = productivity_list[1], prior_brms = prior_phet1_ecop, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("PROD_lag1", "Economy")


PROD_lag2 = make_brms_model(base_formula2_ecop, addprofile = NULL, data = productivity_list[1], prior_brms = prior_phet2_ecop, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("PROD_lag2", "Economy")


PROD_lag1 = loadBRMS("PROD_lag1", "Economy")
PROD_lag2 = loadBRMS("PROD_lag2", "Economy")

AR1_test_prod = check_autocorresiduals(PROD_lag1, runs=100)
AR2_test_prod = check_autocorresiduals(PROD_lag2, runs=100)
saveBRMS("AR1_test_prod", "Economy")
saveBRMS("AR2_test_prod", "Economy")

hdi(AR1_test_prod$draw, 0.95)
hdi(AR2_test_prod$draw, 0.95)


# Test Performance ####
PROD_mod_list = list()

for (i in 3:length(profiles_testing)) {
  PROD_mod_list[[i]] = make_brms_model(base_formula2_ecop, addprofile = profiles_testing[[i]], data = productivity_list, prior_brms = prior_phet2_ecop, 
                                      iter = 3000, warmup=1000, chains = 8, thin = 2)
  saveBRMS("PROD_mod_list", "Economy")
}






# PRODUCTIVITY ####

# Control Model 1 ####
productivity_list[[1]] %>% 
  select_at(vars(ends_with("wi"), ends_with("wi_lag"), -matches("FKM"), -ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")


productivity_control1_lag1 = brm_multiple(
  productivity_eco ~ productivity_eco_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list[1], 
  family = gaussian(),
  prior = prior_base_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_control1_lag1
#generateTableBrms(productivity_control1_lag1)
saveBRMS("productivity_control1_lag1")
#readRDS(paste("Analyse/Performance/SpecificP/brmsModels/", modelname, sep=""))


productivity_control1_lag2 = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
    trend + 
    fiscalcrisis_cat_ctl + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list[1], 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_control1_lag2
saveBRMS("productivity_control1_lag2")


# generateTableBrms(productivity_control1_lag2)
# lag_distribution_bayes(productivity_control1_lag2, "productivity_eco_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_control1_lag2, "productivity_eco_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_control1_lag2, "productivity_eco_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_control1_lag2, "productivity_eco_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# 
# ppc_resid_time(productivity_control1_lag1)
# brmsmodel = productivity_control1_lag1
# 
# test1 = check_autocorresiduals(productivity_control1_lag1, runs=50)
# test2 = check_autocorresiduals(productivity_control1_lag2, runs=50)
# 
# length(which(test1$lower > 0))/50
# length(which(test2$lower > 0))/50


# Control Model 2 ####

productivity_control2 = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list, 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_control2
saveBRMS("productivity_control2")


# Democracy Profiles ####
productivity_list[[1]] %>% 
  select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# 3 Cluster Solution ####
#FEc
productivity_FKM_13FEcc = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 + 
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    FKM3_FEc_wi + FKM3_FEc_wi_lag + FKM3_FEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list, 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("productivity_FKM_13FEcc")

productivity_FKM_23FEcc = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM3_FEc_wi + FKM3_FEc_wi_lag + FKM3_FEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list, 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_FKM_23FEcc
saveBRMS("productivity_FKM_23FEc")


readRDS("Analyse/Performance/SpecificP/brmsModels/productivity_FKM_23FEc.rds")

productivity_FKM_13FeeC = update(productivity_FKM_13FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 + 
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            FKM3_FeC_wi + FKM3_FeC_wi_lag + FKM3_FeC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_13FeeC")

productivity_FKM_23FeeC = update(productivity_FKM_23FEcc, 
                          productivity_eco ~ productivity_eco_lag  + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                            cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                            
                            FKM3_FeC_wi + FKM3_FeC_wi_lag + FKM3_FeC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_23FeeC")

productivity_FKM_13ffEC = update(productivity_FKM_13FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            FKM3_fEC_wi + FKM3_fEC_wi_lag + FKM3_fEC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_13ffEC")


productivity_FKM_23ffEC =  update(productivity_FKM_23FEcc, 
                           productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                             trend + 
                             pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                             corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                             cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                             fiscalcrisis_cat_ctl + 
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                             productivity_eco_spatial_ctl +
                             
                             unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                             cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                             
                             FKM3_fEC_wi + FKM3_fEC_wi_lag + FKM3_fEC_bw + 
                             
                             (1|country_text_id) +  (1|year_0),
                           newdata = productivity_list, 
                           family = gaussian(),
                           prior = prior_base2_prod,
                           warmup = 2000, iter = 3000,
                           #control = list(adapt_delta = 0.95), 
                           chains = 6
)

saveBRMS("productivity_FKM_23ffEC")

# 5 Cluster Solution ####
productivity_FKM_15Feecc = update(productivity_FKM_13FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)

saveBRMS("productivity_FKM_15Feecc")

productivity_FKM_25Feecc = update(productivity_FKM_23FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                            cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                            
                            FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_25Feecc")


productivity_FKM_15FeeC = update(productivity_FKM_13FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)

saveBRMS("productivity_FKM_15FeeC")

productivity_FKM_25FeeC = update(productivity_FKM_23FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                            cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                            
                            FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_25FeeC")

productivity_FKM_15ffEC = update(productivity_FKM_13FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_15ffEC")


productivity_FKM_25ffEC = update(productivity_FKM_23FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                            cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                            
                            FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_25ffEC")

productivity_FKM_15ffEcc = update(productivity_FKM_13FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_15ffEcc")


productivity_FKM_25ffEcc = update(productivity_FKM_23FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                            cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                            
                            FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
saveBRMS("productivity_FKM_25ffEcc")

productivity_FKM_15FEC = update(productivity_FKM_13FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
productivity_FKM_15FEC
saveBRMS("productivity_FKM_15FEC")


productivity_FKM_25FEC = update(productivity_FKM_23FEcc, 
                          productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                            trend + 
                            pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            productivity_eco_spatial_ctl +
                            
                            unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                            cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                            
                            FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = productivity_list, 
                          family = gaussian(),
                          prior = prior_base2_prod,
                          warmup = 2000, iter = 3000,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)
productivity_FKM_25FEC
saveBRMS("productivity_FKM_25FEC")

# Dimensional Profiles ####
productivity_FKM_1E =update(productivity_FKM_13FEcc, 
                      productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                        trend + 
                        pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                        corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                        cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                        corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                        trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                        fiscalcrisis_cat_ctl + 
                        stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                        productivity_eco_spatial_ctl +
                        
                        FKM5_E_wi + FKM5_E_wi_lag + FKM5_E_bw + 
                        
                        (1|country_text_id) +  (1|year_0),
                      newdata = productivity_list, 
                      family = gaussian(),
                      prior = prior_base2_prod,
                      warmup = 2000, iter = 3000,
                      #control = list(adapt_delta = 0.95), 
                      chains = 6
)
saveBRMS("productivity_FKM_1E")

productivity_FKM_2E = update(productivity_FKM_23FEcc, 
                       productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                         trend + 
                         pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                         corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                         corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         fiscalcrisis_cat_ctl + 
                         stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                         productivity_eco_spatial_ctl +
                         
                         unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                         cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                         
                         FKM5_E_wi + FKM5_E_wi_lag + FKM5_E_bw + 
                         
                         (1|country_text_id) +  (1|year_0),
                       newdata = productivity_list, 
                       family = gaussian(),
                       prior = prior_base2_prod,
                       warmup = 2000, iter = 3000,
                       #control = list(adapt_delta = 0.95), 
                       chains = 6
)
productivity_FKM_2E
saveBRMS("productivity_FKM_2E")

productivity_FKM_1c = update(productivity_FKM_13FEcc, 
                       productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                         trend + 
                         pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                         corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                         corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         fiscalcrisis_cat_ctl + 
                         stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                         productivity_eco_spatial_ctl +
                         
                         FKM5_c_wi + FKM5_c_wi_lag + FKM5_c_bw + 
                         
                         (1|country_text_id) +  (1|year_0),
                       newdata = productivity_list, 
                       warmup = 2000, iter = 3000,
                       chains = 6
)
productivity_FKM_1c
saveBRMS("productivity_FKM_1c")

productivity_FKM_2c = update(productivity_FKM_23FEcc, 
                       productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
                         trend + 
                         pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                         corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                         corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         fiscalcrisis_cat_ctl + 
                         stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                         productivity_eco_spatial_ctl +
                         
                         unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                         cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                         
                         FKM5_c_wi + FKM5_c_wi_lag + FKM5_c_bw + 
                         
                         (1|country_text_id) +  (1|year_0),
                       newdata = productivity_list, 
                       warmup = 2000, iter = 3000,
                       chains = 6
)
productivity_FKM_2c
saveBRMS("productivity_FKM_2c")




# Other Democracy Profiles ####

productivity_list[[1]] %>% 
  select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.))) %>% 
  rename_all(funs(gsub("_cat_ctl","",.))) %>% 
  rename_all(funs(gsub("_pr_ctl","",.))) %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# Lijphart
productivity_FKM_1Lij = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list, 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_FKM_1Lij
saveBRMS("productivity_FKM_1Lij")

productivity_FKM_2Lij = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list, 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_FKM_2Lij
saveBRMS("productivity_FKM_2Lij")


# Centripetalism
productivity_FKM_1Cent = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list, 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_FKM_1Cent
saveBRMS("productivity_FKM_1Cent")

# lag_distribution_bayes(productivity_FKM_1Cent, "productivity_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(productivity_FKM_1Cent, "productivity_eco_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# generateTableBrms(productivity_FKM_1Cent, productivity_FKM_2Cent)


productivity_FKM_2Cent = brm_multiple(
  productivity_eco ~ productivity_eco_lag + productivity_eco_lag2 +
    trend + 
    pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    productivity_eco_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = productivity_list, 
  family = gaussian(),
  prior = prior_base2_prod,
  warmup = 2000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
productivity_FKM_2Cent
saveBRMS("productivity_FKM_2Cent")

# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "cbi_w_cbi", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(productivity_FKM_2Cent, "productivity_eco_lag", "trade_wdi", unit = 1, time_periods=7, ci=0.95)
