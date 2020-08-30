# Economy Regression ####
Sys.time()
# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/economy_out_v4.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_eco.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/TSCS_imputation_Economy.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")

prior_phet1_eco <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                     set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                     set_prior("normal(0,2)", class = "Intercept"),
                     set_prior("normal(0,100)", class = "b"),
                     set_prior("normal(0, 1)", class = "b", coef="wealth_eco_wi_lag"))

prior_phet2_eco <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                     set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                     set_prior("normal(0,2)", class = "Intercept"),
                     set_prior("normal(0,100)", class = "b"),
                     set_prior("normal(0,1)", class = "b", coef="wealth_eco_wi_lag"),
                     set_prior("normal(0,1)", class = "b", coef="wealth_eco_wi_lag2"))


warmup = 1000
iter = 3000

# Create TSCS Data ####
wealth_list = make_reg_data(economy_out, "wealth_eco", 
                            naframe = mice_data_naframe_eco, 
                            vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                             "execpar_1981_odempr", "feduni1981_odempr",
                                             "centrip_odempr"),
                            lag2 = T,
                            nr_imputations = 5) 



# Test Model Structure ####
WEALTH_mpar = test_modelparameters("wealth_eco", "wealth_eco_wi_lag", wealth_list[1], chains=8)
saveBRMS("WEALTH_mpar", "Economy")
WEALTH_mpar = loadBRMS("WEALTH_mpar", "Economy")

pairs(WEALTH_mpar[[4]][[1]])


loo_compare(WEALTH_mpar[[1]][[1]], WEALTH_mpar[[2]][[1]], WEALTH_mpar[[3]][[1]], WEALTH_mpar[[4]][[1]], model_names = c("Base", "Unit Heterogeneity", "Contemporaneous Correlation", "Panel Heteroscedasticity")) %>% 
  loo_table()

p1 = ppc_unithet(WEALTH_mpar[[1]][[1]], WEALTH_mpar[[2]][[1]], wealth_list[[1]], "wealth_eco",unit = "country_text_id")
p2 = ppc_unithet(WEALTH_mpar[[2]][[1]], WEALTH_mpar[[3]][[1]], wealth_list[[1]], "wealth_eco",unit = "year_0")
p3 = ppc_panelhet(WEALTH_mpar[[3]][[1]], WEALTH_mpar[[4]][[1]], wealth_list[[1]], "wealth_eco")
ggarrange(p1,p2,p3, nrow=3)




# Test Autocorrelation Structure ####
base_formula1_eco = as.formula(wealth_eco ~ wealth_eco_wi_lag + 
                                 trend + 
                                 fiscalcrisis_cat_ctl + 
                                 pop_over65_wdi_pr_ctl_bw +
                                 corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                 cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                                 corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                 trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                 stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                 wealth_eco_spatial_ctl +
                                 
                                 (1|country_text_id) +  (1|year_0))


base_formula2_eco = as.formula(wealth_eco ~ wealth_eco_wi_lag + wealth_eco_wi_lag2 +
                                 trend + 
                                 fiscalcrisis_cat_ctl + 
                                 pop_over65_wdi_pr_ctl_bw +
                                 corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                 cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                                 corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                 trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                 stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                 wealth_eco_spatial_ctl +
                                 
                                 (1|country_text_id) +  (1|year_0))

base_formula1_eco_ecm = as.formula(wealth_eco_df ~ wealth_eco_wi_lag +
                                 trend + 
                                 fiscalcrisis_cat_ctl + 
                                 pop_over65_wdi_pr_ctl_bw +
                                 corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                 cbi_w_cbi_num_ctl_wi_df + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                                 corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                 trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                 stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                 wealth_eco_spatial_ctl +
                                 
                                 (1|country_text_id) +  (1|year_0))

base_formula2_eco_ecm = as.formula(wealth_eco_df ~ wealth_eco_wi_lag + wealth_eco_wi_lag2 + 
                                     trend + 
                                     fiscalcrisis_cat_ctl + 
                                     pop_over65_wdi_pr_ctl_bw +
                                     corporatism_vdem_pr_ctl_wi_df + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                     cbi_w_cbi_num_ctl_wi_df + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
                                     corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                     trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                     stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                     wealth_eco_spatial_ctl +
                                     
                                     (1|country_text_id) +  (1|year_0))



ECO_lag1 = make_brms_model(base_formula1_eco, addprofile = NULL, data = wealth_list[1], prior_brms = prior_phet1_eco, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("ECO_lag1", "Economy")


ECO_lag2 = make_brms_model(base_formula2_eco, addprofile = NULL, data = wealth_list[1], prior_brms = prior_phet2_eco, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("ECO_lag2", "Economy")

ECO_lag1_ecm = make_brms_model(base_formula1_eco_ecm, addprofile = NULL, data = wealth_list[1], prior_brms = prior_phet1_eco, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("ECO_lag1_ecm", "Economy")

ECO_lag2_ecm = make_brms_model(base_formula2_eco_ecm, addprofile = NULL, data = wealth_list[1], prior_brms = prior_phet2_eco, 
                               iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("ECO_lag2_ecm", "Economy")


ECO_lag1 = loadBRMS("ECO_lag1", "Economy")
ECO_lag2 = loadBRMS("ECO_lag2", "Economy")

AR1_test_eco = check_autocorresiduals(ECO_lag1[[1]], runs=100)
AR2_test_eco = check_autocorresiduals(ECO_lag2[[1]], runs=100)
AR1_test_eco1_ecm = check_autocorresiduals(ECO_lag1_ecm[[1]], runs=100)
AR2_test_eco2_ecm = check_autocorresiduals(ECO_lag2_ecm[[1]], runs=100)

saveBRMS("AR1_test_eco", "Economy")
saveBRMS("AR2_test_eco", "Economy")
saveBRMS("AR1_test_eco2_ecm", "Economy")
saveBRMS("AR2_test_eco2_ecm", "Economy")

AR1_test_eco = loadBRMS("AR1_test_eco", "Economy")
AR2_test_eco = loadBRMS("AR2_test_eco", "Economy")


hdi(AR1_test_eco$draw, 0.95)
hdi(AR2_test_eco$draw, 0.95)
hdi(AR1_test_eco_ecm$draw, 0.95)
hdi(AR2_test_eco2_ecm$draw, 0.95)



# Test Performance ####

ECO_mod_list = list()
for (i in 1:length(profiles_testing)) {
  ECO_mod_list[[i]] = make_brms_model(base_formula2_eco_ecm, addprofile = profiles_testing_ecm[[i]], data = wealth_list, prior_brms = prior_phet2_eco, 
                                      iter = 3000, warmup=1000, chains = 8, thin = 2)
  saveBRMS("ECO_mod_list", "Economy")
}





# Control Model 1 ####
# wealth_list[[1]] %>% 
#   select_at(vars(ends_with("wi"), ends_with("wi_lag"), -matches("FKM"), -ends_with("_odempr"))) %>% 
#   rename_all(funs(gsub("_num_ctl","",.))) %>% 
#   rename_all(funs(gsub("_cat_ctl","",.))) %>% 
#   rename_all(funs(gsub("_pr_ctl","",.))) %>% 
#   cor(use="pairwise", method = "pearson") %>% 
#   corrplot(method="number")
   

wealth_control1_lag1 = brm_multiple(
  bf(wealth_eco ~ wealth_eco_lag +
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
    sigma ~ (1|country_text_id)),
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_control1_lag1
wealth_control1_lag1

#generateTableBrms(wealth_control1_lag1)
saveBRMS("wealth_control1_lag1", "Economy")
#wealth_control1_lag1 = loadBRMS("wealth_control1_lag1", "Economy")
get_prior(bf(wealth_eco ~ wealth_eco_lag +
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
             sigma ~ (1|country_text_id)),
          data = wealth_list[1])


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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
wealth_control1_lag2
saveBRMS("wealth_control1_lag2", "Economy")


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
# test2 = check_autocorresiduals(wealth_control1_lag2, runs=10)
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
  data = wealth_list[1], 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6,
  verbose = T
)
wealth_control2
saveBRMS("wealth_control2", "Economy")


# Democracy Profiles ####
# wealth_list[[1]] %>% 
#   select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
#   rename_all(funs(gsub("_num_ctl","",.))) %>% 
#   rename_all(funs(gsub("_cat_ctl","",.))) %>% 
#   rename_all(funs(gsub("_pr_ctl","",.))) %>% 
#   cor(use="pairwise", method = "pearson") %>% 
#   corrplot(method="number")


# 5 Cluster Solution ####
wealth_FKM_15Feecc = brm_multiple( 
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
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15Feecc", "Economy")

wealth_FKM_25Feecc = update(wealth_FKM_15Feecc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_25Feecc", "Economy")


wealth_FKM_15FeeC = update(wealth_FKM_15Feecc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15FeeC", "Economy")

wealth_FKM_25FeeC = update(wealth_FKM_25Feecc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_25FeeC", "Economy")

wealth_FKM_15ffEC = update(wealth_FKM_15Feecc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15ffEC", "Economy")


wealth_FKM_25ffEC = update(wealth_FKM_25Feecc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_25ffEC", "Economy")

wealth_FKM_15ffEcc = update(wealth_FKM_15Feecc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15ffEcc", "Economy")


wealth_FKM_25ffEcc = brm_multiple(
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
  data = wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_25ffEcc", "Economy")

wealth_FKM_15FEC = brm_multiple(
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
  data= wealth_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_15FEC", "Economy")


wealth_FKM_25FEC = update(wealth_FKM_25ffEcc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_25FEC", "Economy")

# Dimensional Profiles ####
wealth_FKM_1E =update(wealth_FKM_15FEC, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("wealth_FKM_1E", "Economy")

wealth_FKM_2E = update(wealth_FKM_25ffEcc, 
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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_2E", "Economy")

wealth_FKM_1c = brm_multiple( 
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
                      data = wealth_list, 
                      warmup = 2000, iter = 3000,
                      chains = 6
)

saveBRMS("wealth_FKM_1c", "Economy")

wealth_FKM_2c = update(wealth_FKM_25ffEcc, 
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
  warmup = warmup, iter = iter,
  chains = 6
)

saveBRMS("wealth_FKM_2c", "Economy")




# Other Democracy Profiles ####

# wealth_list[[1]] %>% 
#   select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
#   rename_all(funs(gsub("_num_ctl","",.))) %>% 
#   rename_all(funs(gsub("_cat_ctl","",.))) %>% 
#   rename_all(funs(gsub("_pr_ctl","",.))) %>% 
#   cor(use="pairwise", method = "pearson") %>% 
#   corrplot(method="number")

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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_1Lij", "Economy")

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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_2Lij", "Economy")


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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_1Cent", "Economy")

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
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("wealth_FKM_2Cent", "Economy")

# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "cbi_w_cbi", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(wealth_FKM_2Cent, "wealth_eco_lag", "trade_wdi", unit = 1, time_periods=7, ci=0.95)

Sys.time()
