# Environment Regression AIR ####

# Setup ####
library(rstan)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_env_v4.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_env.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/TSCS_imputation_Environment.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")



prior_phet1_env <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                 set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                 set_prior("normal(0,2)", class = "Intercept"),
                 set_prior("normal(0,100)", class = "b"),
                 set_prior("normal(0, 1)", class = "b", coef="GEP_env_wi_lag"))

prior_phet2_env <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                 set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                 set_prior("normal(0,2)", class = "Intercept"),
                 set_prior("normal(0,100)", class = "b"),
                 set_prior("normal(0,1)", class = "b", coef="GEP_env_wi_lag"),
                 set_prior("normal(0,1)", class = "b", coef="GEP_env_wi_lag2"))


# Create TSCS Data ####
GEP_list = make_reg_data(a.out_env, "GEP_env", 
                         naframe = mice_data_naframe_env, 
                         vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl", 
                                          "green_plt_cpds_notrans_ctl", "strength_green_notrans_ctl",
                                          "execpar_1981_odempr", "feduni1981_odempr",
                                          "centrip_odempr"),
                         lag2 = T,
                         nr_imputation=5)

# Test Model Strucutre ####

GEP_mpar = test_modelparameters("GEP_env", "GEP_env_wi_lag", GEP_list[1])
saveBRMS("GEP_mpar", "Environment")
GEP_mpar = loadBRMS("GEP_mpar", "Environment")

GEP_mpar[[1]][[1]] = add_criterion(GEP_mpar[[1]][[1]], "loo")
GEP_mpar[[2]][[1]]  = add_criterion(GEP_mpar[[2]][[1]], "loo")
GEP_mpar[[3]][[1]]  = add_criterion(GEP_mpar[[3]][[1]], "loo")
GEP_mpar[[4]][[1]]  = add_criterion(GEP_mpar[[4]][[1]], "loo")

loo_compare(GEP_mpar[[1]][[1]] , GEP_mpar[[2]][[1]] , GEP_mpar[[3]][[1]] , GEP_mpar[[4]][[1]], model_names = c("Base", "Unit Heterogeneity", "Contemporaneous Correlation", "Panel Heteroscedasticity")) %>% 
  loo_table()

p1 = ppc_unithet(GEP_mpar[[1]][[1]], GEP_mpar[[2]][[1]], GEP_list[[1]], "GEP_env",unit = "country_text_id")
p2 = ppc_unithet(GEP_mpar[[2]][[1]], GEP_mpar[[3]][[1]], GEP_list[[1]], "GEP_env",unit = "year_0")
p3 = ppc_panelhet(GEP_mpar[[3]][[1]], GEP_mpar[[4]][[1]], GEP_list[[1]], "GEP_env")
ggarrange(p1,p2,p3, nrow=3)



# Test Performance ####
# Structural ####

base_formula1_env = as.formula(GEP_env ~ GEP_env_wi_lag +
                         trend + 
                         fiscalcrisis_cat_ctl + 
                         gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                         
                         corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                         GEP_env_spatial_ctl +
                         
                         (1|country_text_id) +  (1|year_0))

base_formula2_env = as.formula(GEP_env ~ GEP_env_wi_lag + GEP_env_wi_lag2 +
                         trend + 
                         fiscalcrisis_cat_ctl + 
                         gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                         
                         corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                         GEP_env_spatial_ctl +
                         
                         (1|country_text_id) +  (1|year_0))


GEP_lag1 = make_brms_model(base_formula1_env, addprofile = NULL, GEP_list[1], prior_phet1_env, warmup = 1000, iter = 2000, chains = 8)
saveBRMS("GEP_lag1", "Environment")

GEP_lag2 = make_brms_model(base_formula2_env, addprofile = NULL, GEP_list[1], prior_phet2_env, warmup = 1000, iter = 2000, chains = 8)
saveBRMS("GEP_lag2", "Environment")


GEP_lag1 = loadBRMS("GEP_lag1", "Environment")
GEP_lag2 = loadBRMS("GEP_lag2", "Environment")


AR1_test_GEP = check_autocorresiduals(GEP_lag1, runs = 100)
AR2_test_GEP = check_autocorresiduals(GEP_lag2, runs = 100)
saveBRMS("AR1_test_GEP", "Environment")
saveBRMS("AR2_test_GEP", "Environment")
AR1_test_GEP = loadBRMS("AR1_test_GEP", "Environment")
AR2_test_GEP = loadBRMS("AR2_test_GEP", "Environment")


# 1 Lag is sufficient
hdi(AR1_test_GEP$draw, 0.95)
hdi(AR2_test_GEP$draw, 0.95)


# Test Performance ####

GEP_mod_list = list()

for (i in 1:length(profiles_testing)) {
  GEP_mod_list[[i]] = make_brms_model(base_formula1_env, addprofile = profiles_testing[[i]], data = GEP_list, prior_brms = prior_phet1_env, 
                                  iter = 3000, warmup=1000, chains = 8, thin = 2)
  saveBRMS("GEP_mod_list", "Environment")
}





##### Partisan Theory ####
base_formula1_party_env = as.formula(GEP_env ~ GEP_env_wi_lag +
                                   trend + 
                                   fiscalcrisis_cat_ctl + 
                                   gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                   
                                   corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                   corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                   trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                   stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                   GEP_env_spatial_ctl +
                                   
                                   #strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                                   green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                                   unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                                   cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                                   
                                   (1|country_text_id) +  (1|year_0))

base_formula2_party_env = as.formula(GEP_env ~ GEP_env_wi_lag + GEP_env_wi_lag2 +
                                   trend + 
                                   fiscalcrisis_cat_ctl + 
                                   gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                   
                                   corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                   corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                   trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                   stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                   GEP_env_spatial_ctl +
                                   
                                   #strength_green_notrans_ctl_wi_df + strength_green_notrans_ctl_wi_lag + strength_green_notrans_ctl_bw + 
                                   green_plt_cpds_notrans_ctl_wi_df + green_plt_cpds_notrans_ctl_wi_lag + green_plt_cpds_notrans_ctl_bw + 
                                   unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                                   cabinet_cpds_cat_ctl_wi_df + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                                   
                                   (1|country_text_id) +  (1|year_0))



GEP_list[[1]] %>% 
  group_by(country_text_id) %>% 
  summarise(cnt = n()) %>% 
  arrange(cnt)

warmup = 1000
iter = 2000

# 1 Lag ####

GEP_control1_lag1 = make_brms_model(base_formula1_env, 
                                         #addprofile = "FKM4_E_wi + FKM4_E_wi_lag + FKM4_E_bw", 
                                         data = GEP_list[1],
                                         prior_brms = prior_base_env,
                                         iter = iter,
                                         warmup = warmup,
                                         chains = 4)
saveBRMS("GEP_control1_lag1", "Environment")
#GEP_control1_lag1 = loadBRMS("GEP_control1_lag1", "Environment")

gc()

GEP_control1_lag2 = make_brms_model(base_formula2_env, 
                                         #addprofile = "FKM4_E_wi + FKM4_E_wi_lag + FKM4_E_bw", 
                                         data = GEP_list[1],
                                         prior_brms = prior_base2_env,
                                         iter = iter,
                                         warmup = warmup,
                                         chains = 4)

saveBRMS("GEP_control1_lag2", "Environment")


# GEP_control1_lag2 = loadBRMS("GEP_control1_lag2", "Environment")

# test1 = check_autocorresiduals(GEP_control1_lag2, runs=1)
# test2 = check_autocorresiduals(ecoequal_control1_lag2, runs=1)
# 
# length(which(test1$lower > 0))/10
# length(which(test2$upper < 0))/10


# Control Model 2 ####

GEP_control2_lag1 = make_brms_model(base_formula1_party_env, 
                                         #addprofile = "FKM4_E_wi + FKM4_E_wi_lag + FKM4_E_bw", 
                                         data = GEP_list[1],
                                         prior_brms = prior_base_env,
                                         iter = iter,
                                         warmup = warmup,
                                         chains = 4)
saveBRMS("GEP_control2_lag1", "Environment")
#GEP_control2_lag1 = loadBRMS("GEP_control2_lag1", "Environment")
rm(ecoequal_control2_lag1)


GEP_control2_lag2 = make_brms_model(base_formula2_party_env, 
                                    #addprofile = "FKM4_E_wi + FKM4_E_wi_lag + FKM4_E_bw", 
                                    data = GEP_list[1],
                                    prior_brms = prior_base2_env,
                                    iter = iter,
                                    warmup = warmup,
                                    chains = 4)
saveBRMS("GEP_control2_lag2", "Environment")
#GEP_control2_lag2 = loadBRMS("GEP_control2_lag2", "Environment")
rm(ecoequal_control2_lag1)




#####







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
  bf_base_formula,
  data = air_list[1], 
  family = gaussian(),
  prior = prior_base_env,
  warmup = 1000, iter = 3000,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
air_control1_lag1
#generateTableBrms(env_control1_lag1)
saveBRMS("air_control1_lag1", "Environment")
#readRDS(paste("Analyse/Performance/SpecificP/brmsModels/", modelname, sep=""))
air_control1_lag1 = loadBRMS("air_control1_lag1", "Environment")


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
# lag_distribution_bayes(air_control1_lag1, "GEP_env_wi_lag", "corruption_vdem", unit = 1, time_periods=7, ci=0.95)



# ppc_resid_time(air_control1_lag1)
# brmsmodel = air_control1_lag1
# 
# test1 = check_autocorresiduals(air_control1_lag1, runs=5)
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