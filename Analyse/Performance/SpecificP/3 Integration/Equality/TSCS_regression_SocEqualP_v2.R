# Social Performance Regression ####
# Social Inequality ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_soc_v4.Rdata")
#source("Analyse/Performance/SpecificP/3 Integration/Equality/TSCS_imputation_SocialP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_soc.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")

# Base Formulas and Priors ####

base_formula1_socequal = as.formula(soc_equal_soc ~ soc_equal_soc_wi_lag +
                                      trend + 
                                      fiscalcrisis_cat_ctl + 
                                      gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                      
                                      corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                      corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                      trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                      stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                      soc_equal_soc_spatial_ctl +
                                      
                                      (1|country_text_id) +  (1|year_0))

base_formula2_socequal = as.formula(soc_equal_soc ~ soc_equal_soc_wi_lag + soc_equal_soc_wi_lag2 +
                                      trend + 
                                      fiscalcrisis_cat_ctl + 
                                      gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                      
                                      corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                      corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                      trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                      stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                      soc_equal_soc_spatial_ctl +
                                      
                                      (1|country_text_id) +  (1|year_0))


prior_phet1_socequal <- c(set_prior("cauchy(0,5)", class = "sd"),  
                 set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),                    
                 set_prior("normal(0,2)", class = "Intercept"),
                 set_prior("normal(0,100)", class = "b"),
                 set_prior("normal(0.5, 1)", class = "b", coef="soc_equal_soc_wi_lag"))

prior_phet2_socequal <- c(set_prior("cauchy(0,5)", class = "sd"),  
                 set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),                    
                 set_prior("normal(0,2)", class = "Intercept"),
                 set_prior("normal(0,100)", class = "b"),
                 set_prior("normal(0.5, 1)", class = "b", coef="soc_equal_soc_wi_lag"),
                 set_prior("normal(0.5, 1)", class = "b", coef="soc_equal_soc_wi_lag2"))



# Create TSCS Data ####
socequalsoc_list = make_reg_data(a.out_soc, "soc_equal_soc", 
                              naframe = mice_data_naframe_soc, 
                              vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                               "execpar_1981_odempr", "feduni1981_odempr",
                                               "centrip_odempr"),
                              lag2 = T,
                              nr_imputations = 5) 




# Test Model Structure ####
SOC_mpar = test_modelparameters("soc_equal_soc", "soc_equal_soc_wi_lag", socequalsoc_list[1], chains=8, iter = 6000, thin=2)
saveBRMS("SOC_mpar", "Equality")
SOC_mpar = loadBRMS("SOC_mpar", "Equality")


loo_compare(SOC_mpar[[1]][[1]], 
            SOC_mpar[[2]][[1]], 
            SOC_mpar[[3]][[1]], 
            SOC_mpar[[4]][[1]], model_names = c("Base", "Unit Heterogeneity", "Contemporaneous Correlation", "Panel Heteroscedasticity")) %>% 
  loo_table()

p1 = ppc_unithet(SOC_mpar[[1]][[1]], SOC_mpar[[2]][[1]], socequalsoc_list[[1]], "soc_equal_soc",unit = "country_text_id")
p2 = ppc_unithet(SOC_mpar[[2]][[1]], SOC_mpar[[3]][[1]], socequalsoc_list[[1]], "soc_equal_soc",unit = "year_0")
p3 = ppc_panelhet(SOC_mpar[[3]][[1]], SOC_mpar[[4]][[1]], socequalsoc_list[[1]], "soc_equal_soc")
ggarrange(p1,p2,p3, nrow=3)


# Test Autocorrelation Structure ####

SOC_lag1 = make_brms_model(base_formula1_socequal, addprofile = NULL, data = socequalsoc_list[1], prior_brms = prior_phet1_socequal, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("SOC_lag1", "Equality")

SOC_lag2 = make_brms_model(base_formula2_socequal, addprofile = NULL, data = socequalsoc_list[1], prior_brms = prior_phet2_socequal, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("SOC_lag2", "Equality")


SOC_lag1 = loadBRMS("SOC_lag1", "Equality")
SOC_lag2 = loadBRMS("SOC_lag2", "Equality")

AR1_test = check_autocorresiduals(SOC_lag1[[1]], runs=100)
AR2_test = check_autocorresiduals(SOC_lag2[[1]], runs=100)
saveBRMS("AR1_test", "Equality")
saveBRMS("AR2_test", "Equality")

AR1_test = loadBRMS("AR1_test", "Equality")
AR2_test = loadBRMS("AR2_test", "Equality")


hdi(AR1_test$draw, 0.95)
hdi(AR2_test$draw, 0.95)


# Test Performance ####

SOC_list_v2 = list()
for (i in 1:length(profiles_testing)) {
  SOC_list_v2[[i]] = make_brms_model(base_formula2_socequal, profiles_testing[[i]], data = socequalsoc_list, prior_brms = prior_phet2_socequal, 
                                  iter = 3000, warmup=1000, chains = 8, thin = 2)
  saveBRMS("SOC_list_v2", "Equality")
}


