# Domestic Security Regression ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_ds_v4.Rdata")
# source("Analyse/Performance/SpecificP/3 Integration/TSCS_imputation_DomesticP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_ds.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")



prior_phet1_dom <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                 set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                 set_prior("normal(0,2)", class = "Intercept"),
                 set_prior("normal(0,100)", class = "b"),
                 set_prior("normal(0, 1)", class = "b", coef="domsec_ds_wi_lag"))

prior_phet2_dom <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                 set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),
                 set_prior("normal(0,2)", class = "Intercept"),
                 set_prior("normal(0,100)", class = "b"),
                 set_prior("normal(0,1)", class = "b", coef="domsec_ds_wi_lag"),
                 set_prior("normal(0,1)", class = "b", coef="domsec_ds_wi_lag2"))



# Create TSCS Data ####
domsec_ds_list = make_reg_data(a.out_ds, "domsec_ds", 
                                 naframe = mice_data_naframe_ds, 
                                 vars_noimput = c("cabinet_cpds_cat_ctl",
                                                  "execpar_1981_odempr", "feduni1981_odempr",
                                                  "centrip_odempr"),
                                 lag2 = T,
                                 nr_imputations = 10) 




# Test Model Structure ####
DOM_mpar = test_modelparameters("domsec_ds", "domsec_ds_wi_lag", domsec_ds_list[1], chains=8, iter = 6000, thin=2)
saveBRMS("DOM_mpar", "Domestic")
DOM_mpar = loadBRMS("DOM_mpar", "Domestic")


loo_compare(DOM_mpar[[1]][[1]], DOM_mpar[[2]][[1]], DOM_mpar[[3]][[1]], DOM_mpar[[4]][[1]], model_names = c("Base", "Unit Heterogeneity", "Contemporaneous Correlation", "Panel Heteroscedasticity")) %>% 
  loo_table()


p1 = ppc_unithet(DOM_mpar[[1]][[1]], DOM_mpar[[2]][[1]], domsec_ds_list[[1]], "domsec_ds",unit = "country_text_id")
p2 = ppc_unithet(DOM_mpar[[2]][[1]], DOM_mpar[[3]][[1]], domsec_ds_list[[1]], "domsec_ds",unit = "year_0")
p3 = ppc_panelhet(DOM_mpar[[3]][[1]], DOM_mpar[[4]][[1]], domsec_ds_list[[1]], "domsec_ds")
ggarrange(p1,p2,p3, nrow=3)



# Test Lag Structure ####
base_formula1_dom = as.formula(domsec_ds ~ domsec_ds_wi_lag +
                             trend + 
                             fiscalcrisis_cat_ctl + 
                             gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                             
                             eco_inequal_num_ctl_wi + eco_inequal_num_ctl_wi_lag + eco_inequal_num_ctl_bw +

                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                             domsec_ds_spatial_ctl +
                             
                             (1|country_text_id) +  (1|year_0))


base_formula2_dom = as.formula(domsec_ds ~ domsec_ds_wi_lag + domsec_ds_wi_lag2 +
                                 trend + 
                                 fiscalcrisis_cat_ctl + 
                                 gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                 
                                 eco_inequal_num_ctl_wi + eco_inequal_num_ctl_wi_lag + eco_inequal_num_ctl_bw +
                                 
                                 corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                 
                                 stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                 domsec_ds_spatial_ctl +
                             
                             (1|country_text_id) +  (1|year_0))


base_formula1_dom_ecm = as.formula(domsec_ds_df ~ domsec_ds_wi_lag +
                                 trend + 
                                 fiscalcrisis_cat_ctl + 
                                 gdppc_wdi_num_ctl_wi_df + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                 
                                 eco_inequal_num_ctl_wi_df + eco_inequal_num_ctl_wi_lag + eco_inequal_num_ctl_bw +
                                 
                                 corruption_vdem_pr_ctl_wi_df + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                 
                                 stateterr_vdem_pr_ctl_wi_df + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                 domsec_ds_spatial_ctl +
                                 
                                 (1|country_text_id) +  (1|year_0))

# Test Autocorrelation Structure ####
DOM_lag1 = make_brms_model(base_formula1_dom, addprofile = NULL, data = domsec_ds_list[1], prior_brms = prior_phet1_dom, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("DOM_lag1", "Domestic")


DOM_lag2 = make_brms_model(base_formula2_dom, addprofile = NULL, data = domsec_ds_list[1], prior_brms = prior_phet2_dom, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("DOM_lag2", "Domestic")

DOM_lag_ecm = make_brms_model(base_formula1_dom_ecm, addprofile = NULL, data = domsec_ds_list[1], prior_brms = prior_phet1_dom, 
                           iter = 2000, warmup=1000, chains = 2, thin = 1)
saveBRMS("DOM_lag_ecm", "Domestic")


DOM_lag1 = loadBRMS("DOM_lag1", "Domestic")
DOM_lag2 = loadBRMS("DOM_lag2", "Domestic")
DOM_lag_ecm = loadBRMS("DOM_lag_ecm", "Domestic")

AR1_test_dom = check_autocorresiduals(DOM_lag1, runs=100)
AR2_test_dom = check_autocorresiduals(DOM_lag2, runs=100)
AR1_test_dom_ecm = check_autocorresiduals(DOM_lag_ecm[[1]], runs=100)

saveBRMS("AR1_test_dom", "Domestic")
saveBRMS("AR2_test_dom", "Domestic")
saveBRMS("AR1_test_dom_ecm", "Domestic")

hdi(AR1_test_dom$draw, 0.95)
hdi(AR2_test_dom$draw, 0.95)
hdi(AR1_test_dom_ecm$draw, 0.95)

# Test Performance ####

DOM_mod_list = list()
for (i in 1:length(profiles_testing)) {
  DOM_mod_list[[i]] = make_brms_model(base_formula1_dom_ecm, addprofile = profiles_testing_ecm[[i]], data = domsec_ds_list, prior_brms = prior_phet1_dom, 
                                      iter = 2000, warmup=1000, chains = 4, thin = 1)
  saveBRMS("DOM_mod_list", "Domestic")
}

