# Social Performance Regression ####
# Economic Inequality ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_soc_v4.Rdata")
#source("Analyse/Performance/SpecificP/3 Integration/Equality/TSCS_imputation_SocialP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_soc.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")



prior_phet1_ecoequal <- c(set_prior("cauchy(0,5)", class = "sd"),  
                set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),                    
                set_prior("normal(0,2)", class = "Intercept"),
                set_prior("normal(0,100)", class = "b"),
                set_prior("normal(0.5, 1)", class = "b", coef="eco_equal_soc_wi_lag"))

prior_phet2_ecoequal <- c(set_prior("cauchy(0,5)", class = "sd"),  
                set_prior("cauchy(0,5)", class = "sd", dpar = "sigma"),                    
                set_prior("normal(0,2)", class = "Intercept"),
                set_prior("normal(0,100)", class = "b"),
                set_prior("normal(0.5, 1)", class = "b", coef="eco_equal_soc_wi_lag"),
                set_prior("normal(0.5, 1)", class = "b", coef="eco_equal_soc_wi_lag2"))


# Create TSCS Data ####
equalsoc_list = make_reg_data(a.out_soc, "eco_equal_soc", 
                              naframe = mice_data_naframe_soc, 
                              vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                               "execpar_1981_odempr", "feduni1981_odempr",
                                               "centrip_odempr"),
                              lag2 = T,
                              nr_imputations = 5) 


# Test Model Structure ####
ECO_mar = test_modelparameters("eco_equal_soc", "eco_equal_soc_wi_lag", equalsoc_list[1], chains=8, iter = 6000, thin=2)
saveBRMS("ECO_mar", "Equality")
ECO_mar = loadBRMS("ECO_mar", "Equality")


loo_compare(ECO_mar[[1]][[1]], 
            ECO_mar[[2]][[1]], 
            ECO_mar[[3]][[1]], 
            ECO_mar[[4]][[1]], model_names = c("Base", "Unit Heterogeneity", "Contemporaneous Correlation", "Panel Heteroscedasticity")) %>% 
  loo_table()


p1 = ppc_unithet(ECO_mar[[1]][[1]], ECO_mar[[2]][[1]], equalsoc_list[[1]], "eco_equal_soc",unit = "country_text_id")
p2 = ppc_unithet(ECO_mar[[2]][[1]], ECO_mar[[3]][[1]], equalsoc_list[[1]], "eco_equal_soc",unit = "year_0")
p3 = ppc_panelhet(ECO_mar[[3]][[1]], ECO_mar[[4]][[1]], equalsoc_list[[1]], "eco_equal_soc")

ggarrange(p1,p2,p3, nrow=3)

# Test Performance ####

base_formula1_ecoequal = as.formula(eco_equal_soc ~ eco_equal_soc_wi_lag +
                             trend + 
                             fiscalcrisis_cat_ctl + 
                             gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                             
                             corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                             eco_equal_soc_spatial_ctl +
                             
                             (1|country_text_id) +  (1|year_0))

base_formula2_ecoequal = as.formula(eco_equal_soc ~ eco_equal_soc_wi_lag + eco_equal_soc_wi_lag2 +
                             trend + 
                             fiscalcrisis_cat_ctl + 
                             gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                             
                             corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                             eco_equal_soc_spatial_ctl +
                             
                             (1|country_text_id) +  (1|year_0))





ECO_lag1 = make_brms_model(base_formula1_ecoequal, addprofile = NULL, data = equalsoc_list[1], prior_brms = prior_phet1_ecoequal, 
                       iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("ECO_lag1", "Equality")

ECO_lag2 = make_brms_model(base_formula2_ecoequal, addprofile = NULL, data = equalsoc_list[1], prior_brms = prior_phet2_ecoequal, 
                           iter = 2000, warmup=1000, chains = 8, thin = 1)
saveBRMS("ECO_lag2", "Equality")


ECO_list = list()
for (i in 1:length(profiles_testing)) {
  ECO_list[[i]] = make_brms_model(base_formula2_ecoequal, profiles_testing[[i]], data = equalsoc_list, prior_brms = prior_phet2_ecoequal, 
                                         iter = 3000, warmup=1000, chains = 8, thin = 2)
  saveBRMS("ECO_list", "Equality")
}









# Partisan ####

base_formula1_party = as.formula(eco_equal_soc ~ eco_equal_soc_wi_lag +
                                   trend + 
                                   fiscalcrisis_cat_ctl + 
                                   gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                   
                                   corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                   corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                   trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                   stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                   eco_equal_soc_spatial_ctl +
                                   
                                   unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                                   cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                                   
                                   
                                   (1|country_text_id) +  (1|year_0))

base_formula2_party = as.formula(eco_equal_soc ~ eco_equal_soc_wi_lag + eco_equal_soc_wi_lag2 +
                                   trend + 
                                   fiscalcrisis_cat_ctl + 
                                   gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_wi_lag + gdppc_wdi_num_ctl_bw +
                                   
                                   corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                                   corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                                   trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                                   stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                                   eco_equal_soc_spatial_ctl +
                                   
                                   unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
                                   cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                                   
                                   (1|country_text_id) +  (1|year_0))

