# Social Performance Regression ####
# Economic Inequality ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_soc_v3.Rdata")
#source("Analyse/Performance/SpecificP/3 Integration/Equality/TSCS_imputation_SocialP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_soc.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")



prior_base <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                set_prior("cauchy(0,5)", class = "sigma"),
                set_prior("normal(0,100)", class = "Intercept"),
                set_prior("normal(0,100)", class = "b"),
                set_prior("normal(0, 1)", class = "b", coef="eco_equal_soc_lag"))

prior_base2 <- c(set_prior("cauchy(0,5)", class = "sd"),                    
                 set_prior("cauchy(0,5)", class = "sigma"),
                 set_prior("normal(0,100)", class = "Intercept"),
                 set_prior("normal(0,100)", class = "b"),
                 set_prior("normal(0,1)", class = "b", coef="eco_equal_soc_lag"),
                 set_prior("normal(0,1)", class = "b", coef="eco_equal_soc_lag2"))

warmup = 1000
iter = 2000

# Create TSCS Data ####
equalsoc_list = make_reg_data(a.out_soc, "eco_equal_soc", 
                            naframe = mice_data_naframe_soc, 
                            vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                             "execpar_1981_odempr", "feduni1981_odempr",
                                             "centrip_odempr"),
                            lag2 = T) 


names(equalsoc_list[[1]])

# Control Model 1 ####
# equalsoc_list[[1]] %>% 
#   select_at(vars(ends_with("wi"), ends_with("wi_lag"), -matches("FKM"), -ends_with("_odempr"))) %>% 
#   rename_all(funs(gsub("_num_ctl","",.))) %>% 
#   rename_all(funs(gsub("_cat_ctl","",.))) %>% 
#   rename_all(funs(gsub("_pr_ctl","",.))) %>% 
#   cor(use="pairwise", method = "pearson") %>% 
#   corrplot(method="number")


ecoequal_control1_lag1 = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag +
    trend + 
    fiscalcrisis_cat_ctl + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    eco_equal_soc_spatial_ctl +
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
# ecoequal_control1_lag1
saveBRMS("ecoequal_control1_lag1", "Equality")
#ecoequal_control1_lag1 = loadBRMS("ecoequal_control1_lag1", "Equality")


ecoequal_control1_lag2 = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    eco_equal_soc_spatial_ctl +
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("ecoequal_control1_lag2", "Equality")
# ecoequal_control1_lag2 = loadBRMS("ecoequal_control1_lag2", "Equality")


# test1 = check_autocorresiduals(ecoequal_control1_lag1, runs=1)
# test2 = check_autocorresiduals(ecoequal_control1_lag2, runs=1)
# 
# length(which(test1$lower > 0))/10
# length(which(test2$upper < 0))/10


# Control Model 2 ####

ecoequal_control2 = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    eco_equal_soc_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6,
  verbose = T
)
saveBRMS("ecoequal_control2", "Equality")




# 5 Cluster Solution ####
ecoequal_FKM_15Feecc = brm_multiple( 
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
   eco_equal_soc_spatial_ctl +
    
    FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("ecoequal_FKM_15Feecc", "Equality")

ecoequal_FKM_25Feecc = update(ecoequal_FKM_15Feecc, 
                            eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                              trend + 
                              gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                              corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                              corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                              trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                              fiscalcrisis_cat_ctl + 
                              stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                             eco_equal_soc_spatial_ctl +
                              
                              unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                              cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                              
                              FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw + 
                              
                              (1|country_text_id) +  (1|year_0),
                            newdata = equalsoc_list, 
                            family = gaussian(),
                            prior = prior_base2,
                            warmup = warmup, iter = iter,
                            #control = list(adapt_delta = 0.95), 
                            chains = 6
)

saveBRMS("ecoequal_FKM_25Feecc", "Equality")
# ecoequal_FKM_25Feecc = loadBRMS("ecoequal_FKM_25Feecc", "Equality")


ecoequal_FKM_15FeeC = brm_multiple( 
                           eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                             trend + 
                             gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                             corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                             fiscalcrisis_cat_ctl + 
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            eco_equal_soc_spatial_ctl +
                             
                             FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
                             
                             (1|country_text_id) +  (1|year_0),
                           data = equalsoc_list, 
                           family = gaussian(),
                           prior = prior_base2,
                           warmup = warmup, iter = iter,
                           #control = list(adapt_delta = 0.95), 
                           chains = 6
)
saveBRMS("ecoequal_FKM_15FeeC", "Equality")
# ecoequal_FKM_15FeeC = loadBRMS("ecoequal_FKM_15FeeC", "Equality")
# lag_distribution_bayes(ecoequal_FKM_15FeeC, "eco_equal_soc_lag", "FKM5_FeC", unit = 1, time_periods=7, ci=0.95)

ecoequal_FKM_25FeeC = update(ecoequal_FKM_25Feecc, 
                           eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                             trend + 
                             gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                             corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                             fiscalcrisis_cat_ctl + 
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            eco_equal_soc_spatial_ctl +
                             
                             unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                             cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                             
                             FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw + 
                             
                             (1|country_text_id) +  (1|year_0),
                           newdata = equalsoc_list, 
                           family = gaussian(),
                           prior = prior_base2,
                           warmup = warmup, iter = iter,
                           #control = list(adapt_delta = 0.95), 
                           chains = 6
)
saveBRMS("ecoequal_FKM_25FeeC", "Equality")

ecoequal_FKM_15ffEC = update(ecoequal_FKM_15Feecc, 
                           eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                             trend + 
                             gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                             corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                             fiscalcrisis_cat_ctl + 
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            eco_equal_soc_spatial_ctl +
                             
                             FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
                             
                             (1|country_text_id) +  (1|year_0),
                           newdata = equalsoc_list, 
                           family = gaussian(),
                           prior = prior_base2,
                           warmup = warmup, iter = iter,
                           #control = list(adapt_delta = 0.95), 
                           chains = 6
)
saveBRMS("ecoequal_FKM_15ffEC", "Equality")


ecoequal_FKM_25ffEC = update(ecoequal_FKM_25Feecc, 
                           eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                             trend + 
                             gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                             corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                             corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                             trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                             fiscalcrisis_cat_ctl + 
                             stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                            eco_equal_soc_spatial_ctl +
                             
                             unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                             cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                             
                             FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw + 
                             
                             (1|country_text_id) +  (1|year_0),
                           newdata = equalsoc_list, 
                           family = gaussian(),
                           prior = prior_base2,
                           warmup = warmup, iter = iter,
                           #control = list(adapt_delta = 0.95), 
                           chains = 6
)
saveBRMS("ecoequal_FKM_25ffEC", "Equality")

ecoequal_FKM_15ffEcc = update(ecoequal_FKM_15Feecc, 
                              eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                              trend + 
                              gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                              pop_over65_wdi_pr_ctl_wi + pop_over65_wdi_pr_ctl_bw +
                              corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                              corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                              trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                              fiscalcrisis_cat_ctl + 
                              stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                             eco_equal_soc_spatial_ctl +
                              
                              FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
                              
                              (1|country_text_id) +  (1|year_0),
                            newdata = equalsoc_list, 
                            family = gaussian(),
                            prior = prior_base2,
                            warmup = warmup, iter = iter,
                            #control = list(adapt_delta = 0.95), 
                            chains = 6
)
saveBRMS("ecoequal_FKM_15ffEcc", "Equality")


ecoequal_FKM_25ffEcc = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
   eco_equal_soc_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("ecoequal_FKM_25ffEcc", "Equality")

ecoequal_FKM_15FEC = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
   eco_equal_soc_spatial_ctl +
    
    FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data= equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)
saveBRMS("ecoequal_FKM_15FEC", "Equality")


ecoequal_FKM_25FEC = update(ecoequal_FKM_25ffEcc, 
                          eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                            trend + 
                            gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                            corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                            corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                            trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                            fiscalcrisis_cat_ctl + 
                            stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                           eco_equal_soc_spatial_ctl +
                            
                            unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                            cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                            
                            FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw + 
                            
                            (1|country_text_id) +  (1|year_0),
                          newdata = equalsoc_list, 
                          family = gaussian(),
                          prior = prior_base2,
                          warmup = warmup, iter = iter,
                          #control = list(adapt_delta = 0.95), 
                          chains = 6
)

saveBRMS("ecoequal_FKM_25FEC", "Equality")

# Dimensional Profiles ####
ecoequal_FKM_1E = brm_multiple(
                      eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                        trend + 
                        gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                        corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                        corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                        trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                        fiscalcrisis_cat_ctl + 
                        stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                       eco_equal_soc_spatial_ctl +
                        
                        FKM5_E_wi + FKM5_E_wi_lag + FKM5_E_bw + 
                        
                        (1|country_text_id) +  (1|year_0),
                      data = equalsoc_list, 
                      family = gaussian(),
                      prior = prior_base2,
                      warmup = warmup, iter = iter,
                      #control = list(adapt_delta = 0.95), 
                      chains = 6
)
saveBRMS("ecoequal_FKM_1E", "Equality")

ecoequal_FKM_2E = brm_multiple( 
                       eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                         trend + 
                         gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                         corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         fiscalcrisis_cat_ctl + 
                         stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                        eco_equal_soc_spatial_ctl +
                         
                         unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                         cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                         
                         FKM5_E_wi + FKM5_E_wi_lag + FKM5_E_bw + 
                         
                         (1|country_text_id) +  (1|year_0),
                       data = equalsoc_list, 
                       family = gaussian(),
                       prior = prior_base2,
                       warmup = warmup, iter = iter,
                       #control = list(adapt_delta = 0.95), 
                       chains = 6
)

saveBRMS("ecoequal_FKM_2E", "Equality")

ecoequal_FKM_1c = brm_multiple( 
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
   eco_equal_soc_spatial_ctl +
    
    FKM5_c_wi + FKM5_c_wi_lag + FKM5_c_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  warmup = 2000, iter = 3000,
  chains = 6
)

saveBRMS("ecoequal_FKM_1c", "Equality")

ecoequal_FKM_2c = update(ecoequal_FKM_1c, 
                       eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
                         trend + 
                         gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
                         corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
                         corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
                         trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
                         fiscalcrisis_cat_ctl + 
                         stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
                        eco_equal_soc_spatial_ctl +
                         
                         unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
                         cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
                         
                         FKM5_c_wi + FKM5_c_wi_lag + FKM5_c_bw + 
                         
                         (1|country_text_id) +  (1|year_0),
                       newdata = equalsoc_list, 
                       warmup = warmup, iter = iter,
                       chains = 6
)

saveBRMS("ecoequal_FKM_2c", "Equality")




# Other Democracy Profiles ####

# equalsoc_list[[1]] %>% 
#   select_at(vars(starts_with("FKM") & matches("wi"), -matches("df"), -matches("bw"), ends_with("_odempr"))) %>% 
#   rename_all(funs(gsub("_num_ctl","",.))) %>% 
#   rename_all(funs(gsub("_cat_ctl","",.))) %>% 
#   rename_all(funs(gsub("_pr_ctl","",.))) %>% 
#   cor(use="pairwise", method = "pearson") %>% 
#   corrplot(method="number")

# Lijphart
ecoequal_FKM_1Lij = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
   eco_equal_soc_spatial_ctl +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("ecoequal_FKM_1Lij", "Equality")

ecoequal_FKM_2Lij = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
   eco_equal_soc_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    execpar_1981_odempr + 
    feduni1981_odempr +
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("ecoequal_FKM_2Lij", "Equality")


# Centripetalism
ecoequal_FKM_1Cent = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    #fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
   eco_equal_soc_spatial_ctl +
    
    centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("ecoequal_FKM_1Cent", "Equality")

# lag_distribution_bayes(ecoequal_FKM_1Cent, "wealth_eco_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_1Cent, "wealth_eco_lag", "trade", unit = 1, time_periods=7, ci=0.95)
# generateTableBrms(ecoequal_FKM_1Cent, ecoequal_FKM_2Cent)


ecoequal_FKM_2Cent = brm_multiple(
  eco_equal_soc ~ eco_equal_soc_lag + eco_equal_soc_lag2 +
    trend + 
    gdppc_wdi_num_ctl_wi + gdppc_wdi_num_ctl_bw +
    corporatism_vdem_pr_ctl_wi + corporatism_vdem_pr_ctl_wi_lag + corporatism_vdem_pr_ctl_bw +
    corruption_vdem_pr_ctl_wi + corruption_vdem_pr_ctl_wi_lag + corruption_vdem_pr_ctl_bw +  
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    #fiscalcrisis_cat_ctl + 
    stateterr_vdem_pr_ctl_wi + stateterr_vdem_pr_ctl_wi_lag + stateterr_vdem_pr_ctl_bw + 
    eco_equal_soc_spatial_ctl +
    
    unions_vi_num_ctl_wi + unions_vi_num_ctl_bw +
    cabinet_cpds_cat_ctl_wi + cabinet_cpds_cat_ctl_wi_lag + cabinet_cpds_cat_ctl_bw +
    
    centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw + 
    
    (1|country_text_id) +  (1|year_0),
  data = equalsoc_list, 
  family = gaussian(),
  prior = prior_base2,
  warmup = warmup, iter = iter,
  #control = list(adapt_delta = 0.95), 
  chains = 6
)

saveBRMS("ecoequal_FKM_2Cent", "Equality")

# lag_distribution_bayes(ecoequal_FKM_2Cent, "eco_equal_soc_lag", "centrip", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_2Cent, "wealth_eco_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_2Cent, "wealth_eco_lag", "pop_over65", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_2Cent, "wealth_eco_lag", "corporatism", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_2Cent, "wealth_eco_lag", "cbi_w_cbi", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_2Cent, "wealth_eco_lag", "corruption", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_2Cent, "wealth_eco_lag", "stateterr", unit = 1, time_periods=7, ci=0.95)
# lag_distribution_bayes(ecoequal_FKM_2Cent, "wealth_eco_lag", "trade_wdi", unit = 1, time_periods=7, ci=0.95)

Sys.time()