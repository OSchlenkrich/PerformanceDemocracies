# Domestic Security Regression ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_ds_v3.Rdata")
# source("Analyse/Performance/SpecificP/3 Integration/TSCS_imputation_DomesticP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_ds.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")



# Create TSCS Data ####
domsecds_list = make_reg_data(a.out_ds, "domsec_ds", 
                                 naframe = mice_data_naframe_ds, 
                                 vars_noimput = c("cabinet_cpds_cat_ctl",
                                                  "execpar_1981_odempr", "feduni1981_odempr",
                                                  "centrip_odempr"),
                                 lag2 = T,
                                 lag3 = F) 

