# Goal Attainment Regression ####

# source("Analyse/Performance/SpecificP/2 Goal-Attainment/TSCS_GA_imputation.R")
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/tscs_data_GA_Lutz_trans_v2.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")

profiles_testing = list()
profiles_testing[[1]] = "FKM5_Fec_wi + FKM5_Fec_wi_lag + FKM5_Fec_bw"
profiles_testing[[2]] = "FKM5_FeC_wi + FKM5_FeC_wi_lag + FKM5_FeC_bw"
profiles_testing[[3]] = "FKM5_fEC_wi + FKM5_fEC_wi_lag + FKM5_fEC_bw"
profiles_testing[[4]] = "FKM5_fEc_wi + FKM5_fEc_wi_lag + FKM5_fEc_bw"
profiles_testing[[5]] = "FKM5_FEC_wi + FKM5_FEC_wi_lag + FKM5_FEC_bw"
profiles_testing[[6]] = "FKM4_E_wi + FKM4_E_wi_lag + FKM4_E_bw"
profiles_testing[[7]] = "FKM4_c_wi + FKM4_c_wi_lag + FKM4_c_bw"
profiles_testing[[8]] = "execpar_1981_odempr + feduni1981_odempr"
profiles_testing[[9]] = "feduni1981_odempr + execpar_1981_odempr"
profiles_testing[[10]] = "centrip_odempr_wi + centrip_odempr_wi_lag + centrip_odempr_bw"



make_ga_brms = function(profile_nr, base_formula_ga, GA_prior, dataset) {
  
  profile = unlist(strsplit(profiles_testing[[profile_nr]], " + ", fixed=T))[1]
  add_to_formula = gsub("_wi","", profile)
  
  new_formula = gsub("age_const_num_ctl", paste("age_const_num_ctl + ", add_to_formula, sep=""), base_formula_ga)
  
  ga_model = brm(new_formula, 
                              family=gaussian, 
                              prior=GA_prior,
                              dataset)
  ga_model = add_criterion(ga_model, "loo")
  
  return(ga_model)
    
}


# Setup ####
GA_prior = c(set_prior("normal(0,100)", class = "b"),
             set_prior("cauchy(0,5)", class="sigma"))

names(tscs_data_GA_Lutz_trans)
summary(tscs_data_GA_Lutz_trans)


# Lutz ####
# Create Lutz Dataset ####
lutz_reg = tscs_data_GA_Lutz_trans %>% 
  select_at(vars(
    country_text_id,
    arate_lutz_ga,
    length_const_num_ctl,
    age_const_num_ctl,
    cabinet_cpds_cat_ctl, 
    starts_with("FKM"), 
    matches("odempr"))
    ) %>% 
  filter(is.na(arate_lutz_ga) == F)

base_formula_ga = "arate_lutz_ga ~ length_const_num_ctl + age_const_num_ctl"


# Structure ####
Lutz_m1_normal = brm(base_formula_ga, 
              family=gaussian, 
              prior=GA_prior,
              lutz_reg)
summary(Lutz_m1_normal)
prior_summary(Lutz_m1_normal)
Lutz_m1_normal = add_criterion(Lutz_m1_normal, "loo")
saveBRMS("Lutz_m1_normal", "GoalAttainment")


Lutz_m1_student = brm(arate_lutz_ga ~ length_const_num_ctl + age_const_num_ctl, 
              family=student, 
              prior=GA_prior,
              lutz_reg)
summary(Lutz_m1_student)
prior_summary(Lutz_m1_student)
Lutz_m1_student = add_criterion(Lutz_m1_student, "loo")
saveBRMS("Lutz_m1_student", "GoalAttainment")

loo_compare(Lutz_m1_normal, Lutz_m1_student)


# Democracy Profiles
model_list_lutz_1 = list()

length(profiles_testing)

for (i in 1:length(profiles_testing)) {
  model_list_lutz_1[[i]] = make_ga_brms(i, base_formula_ga, GA_prior, lutz_reg)
}
saveBRMS("model_list_lutz_1", "GoalAttainment")



# Cabinet ####
base_formula_ga_party = "arate_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + cabinet_cpds_cat_ctl"

Lutz_m2_cab = brm(arate_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + cabinet_cpds_cat_ctl, 
                      family=gaussian, 
                      prior=GA_prior,
                      lutz_reg)
summary(Lutz_m2_cab)
prior_summary(Lutz_m2_student)
Lutz_m2_cab = add_criterion(Lutz_m2_cab, "loo")
saveBRMS("Lutz_m2_cab", "GoalAttainment")


# Democracy Profiles
model_list_lutz_2 = list()


for (i in 1:length(profiles_testing)) {
  model_list_lutz_2[[i]] = make_ga_brms(i, base_formula_ga_party, GA_prior, lutz_reg)
}
saveBRMS("model_list_lutz_2", "GoalAttainment")





# CCP ####

# Create CCP Dataset ####
ccp_ga = tscs_data_GA_Lutz_trans %>% 
  select_at(vars(
    country_text_id,
    arate_ccp_ga,
    length_const_num_ctl,
    age_const_num_ctl,
    cabinet_cpds_cat_ctl, 
    starts_with("FKM"), 
    matches("odempr"))
  ) %>% 
  filter(is.na(arate_ccp_ga) == F)

base_formula_ccp = "arate_ccp_ga ~ length_const_num_ctl + age_const_num_ctl"


# Structure ####

ccp_m1_normal = brm(base_formula_ccp, 
                     family=gaussian, 
                     prior=GA_prior,
                    ccp_ga)
summary(ccp_m1_normal)
prior_summary(ccp_m1_normal)
ccp_m1_normal = add_criterion(ccp_m1_normal, "loo")
saveBRMS("ccp_m1_normal", "GoalAttainment")


ccp_m1_student = brm(base_formula_ccp, 
                      family=student, 
                      prior=GA_prior,
                     ccp_ga)
summary(ccp_m1_student)
prior_summary(ccp_m1_student)
ccp_m1_student = add_criterion(ccp_m1_student, "loo")
saveBRMS("ccp_m1_student", "GoalAttainment")

loo_compare(ccp_m1_normal, ccp_m1_student)


# Democracy Profiles
model_list_ccp_1 = list()

length(profiles_testing)

for (i in 1:length(profiles_testing)) {
  model_list_ccp_1[[i]] = make_ga_brms(i, base_formula_ccp, GA_prior, ccp_ga)
}
saveBRMS("model_list_ccp_1", "GoalAttainment")



# Cabinet ####
base_formula_ga_ccp_party = "arate_ccp_ga ~ length_const_num_ctl + age_const_num_ctl + cabinet_cpds_cat_ctl"

ccp_m2_cab = brm(base_formula_ga_ccp_party, 
                  family=gaussian, 
                  prior=GA_prior,
                  ccp_ga)
summary(ccp_m2_cab)
prior_summary(ccp_m2_cab)
ccp_m2_cab = add_criterion(ccp_m2_cab, "loo")
saveBRMS("ccp_m2_cab", "GoalAttainment")


# Democracy Profiles
model_list_ccp_2 = list()


for (i in 1:length(profiles_testing)) {
  model_list_ccp_2[[i]] = make_ga_brms(i, base_formula_ga_ccp_party, GA_prior, ccp_ga)
}
saveBRMS("model_list_ccp_2", "GoalAttainment")


# CCP - Lutz Comparison ####
# Create CCP Dataset ####
ccp_ga_lutz = tscs_data_GA_Lutz_trans %>% 
  select_at(vars(
    country_text_id,
    arate_ccp_ga,
    arate_lutz_ga,
    length_const_num_ctl,
    age_const_num_ctl,
    cabinet_cpds_cat_ctl, 
    starts_with("FKM"), 
    matches("odempr"))
  ) %>% 
  filter(is.na(arate_lutz_ga) == F)

base_formula_ccp = "arate_ccp_ga ~ length_const_num_ctl + age_const_num_ctl"


# Structure ####

ccp_m1_normal = brm(base_formula_ccp, 
                    family=gaussian, 
                    prior=GA_prior,
                    ccp_ga_lutz)
summary(ccp_m1_normal)
prior_summary(ccp_m1_normal)
ccp_m1_normal = add_criterion(ccp_m1_normal, "loo")
#saveBRMS("ccp_m1_normal", "GoalAttainment")


ccp_m1_student = brm(base_formula_ccp, 
                     family=student, 
                     prior=GA_prior,
                     ccp_ga_lutz)
summary(ccp_m1_student)
prior_summary(ccp_m1_student)
ccp_m1_student = add_criterion(ccp_m1_student, "loo")
saveBRMS("ccp_m1_student", "GoalAttainment")

loo_compare(ccp_m1_normal, ccp_m1_student)


# Democracy Profiles
model_list_ccplutz_1 = list()

length(profiles_testing)

for (i in 1:length(profiles_testing)) {
  model_list_ccplutz_1[[i]] = make_ga_brms(i, base_formula_ccp, GA_prior, ccp_ga_lutz)
}
saveBRMS("model_list_ccplutz_1", "GoalAttainment")



# Cabinet ####
base_formula_ga_ccp_party = "arate_ccp_ga ~ length_const_num_ctl + age_const_num_ctl + cabinet_cpds_cat_ctl"

ccp_m2_cab = brm(base_formula_ga_ccp_party, 
                 family=gaussian, 
                 prior=GA_prior,
                 ccp_ga_lutz)
summary(ccp_m2_cab)
prior_summary(ccp_m2_cab)
ccp_m2_cab = add_criterion(ccp_m2_cab, "loo")
# saveBRMS("ccp_m2_cab", "GoalAttainment")


# Democracy Profiles
model_list_ccplutz_2 = list()


for (i in 1:length(profiles_testing)) {
  model_list_ccplutz_2[[i]] = make_ga_brms(i, base_formula_ga_ccp_party, GA_prior, ccp_ga_lutz)
}
saveBRMS("model_list_ccp_2", "GoalAttainment")




