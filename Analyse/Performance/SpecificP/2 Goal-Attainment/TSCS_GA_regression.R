# Goal Attainment Regression ####

# source("Analyse/Performance/SpecificP/2 Goal-Attainment/TSCS_GA_imputation.R")
load("Analyse/Performance/SpecificP/Datasets/tscs_data_GA_Lutz_trans.Rdata")


# Setup ####
GA_prior = c(set_prior("normal(0,100)", class = "b"),
             set_prior("cauchy(0,5)", class="sigma"))

names(tscs_data_GA_Lutz_trans)
summary(tscs_data_GA_Lutz_trans)
# Lutz ####
Lutz_m1 = brm(GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl, 
              family=student, 
              prior=GA_prior,
              tscs_data_GA_Lutz_trans)
summary(Lutz_m1)
prior_summary(Lutz_m1)
Lutz_m1 = add_criterion(Lutz_m1, "loo")

# Dimensional Solution
Lutz_dimc = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_c, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m2)
Lutz_m2 = add_criterion(Lutz_m2, "loo", reloo=T)
loo_compare(Lutz_m1, Lutz_m2)
test = conditional_effects(Lutz_m2)
summary(Lutz_m2$data)

predict(Lutz_m2, newdata=data.frame(length_const_num_ctl = -0.2567, age_const_num_ctl=0.949852, FKM5_c = seq(-1.67674, 1.46841, length.out = 10)))

Lutz_dimE = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_E, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m3)
Lutz_m3 = add_criterion(Lutz_m3, "loo", reloo=T)
loo_compare(Lutz_m1, Lutz_m3)

# Three Cluster Solution
Lutz_3FEc = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM3_FEc, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m4)
Lutz_m4 = add_criterion(Lutz_m4, "loo", reloo=T)
loo_compare(Lutz_m1, Lutz_m4)


Lutz_3FeC = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM3_FeC, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m5)

Lutz_3fEC = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM3_fEC, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m6)

# Five Cluster Solution
Lutz_5fEc = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_fEc, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m7)
Lutz_5fEc = add_criterion(Lutz_m7, "loo", reloo=T)
loo_compare(Lutz_m1, Lutz_m7)

Lutz_5FEC = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_FEC, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m8)
Lutz_m8 = add_criterion(Lutz_m8, "loo", reloo=T)
loo_compare(Lutz_m1, Lutz_m8)

Lutz_5FeC = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_FeC, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m9)

Lutz_5fEC = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_fEC, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m10)

Lutz_5Fec = update(Lutz_m1, GA_lutz_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_Fec, newdata = tscs_data_GA_Lutz_trans)
summary(Lutz_m11)
Lutz_m11 = add_criterion(Lutz_m11, "loo")
loo_compare(Lutz_m1, Lutz_m11)






# CCP ####
test = lm(GA_ccp_ga ~ length_const_num_ctl + age_const_num_ctl + FKM5_c + FKM5_E, tscs_data_GA_Lutz_trans)
summary(test)


test = lm(GA_ccp_ga ~ length_const_num_ctl + age_const_num_ctl + FKM3_FEc + FKM3_FeC + FKM3_fEC, tscs_data_GA_Lutz_trans)
summary(test)


test = lm(GA_ccp_ga ~ cabinet_cpds_cat_ctl + length_const_num_ctl + age_const_num_ctl + FKM5_fEc + FKM5_Fec + FKM5_FEC + FKM5_FeC + FKM5_fEC, tscs_data_GA_Lutz_trans)
summary(test)
