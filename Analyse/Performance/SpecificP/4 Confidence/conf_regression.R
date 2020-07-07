# Regression Confidence ####

# SETUP ####
library(glmmTMB)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/survey_conf_trans.Rdata")

#source("Analyse/Performance/SpecificP/4 Confidence/conf_dataset.R")





smallsample = survey_conf_trans %>% 
  filter(year_study >= 2000)
#is.na(exec_parties_1945_2010_ctl) == F)
# NullModel
m0 = glmmTMB(conf_index ~ 1 + (1| country_text_id), 
             smallsample)
summary(m0)



m1 = glmmTMB(conf_index ~ 
               age_num_ivs_ctl +
               female_ord_ivs_ctl +
               education_ord_ivs_ctl +
               highincome_scale_ord_ivs_ctl +
               highsubjincome_ord_ivs_ctl +
               interestpol_ord_ivs_ctl +
               postmat3_ord_ivs_ctl +
               # postmat5_ord_ivs_ctl +
               trust_ord_ivs_ctl +
               
               gdppc_wdi_num_l2_ctl +
               FKM5_FEC + 
               
               (1| country_text_id), 
             smallsample)
summary(m1)

anova(m0,m1)

plot(effect(term = "FKM5_E", mod = m1))

table(smallsample$gdppc_wdi_num_l2_ctl)


# Regression: Other Demoocracy Profiles ####

m_consensus = glmmTMB(conf_index ~ 
                        age_num_ivs_ctl +
                        female_ord_ivs_ctl +
                        education_ord_ivs_ctl +
                        highincome_scale_ord_ivs_ctl +
                        highsubjincome_ord_ivs_ctl +
                        interestpol_ord_ivs_ctl +
                        postmat3_ord_ivs_ctl +
                        # postmat5_ord_ivs_ctl +
                        trust_ord_ivs_ctl +
                        
                        gdppc_wdi_num_l2_ctl +
                        exec_parties_1945_2010_ctl  + 
                        (1| country_text_id), 
                      smallsample)
summary(m_consensus)

plot(effect(term = "exec_parties_1945_2010_ctl", mod = m_consensus))


m_consensus = glmmTMB(conf_index ~ 
                        age_num_ivs_ctl +
                        female_ord_ivs_ctl +
                        education_ord_ivs_ctl +
                        highincome_scale_ord_ivs_ctl +
                        highsubjincome_ord_ivs_ctl +
                        interestpol_ord_ivs_ctl +
                        postmat3_ord_ivs_ctl +
                        # postmat5_ord_ivs_ctl +
                        trust_ord_ivs_ctl +
                        
                        gdppc_wdi_num_l2_ctl +
                        exec_parties_1945_2010_ctl  + 
                        # eff_num_parl_parties_1945_2010_ctl +
                        # pct_minimal_winning_one_party_cabinet_1945_2010_ctl + 
                        # index_of_exec_dominance_1945_2010_ctl + 
                        # index_of_disproportionality_1945_2010_ctl +
                        # index_of_interest_group_pluralism_1945_2010_ctl + 
                        federal_unitary_1945_2010_ctl +
                        
                        (1| country_text_id), 
                      smallsample)
summary(m_consensus)

plot(effect(term = "exec_parties_1945_2010_ctl", mod = m_consensus))
