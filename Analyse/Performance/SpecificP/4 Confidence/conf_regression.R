# Regression Confidence ####

# SETUP ####
library(glmmTMB)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/survey_conf_trans_v2.Rdata")
source("Setup/Sig_Tables.R")
source("Analyse/Performance/SpecificP/4 Confidence/conf_table.R")

#source("Analyse/Performance/SpecificP/4 Confidence/conf_dataset.R")

table(survey_conf_trans$year_study)


newestSurvey = survey_conf_trans %>% 
  filter(year_study >= 2000) %>% 
  select(country_text_id, year_study) %>% 
  distinct() %>% 
  arrange(country_text_id, year_study) %>% 
  group_by(country_text_id) %>% 
  top_n(1,  year_study) %>% 
  ungroup()

survey_conf_trans_small =  survey_conf_trans %>% 
  select_at(vars(-matches("odempr"), -postmat5_ord_ivs_ctl, -highincome_scale_ord_ivs_ctl, -highsubjincome_ord_ivs_ctl)) %>% 
  right_join(newestSurvey, by=c("country_text_id", "year_study")) %>% 
  na.omit() 

survey_conf_trans_small %>% 
  filter(is.na(conf_index) == F) %>% 
  select_at(vars(-matches("lag"),-matches("lead"), -country_text_id, -matches("odempr"), -id, -year_study, -weights)) %>% 
  #group_by(year_0) %>% 
  summarise_all(pMiss_01) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_notrans_ctl", "", name),
         name = gsub("_ord_ivs", "", name),
         name = gsub("_ctl", "", name),
         name = gsub("_ctl", "", name)) %>% 
  ggplot(aes(x=name, y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in TSCS - Confidence") +
  xlab("")

# NullModel ####
m0 = glmmTMB(conf_index ~ 1 + (1| country_text_id), 
             survey_conf_trans_small)
summary(m0)


# Lvl 1 #####
m1 = glmmTMB(conf_index ~ 
               age_num_ivs_ctl +
               female_ord_ivs_ctl +
               education_ord_ivs_ctl +
               # highincome_scale_ord_ivs_ctl +
               # highsubjincome_ord_ivs_ctl +
               interestpol_ord_ivs_ctl +
               postmat3_ord_ivs_ctl +
               # postmat5_ord_ivs_ctl +
               trust_ord_ivs_ctl +

               (1| country_text_id), 
             survey_conf_trans_small)
summary(m1)

anova(m0,m1)

# Lvl2 ####
m2_Fec = glmmTMB(conf_index ~ 
                   age_num_ivs_ctl +
                   female_ord_ivs_ctl +
                   education_ord_ivs_ctl +
                   # highincome_scale_ord_ivs_ctl +
                   # highsubjincome_ord_ivs_ctl +
                   interestpol_ord_ivs_ctl +
                   postmat3_ord_ivs_ctl +
                   # postmat5_ord_ivs_ctl +
                   trust_ord_ivs_ctl +
                   
                   gdppc_wdi_num_ctl +
                   FKM5_Fec + 
                   
                   (1| country_text_id), 
                 survey_conf_trans_small)
summary(m2_Fec)
anova(m1,m2_Fec)

m2_fEc = glmmTMB(conf_index ~ 
                   age_num_ivs_ctl +
                   female_ord_ivs_ctl +
                   education_ord_ivs_ctl +
                   # highincome_scale_ord_ivs_ctl +
                   # highsubjincome_ord_ivs_ctl +
                   interestpol_ord_ivs_ctl +
                   postmat3_ord_ivs_ctl +
                   # postmat5_ord_ivs_ctl +
                   trust_ord_ivs_ctl +
                   
                   gdppc_wdi_num_ctl +
                   FKM5_fEc + 
                   
                   (1| country_text_id), 
                 survey_conf_trans_small)
summary(m2_fEc)
anova(m1,m2_fEc)

m2_fEC = glmmTMB(conf_index ~ 
                   age_num_ivs_ctl +
                   female_ord_ivs_ctl +
                   education_ord_ivs_ctl +
                   # highincome_scale_ord_ivs_ctl +
                   # highsubjincome_ord_ivs_ctl +
                   interestpol_ord_ivs_ctl +
                   postmat3_ord_ivs_ctl +
                   # postmat5_ord_ivs_ctl +
                   trust_ord_ivs_ctl +
                   
                   gdppc_wdi_num_ctl +
                   FKM5_fEC + 
                   
                   (1| country_text_id), 
                 survey_conf_trans_small)
summary(m2_fEC)
anova(m1,m2_fEC)

m2_FeC = glmmTMB(conf_index ~ 
                   age_num_ivs_ctl +
                   female_ord_ivs_ctl +
                   education_ord_ivs_ctl +
                   # highincome_scale_ord_ivs_ctl +
                   # highsubjincome_ord_ivs_ctl +
                   interestpol_ord_ivs_ctl +
                   postmat3_ord_ivs_ctl +
                   # postmat5_ord_ivs_ctl +
                   trust_ord_ivs_ctl +
                   
                   gdppc_wdi_num_ctl +
                   FKM5_FeC + 
                   
                   (1| country_text_id), 
                 survey_conf_trans_small)
summary(m2_FeC)
anova(m1,m2_FeC)

m2_FEC = glmmTMB(conf_index ~ 
               age_num_ivs_ctl +
               female_ord_ivs_ctl +
               education_ord_ivs_ctl +
                 # highincome_scale_ord_ivs_ctl +
                 # highsubjincome_ord_ivs_ctl +
               interestpol_ord_ivs_ctl +
               postmat3_ord_ivs_ctl +
               # postmat5_ord_ivs_ctl +
               trust_ord_ivs_ctl +
               
               gdppc_wdi_num_ctl +
               FKM5_FEC + 
               
               (1| country_text_id), 
             survey_conf_trans_small)
summary(m2_FEC)
anova(m1,m2_FEC)


m2_E = glmmTMB(conf_index ~ 
                   age_num_ivs_ctl +
                   female_ord_ivs_ctl +
                   education_ord_ivs_ctl +
                 # highincome_scale_ord_ivs_ctl +
                 # highsubjincome_ord_ivs_ctl +
                   interestpol_ord_ivs_ctl +
                   postmat3_ord_ivs_ctl +
                   # postmat5_ord_ivs_ctl +
                   trust_ord_ivs_ctl +
                   
                   gdppc_wdi_num_ctl +
                   FKM4_E + 
                   
                   (1| country_text_id), 
                 survey_conf_trans_small)
summary(m2_E)
anova(m1,m2_E)


m2_c = glmmTMB(conf_index ~ 
                   age_num_ivs_ctl +
                   female_ord_ivs_ctl +
                   education_ord_ivs_ctl +
                 # highincome_scale_ord_ivs_ctl +
                 # highsubjincome_ord_ivs_ctl +
                   interestpol_ord_ivs_ctl +
                   postmat3_ord_ivs_ctl +
                   # postmat5_ord_ivs_ctl +
                   trust_ord_ivs_ctl +
                   
                   gdppc_wdi_num_ctl +
                   FKM4_c + 
                   
                   (1| country_text_id), 
                 survey_conf_trans_small)
summary(m2_c)
anova(m1,m2_c)


# Create Summary Table ####
make_glmm_tables(m0, m1,m2_Fec, m2_fEc, m2_fEC, m2_FeC, m2_FEC, m2_E,m2_c)


effect_plot = effect(term = "FKM5_fEC", mod = m2_fEC, xlevels=10)

data.frame(est = effect_plot$fit,
           lower = effect_plot$lower,
           upper = effect_plot$upper,
           FKM5_fEC = effect_plot$x) %>% 
  ggplot(aes(x=FKM5_fEC, y=est, ymin=lower, ymax=upper)) +
  geom_line() +
  geom_ribbon(alpha=0.2) +
  theme_bw() +
  ylab("Confidence Index")




# Regression: Other Demoocracy Profiles ####

m_exec = glmmTMB(conf_index ~ 
                        age_num_ivs_ctl +
                        female_ord_ivs_ctl +
                        education_ord_ivs_ctl +
                        highincome_scale_ord_ivs_ctl +
                        highsubjincome_ord_ivs_ctl +
                        interestpol_ord_ivs_ctl +
                        postmat3_ord_ivs_ctl +
                        # postmat5_ord_ivs_ctl +
                        trust_ord_ivs_ctl +
                        
                        gdppc_wdi_num_ctl +
                        exec_parties_1981_2010_odempr_ctl  + 
                        (1| country_text_id), 
                 survey_conf_trans_small)
summary(m_exec)


plot(effect(term = "exec_parties_1981_2010_odempr_ctl", mod = m_exec))


m_federal = glmmTMB(conf_index ~ 
                        age_num_ivs_ctl +
                        female_ord_ivs_ctl +
                        education_ord_ivs_ctl +
                        highincome_scale_ord_ivs_ctl +
                        highsubjincome_ord_ivs_ctl +
                        interestpol_ord_ivs_ctl +
                        postmat3_ord_ivs_ctl +
                        # postmat5_ord_ivs_ctl +
                        trust_ord_ivs_ctl +
                        
                        gdppc_wdi_num_ctl +
                        federal_unitary_1981_2010_odempr_ctl +
                        
                        (1| country_text_id), 
                    survey_conf_trans_small)
summary(m_federal)

plot(effect(term = "federal_unitary_1981_2010_odempr_ctl", mod = m_federal))


m_cent = glmmTMB(conf_index ~ 
                      age_num_ivs_ctl +
                      female_ord_ivs_ctl +
                      education_ord_ivs_ctl +
                      highincome_scale_ord_ivs_ctl +
                      highsubjincome_ord_ivs_ctl +
                      interestpol_ord_ivs_ctl +
                      postmat3_ord_ivs_ctl +
                      # postmat5_ord_ivs_ctl +
                      trust_ord_ivs_ctl +
                      
                      gdppc_wdi_num_ctl +
                      centrip_odempr_ctl +
                      
                      (1| country_text_id),
                    survey_conf_trans_small)
summary(m_cent)

plot(effect(term = "centrip_odempr_ctl", mod = m_cent))




make_glmm_tables(m0, m1,m2_Fec, m2_fEc, m2_fEC, m2_FeC, m2_FEC, m2_E,m2_c,m_exec, m_federal, m_cent)
