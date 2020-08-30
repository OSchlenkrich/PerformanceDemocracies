# CONFIDENCE REGRESSION ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
source("Analyse/Cluster/OtherDemocracyProfiles.R")

# Create IVS #### 
source("Analyse/PerformanceAreas/Confidence/CreateIVS.R")


# Control Variables ####
confidence_IVars = IVS %>% 
  dplyr::select(
    survey = S001,
    unified_resp = S007,
    orig_resp = S006,
    
    country_text_id,
    year_study = S020,
    

    # SES
    education_ord_ivs_ctl = X025,
    female_ord_ivs_ctl = X001,
    age_num_ivs_ctl = X003,
    highincome_scale_ord_ivs_ctl = X047,
    highsubjincome_ord_ivs_ctl = C006,
    
    
    # Participation
    interestpol_ord_ivs_ctl = E023, # needs to be reversed
    
    # Culture, Opinion
    trust_ord_ivs_ctl = A165, # needs to be reversed
    postmat3_ord_ivs_ctl=Y002,
    postmat5_ord_ivs_ctl=Y001,
  ) %>%
  mutate(survey = if_else(survey == 1, "EVS", "WVS"),
         # reversing...
         interestpol_ord_ivs_ctl = max(interestpol_ord_ivs_ctl, na.rm=T) - interestpol_ord_ivs_ctl  + 1,
         trust_ord_ivs_ctl = max(trust_ord_ivs_ctl, na.rm=T) - trust_ord_ivs_ctl + 1,
         postmat5_ord_ivs_ctl = postmat5_ord_ivs_ctl + 1) %>% 

  filter(year_study > 1950) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>%  
  
  left_join(dmx_trade_cluster %>%  select(country_text_id, year_study = year, classification_core), 
            by=c("country_text_id", "year_study"))  %>%

  mutate(id = paste(unified_resp, orig_resp, year_study, sep="")) %>% 
  
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  filter(year_study >= 1990) %>%
  
  select_at(vars(id, starts_with("FKM"), ends_with("_ctl"))) 




#QoC
QoC_control_survey = QoC_data %>% 
  dplyr::select(country_text_id, year,
                # Economic Modernization
                gdppc_wdi_num_ctl  = wdi_gdpcapcon2010,
                
  ) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  mutate_at(vars(matches("pr")), funs(./100)) %>% 
  arrange(country_text_id, year) %>%
  
  filter(year >= 1974, year <= 2010) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(-year)


# Other Democracy Profiles ####

consensusdemocracy_survey = consensusdemocracy %>% 
  select(country_text_id, 
         exec_parties_1945_2010_odempr_ctl = exec_parties_1945_2010, 
         exec_parties_1981_2010_odempr_ctl = exec_parties_1981_2010, 
         federal_unitary_1945_2010_odempr_ctl = federal_unitary_1945_2010, 
         federal_unitary_1981_2010_odempr_ctl = federal_unitary_1981_2010)

centripetalism_survey = centripetalism %>% 
  select(country_text_id, year_study = year, centrip_odempr = Cent_Sdep1)   %>% 
  filter(year_study >= 1974, year_study <= 2010) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(-year_study)

# Main Independent Variable ####
# Log Ratios Democracy Profiles
source("Analyse/Cluster/LogRatios_v2.R")

profiles_survey = LogRATIOS_eco_dim %>% 
  rename(year_study = year) %>% 
  left_join(LogRATIOS_eco_total %>% rename(year_study = year), 
            by=c("country_text_id", "year_study"))  %>% 
  select(country_text_id, year_study, everything()) %>% 
  
  filter(year_study >= 1974, year_study <= 2010) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(-year_study)


# Create Final Dataset ####

survey_conf = performance_pclvl1 %>% 
  
  select_at(vars(-matches("_ord"))) %>% 
  left_join(confidence_IVars, by="id") %>% 
  # main independent variable 
  left_join(profiles_survey, by=c("country_text_id")) %>% 
  left_join(QoC_control_survey, 
            by=c("country_text_id")) %>% 
  left_join(consensusdemocracy_survey, 
            by=c("country_text_id")) %>% 
  left_join(centripetalism_survey, 
            by=c("country_text_id")) %>% 
  arrange(year_study, country_text_id) %>% 
  ungroup() %>% 
  filter(is.na(FKM4_E) == F,
         year_study >= 2000)



# Transformation ####
survey_conf_trans = survey_conf %>% 
  mutate_at(vars(matches("ord")), funs(. - 1)) %>% 
  mutate_at(vars(starts_with("age")), funs(. - median(., na.rm=T))) %>% 
  # GMC  
  mutate(gdppc_wdi_num_ctl = log(gdppc_wdi_num_ctl),
         gdppc_wdi_num_ctl = scale_this(gdppc_wdi_num_ctl)) 



# Missings Plot ####
survey_conf_trans %>% 
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


# Distributions ####
p1 = survey_conf %>% 
  select_at(vars(ends_with("_ctl"), conf_index, starts_with("FKM"), -matches("odempr"))) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = gsub("_num_ivs_ctl", "", name),
         name = gsub("_ord_ivs_ctl", "", name),
         name = gsub("_num_l2_ctl", "", name),
         name = gsub("_pr_l2_ctl", "", name),
         name = gsub("_ctl", "", name)) %>%  
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(name~., scales = "free")  +
  theme_bw() +
  ggtitle("Raw Values") +
  xlab("") +
  ylab("")


p2 = survey_conf_trans %>% 
  select_at(vars(ends_with("_ctl"), conf_index, starts_with("FKM"), -matches("odempr"))) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = gsub("_num_ivs_ctl", "", name),
         name = gsub("_ord_ivs_ctl", "", name),
         name = gsub("_num_l2_ctl", "", name),
         name = gsub("_pr_l2_ctl", "", name),
         name = gsub("_ctl", "", name)) %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(name~., scales = "free")  +
  theme_bw() +
  ggtitle("Transformed Values") +
  xlab("") +
  ylab("")

ggarrange(p1, p2, ncol=1)


# Correlation Plot ####

corrplot(cor(survey_conf_trans %>% 
               select_if(is.numeric) %>%
               select_at(vars(-matches("odempr"), -year_study, -weights)) %>% 
               rename_all(funs(gsub("_num_ivs_ctl", "", .)))  %>% 
               rename_all(funs(gsub("_ord_ivs_ctl", "", .))) %>% 
               rename_all(funs(gsub("_num_l2_ctl", "", .))) %>% 
               rename_all(funs(gsub("_ctl", "", .))) %>% 
               rename_all(funs(gsub("_num", "", .))), use="pairwise"))



# XY Plots ####  


survey_conf_trans %>% 
  select_at(vars(country_text_id, conf_index, starts_with("FKM"), gdppc_wdi_num_ctl)) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(-country_text_id) %>% 
  pivot_longer(cols=-conf_index) %>% 
  ggplot(aes(x=value, y=conf_index)) +
  geom_point() +
  geom_smooth(se=F, color ="red") +
  facet_wrap(name ~ .) +
  theme_bw() +
  xlab("") +
  ylab("Confidence")

survey_conf_trans %>% 
  select_at(vars(country_text_id, conf_index, matches("odempr"))) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(-country_text_id) %>% 
  pivot_longer(cols=-conf_index) %>% 
  ggplot(aes(x=value, y=conf_index)) +
  geom_point() +
  geom_smooth(se=F, color ="red") +
  facet_wrap(name ~ ., scales="free_x") +
  theme_bw() +
  xlab("") +
  ylab("Confidence")


# Saving ####
save(survey_conf_trans, file = "Analyse/Performance/SpecificP/Datasets/survey_conf_trans_v2.Rdata")

