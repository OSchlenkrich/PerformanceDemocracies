# CONFIDENCE REGRESSION ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")

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
                gdppc_wdi_num_l2_ctl  = wdi_gdpcapcon2010,
                
  ) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  mutate_at(vars(matches("pr")), funs(./100)) %>% 
  arrange(country_text_id, year) %>%
  
  filter(year >= 1990, year <= 2010) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(-year)


# Other Democracy Profiles ####
names(consensusdemocracy)
consensusdemocracy_survey = consensusdemocracy %>% 
  select(country_text_id, 
         exec_parties_1945_2010_ctl = exec_parties_1945_2010, 
         exec_parties_1981_2010_ctl = exec_parties_1981_2010, 
         federal_unitary_1945_2010_ctl = federal_unitary_1945_2010, 
         federal_unitry_1981_2010_ctl = federal_unitry_1981_2010,
         eff_num_parl_parties_1945_2010_ctl = eff_num_parl_parties_1945_2010,
         pct_minimal_winning_one_party_cabinet_1945_2010_ctl = pct_minimal_winning_one_party_cabinet_1945_2010,
         index_of_exec_dominance_1945_2010_ctl = index_of_exec_dominance_1945_2010,
         index_of_disproportionality_1945_2010_ctl = index_of_disproportionality_1945_2010,
         index_of_interest_group_pluralism_1945_2010_ctl = index_of_interest_group_pluralism_1945_2010)


# Main Independent Variable ####
# Log Ratios Democracy Profiles
source("Analyse/Cluster/LogRatios.R")

AllProfiles = LogRATIOS_3_eco_total %>% 
  rename(year_study = year) %>% 
  left_join(LogRATIOS_eco_dim %>% rename(year_study = year), 
            by=c("country_text_id", "year_study")) %>% 
  left_join(LogRATIOS_eco_total %>% rename(year_study = year), 
            by=c("country_text_id", "year_study")) %>% 
  select(country_text_id, year_study, everything()) %>% 
  
  filter(year_study >= 1945, year_study <= 2010) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(-year_study)


# Create Final Dataset ####

survey_conf = performance_pclvl1 %>% 
  left_join(confidence_IVars, by="id") %>% 
  # main independent variable 
  left_join(AllProfiles, by=c("country_text_id")) %>% 
  left_join(QoC_control_survey, 
            by=c("country_text_id")) %>% 
  left_join(consensusdemocracy_survey, 
            by=c("country_text_id")) %>% 
  arrange(year_study, country_text_id) %>% 
  ungroup()



# Transformation ####
survey_conf_trans = survey_conf %>% 
  mutate_at(vars(matches("ord")), funs(. - 1)) %>% 
  mutate_at(vars(starts_with("age")), funs(. - median(., na.rm=T))) %>% 
  # GMC  
  mutate(gdppc_wdi_num_l2_ctl = log(gdppc_wdi_num_l2_ctl),
         gdppc_wdi_num_l2_ctl = scale_this(gdppc_wdi_num_l2_ctl)) 


# Distributions ####
survey_conf %>% 
  select_at(vars(ends_with("_ctl"), starts_with("FKM"))) %>% 
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


survey_conf_trans %>% 
  select_at(vars(ends_with("_ctl"))) %>% 
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

# Correlation Plot ####

corrplot(cor(survey_conf_trans %>% 
               select_if(is.numeric) %>%
               rename_all(funs(gsub("_num_ivs_ctl", "", .)))  %>% 
               rename_all(funs(gsub("_ord_ivs_ctl", "", .))) %>% 
               rename_all(funs(gsub("_num_l2_ctl", "", .)))%>% 
               rename_all(funs(gsub("_ctl", "", .))), use="pairwise"))



# XY Plots ####  
survey_conf %>% 
  filter(year_study >= 2010) %>% 
  ggplot(aes(y=conf_index, x=FKM5_c)) +
  geom_point() +
  geom_smooth(se=F)


survey_conf %>% 
  #filter(year_study >= 2010) %>% 
  group_by(country_text_id) %>% 
  summarise(conf_index = mean(conf_index, na.rm=T),
            FKM5_E =  mean(FKM5_c)) %>% 
  ggplot(aes(y=conf_index, x=FKM5_E)) +
  geom_point() +
  geom_smooth(se=F)


# Saving ####
save(survey_conf_trans, file = "Analyse/Performance/SpecificP/Datasets/survey_conf_trans.Rdata")

