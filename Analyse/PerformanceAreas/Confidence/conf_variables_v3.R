# Latent Pattern Maintenance Performance
#### Confidence ####

source("Analyse/PerformanceAreas/Confidence/CreateIVS.R")

### Extract Confidence Variables

confidence_IVS = IVS %>% 
  dplyr::select(
    survey = S001,
    unified_resp = S007,
    orig_resp = S006,

    country,
    country_text_id,
    year_study = S020,
    
    weights = S017,
    
    conf_parliament_ord_ivs = E069_07,
    conf_civilservice_ord_ivs = E069_08,
    conf_judiciary_ord_ivs = E069_17,
    conf_parties_ord_ivs = E069_12,
    conf_govt_ord_ivs = E069_11,
    
  ) %>%
  mutate(survey = if_else(survey == 1, "EVS", "WVS")) %>% 
  mutate_at(vars(weights, ends_with("_ivs")),  ~if_else(. < 0, NA_real_, .)) %>% 
  filter(year_study > 1950) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>%  
 
  left_join(dmx_trade_cluster %>%  select(country_text_id, year_study = year, classification_core), 
            by=c("country_text_id", "year_study"))  %>%
  # Create ID Variable
  mutate(id = paste(unified_resp, orig_resp, year_study, sep="")) %>%
  select(country, country_text_id, id, year_study, everything(), -unified_resp, -orig_resp)  %>% 
  arrange(country_text_id, year_study) 


##### NA-Plots ####


# each study year - and + three years
# vars are not missing since 1990
confidence_IVS %>%
  select_at(vars("country_text_id", "year_study", ends_with("_ivs"), weights, -matches("_ind"))) %>% 
  # na.omit() %>% 
  pivot_longer(cols=c(-country_text_id, -year_study)) %>% 
  group_by(country_text_id, year_study, name) %>% 
  na.omit() %>% 
  summarise(cases = n()) %>% 
  arrange(country_text_id, year_study) %>% 
  group_by(country_text_id, name)%>% 
  tidyr::complete(country_text_id, year_study = full_seq(year_study, 1)) %>% 
  # group_by(country_text_id) %>% 
  # mutate(cases_lag1 = dplyr::lag(cases,1),
  #        cases_lag2 = dplyr::lag(cases,2),
  #        cases_lag3 = dplyr::lag(cases,3),
  #        cases_lead1 = dplyr::lead(cases,1),
  #        cases_lead2 = dplyr::lead(cases,2),
  #        cases_lead3 = dplyr::lead(cases,3)
  # ) %>% 
  filter_at(vars(starts_with("cases")), any_vars(is.na(.)==F)) %>% 
  dplyr::select(country_text_id, year = year_study, name) %>% 
  mutate(helper_ivs = 1) %>% 
  right_join(dmx_trade_cluster %>% 
               dplyr::select(country, country_text_id, year, FKM_5_cluster) %>% 
               filter(year >= 1950), by=c("country_text_id", "year")) %>%
  pivot_wider(names_from = name,
              values_from = helper_ivs) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_ivs"), -weights)) %>% 
  summarise_all(pMiss_01) %>% 
  pivot_longer(cols=-year) %>% 
  ggplot(aes(x=year, y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(name~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")



confidence_IVS %>%
  select_at(vars("country_text_id", "year_study", ends_with("ivs"))) %>% 
  # na.omit() %>% 
  pivot_longer(cols=c(-country_text_id, -year_study)) %>% 
  group_by(country_text_id, year_study, name) %>% 
  na.omit() %>% 
  summarise(cases = n()) %>% 
  arrange(country_text_id, year_study) %>% 
  group_by(country_text_id, name)%>% 
  tidyr::complete(country_text_id, year_study = full_seq(year_study, 1)) %>% 
  group_by(country_text_id) %>% 
  mutate(cases_lag1 = dplyr::lag(cases,1),
         cases_lag2 = dplyr::lag(cases,2),
         cases_lag3 = dplyr::lag(cases,3),
         cases_lead1 = dplyr::lead(cases,1),
         cases_lead2 = dplyr::lead(cases,2),
         cases_lead3 = dplyr::lead(cases,3)
  ) %>% 
  filter_at(vars(starts_with("cases")), any_vars(is.na(.)==F)) %>% 
  dplyr::select(country_text_id, year = year_study, name) %>% 
  mutate(helper_ivs = 1) %>% 
  right_join(dmx_trade_cluster %>% 
               dplyr::select(country, country_text_id, year, FKM_5_cluster) %>% 
               filter(year >= 1950), by=c("country_text_id", "year")) %>%
  pivot_wider(names_from = name,
              values_from = helper_ivs) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_ivs"))) %>% 
  summarise_all(pMiss) %>% 
  pivot_longer(cols=-year) %>% 
  ggplot(aes(x=year, y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(name~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10), limits=c(1990, 2020)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - IVS (WVS/EVS)")


####

my_years = distinct(confidence_IVS, year_study) %>% 
  arrange(year_study) %>% 
  pull(year_study)

plot_list = list()
for (i in 1:length(my_years)) {
  plot_list[[i]] = confidence_IVS  %>% 
    filter(year_study == my_years[i]) %>% 
    select_at(vars(ends_with("_ivs"))) %>% 
    missd_pattern() +
    ggtitle(my_years[i])
}

# marrangeGrob(plot_list, nrow=2, ncol=2)


# Time Series: Overview

confidence_IVS %>% 
  dplyr::select(survey, country, year_study) %>% 
  filter(country %in% unique(confidence_IVS$country)[sample(length(unique(confidence_IVS$country)), 15)]) %>% 
  mutate(data = "obs",
         line = 1) %>% 
  distinct() %>% 
  group_by(country) %>% 
  tidyr::complete(country, year_study = min(year_study):max(year_study), fill = list(data = "miss", line=1)) %>% 
  unite(survey_data, c(survey, data)) %>% 
  ggplot(aes(x=year_study, y=line, fill=survey_data)) +
  geom_tile(alpha=0.5) +
  facet_grid(country ~ .)




### NA Frame & Transfer to FA FIML

fa_data_conf_frame = confidence_IVS %>%  
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  dplyr::select(id, survey, country, country_text_id, weights, year_study, everything(), -classification_core) %>% 
  filter(year_study >= 1990) 


