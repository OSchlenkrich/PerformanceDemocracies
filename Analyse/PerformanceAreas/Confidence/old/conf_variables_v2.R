# Latent Pattern Maintenance Performance
#### Confidence ####

IVS_ctry_id = read.csv("C:/RTest/IVS/ctry_id.csv", header=T, encoding = "UTF-8") %>% 
  rename(S003 = V2) %>% 
  mutate(country = fct_recode(country,
                              "United Kingdom" = "Great Britain",
                              "Czech Republic" = "Czech Rep.",
                              "Bosnia and Herzegovina" = "Bosnian Federation",
                              "Republic of Vietnam" = "Viet Nam",
                              "Bosnia and Herzegovina" = "Bosnia",
                              "United States of America" = "United States")) %>% 
  filter(S003 > 0)



cty_identifier = V_dem %>% 
  dplyr::select(country, country_text_id) %>% 
  group_by(country) %>% 
  slice(1) 


# Check if identifier matches with own sample (Cyprus?)

dmx_trade_cluster %>% 
  dplyr::select(country) %>% 
  distinct() %>% 
  left_join(cty_identifier, by="country") %>% 
  mutate(DMX = "DMX") %>% 
  left_join(IVS_ctry_id%>% 
              mutate(IVS = "IVS"), by="country")
  

  
# Create Integrated Value Survey + country_text_id for matching
EVS = read_spss("C:/RTest/IVS/ZA4804_v3-0-0.sav", user_na = T)
WVS = read_spss("C:/RTest/IVS/WVS_Longitudinal_1981_2016_Spss_v20180912.sav", user_na = T)

# remove labels; needed for bind_rows
WVS = zap_labels(WVS)
EVS = zap_labels(EVS)

IVS = WVS %>% 
  bind_rows(EVS) %>% 
  left_join(IVS_ctry_id, by="S003") %>% 
  left_join(cty_identifier, by="country")

rm(EVS)
rm(WVS)
gc()


table(is.na(confidence_IVS$X047))

### Extract Confidence Variables

confidence_IVS = IVS %>% 
  dplyr::select(
    survey = S001,
    
    country,
    country_text_id,
    year_study = S020,
    
    weights = S017,
    
    conf_parliament_ord_ivs = E069_07,
    conf_civilservice_ord_ivs = E069_08,
    conf_judiciary_ord_ivs = E069_17,
    conf_parties_ord_ivs = E069_12,
    conf_govt_ord_ivs = E069_11,
    
    # SES
    education_ind_ord_ivs = X025,
    gender_ind_ord_ivs = X001,
    age_ind_num_ivs = X003,
    scaleincome_ord_ivs = X047,
    subjincome_ord_ivs = C006,

    
    # Participation
    interest_ind_ord_ivs = E023,

    # Culture, Opinion
    trust_ind_ord_ivs = A165,
    postmat_ord_ivs=Y002,
  ) %>%
  mutate(survey = if_else(survey == 1, "EVS", "WVS")) %>% 
  mutate_at(vars(weights, ends_with("_ivs")),  ~if_else(. < 0, NA_real_, .)) %>% 
  filter(year_study > 1950) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>%  
 
  left_join(dmx_trade_cluster %>%  select(-country, -regions) %>% rename(year_study = year), 
            by=c("country_text_id", "year_study"))  %>%
  # add regions
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  select(country, country_text_id, regions, year_study, everything())  %>% 
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
  select_at(vars(ends_with("_ivs"), weights)) %>% 
  summarise_all(pMiss) %>% 
  pivot_longer(cols=-year) %>% 
  ggplot(aes(x=year, y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(name~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10), limits=c(1980, 2020)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - IVS (WVS/EVS)")


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


# Time Series: Align Data

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


#### Bar Plots: 
# transform age
# anchor ord at 0
# reverse ord

my_years = distinct(confidence_IVS, year_study) %>% 
  arrange(year_study) %>% 
  pull(year_study)

plot_bar_list = list()
for (i in 1:length(my_years)) {
  plot_bar_list[[i]] = confidence_IVS %>% 
    filter(year_study == my_years[i]) %>% 
    select_at(vars( ends_with("_ivs"))) %>% 
    melt() %>% 
    ggplot(aes(x=value)) + 
    geom_histogram(aes(y= ..density..))  +
    facet_wrap(variable~., scales = "free") +
    ggtitle(my_years[i])
}

# marrangeGrob(plot_bar_list, nrow=2, ncol=2)

trans_function = function(x,lambda) {
  pow <- function(y, p) {
    if(p == 0) log(y) else (y^p - 1)/p
  }

  minimum = min(x, na.rm=T)
  constant = abs(minimum)

  if (minimum >= 0) {
    x_aligned = x - constant + 1
  } else {
    x_aligned = x + constant + 1
  }

  return(pow(x_aligned, lambda))
}

lambda = 0.5
confidence_IVS %>%
  mutate_at(vars(age_ind_num_ivs), funs(trans_function(.,lambda))) %>%
  ggplot(aes(x=age_ind_num_ivs)) +
  geom_histogram()

confidence_IVS_norm = confidence_IVS %>%
  mutate_at(vars( starts_with("age_ind_num_ivs")), funs(trans_function(.,lambda)))


### NA Frame & Transfer to FA FIML

fa_data_conf_frame = confidence_IVS_norm %>% 
  select_at(vars(ends_with("_ivs"))) %>% 
  mutate(non_na_perc = rowSums(is.na(.)==F)/dim(.)[2]) %>% 
  bind_cols(confidence_IVS %>%  dplyr::select(survey, country, country_text_id, weights, year_study, classification_core)) %>% 
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  dplyr::select(survey, country, country_text_id, weights, year_study, everything(), -classification_core) %>% 
  filter(year_study >= 1990) 


