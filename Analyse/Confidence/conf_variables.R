# Latent Pattern Maintenance Performance
source("Analyse/Cluster_v3.R")


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)


#### Political Culture ####

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
  select(country, country_text_id) %>% 
  group_by(country) %>% 
  slice(1) 


# Check if identifier matches with own sample (Cyprus?)

dmx_trade_cluster %>% 
  select(country) %>% 
  distinct() %>% 
  left_join(cty_identifier, by="country") %>% 
  mutate(DMX = "DMX") %>% 
  left_join(IVS_ctry_id%>% 
              mutate(IVS = "IVS"), by="country")
  

  
# Create Integrated Value Survey + country_text_id for matching
EVS = read_spss("C:/RTest/IVS/ZA4804_v3-0-0.sav", user_na = T)
WVS = readRDS("C:/RTest/IVS/F00008390-WVS_Longitudinal_1981_2016_r_v20180912.rds")

IVS = WVS %>% 
  bind_rows(EVS) %>% 
  left_join(IVS_ctry_id, by="S003") %>% 
  left_join(cty_identifier, by="country")



### Extract Confidence Variables

confidence_IVS = IVS %>% 
  select(
    survey = S001,

    country,
    country_text_id,
    year_study = S020,
    conf_parliament_ord_ivs = E069_07,
    conf_civilservice_ord_ivs = E069_08,
    conf_judiciary_ord_ivs = E069_17,
    conf_parties_ord_ivs = E069_12,
    conf_govt_ord_ivs = E069_11,
    
    # SES
    education_ord_aux_ivs = X025,
    gender_ord_aux_ivs = X001,
    age_num_aux_ivs = X003,
    
    # Participation
    interest_ord_aux_ivs = E023,

    # Culture, Opinion
    trust_ord_aux_ivs = A165,
    conf_press_ord_aux_ivs = E069_04,
  ) %>%
  mutate(survey = if_else(survey == 1, "EVS", "WVS")) %>% 
  mutate_at(vars(ends_with("_ivs")),  ~if_else(. < 0, NA_real_, .)) %>% 
  filter(year_study > 1950) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  arrange(country_text_id, year_study)

table(confidence_IVS$wave, confidence_IVS$survey)

##### NA-Plots ####
# each study year - and + three years
# vars are not missing since 1990
confidence_IVS %>%
  select_at(vars("country_text_id", "year_study", ends_with("ivs"))) %>% 
  # na.omit() %>% 
  melt(id.vars=c("country_text_id", "year_study")) %>% 
  group_by(country_text_id, year_study, variable) %>% 
  na.omit() %>% 
  summarise(cases = n()) %>% 
  arrange(country_text_id, year_study) %>% 
  group_by(country_text_id, variable)%>% 
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
  select(country_text_id, year = year_study, variable) %>% 
  mutate(helper_ivs = 1) %>% 
  right_join(dmx_trade_cluster %>% 
               select(country, country_text_id, year, cluster_label_1st) %>% 
               filter(year >= 1950), by=c("country_text_id", "year")) %>%
  pivot_wider(names_from = variable,
              values_from = helper_ivs) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_ivs"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
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

# Is conf_media missing, when the other conf_variables are missing?

confidence_IVS  %>% 
  select_at(vars(starts_with("conf"))) %>% 
  is.na %>%
  data.frame() %>% 
  mutate_all(~if_else(.==T, "miss", "obs")) %>% 
  melt(id.vars="conf_press_ord_aux_ivs") %>% 
  group_by(conf_press_ord_aux_ivs, variable, value) %>% 
  tally() %>% 
  ggplot(aes(x = value, y=n, fill=conf_press_ord_aux_ivs)) +
  geom_bar(stat="identity") +
  facet_wrap(variable~1)


# Time Series: Align Data

confidence_IVS %>% 
  select(survey, country, year_study) %>% 
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


## Delete Duplicates (-3, +3 years)
remove_near_times = function(x) {
  if (length(x) == 1) {
    return(T)
  }
  y = rep(FALSE, length(x))
  y[1] = TRUE
  start_t = x[1]

  for (i in 2:length(x)) {
    diff = x[i] - start_t
    if (diff > 5) {
      y[i] = T
      start_t = x[i]
    }
  }
  return(y)
}

time_frame = confidence_IVS %>% 
  select(country, country_text_id, year_study, survey) %>% 
  # filter(country == "Germany") %>% 
  mutate(data = "obs",
         line = 1) %>% 
  distinct() %>% 
  arrange(country_text_id, year_study) %>% 
  group_by(country_text_id) %>% 
  mutate(replace = remove_near_times(year_study)) %>% 
  filter(replace == T) %>% 
  ungroup() %>% 
  select(country_text_id, survey, year_study) 


time_frame %>% 
  left_join(confidence_IVS, by=c("country_text_id", "year_study", "survey")) %>% 
  dim()


time_frame %>% 
  left_join(confidence_IVS, by=c("country_text_id", "year_study", "survey")) %>%  
  select(survey, country, country_text_id, year_study) %>% 
  filter(country %in% unique(confidence_IVS$country)[sample(length(unique(confidence_IVS$country)), 15)]) %>% 
  mutate(country = gsub(" ", "\n", country)) %>% 
  mutate(data = "obs",
         line = 1) %>% 
  distinct() %>% 
  group_by(country) %>% 
  tidyr::complete(country, year_study = min(year_study):max(year_study), fill = list(data = "miss", line=1)) %>% 
  unite(survey_data, c(survey, data)) %>% 
  ggplot(aes(x=year_study, y=line, fill=survey_data)) +
  geom_tile(alpha=0.5) +
  facet_grid(country ~ .) +
  ylab("") +
  scale_x_continuous(breaks=seq(1980, 2020,5)) +
  geom_vline(xintercept = seq(1980, 2020, 10), alpha=0.5) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())



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
  mutate_at(vars(age_num_aux_ivs), funs(trans_function(.,lambda))) %>% 
  ggplot(aes(x=age_num_aux_ivs)) +
  geom_histogram()

confidence_IVS_norm = confidence_IVS %>% 
  mutate_at(vars( starts_with("age_num_aux_ivs")), funs(trans_function(.,lambda))) %>%
  mutate_at(vars(starts_with("ord")), funs(. - 1)) %>% 
  mutate_at(vars(starts_with("conf")), funs(max(., na.rm=T) - .)) 

table(confidence_IVS$conf_parliament_ord_ivs)

### NA Frame & Transfer to MI

fa_data_conf_frame = confidence_IVS_norm %>% 
  select_at(vars(ends_with("_ivs"))) %>% 
  mutate(non_na_perc = rowSums(is.na(.)==F)/dim(.)[2]) %>% 
  bind_cols(confidence_IVS %>%  dplyr::select(survey, country, country_text_id, year_study)) %>% 
  # shrink data.frame
  right_join(time_frame, by=c("country_text_id", "year_study", "survey")) %>% 
  select(survey, country, country_text_id, year_study, everything())


