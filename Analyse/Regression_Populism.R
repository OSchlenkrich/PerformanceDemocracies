source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster.R")
source("Setup/Base_Functions.R")
source("Setup/MergeToFinalDataset.R")

# Populism

Populism_data = dmx_trade_cluster_ext %>% 
  dplyr::select(country, 
                region,
                year,   
                mod_cluster_1st,
                classification_context,
                populist, 
                populist_vote_share,
                populist_perc_seats,
                populist_cabinet,
                populist_prime_minister,
                populist_pres, 
                populist_is_gov, 
                mod_cluster_2nd, 
                gdp_capita) %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  filter(country != "Greece") %>% 
  filter(is.na(populist_pres) == F | is.na(populist_prime_minister) == F) %>% 
  mutate(populist_vote_share = if_else(is.na(populist_vote_share) == T, 0, populist_vote_share)) %>% 
  mutate(populist_perc_seats = if_else(is.na(populist_perc_seats) == T, 0, populist_perc_seats))



#### Some Plots


Populism_data %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(populist_vote_share, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)

Populism_data %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(populist_perc_seats, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)

Populism_data %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(populist_is_gov, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)
####

lag_df = function(x1) {
  x1_df = x1 - dplyr::lag(x1, 1)
  return(x1_df)
}


Populism_data_all = Populism_data %>% 
  filter(year >= 2000) %>% 
  group_by(country) %>%
  mutate(populist_lag = dplyr::lag(populist, 1),
         populist_vote_share_lag = dplyr::lag(populist_vote_share,1),
         populist_perc_seats_lag = dplyr::lag(populist_perc_seats,1),
         populist_cabinet_lag = dplyr::lag(populist_cabinet,1),
         populist_is_gov_lag = dplyr::lag(populist_is_gov,1),
         classification_lag = dplyr::lag(classification_context, 1),
         gdp_capita_sc = scale(gdp_capita),
         gdp_capita_lag = dplyr::lag(gdp_capita_sc, 1)) %>% 
  dplyr::select(country, 
                region,
                year,   
                mod_cluster_1st,
                classification_context,
                classification_lag,
                populist,
                populist_lag,
                populist_vote_share,
                populist_vote_share_lag,
                populist_perc_seats,
                populist_perc_seats_lag,
                populist_cabinet,
                populist_cabinet_lag,
                populist_prime_minister,
                populist_pres, 
                populist_is_gov,
                populist_is_gov_lag,
                mod_cluster_2nd, 
                gdp_capita,
                gdp_capita_lag)  %>%
  mutate(year_id = NA) 
table(Populism_data_all$mod_cluster_1st)
table(Populism_data_all$mod_cluster_1st, Populism_data_all$country)

# 
# countriesManyRows = WGI_RQ_data_all %>%  
#   group_by(country) %>% 
#   summarise(nrows = n()) %>% 
#   filter(nrows > 10) %>% 
#   pull(country)
# 
# WGI_RQ_data_all = WGI_RQ_data_all %>% 
#   filter(country %in% countriesManyRows)
# 
# dim(WGI_RQ_data_all)

ordered_year = unique(Populism_data_all$year)[order(unique(Populism_data_all$year))]
for (i in 1:length(unique(Populism_data_all$year))) {
  Populism_data_all$year_id[Populism_data_all$year == ordered_year[i]] = i
}


Populism_data_all$trend = Populism_data_all$year_id - median(Populism_data_all$year_id) 


Populism_data_plm_all <- pdata.frame(data.frame(Populism_data_all), index=c("country", "year_id"))


#


#### Prais-Winsten: 1st Cluster Solution
populist_vote_shareAR_mod_cluster_1st = panelAR(populist_vote_share ~ 
                                      poly(trend,2) +
                                      gdp_capita +
                                      gdp_capita_lag +
                                      mod_cluster_1st, 
                                    panelVar = "country", timeVar = "year_id", 
                                    data.frame(Populism_data_all), 
                                    rho.na.rm=T,
                                    panelCorrMethod ="pcse", 
                                    bound.rho=T,
                                    rhotype = "scorr",
                                    autoCorr='psar1')

summary(populist_vote_shareAR_mod_cluster_1st)
plot(populist_vote_shareAR_mod_cluster_1st)

### Random Effects Models

re_populist_vote_share_plm_cluster_1st <- plm::plm(populist_vote_share ~ 
                                    populist_vote_share_lag +
                                    poly(trend,2) +
                                    mod_cluster_1st, 
                                   data=Populism_data_plm_all,
                                   model ="random")
summary(re_populist_vote_share_plm_cluster_1st)

round(coeftest(re_populist_vote_share_plm_cluster_1st, .vcov=vcovBK(re_populist_vote_share_plm_cluster_1st, cluster="time")),3)



library(car)
data.frame(effect("mod_cluster_1st", re_WGI_RQ_cluster_1st)) %>% 
  mutate(mod_cluster_1st = fct_reorder(mod_cluster_1st,fit)) %>% 
  ggplot(aes(x=mod_cluster_1st, y=fit)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  theme_bw() +
  ylab("Gini-Index") +
  xlab("") +
  ggtitle("Democracy Profiles: Means-tested vs. universalistic policy (RE-Model)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle=90))



# Smaller Dataset OECD
OECD_countries = c("Belgium","Denmark","Germany","France","Greece",
                   "Ireland","Iceland","Italy","Canada","Luxembourg",
                   "Netherlands","Norway","Austria","Portugal","Sweden",
                   "Switzerland","Spain","United States of America","United Kingdom",
                   "Japan","Finland","Australia","New Zealand",
                   "Mexico", "Czech Republic", "South Korea", "Hungary", "Poland",
                   "Slovakia", "Chile", "Slovenia", "Israel", "Latvia", "Lithuania", "Estonia")

WGI_RQ_data_small = WGI_RQ_data %>% 
  filter(year > 1980) %>% 
  filter(country %in% OECD_countries) %>% 
  group_by(country) %>% 
  mutate(WGI_RQ =  WGI_RQ,
         WGI_RQ_lag = dplyr::lag(WGI_RQ, 1),
         gdp_capita_df_lag = dplyr::lag(lag_df(gdp_capita),1),
         gdp_capita_lag = dplyr::lag(gdp_capita,1),
         
         log_pop_lag = dplyr::lag(lag_df(log_pop), 1),
         age65_lag = dplyr::lag(age65,1), 
         
         gdp_export_lag = dplyr::lag(gdp_export,1), 
         
         cso_lag = dplyr::lag(cso, 1),
         
         classification_lag = dplyr::lag(classification_context, 1),
         gdp_capita_beck = dplyr::lag(gdp_capita, 1),
         age65_beck = dplyr::lag(age65, 1),
         log_pop_beck = dplyr::lag(log_pop, 1),
         
         left_right_lag = dplyr::lag(left_right,1), 
         family_name_short_lag = dplyr::lag(family_name_short,1), 
         
  ) %>%
  mutate(log_pop_lag = scale(log_pop_lag),
         age65_lag = scale(age65_lag),
         gdp_capita_lag = scale(gdp_capita_lag)) %>% 
  dplyr::select(country,
                year,
                WGI_RQ, 
                WGI_RQ_lag, 
                gdp_capita_lag, 
                age65_lag, 
                gdp_export_lag, 
                cso_lag,
                log_pop_lag,
                classification_lag,
                mod_cluster_1st,
                gdp_capita,
                age65,
                log_pop,
                gdp_capita_beck,
                age65_beck,
                log_pop_beck,
                gdp_capita_df_lag,
                left_right_lag,
                family_name_short_lag
  )  %>% 
  na.omit()  


## Exclude cases with only few gini observations

countriesManyRows = WGI_RQ_data_small %>%  
  group_by(country) %>% 
  summarise(nrows = n()) %>% 
  filter(nrows > 5) %>% 
  pull(country)

WGI_RQ_data_small = WGI_RQ_data_small %>% 
  filter(country %in% countriesManyRows) %>% 
  mutate(year_id = NA)

dim(WGI_RQ_data_small)

ordered_year = unique(WGI_RQ_data_small$year)[order(unique(WGI_RQ_data_small$year))]
for (i in 1:length(unique(WGI_RQ_data_small$year))) {
  WGI_RQ_data_small$year_id[WGI_RQ_data_small$year == ordered_year[i]] = i
}

WGI_RQ_data_small$trend = WGI_RQ_data_small$year_id - median(WGI_RQ_data_small$year_id) 

min(WGI_RQ_data_small$year)
max(WGI_RQ_data_small$year)

WGI_RQ_plm_small <- pdata.frame(data.frame(WGI_RQ_data_small), index=c("country", "year"))



WGI_RQ_plm_small %>% 
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

WGI_RQ_plm_small %>% 
  group_by(mod_cluster_1st, country) %>%
  slice(1) %>%
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

# not signficant
WGI_RQAR_mod_cluster_1st_small = panelAR(WGI_RQ ~ 
                                            poly(trend,2) + 
                                            
                                            gdp_capita_lag +
                                            log_pop_lag +
                                            age65_lag +
                                            
                                            cso_lag + 
                                            
                                            gdp_export_lag +
                                            classification_lag +
                                            left_right_lag +
                                            family_name_short_lag +
                                            
                                            mod_cluster_1st, 
                                          panelVar = "country", timeVar = "year_id", 
                                          data.frame(WGI_RQ_data_small), 
                                          rho.na.rm=T,
                                          panelCorrMethod ="pcse", 
                                          bound.rho=T,
                                          rhotype = "scorr",
                                          autoCorr='psar1')

summary(WGI_RQAR_mod_cluster_1st_small)
plot(giniAR_mod_cluster_1st_small)
round(summary(giniAR_mod_cluster_1st_small)$coefficients, 3)
giniAR_mod_cluster_1st_small$panelStructure$rho

# the more robust model is actually significant
re_WGI_RQ_cluster_1st_small <- plm::plm(WGI_RQ ~ WGI_RQ_lag + 
                                           poly(trend,2) + 
                                           
                                           gdp_capita_lag +
                                           log_pop_lag +
                                           age65_lag +
                                           
                                           cso_lag + 
                                           
                                           gdp_export_lag +
                                           classification_lag +
                                           left_right_lag +
                                           family_name_short_lag +
                                           
                                           mod_cluster_1st, 
                                         
                                         data=WGI_RQ_plm_small,
                                         model ="random",
                                         random.method = "walhus")

round(coeftest(re_WGI_RQ_cluster_1st_small, .vcov=vcovBK(re_mod_cluster_1st, cluster="time")),3)
#*summary(re_mod_cluster_1st_small)
plot(effect("mod_cluster_1st", re_WGI_RQ_cluster_1st_small))
