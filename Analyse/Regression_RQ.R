source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster.R")
source("Setup/Base_Functions.R")
source("Setup/MergeToFinalDataset.R")

# WGI_RQ

WGI_RQ_data = dmx_trade_cluster_ext %>% 
  dplyr::select(country, 
                region,
                year,   
                WGI_RQ, 
                mod_cluster_1st,
                classification_context, 
                family_name_short, 
                left_right, 
                log_pop, 
                union_density, 
                inflation, 
                age65, 
                cso, 
                gdp_export, 
                mod_cluster_2nd, 
                gdp_capita) %>% 
  group_by(country) %>% 
  arrange(country, year)



#### Some Plots


WGI_RQ_data %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(WGI_RQ, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)


####

lag_df = function(x1) {
  x1_df = x1 - dplyr::lag(x1, 1)
  return(x1_df)
}


WGI_RQ_data_all = WGI_RQ_data %>% 
  filter(year >= 1980) %>% 
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
         
  ) %>%
  mutate(log_pop_lag = scale(log_pop_lag),
         gdp_capita_lag = scale(gdp_capita_lag)) %>% 
  dplyr::select(country,
                region,
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
                gdp_capita_df_lag)  %>%
  na.omit() %>% 
  mutate(year_id = NA) 



countriesManyRows = WGI_RQ_data_all %>%  
  group_by(country) %>% 
  summarise(nrows = n()) %>% 
  filter(nrows > 10) %>% 
  pull(country)

WGI_RQ_data_all = WGI_RQ_data_all %>% 
  filter(country %in% countriesManyRows)

dim(WGI_RQ_data_all)

ordered_year = unique(WGI_RQ_data_all$year)[order(unique(WGI_RQ_data_all$year))]
for (i in 1:length(unique(WGI_RQ_data_all$year))) {
  WGI_RQ_data_all$year_id[WGI_RQ_data_all$year == ordered_year[i]] = i
}


WGI_RQ_data_all$trend = WGI_RQ_data_all$year_id - median(WGI_RQ_data_all$year_id) 


unique(WGI_RQ_data_all$country)

WGI_RQ_plm_all <- pdata.frame(data.frame(WGI_RQ_data_all), index=c("country", "year"))


#


#### Prais-Winsten: 1st Cluster Solution
WGI_RQAR_mod_cluster_1st = panelAR(WGI_RQ ~ 
                                      poly(trend,2) +
                                      
                                      gdp_capita_lag +
                                      log_pop_lag +
                                      age65_lag +
                                      
                                      cso_lag + 
                                      gdp_export_lag +
                                      classification_lag +
                                      region +
                                      mod_cluster_1st, 
                                    panelVar = "country", timeVar = "year_id", 
                                    data.frame(WGI_RQ_data_all), 
                                    rho.na.rm=T,
                                    panelCorrMethod ="pcse", 
                                    bound.rho=T,
                                    rhotype = "scorr",
                                    autoCorr='psar1')

summary(WGI_RQAR_mod_cluster_1st)

plot(WGI_RQAR_mod_cluster_1st)
### Random Effects Models

re_WGI_RQ_cluster_1st <- plm::plm(WGI_RQ ~ WGI_RQ_lag + 
                                     poly(trend,2) + 
                                     
                                     gdp_capita_lag +
                                     log_pop_lag +
                                     age65_lag +
                                     
                                     cso_lag + 
                                     
                                     gdp_export_lag +
                                     classification_lag +
                                     region +
                                     mod_cluster_1st, 
                                   data=WGI_RQ_plm_all,
                                   model ="random",
                                   random.method = "walhus")
summary(re_WGI_RQ_cluster_1st)
round(coeftest(re_WGI_RQ_cluster_1st, .vcov=vcovBK(re_WGI_RQ_cluster_1st, cluster="time")),3)

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
