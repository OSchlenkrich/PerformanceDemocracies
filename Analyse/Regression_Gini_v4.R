source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster.R")
source("Setup/Base_Functions.R")
source("Setup/MergeToFinalDataset.R")

# GINI
GINI_data = dmx_trade_cluster_ext %>% 
  select(country, 
         year,   
         Gini = gini_vdem, 
         mod_cluster_1st,
         SOCX,  
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
  filter(year >= 1950) %>% 
  arrange(country, year) %>% 
  mutate(year_id = NA) 



Gini_final = GINI_data
for (i in 1:length(unique(Gini_final$year))) {
  Gini_final$year_id[Gini_final$year == unique(Gini_final$year)[i]] = i
}

#### Some Plots

# Gini_final %>%
#   group_by(country, year) %>%
#   summarise(variable = mean(Gini, na.rm=T)) %>%
#   na.omit() %>% 
#   ggplot(aes(x=year, y=variable, col=country)) +
#   geom_point(size=1.1)


Gini_final %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(Gini, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)


####

lag_df = function(x1) {
  x1_df = x1 - dplyr::lag(x1, 1)
  return(x1_df)
}


GINI_df_data_all = Gini_final %>% 
  group_by(country) %>% 
  mutate(Gini =  Gini,
         Gini_lag = dplyr::lag(Gini, 1),
         gdp_capita_lag = dplyr::lag(lag_df(gdp_capita),1),

         age65_lag = dplyr::lag(lag_df(age65),1), 
         gdp_export_lag = dplyr::lag(gdp_export,1), 
         cso_lag = dplyr::lag(cso, 1),
         log_pop_lag = dplyr::lag(lag_df(log_pop), 1),
         classification_lag = dplyr::lag(classification_context, 1),
         total_index_context_lag = dplyr::lag(total_index_context, 1)) %>%
  mutate(log_pop_lag = scale(log_pop_lag),
         age65_lag = scale(age65_lag),
         gdp_capita_lag = scale(gdp_capita_lag)) %>% 
  select(country,
         year,
         year_id, 
         Gini, 
         Gini_lag, 
         gdp_capita_lag, 
         age65_lag, 
         gdp_export_lag, 
         cso_lag,
         log_pop_lag,
         classification_lag,
         mod_cluster_1st
         )  %>%
  mutate(trend = year_id - median(year_id)) %>% 
  na.omit()


## Exclude cases with only few gini observations

countriesManyRows = GINI_df_data_all %>%  
  group_by(country) %>% 
  summarise(nrows = n()) %>% 
  filter(nrows > 10) %>% 
  pull(country)

GINI_df_data_all = GINI_df_data_all %>% 
  filter(country %in% countriesManyRows)

dim(GINI_df_data_all)

ordered_year = unique(GINI_df_data_all$year)[order(unique(GINI_df_data_all$year))]
for (i in 1:length(unique(GINI_df_data_all$year))) {
  GINI_df_data_all$year_id[GINI_df_data_all$year == ordered_year[i]] = i
}


GINI_plm_all <- pdata.frame(data.frame(GINI_df_data_all), index=c("country", "year"))


#### Beck's Test for Unit Root
BeckTest = plm(Gini_imp ~ Gini_imp_lag, GINI_plm_all,
               model="pooling")
summary(BeckTest)

demo_resid = data.frame(index(BeckTest), dResid = BeckTest$residuals) %>% 
  group_by(country) %>% 
  mutate(lagdResid = dplyr::lag(dResid, 1)) %>% 
  as.data.frame() %>% 
  pdata.frame(., index=c("country", "year_id") )

rBeckTest = plm(dResid ~ lagdResid, demo_resid,
                model="pooling")
summary(rBeckTest)

#### Descriptive Statistics

min(GINI_df_data_all$year)
max(GINI_df_data_all$year)
min(GINI_df_data_all$Gini)
max(GINI_df_data_all$Gini)
hist(GINI_df_data_all$Gini)
hist(GINI_df_data_all$total_index_context_lag)

unique(GINI_df_data_all$country)

GINI_df_data_all %>% 
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

GINI_df_data_all %>% 
  group_by(mod_cluster_1st, country) %>%
  slice(1) %>%
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())


#### Prais-Winsten: 1st Cluster Solution
giniAR_mod_cluster_1st = panelAR(Gini ~ 
                                   poly(trend,3) +
                                   
                                   gdp_capita_lag +
                                   log_pop_lag +
                                   age65_lag +
                                   
                                   cso_lag + 
                                   classification_lag +
                                   gdp_export_lag +
                                   mod_cluster_1st, 
                      panelVar = "country", timeVar = "year_id", 
                      data.frame(GINI_df_data_all), 
                      rho.na.rm=T,
                      panelCorrMethod ="pcse", 
                      bound.rho=T,
                      rhotype = "scorr",
                      autoCorr='ar1')

summary(giniAR_mod_cluster_1st)
plot(giniAR_mod_cluster_1st)
round(summary(giniAR_mod_cluster_1st)$coefficients, 3)
giniAR_mod_cluster_1st$panelStructure$rho
results_to_excel(giniAR_mod_cluster_1st, "PW_1")

acf(giniAR_mod_cluster_1st$residuals)



# Residuals
test = data.frame(row_nr = names(giniAR_mod_cluster_1st$residuals), 
                  resid = giniAR_mod_cluster_1st$residuals, 
                  year_id = giniAR_mod_cluster_1st$model$year_id,
                  country = giniAR_mod_cluster_1st$model$country)

test %>% 
  filter(country=="Germany") %>% 
  ggplot(aes(x=year_id, y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F)


### Random Effects Models

re_mod_cluster_1st <- plm::plm(Gini ~ Gini_lag + 
                                 trend + 
                                 
                                 gdp_capita_lag +
                                 log_pop_lag +
                                 age65_lag +
                                 
                                 cso_lag + 
                                 
                                 gdp_export_lag +
                                 classification_lag +
                                 mod_cluster_1st, 
                          data=GINI_plm_all,
                          model ="random",
                          random.method = "walhus")

round(coeftest(re_mod_cluster_1st, vcov=vcovBK),3)
summary(re_mod_cluster_1st)
ranef(re_mod_cluster_1st)
results_to_excel(re_mod_cluster_1st, "RE_1")


# Residuals
re_mod_cluster_1st_res = data.frame(row_nr = names(re_mod_cluster_1st$residuals), 
                  resid = as.numeric(re_mod_cluster_1st$residuals), 
                  index(re_mod_cluster_1st)) %>% 
  mutate(resid = scale(resid))

re_mod_cluster_1st_res %>% 
  filter(country==sample(country, 3)) %>% 
  ggplot(aes(x=as.numeric(year), y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F, na.rm=F) +
  theme_bw() +
  xlab("Year") +
  ylim(-3,3) +
  ggtitle("Residual Plot (Loess)")

acf(re_mod_cluster_1st$residuals)

# Smaller Dataset OECD
OECD_countries = c("Belgium","Denmark","Germany","France","Greece",
                   "Ireland","Iceland","Italy","Canada","Luxembourg",
                   "Netherlands","Norway","Austria","Portugal","Sweden",
                   "Switzerland","Spain","United States of America","United Kingdom",
                   "Japan","Finland","Australia","New Zealand",
                   "Mexico", "Czech Republic", "South Korea", "Hungary", "Poland",
                   "Slovakia", "Chile", "Slovenia", "Israel", "Latvia", "Lithuania", "Estonia")


GINI_df_data_small = Gini_final %>% 
  filter(year > 1950) %>% 
  filter(country %in% OECD_countries) %>% 
  group_by(country) %>% 
  mutate(Gini =  Gini,
         Gini_lag = dplyr::lag(Gini, 1),
         gdp_capita_lag = dplyr::lag(lag_df(gdp_capita),1),
         
         SOCX_lag = dplyr::lag(SOCX,1), 
         age65_lag = dplyr::lag(lag_df(age65),1), 
         
         gdp_export_lag = dplyr::lag(gdp_export,1), 
         cso_lag = dplyr::lag(cso, 1),
         log_pop_lag = dplyr::lag(lag_df(log_pop), 1),
         classification_lag = dplyr::lag(classification_context, 1),
         
         left_right_lag = dplyr::lag(left_right,1), 
         family_name_short_lag = dplyr::lag(family_name_short,1), 
         
  ) %>%
  mutate(log_pop_lag = scale(log_pop_lag),
         age65_lag = scale(age65_lag),
         gdp_capita_lag = scale(gdp_capita_lag)) %>% 
  select(country,
         year,
         year_id, 
         Gini, 
         Gini_lag, 
         gdp_capita_lag, 
         age65_lag, 
         gdp_export_lag, 
         cso_lag,
         log_pop_lag,
         classification_lag,
         mod_cluster_1st,
         left_right_lag,
         family_name_short_lag
  )  %>%
  mutate(trend = year_id - median(year_id)) %>% 
  na.omit()


## Exclude cases with only few gini observations

countriesManyRows = GINI_df_data_small %>%  
  group_by(country) %>% 
  summarise(nrows = n()) %>% 
  filter(nrows > 5) %>% 
  pull(country)

GINI_df_data_small = GINI_df_data_small %>% 
  filter(country %in% countriesManyRows)

dim(GINI_df_data_small)

ordered_year = unique(GINI_df_data_small$year)[order(unique(GINI_df_data_small$year))]
for (i in 1:length(unique(GINI_df_data_small$year))) {
  GINI_df_data_small$year_id[GINI_df_data_small$year == ordered_year[i]] = i
}

GINI_plm_small <- pdata.frame(data.frame(GINI_df_data_small), index=c("country", "year"))


min(GINI_df_data_small$year)
max(GINI_df_data_small$year)

GINI_df_data_small %>% 
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

GINI_df_data_small %>% 
  group_by(mod_cluster_1st, country) %>%
  slice(1) %>%
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())


giniAR_mod_cluster_1st_small = panelAR(Gini ~ 
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
                                 data.frame(GINI_df_data_small), 
                                 rho.na.rm=T,
                                 panelCorrMethod ="pcse", 
                                 bound.rho=T,
                                 rhotype = "scorr",
                                 autoCorr='ar1')

summary(giniAR_mod_cluster_1st_small)
plot(giniAR_mod_cluster_1st_small)
round(summary(giniAR_mod_cluster_1st_small)$coefficients, 3)
giniAR_mod_cluster_1st_small$panelStructure$rho


re_mod_cluster_1st_small <- plm::plm(Gini ~ Gini_lag + 
                                 poly(trend,1) + 
                                 
                                 gdp_capita_lag +
                                 log_pop_lag +
                                 age65_lag +
                                 
                                 cso_lag + 

                                 gdp_export_lag +
                                 classification_lag +
                                   left_right_lag +
                                   family_name_short_lag +

                                 mod_cluster_1st, 
                               
                               data=GINI_plm_small,
                               model ="random",
                               random.method = "walhus")

round(coeftest(re_mod_cluster_1st_small, vcov=vcovBK),3)
#*summary(re_mod_cluster_1st_small)
plot(effect("left_right_lag:mod_cluster_1st", re_mod_cluster_1st_small))
