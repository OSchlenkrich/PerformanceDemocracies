source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster.R")
source("Setup/Base_Functions.R")
source("Setup/MergeToFinalDataset.R")

# GINI
GINI_data = dmx_trade_cluster_ext %>% 
  dplyr::select(country, 
         region,
         year,   
         Gini = gini_vdem, 
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

# Gini_final %>%
#   group_by(country, year) %>%
#   summarise(variable = mean(Gini, na.rm=T)) %>%
#   na.omit() %>% 
#   ggplot(aes(x=year, y=variable, col=country)) +
#   geom_point(size=1.1)


GINI_data %>%
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


GINI_df_data_all = GINI_data %>% 
  filter(year >= 1950) %>% 
  group_by(country) %>% 
  mutate(Gini =  Gini,
         Gini_lag = dplyr::lag(Gini, 1),
         
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
         Gini, 
         Gini_lag, 
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


GINI_df_data_all$trend = GINI_df_data_all$year_id - median(GINI_df_data_all$year_id) 
  

unique(GINI_df_data_all$country)

# GINI_df_data_all = GINI_df_data_all %>% 
#   filter(country != "Namibia") %>% 
#   filter(country != "South Africa") %>% 
#   filter(country != "Senegal")

GINI_plm_all <- pdata.frame(data.frame(GINI_df_data_all), index=c("country", "year"))



#### Beck's "Test" for Unit Root
BeckTest = plm(gdp_capita ~ gdp_capita_beck, GINI_plm_all,
               model="pooling")
summary(BeckTest)

BeckTest = plm(log_pop ~ log_pop_beck, GINI_plm_all,
               model="pooling")
summary(BeckTest)

###
pbg = plm(Gini ~ 1, GINI_plm_all,
               model="pooling")
pbgtest(pbg)


### Heterogenity
length(na.omit(GINI_df_data_all$Gini))
GINI_df_data_all %>% 
  ggplot(aes(x=country, y=Gini)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Gini-Index per Country") +
  ylab("Gini-Index") +
  xlab("") + 
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust=0.5))


#### Descriptive Statistics
dim(GINI_df_data_all)

min(GINI_df_data_all$year)
max(GINI_df_data_all$year)
min(GINI_df_data_all$Gini)
max(GINI_df_data_all$Gini)
hist(GINI_df_data_all$Gini)

write.csv(paste(unique(GINI_df_data_all$country), collapse = ", "), "Results/countries.csv", row.names = F)

GINI_df_data_all %>% 
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

GINI_df_data_all %>% 
  group_by(mod_cluster_1st, country) %>%
  slice(1) %>%
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

GINI_df_data_all %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(Gini, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable/100, col=mod_cluster_1st)) +
  geom_smooth(size=1.1, span = 1) +
  theme_bw() +
  ggtitle("Trend in Social Inequality per Democracy Profile") +
  scale_y_continuous(label=percent) +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_color_discrete(name="Democracy Profile") +
  xlab("") +
  ylab("Gini-Index") +
  theme(plot.title = element_text(hjust=0.5))


GINI_df_data_all %>%
  group_by(region, year) %>%
  summarise(variable = mean(Gini, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable/100, col=region)) +
  geom_smooth(size=1.1, span = 1) +
  theme_bw() +
  ggtitle("Trend in Social Inequality per Region") +
  scale_y_continuous(label=percent) +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_color_discrete(name="Region") +
  xlab("") +
  ylab("Gini-Index") +
  theme(plot.title = element_text(hjust=0.5))

# Multicollinearity
M = cor(GINI_df_data_all %>% 
  ungroup() %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select_at(vars(matches("lag"), -matches("df"))))
cor.plot(M, numbers=T)


#### Prais-Winsten: 1st Cluster Solution
giniAR_mod_cluster_1st = panelAR(Gini ~ 
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
                      data.frame(GINI_df_data_all), 
                      rho.na.rm=T,
                      panelCorrMethod ="pcse", 
                      bound.rho=T,
                      rhotype = "scorr",
                      autoCorr='psar1')

summary(giniAR_mod_cluster_1st)



plot(effect("poly(trend, 2)", giniAR_mod_cluster_1st))
plot(effect("gdp_export_lag", giniAR_mod_cluster_1st))
plot(effect("cso_lag", giniAR_mod_cluster_1st))
plot(effect("classification_lag", giniAR_mod_cluster_1st))
plot(effect("mod_cluster_1st", giniAR_mod_cluster_1st))
plot(effect("poly(trend, 2):gdp_capita_lag", giniAR_mod_cluster_1st))
plot(effect("region", giniAR_mod_cluster_1st))


plot(giniAR_mod_cluster_1st)
round(summary(giniAR_mod_cluster_1st)$coefficients, 3)
giniAR_mod_cluster_1st$panelStructure$rho
results_to_excel(giniAR_mod_cluster_1st, "PW_1")


# Residuals
test = data.frame(row_nr = names(giniAR_mod_cluster_1st$residuals), 
                  resid = giniAR_mod_cluster_1st$residuals, 
                  year_id = giniAR_mod_cluster_1st$model$year_id,
                  country = giniAR_mod_cluster_1st$model$country)

test %>% 
  filter(country=="Canada") %>% 
  ggplot(aes(x=year_id, y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F)


acf(test %>% 
      filter(country=="Canada") %>% 
      pull(resid))



### Random Effects Models

re_mod_cluster_1st <- plm::plm(Gini ~ Gini_lag + 
                                 poly(trend,2) + 
                                 
                                 gdp_capita_lag +
                                 log_pop_lag +
                                 age65_lag +
                                 
                                 cso_lag + 
                                 
                                 gdp_export_lag +
                                 classification_lag +
                                 region +
                                 mod_cluster_1st, 
                          data=GINI_plm_all,
                          model ="random",
                          random.method = "walhus")

round(coeftest(re_mod_cluster_1st, .vcov=vcovBK(re_mod_cluster_1st, cluster="time")),3)

# m1 = lmer(Gini ~ Gini_lag + 
#        poly(trend,1) + 
#        
#        gdp_capita_lag +
#        log_pop_lag +
#        age65_lag +
#        
#        cso_lag + 
#        
#        gdp_export_lag +
#        classification_lag +
#        region +
#        mod_cluster_1st +
#        (1 + poly(trend,1)|country),
#      GINI_df_data_all
#      )
# summary(m1)


summary(re_mod_cluster_1st)
ranef(re_mod_cluster_1st)
results_to_excel(re_mod_cluster_1st, "RE_1", PCSE=T)

plot(effect("poly(trend, 2)", re_mod_cluster_1st))
plot(effect("region", re_mod_cluster_1st))
plot(effect("gdp_export_lag", re_mod_cluster_1st))
plot(effect("mod_cluster_1st", re_mod_cluster_1st))

data.frame(effect("gdp_export_lag", re_mod_cluster_1st))  %>% 
  ggplot(aes(x=gdp_export_lag/100, y=fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill="lightblue") +
  theme_bw() +
  ylab("Gini-Index") +
  xlab("Export (%GDP)") +
  ggtitle("Effect of Export (%GDP) on Social Inequality") +
  scale_x_continuous(breaks=seq(0, 1, .1), label=percent) +
  theme(plot.title = element_text(hjust=0.5))


data.frame(effect("poly(trend, 2)", re_mod_cluster_1st, xlevels=list(trend=seq(-40, 20,10)))) %>% 
  mutate(trend = seq(1960, 2020,10)) %>% 
  ggplot(aes(x=trend, y=fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill="lightblue") +
  theme_bw() +
  ylab("Gini-Index") +
  xlab("") +
  ggtitle("Curvilinear: Gini-Index (RE-Model)") +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  theme(plot.title = element_text(hjust=0.5))

data.frame(effect("region", re_mod_cluster_1st)) %>% 
  mutate(region = fct_reorder(region,fit)) %>% 
  ggplot(aes(x=region, y=fit)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  theme_bw() +
  ylab("Gini-Index") +
  xlab("") +
  ggtitle("Regions: Gini-Index (RE-Model)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle=90))


data.frame(effect("mod_cluster_1st", re_mod_cluster_1st)) %>% 
  mutate(mod_cluster_1st = fct_reorder(mod_cluster_1st,fit)) %>% 
  ggplot(aes(x=mod_cluster_1st, y=fit)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  theme_bw() +
  ylab("Gini-Index") +
  xlab("") +
  ggtitle("Democracy Profiles: Gini-Index (RE-Model)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle=90))


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

GINI_df_data_small = GINI_data %>% 
  filter(year > 1950) %>% 
  filter(country %in% OECD_countries) %>% 
  group_by(country) %>% 
  mutate(Gini =  Gini,
         Gini_lag = dplyr::lag(Gini, 1),
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
                Gini, 
                Gini_lag, 
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

countriesManyRows = GINI_df_data_small %>%  
  group_by(country) %>% 
  summarise(nrows = n()) %>% 
  filter(nrows > 5) %>% 
  pull(country)

GINI_df_data_small = GINI_df_data_small %>% 
  filter(country %in% countriesManyRows) %>% 
  mutate(year_id = NA)

dim(GINI_df_data_small)

ordered_year = unique(GINI_df_data_small$year)[order(unique(GINI_df_data_small$year))]
for (i in 1:length(unique(GINI_df_data_small$year))) {
  GINI_df_data_small$year_id[GINI_df_data_small$year == ordered_year[i]] = i
}

GINI_df_data_small$trend = GINI_df_data_small$year_id - median(GINI_df_data_small$year_id) 


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

# not signficant
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
                                 autoCorr='psar1')

summary(giniAR_mod_cluster_1st_small)
plot(giniAR_mod_cluster_1st_small)
round(summary(giniAR_mod_cluster_1st_small)$coefficients, 3)
giniAR_mod_cluster_1st_small$panelStructure$rho

# the more robust model is actually significant
re_mod_cluster_1st_small <- plm::plm(Gini ~ Gini_lag + 
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
                               
                               data=GINI_plm_small,
                               model ="random",
                               random.method = "walhus")

round(coeftest(re_mod_cluster_1st_small, .vcov=vcovBK(re_mod_cluster_1st, cluster="time")),3)
#*summary(re_mod_cluster_1st_small)
plot(effect("mod_cluster_1st", re_mod_cluster_1st_small))


# m2 = lmer(Gini ~ Gini_lag + 
#             poly(trend,1) + 
#             
#             gdp_capita_lag +
#             log_pop_lag +
#             age65_lag +
#             
#             cso_lag + 
#             
#             gdp_export_lag +
#             classification_lag +
#             left_right_lag +
#             family_name_short_lag +
#             
#             mod_cluster_1st +
#        (1 + poly(trend,1)|country),
#        GINI_df_data_small
#      )
# summary(m2)
