source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster.R")
source("Setup/Base_Functions.R")
source("Setup/MergeToFinalDataset.R")




# SOCX
SOCX_data = dmx_trade_cluster_ext %>% 
  select(country, year, year_factor, total_index_context, classification_context, 
         SOCX, region, 
         mod_cluster_1st, mod_cluster_2nd, cluster_1st,
         gdp_export, gdp_capita, cso, age65, union_density, log_pop, left_right, inflation,
         family_name_short) %>% 
  filter(year >= 1980) %>% 
  # filter(year == 1980 | year == 1985 | year == 1990  
  #        | year == 1995 | year == 2000 |   year == 2005 |  year == 2010 |   year == 2015) %>%
  group_by(country) %>% 
  filter(all(is.na(SOCX) == T) != T) %>% 
  mutate(year_id = NA) %>% 
  #filter(country != "Turkey") %>% 
  arrange(country, year)



SOCX_data$mod_cluster_1st[SOCX_data$country == "Norway"] = "fEc"
SOCX_data$mod_cluster_2nd[SOCX_data$country == "Norway"] = "fEc"

table(SOCX_data$mod_cluster_1st)

for (i in 1:length(unique(SOCX_data$year))) {
  SOCX_data$year_id[SOCX_data$year == unique(SOCX_data$year)[i]] = i
}


SOCXAR$model %>% 
  ggplot(aes(x=country, y=SOCX)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90))


SOCX_data %>%
  #mutate(SOCX_df = SOCX - dplyr::lag(SOCX, 1)) %>% 
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(SOCX, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)


SOCXAR = panelAR(SOCX ~  year_factor + 
                   union_density + 
                   gdp_export + 
                   cso + 
                   log_pop +
                   mod_cluster_1st +
                   total_index_context + 
                   gdp_capita + 
                   age65 +
                   inflation +
                   left_right +
                   family_name_short, 
               panelVar = "country", timeVar = "year_id", data.frame(SOCX_data), 
               rho.na.rm=T,
               panelCorrMethod ="pcse", 
               bound.rho=T,
               seq.times = F,
               autoCorr='ar1')
summary(SOCXAR)

# GINI
GINI_data = dmx_trade_cluster_ext %>% 
  select(country, year, year_factor,  total_index_context, 
         classification_context, family_name_short, left_right, log_pop, union_density, 
         inflation, Gini, age65, cso, gdp_export, mod_cluster_1st, mod_cluster_2nd, gdp_capita) %>% 
  group_by(country) %>% 
  filter(all(is.na(Gini) == T) != T) %>% 
  mutate(year_id = NA) %>% 
  filter(country != "Turkey") %>% 
  filter(year >= 1974) %>% 
  arrange(country, year) %>% 
  mutate(year_id = NA) 



for (i in 1:length(unique(GINI_data$year))) {
  GINI_data$year_id[GINI_data$year == unique(GINI_data$year)[i]] = i
}




GINI_data %>%
  mutate(gini_vdem_df = Gini - dplyr::lag(Gini, 1)) %>% 
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(gini_vdem_df, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)
###

giniAR = panelAR(Gini ~ year_factor +  
                   (union_density) + 
                   (gdp_export) + 
                   (cso) + 
                   (log_pop) +
                   mod_cluster_1st + 
                   (classification_context) + 
                   (gdp_capita) + 
                   (age65) +
                   (inflation) +
                   (family_name_short) + 
                   (left_right), 
               panelVar = "country", timeVar = "year_id", data.frame(GINI_data), 
               rho.na.rm=T,
               panelCorrMethod ="pcse", 
               bound.rho=T,
               autoCorr='ar1')

summary(giniAR)


my_factor_cluster = factor("fEC", levels=levels(GINI_data$mod_cluster))
my_factor_class = factor("Working Democracy", levels=levels(GINI_data$classification_context))
my_factor_year = factor(1982:2017)

newdata = data.frame(
  mod_cluster = my_factor_cluster,
  timeVar = 1982:2017,
  classification_context = my_factor_class,
  gdp_capita = mean(giniAR$model$gdp_capita),
  union_density = mean(giniAR$model$union_density),
  gdp_export = mean(giniAR$model$gdp_export),
  cso = mean(giniAR$model$cso),
  log_pop = mean(giniAR$model$log_pop),
  age65 = mean(giniAR$model$age65),
  inflation = mean(giniAR$model$inflation)
)

predict(giniAR, newdata=newdata)

predict_fEc = predict(giniAR, newdata=newdata, se.fit=T)$fit %>% 
  mutate(cluster = "fEc",
         x = as.numeric(my_factor_year))

predict_Fec = predict(giniAR, newdata=newdata, se.fit=T)$fit  %>% 
  mutate(cluster = "Fec",
         x = as.numeric(my_factor_year))

predict_fEc %>% 
  bind_rows(predict_Fec) %>% 
  ggplot(aes(x = x, y=fit, fill=cluster)) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=fit - se, ymax= fit + se), alpha=0.5)



###


RQ_data = dmx_trade_cluster_ext %>% 
  select(country, year, year_factor, classification_context, gdp_export, wgi_rq, cso, mod_cluster, gdp_capita) %>% 
  group_by(country) %>% 
  filter(all(is.na(wgi_rq) == T) != T) %>% 
  mutate(year_id = NA) %>% 
  filter(country != "Turkey") %>% 
  filter(year != 1997, year != 1999, year != 2001, year != 2017, year >= 1996) %>% 
  arrange(country, year) %>% 
  mutate(year_id = NA) 



for (i in 1:length(unique(RQ_data$year))) {
  RQ_data$year_id[RQ_data$year == unique(RQ_data$year)[i]] = i
}


RQ_data %>%
  group_by(mod_cluster, year) %>%
  summarise(variable = mean(wgi_rq, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster)) +
  geom_line(size=1.1)

###

reqAR = panelAR(wgi_rq ~ cluster_label + lag(gdp_capita) + lag(classification_context), 
                 panelVar = "country", timeVar = "year_id", data.frame(RQ_data), 
                 rho.na.rm=F,
                 panelCorrMethod ="pcse", 
                 bound.rho=T,
                 autoCorr='ar1',
                 rhotype = "scorr")

summary(reqAR)

##


edu_data = dmx_trade_cluster_ext %>% 
  select(country, year, year_factor, classification_context, gdp_export, educ_equal, cso, mod_cluster, gdp_capita) %>% 
  group_by(country) %>% 
  filter(all(is.na(educ_equal) == T) != T) %>% 
  mutate(year_id = NA) %>% 
  filter(country != "Turkey") %>% 
  filter(year > 1980) %>% 
  arrange(country, year) %>% 
  mutate(year_id = NA) 



for (i in 1:length(unique(edu_data$year))) {
  edu_data$year_id[edu_data$year == unique(edu_data$year)[i]] = i
}


edu_data %>%
  group_by(mod_cluster, year) %>%
  summarise(variable = mean(educ_equal, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster)) +
  geom_line(size=1.1)

###

eduAR = panelAR(educ_equal ~ cluster_label + year_factor + lag(gdp_capita) + lag(classification_context), 
                panelVar = "country", timeVar = "year_id", data.frame(edu_data), 
                rho.na.rm=F,
                panelCorrMethod ="pcse", 
                bound.rho=T,
                autoCorr='ar1',
                rhotype = "scorr")

summary(eduAR)



AGLData <- pdata.frame(data.frame(RQ_data), index=c("country", "year_id"))

olsmodel2 <- plm(wgi_rq ~ lag(wgi_rq) + mod_cluster + lag(gdp_capita) |  
                   lag(wgi_rq) + mod_cluster + lag(gdp_capita),
                 data=AGLData, 
                 model ="random")

summary(olsmodel2)

coeftest(olsmodel2, vcov=vcovBK)

AGLData <- pdata.frame(data.frame(SOCX_data), index=c("country", "year_id"))

SOCX_data_test = SOCX_data %>%
  group_by(country) %>% 
  mutate(SOCX_df = SOCX - dplyr::lag(SOCX, 1),
         SOCX_lag = dplyr::lag(SOCX, 1),
         gdp_capita_lag = lag(gdp_capita - dplyr::lag(gdp_capita, 1),1), 
         age65_lag = lag(age65 - dplyr::lag(age65, 1),1), 
         inflation_lag = lag(inflation - dplyr::lag(inflation, 1),1), 
         left_right_lag = dplyr::lag(left_right, 1), 
         gdp_export_lag = lag(gdp_export - dplyr::lag(gdp_export, 1),1), 
         cso_lag = dplyr::lag(cso, 1),
         family_name_short_lag = dplyr::lag(family_name_short, 1),
         log_pop_lag = dplyr::lag(log_pop, 1), 
         classification_context_lag = dplyr::lag(classification_context, 1)
         )

SOCX_data_test = GINI_data %>% 
  #filter(country != "United States of America", country != "Switzerland") %>% 
  group_by(country) %>% 
  mutate(Gini_df = (Gini - dplyr::lag(Gini, 1)),
         Gini_lag = dplyr::lag(Gini, 1),
         gdp_capita_lag = lag(gdp_capita - dplyr::lag(gdp_capita, 1),1), 
         age65_lag = lag(age65 - dplyr::lag(age65, 1),1), 
         inflation_lag = lag(inflation - dplyr::lag(inflation, 1),1), 
         left_right_lag = dplyr::lag(left_right, 1), 
         gdp_export_lag = lag(gdp_export - dplyr::lag(gdp_export, 1),1), 
         cso_lag = dplyr::lag(cso, 1),
         family_name_short_lag = dplyr::lag(family_name_short, 1),
         log_pop_lag = dplyr::lag(log_pop, 1), 
         classification_context_lag = dplyr::lag(classification_context, 1)
  )
AGLData <- pdata.frame(data.frame(SOCX_data_test), index=c("country", "year_id"))

purtest(AGLData$gdp_capita, test="hadri", exo="intercept")
purtest(AGLData$age65, test="hadri", exo="intercept")
purtest(AGLData$inflation, test="hadri", exo="intercept")
purtest(AGLData$gdp_export, test="hadri", exo="intercept")




femod_between <- plm::plm(SOCX_df ~  year_id + 
                            SOCX_lag +
                            union_density +
                            gdp_export_lag +
                            cso_lag +
                            gdp_capita_lag +
                            age65_lag + 
                            inflation_lag +
                            family_name_short_lag +
                            log_pop_lag + 
                            mod_cluster_1st,
                          data=AGLData,
                          model ="random",
                          random.method="walhus")
coeftest(femod_between, vcov=vcovBK)
summary(femod_between)
femod_between$model
ranef(femod_between)

# SOCX_data_test$mod_cluster_1st[SOCX_data_test$country == "United States of America"] = "FeC"
AGLData <- pdata.frame(data.frame(SOCX_data_test), index=c("country", "year_id"))

femod_between <- plm::plm(Gini_df ~   
                            Gini_lag +
                            union_density +
                            gdp_export_lag +
                            cso_lag +
                            gdp_capita_lag +
                            age65_lag + 
                            inflation_lag+
                            mod_cluster_1st,
                          data=AGLData,
                          model ="random",
                          random.method="walhus")
coeftest(femod_between, vcov=vcovBK)
summary(femod_between)

ranef(femod_between)

giniAR = panelAR(Gini_df ~ year_factor + 
                   Gini_lag +
                   union_density +
                   gdp_export_lag +
                   cso_lag +
                   gdp_capita_lag +
                   age65_lag + 
                   inflation_lag +
                   family_name_short_lag +
                   left_right_lag +
                   mod_cluster_1st, 
                 panelVar = "country", timeVar = "year_id", 
                 data.frame(SOCX_data_test), 
                 rho.na.rm=T,
                 panelCorrMethod ="pcse", 
                 bound.rho=T,
                 autoCorr='ar1')

summary(giniAR)


femod_between$model


phtest(femod_between, femod_with)

summary(femod_between)
ranef(femod_between)
coeftest(femod_between, vcov=vcovSCC)

coeftest(femod_with, vcov.=function(x) vcovBK(x, type="HC1", cluster = c("group")))



predict_data = femod_with$model


min(SOCX_data$year_id)

my_factor_cluster = factor("fEC", levels=levels(SOCX_data$mod_cluster))
my_factor_class = factor("Working Democracy", levels=levels(SOCX_data$classification_context))

femod_with <- plm(gini_vdem ~ lag(gini_vdem) + age65,
                  data=AGLData,
                  model="random")

summary(femod_with)
ranef(femod_with)


summary(femod_with)

newdata = data.frame(
  gini_vdem = 14,
  age65 =  2:4
)

predict_fEc = predict(femod_with, newdata = newdata)



plm::pmodel.response()
predict.plm


my_factor_year = min(SOCX_data$year_id):max(SOCX_data$year_id)


predict_fEc = predict(giniAR, newdata=newdata, se.fit=T)$fit %>% 
  mutate(cluster = "fEc",
         x = as.numeric(my_factor_year))
predict_Fec = predict(giniAR, newdata=newdata, se.fit=T)$fit  %>% 
  mutate(cluster = "Fec",
         x = as.numeric(my_factor_year))

predict_fEc %>% 
  bind_rows(predict_Fec) %>% 
  ggplot(aes(x = x, y=fit, fill=cluster)) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=fit - se, ymax= fit + se), alpha=0.5)
