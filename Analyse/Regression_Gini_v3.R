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
  #filter(all(is.na(Gini) == T) != T) %>% 
  mutate(year_id = NA) %>% 
  # filter(country != "Turkey") %>% 
  filter(year >= 1974) %>% 
  arrange(country, year) %>% 
  mutate(year_id = NA) 




GINI_data %>%
  group_by(country, year) %>%
  summarise(variable = mean(Gini, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=country)) +
  geom_point(size=1.1)

# treat NAs

gapsize = GINI_data %>% 
  filter(is.na(Gini) == F) %>% 
  group_by(country) %>% 
  mutate(gapsize = year - dplyr::lag(year,1),
         year = dplyr::lag(year,1)) %>% 
  select(country, year, gapsize) %>% 
  filter(is.na(year) == F) %>% 
  mutate(flag = if_else(gapsize > 5, 1, 0))

withoutNA = GINI_data %>% 
  left_join(gapsize, by= c("country", "year")) %>% 
  filter(is.na(Gini) == F, flag == 0) %>% 
  group_by(country) %>% 
  summarise(minimum = min(year), maximum = max(year))



Gini_final = data.frame()
for (i in 1:length(unique(GINI_data$country))) {
  Gini2_bind = GINI_data %>% 
    filter(country == unique(GINI_data$country)[i]) %>% 
    filter(year >= withoutNA$minimum[i], year <= withoutNA$maximum[i])
  
  Gini_final = bind_rows(Gini_final, Gini2_bind)
}

# test = Gini_final %>% 
#   select(country, Gini) %>% 
#   group_by(country) %>%
#   na.omit() %>% 
#   summarise(n())


Gini_final = Gini_final %>%
  group_by(country) %>%
  mutate(Gini_imp = na.interpolation(Gini)) %>% 
  filter(year >= 1985) %>%  # canada is only country with values of gini before 1985
  arrange(year, country)

Gini_final = GINI_data
for (i in 1:length(unique(Gini_final$year))) {
  Gini_final$year_id[Gini_final$year == unique(Gini_final$year)[i]] = i
}

#### Some Plots

Gini_final %>%
  group_by(country, year) %>%
  summarise(variable = mean(Gini_imp, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=country)) +
  geom_point(size=1.1)


Gini_final %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(Gini, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)

####

lag_percentage = function(x1) {
  x1_df = x1 - dplyr::lag(x1, 1)
  return(x1_df)
}

GINI_df_data = Gini_final %>% 
  group_by(country) %>% 
  mutate(Gini =  Gini,
         #Gini_imp =  Gini_imp  * 100,
         
         Gini_df = (Gini - dplyr::lag(Gini, 1)),
         Gini_df_lag = dplyr::lag(Gini_df, 1),
         
         Gini_lag = dplyr::lag(Gini, 1),
         #Gini_imp_lag = dplyr::lag(Gini_imp, 1),
         
         SOCX_lag = dplyr::lag(lag_percentage(SOCX),1),

         gdp_capita_lag = dplyr::lag(gdp_capita,1),

         age65_lag = dplyr::lag(lag_percentage(age65),1), 

         left_right_lag = dplyr::lag(left_right, 1), 
         
         gdp_export_lag = dplyr::lag(gdp_export,1), 
         
         cso_lag = dplyr::lag(cso, 1),
         
         family_name_short_lag = dplyr::lag(family_name_short, 1),
         
         log_pop_lag = dplyr::lag(lag_percentage(log_pop), 1),
         
  )

GINI_df_data_mean = GINI_df_data %>% 
  group_by(country) %>% 
  summarize(mean_SOCX = mean(SOCX_lag, na.rm=T),
            mean_gdp_capita = mean(gdp_capita_lag, na.rm=T),
            mean_economy = mean(gdp_capita, na.rm=T),
            
            mean_age65 = mean(age65_lag, na.rm=T),
            mean_log_pop = mean(log_pop_lag, na.rm=T),
            mean_cso = mean(cso_lag, na.rm=T),
            mean_gdp_export = mean(gdp_export_lag, na.rm=T),
            mean_left_right = mean(left_right_lag, na.rm=T)) %>% 
  right_join(GINI_df_data, by="country") %>% 
  mutate(
    SOCX_w = SOCX_lag - mean_SOCX,
    gdp_capita_w = gdp_capita_lag - mean_gdp_capita,
    age65_w = age65_lag - mean_age65,
    log_pop_w = log_pop_lag - mean_log_pop,
    cso_w = cso_lag - mean_cso,
    gdp_export_w = gdp_export_lag - mean_gdp_export,
    left_right_w = left_right_lag - mean_left_right,
  ) %>% 
  mutate_at(vars(ends_with("_w")), scale) %>% 
  mutate_at(vars(starts_with("mean")), scale)


GINI_plm_all <- pdata.frame(data.frame(GINI_df_data_mean), index=c("country", "year_id"))




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

min(GINI_df_data_mean$year)
max(GINI_df_data_mean$year)

GINI_df_data_mean %>% 
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

GINI_df_data_mean %>% 
  group_by(mod_cluster_1st, country) %>%
  slice(1) %>%
  group_by(mod_cluster_1st) %>% 
  summarise(number = n())

GINI_df_data_mean %>% 
  group_by(mod_cluster_2nd) %>% 
  summarise(number = n())

GINI_df_data_mean %>% 
  group_by(mod_cluster_2nd, country) %>%
  slice(1) %>%
  group_by(mod_cluster_2nd) %>% 
  summarise(number = n())

#### Prais-Winsten: 1st Cluster Solution
giniAR_mod_cluster_1st = panelAR(Gini ~ 
                                   year_id +
                                   
                                   gdp_capita_lag +
                                   
                                   cso_lag + 
                                   
                                   gdp_export_lag +
                                   
                                   mod_cluster_1st*year_id, 
                      panelVar = "country", timeVar = "year_id", 
                      data.frame(GINI_df_data_mean), 
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

dwtest(giniAR_mod_cluster_1st)
acf(giniAR_mod_cluster_1st$residuals)

giniAR_mod_cluster_2nd = panelAR(Gini ~  
                                   poly(year_id,2) +
                                   
                                   gdp_capita_lag +
                                   
                                   cso_lag + 
                                   
                                   gdp_export_lag +
                   mod_cluster_1st, 
                   
                 panelVar = "country", timeVar = "year_id", 
                 data.frame(GINI_df_data_mean), 
                 rho.na.rm=T,
                 panelCorrMethod ="pcse", 
                 bound.rho=T,
                 rhotype = "scorr",
                 autoCorr='ar1')

summary(giniAR_mod_cluster_2nd)
round(summary(giniAR_mod_cluster_2nd)$coefficients, 3)
giniAR_mod_cluster_2nd$panelStructure$rho
results_to_excel(giniAR_mod_cluster_2nd, "PW_2")



# Residuals
test = data.frame(row_nr = names(giniAR_mod_cluster_2nd$residuals), 
                  resid = giniAR_mod_cluster_2nd$residuals, 
                  year_id = giniAR_mod_cluster_2nd$model$year_id,
                  country = giniAR_mod_cluster_2nd$model$country)

test %>% 
  filter(country=="Germany") %>% 
  ggplot(aes(x=year_id, y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F)

qqnorm(giniAR_mod_cluster_1st$residuals)
qqline(giniAR_mod_cluster_1st$residuals)

### Random Effects Models


re_mod_cluster_1st <- plm::plm(Gini ~  
                                 
                            as.numeric(year_id)  +
                            mod_cluster_1st*as.numeric(year_id), 
                          data=GINI_plm_all,
                          model ="random",
                          random.method = "walhus")

round(coeftest(re_mod_cluster_1st, vcov=vcovBK),3)
summary(re_mod_cluster_1st)
fixef(re_mod_cluster_1st)
results_to_excel(re_mod_cluster_1st, "RE_1")

library(lme4)
library(lmerTest)


GINI_df_data_mean$year_id = GINI_df_data_mean$year_id - median(GINI_df_data_mean$year_id)
test = lmer(Gini ~  Gini_lag +
                 year_id +
                 
                 gdp_capita_lag +

                 
                 cso_lag + 

                 gdp_export_lag +
                 classification_context +
                 
                 mod_cluster_1st*year_id+ 
                 (1|country),
                  data=GINI_df_data_mean, REML=T)

summary(test)
confint(test)
library(effects)
plot(effect(term="year_id:mod_cluster_1st", test))
library(nlme)

test = lme(Gini ~  
              year_id +
              
              gdp_capita_lag +
              
              
              cso_lag + 
              
              gdp_export_lag +
              classification_context +
              
              mod_cluster_1st*year_id, 
           random =  ~ 1|country,
            data=GINI_df_data_mean,
           na.action=na.omit,
           correlation = corCAR1(form = ~year_id | country))

summary(test)
acf(test$residuals)
plot(effect(term="year_id:mod_cluster_1st", test))


re_mod_cluster_2nd <- plm::plm(Gini ~  Gini_lag +
                                 poly(as.numeric(year_id),1) +

                                 gdp_capita_lag +

                                 cso_lag + 
                                 
                                 gdp_export_lag +
                                 classification_context +
                                 
                                 mod_cluster_2nd*poly(as.numeric(year_id),1),
                               
                               data=GINI_plm_all,
                               
                               model ="random",
                               random.method = "walhus")


# Residuals
test = data.frame(row_nr = names(re_mod_cluster_2nd$residuals), 
                  resid = as.numeric(re_mod_cluster_2nd$residuals), 
                  index(re_mod_cluster_2nd)) %>% 
  mutate(resid = scale(resid))

test %>% 
  filter(country=="United States of America") %>% 
  ggplot(aes(x=as.numeric(year_id), y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F)


acf(re_mod_cluster_2nd$residuals)

round(coeftest(re_mod_cluster_2nd, vcov=vcovBK),3)
summary(re_mod_cluster_2nd)
ranef(re_mod_cluster_2nd)
results_to_excel(re_mod_cluster_2nd, "RE_2")


#####
qqnorm(residuals(femod_between), ylab = 'Residuals')
qqline(residuals(femod_between))


X <- model.matrix(femod_between, model = "pooling")
y <- pmodel.response(femod_between, model = "pooling")
est <-  X %*% coef(femod_between)
res <- y - est

phtest(femod_between, femod_between_w)


plot(effect(mod=femod_between, term="log_pop_lag2:mod_cluster_1st", x.var="log_pop_lag2"))

phtest(femod_between, femod_between, vcov=vcovBK)

plot(femod_between$residuals)
test = data.frame(row_nr = names(femod_between$residuals), index(femod_between), 
                  resid = femod_between$residuals)

femod_between$residuals

qqnorm(scale(residuals(giniAR_year)), ylab = 'Residuals')
qqline(scale(residuals(giniAR_year)))

plot(preds, residuals(m3))


test %>% 
  ggplot(aes(x=year_id, y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F)

test %>% 
  ggplot(aes(x=year_id, y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F)



round(coef(femod_between),3)

summary(femod_between)
plm::ranef(femod_between)

pbgtest(femod_between)
pcdtest(femod_between)


test = GINI_df_data %>% 
  filter(year == 2015)


m1 = lm(Gini ~ mod_cluster_1st + 
          union_density +
          gdp_export +
          cso +
          gdp_capita +
          age65 + 
          inflation +
          family_name_short +
          left_right, 
        test)
summary(m1)
