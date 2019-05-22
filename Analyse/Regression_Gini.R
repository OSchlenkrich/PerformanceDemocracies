source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster.R")
source("Setup/Base_Functions.R")
source("Setup/MergeToFinalDataset.R")

# GINI
GINI_data = dmx_trade_cluster_ext %>% 
  select(country, year, year_factor,  Gini, SOCX, total_index_context, 
         classification_context, family_name_short, left_right, log_pop, union_density, 
         inflation, age65, cso, gdp_export, mod_cluster_1st, mod_cluster_2nd, gdp_capita) %>% 
  group_by(country) %>% 
  filter(all(is.na(Gini) == T) != T) %>% 
  mutate(year_id = NA) %>% 
  filter(country != "Turkey") %>% 
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

withoutNA = GINI_data %>% 
  filter(is.na(Gini) == F) %>% 
  group_by(country) %>% 
  summarise(minimum = min(year), maximum = max(year))

Gini_final = data.frame()
for (i in 1:length(unique(GINI_data$country))) {
  Gini2_bind = GINI_data %>% 
    filter(country == unique(GINI_data$country)[i]) %>% 
    filter(year >= withoutNA$minimum[i], year <= withoutNA$maximum[i])
  
  Gini_final = bind_rows(Gini_final, Gini2_bind)
}

Gini_final = Gini_final %>% 
  group_by(country) %>% 
  mutate(Gini_imp = na.interpolation(Gini))


for (i in 1:length(unique(Gini_final$year))) {
  Gini_final$year_id[Gini_final$year == unique(Gini_final$year)[i]] = i
}

Gini_final %>%
  group_by(country, year) %>%
  summarise(variable = mean(Gini_imp, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=country)) +
  geom_point(size=1.1)



year()Gini_final %>%
  group_by(mod_cluster_1st, year) %>%
  summarise(variable = mean(Gini, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=mod_cluster_1st)) +
  geom_line(size=1.1)



GINI_df_data = Gini_final %>% 
  #filter(country != "United States of America", country != "Switzerland") %>%
  group_by(country) %>% 
  mutate(Gini =  Gini *100,
         Gini_imp =  Gini_imp *100,
         
         Gini_df = (Gini - dplyr::lag(Gini, 1)),
         Gini_df_lag = dplyr::lag(Gini_df, 1),
         Gini_lag = dplyr::lag(Gini, 1),
         Gini_imp_lag = dplyr::lag(Gini_imp, 1),
         
         SOCX_lag = SOCX - dplyr::lag(SOCX,1),
         SOCX_tr = dplyr::lag(detrending(SOCX, year_id, "SOCX"),1),
         
         gdp_capita_lag = lag(gdp_capita - dplyr::lag(gdp_capita, 1),1),
         gdp_capita_lag2 = lag(gdp_capita_lag - dplyr::lag(gdp_capita_lag, 1),1),
         gdp_capita_tr = dplyr::lag(detrending(gdp_capita, year_id, "gdp_capita"),1),
         
         age65_lag = lag(age65 - dplyr::lag(age65, 1),1), 
         age65_lag2 = lag(age65_lag - dplyr::lag(age65_lag, 1),1),
         age65_tr = dplyr::lag(detrending(age65, year_id, "age65"),1),
         
         inflation_lag = lag(inflation - dplyr::lag(inflation, 1),1), 
         left_right_lag = dplyr::lag(left_right, 1), 
         
         gdp_export_lag = lag(gdp_export - dplyr::lag(gdp_export, 1),1), 
         gdp_export_tr = dplyr::lag(detrending(gdp_export, year_id, "gdp_export"),1),
         
         
         cso_lag = lag(cso - dplyr::lag(cso, 1), 1),
         family_name_short_lag = dplyr::lag(family_name_short, 1),
         log_pop_lag = dplyr::lag(log_pop  - dplyr::lag(log_pop, 1), 1),
         log_pop_lag2 = dplyr::lag(log_pop_lag  - dplyr::lag(log_pop_lag, 1), 1), 
         log_pop_tr = dplyr::lag(detrending(log_pop, year_id, "log_pop"),1),
         
         classification_context_lag = dplyr::lag(classification_context, 1)
  )

GINI_df_data_mean = GINI_df_data %>% 
  group_by(country) %>% 
  summarize(mean_SOCX = mean(SOCX, na.rm=T),
            mean_gdp_capita = mean(gdp_capita, na.rm=T),
            mean_age65 = mean(age65, na.rm=T),
            mean_log_pop = mean(log_pop, na.rm=T),
            mean_cso = mean(cso, na.rm=T),
            mean_gdp_export = mean(gdp_export, na.rm=T),
            mean_left_right = mean(left_right, na.rm=T)) %>% 
  right_join(GINI_df_data, by="country")


GINI_plm_all <- pdata.frame(data.frame(GINI_df_data_mean), index=c("country", "year_id"))


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



purtest(GINI_plm_all$Gini_imp_lag, test="hadri", exo="trend")
purtest_function(GINI_df_data, "Gini",2)


reduced_Gini_dataset = GINI_df_data_mean %>% 
  filter(country!="Canada",
         country!="Denmark",
         country!="Finland",
         country!="Germany",
         country!="United States of America"
         ) 

reduced_Gini_dataset %>% 
  select(country, Gini) %>% 
  na.omit() %>% 
  group_by(country) %>% 
  summarise(n())


GINI_plm <- pdata.frame(data.frame(reduced_Gini_dataset), index=c("country", "year_id"))

purtest(GINI_plm_all$gdp_capita, test="hadri", exo="trend", lags=0)




GINI_plm %>% 
  group_by(year) %>% 
  summarise(y=mean(Gini, na.rm=T)) %>% 
  ggplot(aes(y=y, x=year)) +
  geom_line()


GINI_df_data %>%
  group_by(country, year) %>%
  summarise(variable = mean(SOCX_tr, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=country)) +
  geom_line(size=1.1) 

giniAR = panelAR(Gini ~ 
                   log_pop_tr +
                   age65_tr + 
                   
                   SOCX_tr +
                   
                   cso_lag +
                   union_density +
                   
                   gdp_export_tr +
                   left_right_lag +
                   family_name_short_lag +
                   mod_cluster_1st, 
                 panelVar = "country", timeVar = "year_id", 
                 data.frame(reduced_Gini_dataset), 
                 rho.na.rm=T,
                 panelCorrMethod ="pcse", 
                 bound.rho=T,
                 autoCorr='ar1')
summary(giniAR)


giniAR_year = panelAR(Gini_imp ~ 
                        year_id +
                   gdp_capita_lag +
                    mean_gdp_capita +
                     
                   log_pop_lag +
                    mean_log_pop +
                     
                   age65_lag + 
                   mean_age65 +
                   
                   SOCX_lag +
                    mean_SOCX +
                   
                   cso_lag +
                    mean_cso +
                   union_density +
                   
                   gdp_export_lag +
                     mean_gdp_export + 
                   left_right_lag +
                  mean_left_right + 
                   mod_cluster_1st, 
                   
                 panelVar = "country", timeVar = "year_id", 
                 data.frame(GINI_df_data_mean), 
                 rho.na.rm=T,
                 panelCorrMethod ="pcse", 
                 bound.rho=T,
                 rhotype = "scorr",
                 autoCorr='ar1')
summary(giniAR_year)


# Residuals
test = data.frame(row_nr = names(giniAR_year$residuals), resid = giniAR_year$residuals, 
                  year_id = giniAR_year$model$year_id,
                  country = giniAR_year$model$country)

test %>% 
  ggplot(aes(x=year_id, y=resid, col=country)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5,se=F)



femod_between <- plm::plm(Gini_imp ~  Gini_imp_lag +
                            
                            year_id +
                            
                            gdp_capita_lag +
                            mean_gdp_capita +
                            

                            age65_lag + 
                            mean_age65 +

                            SOCX_lag + 
                            mean_SOCX +
                            
                            cso_lag + 
                            mean_cso +
                            
                            gdp_export_lag +
                            mean_gdp_export +
                            
                            left_right_lag +
                            mean_left_right +
                            
                            mod_cluster_1st,
                          
                          data=GINI_plm_all,
                          model ="random",
                          random.method = "walhus")

coeftest(femod_between, vcov=vcovBK)
summary(femod_between)
ranef(femod_between)


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
