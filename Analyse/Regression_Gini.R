source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster.R")
source("Setup/Base_Functions.R")
source("Setup/MergeToFinalDataset.R")



# GINI
GINI_data = dmx_trade_cluster_ext %>% 
  select(country, year, year_factor,  SOCX, total_index_context, 
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


GINI_df_data = GINI_data %>% 
  #filter(country != "United States of America", country != "Switzerland") %>%
  group_by(country) %>% 
  mutate(Gini =  Gini *100,
         Gini_df = (Gini - dplyr::lag(Gini, 1)),
         Gini_df_lag = dplyr::lag(Gini_df, 1),
         Gini_lag = dplyr::lag(Gini, 1),
         SOCX_lag = SOCX - dplyr::lag(SOCX,1),
         gdp_capita_lag = lag(gdp_capita - dplyr::lag(gdp_capita, 1),1),
         gdp_capita_lag2 = lag(gdp_capita_lag - dplyr::lag(gdp_capita_lag, 1),1), 
         age65_lag = lag(age65 - dplyr::lag(age65, 1),1), 
         inflation_lag = lag(inflation - dplyr::lag(inflation, 1),1), 
         left_right_lag = dplyr::lag(left_right, 1), 
         gdp_export_lag = lag(gdp_export - dplyr::lag(gdp_export, 1),1), 
         cso_lag = dplyr::lag(cso, 1),
         family_name_short_lag = dplyr::lag(family_name_short, 1),
         log_pop_lag = dplyr::lag(log_pop  - dplyr::lag(log_pop, 1), 1),
         log_pop_lag2 = dplyr::lag(log_pop_lag  - dplyr::lag(log_pop_lag, 1), 1), 
         classification_context_lag = dplyr::lag(classification_context, 1)
  )
GINI_plm <- pdata.frame(data.frame(GINI_df_data), index=c("country", "year_id"))

unique(GINI_df_data$country)


purtest_function(GINI_df_data, "SOCX_lag",1)


test = GINI_df_data %>% 
  filter(country!="Canada",
         country!="Denmark",
         country!="Finland",
         country!="Germany",
         country!="United States of America") 


GINI_plm <- pdata.frame(data.frame(test), index=c("country", "year_id"))


bgtest(test$Gini ~ test$Gini_lag, order=1)




giniAR = panelAR(Gini ~ 
                   gdp_capita_lag2 +
                   log_pop_lag2 +
                   age65_lag + 
                   
                   SOCX_lag +
                   
                   cso_lag +
                   union_density +
                   
                   gdp_export_lag +
                   left_right_lag +
                   family_name_short_lag +
                   mod_cluster_1st, 
                 panelVar = "country", timeVar = "year_id", 
                 data.frame(test), 
                 rho.na.rm=T,
                 panelCorrMethod ="pcse", 
                 bound.rho=T,
                 autoCorr='ar1')
summary(giniAR)


giniAR = panelAR(Gini ~ Gini_lag +
                   as.factor(year_id) +
                   
                   gdp_capita_lag2 +
                   log_pop_lag2 +
                   age65_lag + 
                   
                   cso_lag +
                   union_density +
                   
                   gdp_export_lag +
                   left_right_lag +
                   family_name_short_lag +
                   mod_cluster_1st, 
                 panelVar = "country", timeVar = "year_id", 
                 data.frame(test), 
                 rho.na.rm=T,
                 panelCorrMethod ="pcse", 
                 bound.rho=T,
                 autoCorr='ar1')
summary(giniAR)

test = lm(Gini ~ Gini_lag, GINI_df_data)
dwtest(test)

femod_between <- plm::plm(Gini ~  Gini_lag +

                            gdp_capita_lag2 +
                            log_pop_lag2 +
                            age65_lag + 
                            
                            SOCX_lag +
                            
                            cso_lag +
                            union_density +
                            
                            gdp_export_lag +
                            left_right_lag +
                            family_name_short_lag +
                            
                            mod_cluster_1st,
                          data=GINI_plm,
                          model ="random",
                          random.method="walhus")
coeftest(femod_between, vcov=vcovBK)

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
