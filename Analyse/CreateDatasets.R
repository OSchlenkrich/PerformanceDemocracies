source("Setup/Packages.R")
source("Setup/Base_Functions.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")

clustersetup = F
setup = T
Plot_Impu = F


#### Create All Datasets ########
# source("Analyse/Cluster/Cluster_v5.R")
# 
# source("Analyse/Economy/eco_desc.R")
# source("Analyse/Environment/env_desc.R")
# source("Analyse/GoalA/AR_variables.R")
# source("Analyse/Social/soc_desc.R")
# source("Analyse/DomesticSecurity/ds_desc.R")
# source("Analyse/Confidence/conf_desc.R")


# Combine all Datasets ####

# performance_all = V_dem %>% 
#   select(country, country_text_id, year) %>%  
#   filter(year >= 1950) %>% 
#   filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
#   left_join(Economy_Performance_final %>% 
#               select(country_text_id, year, eco_oecd_index, eco_wdi_index), by=c("country_text_id", "year")) %>%  
#   left_join(Environment_Performance_final %>% 
#               select(country_text_id, year, environment_oecd_index, environment_wdi_index), by=c("country_text_id", "year")) %>% 
#   left_join(AR_final %>% 
#               select(country_text_id, year,  GA_ccp_index, GA_lutz_index), by=c("country_text_id", "year")) %>% 
#   left_join(social_final %>% 
#               select(country_text_id, year, soc_index), by=c("country_text_id", "year")) %>% 
#   left_join(domestic_security_final %>% 
#               select(country_text_id, year,  ds_life_index, ds_order_index), by=c("country_text_id", "year")) %>% 
#   left_join(confidence_final_agg %>% 
#               select(country_text_id, year,  conf_index), by=c("country_text_id", "year"))  



# Write Datasets

# write.csv(confidence_final, file="Datasets/performance_data/confidence_individual.csv", row.names = F, fileEncoding ="UTF-8")
# write.csv(performance_all, file="Datasets/performance_data/performance_all.csv", row.names = F, fileEncoding ="UTF-8")
# write.csv(dmx_trade_cluster, file="Datasets/performance_data/dmx_trade_cluster_v5.csv", row.names = F, fileEncoding ="UTF-8")


#### LOAD DATASETS #######
dmx_trade_cluster = fread(file="Datasets/performance_data/dmx_trade_cluster_v5.csv", encoding = "UTF-8") %>% 
  mutate(cluster_label_1st = relevel(as.factor(cluster_label_1st), ref="FeC")) %>% 
  arrange(country, year) %>% 
  rename(X_fEC = X1,
         X_fEc = X2,
         X_FeC = X3,
         X_Fec = X4)

confidence_individual = fread(file="Datasets/performance_data/confidence_individual.csv", encoding = "UTF-8")
performance_all = fread(file="Datasets/performance_data/performance_all.csv", encoding = "UTF-8") %>% 
  mutate(conf_index  = na_interpol(conf_index, 10)) 



