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

dmx_trade_cluster = fread(file="Datasets/performance_data/dmx_trade_cluster_v8.csv", encoding = "UTF-8")
  # mutate(cluster_label_1st = relevel(as.factor(cluster_label_1st), ref="FeC")) %>% 
  # arrange(country, year) %>% 
  # rename(X_fEC = X1,
  #        X_fEc = X2,
  #        X_FeC = X3,
  #        X_Fec = X4)


# source("Analyse/PerformanceAreas/Economy/eco_FA_v2.R")
# source("Analyse/PerformanceAreas/Environment/env_FA.R") # takes some time due to download of OECD data
# source("Analyse/PerformanceAreas/GoalA/ga_variables_v2.R")
# source("Analyse/PerformanceAreas/Social/soc_FA_v2.R")
# source("Analyse/PerformanceAreas/DomesticSecurity/ds_FA.R")
# source("Analyse/PerformanceAreas/Confidence/conf_FA_v2.R")

#### LOAD DATASETS #######
performance_eco = fread("Datasets/performance_data/ImputedDatasets/performance_eco.csv", encoding = "UTF-8")
performance_env = fread("Datasets/performance_data/ImputedDatasets/performance_env.csv", encoding = "UTF-8")
performance_wdi_env = fread("Datasets/performance_data/ImputedDatasets/performance_wdi_env.csv", encoding = "UTF-8")
performance_ga = fread("Datasets/performance_data/ImputedDatasets/performance_ga.csv", encoding = "UTF-8")
performance_soc = fread("Datasets/performance_data/ImputedDatasets/performance_soc.csv", encoding = "UTF-8")
performance_ds = fread("Datasets/performance_data/ImputedDatasets/performance_ds.csv", encoding = "UTF-8")
performance_pc = fread("Datasets/performance_data/ImputedDatasets/performance_pc.csv", encoding = "UTF-8")
performance_pclvl1 = fread("Datasets/performance_data/ImputedDatasets/conf_scores.csv", encoding = "UTF-8")

# Combine all Datasets ####

performance_all = V_dem %>%
  select(country, country_text_id, year) %>%
  filter(year >= 1950) %>%
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>%
  # include regions and classfications
  left_join(dmx_data %>%
              select(country_text_id, year, regions, classification_core, classification_context), by=c("country_text_id", "year")) %>%
  
  left_join(performance_eco %>%
              select(country_text_id, year, wealth_eco, productivity_eco), by=c("country_text_id", "year")) %>%
  left_join(performance_env %>%
              select(country_text_id, year, GEP_env), by=c("country_text_id", "year")) %>%
  # left_join(performance_wdi_env %>%
  #             select(country_text_id, year, air_wdi_env), by=c("country_text_id", "year")) %>%
  left_join(performance_ga %>%
              select(country_text_id, year,  GA_ccp_ga, GA_lutz_ga, systid_ccp), by=c("country_text_id", "year")) %>%
  left_join(performance_soc %>%
              select(country_text_id, year, eco_inequal_soc, soc_inequal_soc), by=c("country_text_id", "year")) %>%
  left_join(performance_ds %>%
              select(country_text_id, year,  domsec_ds), by=c("country_text_id", "year")) %>%
  left_join(performance_pc %>%
              select(country_text_id, year,  conf_pc), by=c("country_text_id", "year"))  %>% 
  # rename, becomes obsolete with new calculation of indices
  rename(arate_ccp_ga = GA_ccp_ga,
         arate_lutz_ga = GA_lutz_ga
         ) %>% 
  mutate_at(vars(ends_with("_eco"), 
                 ends_with("_env"), 
                 ends_with("_soc"), 
                 ends_with("_ds"), 
                 ends_with("_pc"), 
                 ends_with("_ga")),funs("index" = EPI_fun(., lower = 0.01, upper=0.99))) 
# Other Democracy Profiles ####
source("Analyse/Cluster/OtherDemocracyProfiles.R")


