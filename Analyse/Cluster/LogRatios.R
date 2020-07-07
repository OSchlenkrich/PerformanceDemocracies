# Calculate LogRatios

# dmx_trade_cluster = fread(file="Datasets/performance_data/dmx_trade_cluster_v8.csv", encoding = "UTF-8")


# 3 Cluster
LogRATIOS_3_eco_total_raw = LR(dmx_trade_cluster %>%  
                             select_at(vars(starts_with("FKM_3"), -matches("_cluster"))) %>% 
                             mutate(
                               FKM_3_mb_tot_FEcT = FKM_3_mb_FeC + FKM_3_mb_fEC,
                               FKM_3_mb_tot_FeCT = FKM_3_mb_FEc + FKM_3_mb_fEC,
                               FKM_3_mb_tot_fECT = FKM_3_mb_FeC + FKM_3_mb_FEc)%>% 
                             mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_3_eco_total = data.frame(LogRATIOS_3_eco_total_raw$LR) %>% 
  select_at(vars(matches("_tot_")))  %>% 
  select(FKM3_FEc = FKM_3_mb_FEc.FKM_3_mb_tot_FEcT,
         FKM3_FeC = FKM_3_mb_FeC.FKM_3_mb_tot_FeCT,
         FKM3_fEC = FKM_3_mb_fEC.FKM_3_mb_tot_fECT)  %>%  
  mutate_at(vars(starts_with("FKM")), funs(scale_this(.))) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year))

# Dimensional Clusters

LogRATIOS_eco_dim_raw = LR(dmx_trade_cluster %>%  
                         select_at(vars(starts_with("FKM_5"), -matches("_cluster"))) %>%
                         mutate(FKM_5_mb_dim_c = FKM_5_mb_fEc + FKM_5_mb_Fec,
                                FKM_5_mb_dim_C = FKM_5_mb_fEC + FKM_5_mb_FeC,
                                FKM_5_mb_dim_E = FKM_5_mb_fEc + FKM_5_mb_fEC,
                                FKM_5_mb_dim_e = FKM_5_mb_Fec + FKM_5_mb_FeC) %>% 
                         select_at(vars(matches("_dim_"))) %>% 
                         mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))

LogRATIOS_eco_dim = data.frame(LogRATIOS_eco_dim_raw$LR) %>% 
  select_at(vars(matches("_dim_")))  %>% 
  select(FKM5_c = FKM_5_mb_dim_c.FKM_5_mb_dim_C,
         FKM5_E = FKM_5_mb_dim_E.FKM_5_mb_dim_e) %>%  
  mutate_at(vars(starts_with("FKM")), funs(scale_this(.))) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year)) 

# 5 Clusters
LogRATIOS_eco_total_raw = LR(dmx_trade_cluster %>%  
                           select_at(vars(starts_with("FKM_5"), -matches("_cluster"))) %>% 
                           mutate(
                             FKM_5_mb_tot_fEcT = FKM_5_mb_Fec + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_FEC,
                             FKM_5_mb_tot_FecT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_FEC,
                             FKM_5_mb_tot_FECT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_Fec,
                             FKM_5_mb_tot_FeCT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_Fec + FKM_5_mb_FEC,
                             FKM_5_mb_tot_fECT = FKM_5_mb_fEc + FKM_5_mb_Fec + FKM_5_mb_FeC + FKM_5_mb_FEC)%>% 
                           mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_eco_total = data.frame(LogRATIOS_eco_total_raw$LR) %>% 
  select_at(vars(matches("_tot_"))) %>% 
  select(FKM5_fEc = FKM_5_mb_fEc.FKM_5_mb_tot_fEcT,
         FKM5_Fec = FKM_5_mb_Fec.FKM_5_mb_tot_FecT,
         FKM5_FEC = FKM_5_mb_FEC.FKM_5_mb_tot_FECT,
         FKM5_FeC = FKM_5_mb_FeC.FKM_5_mb_tot_FeCT,
         FKM5_fEC = FKM_5_mb_fEC.FKM_5_mb_tot_fECT)  %>%  
  mutate_at(vars(starts_with("FKM")), funs(scale_this(.))) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year))

# Cleaning
rm(LogRATIOS_3_eco_total_raw)
rm(LogRATIOS_eco_dim_raw)
rm(LogRATIOS_eco_total_raw)