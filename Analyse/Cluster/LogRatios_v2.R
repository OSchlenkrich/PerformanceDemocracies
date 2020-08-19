# Calculate LogRatios

# dmx_trade_cluster = fread(file="Datasets/performance_data/dmx_trade_cluster_v9.csv", encoding = "UTF-8")


# 3 Cluster
LogRATIOS_3_eco_total_raw = LR(dmx_trade_cluster %>%  
                             select_at(vars(starts_with("mp_Cluster3"))) %>% 
                             mutate(
                               FKM_3_mb_tot_FEcT = mp_Cluster3_FeC + mp_Cluster3_fEC,
                               FKM_3_mb_tot_FeCT = mp_Cluster3_FEc + mp_Cluster3_fEC,
                               FKM_3_mb_tot_fECT = mp_Cluster3_FeC + mp_Cluster3_FEc)%>% 
                             mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_3_eco_total = data.frame(LogRATIOS_3_eco_total_raw$LR) %>% 
  select_at(vars(matches("_tot_")))  %>% 
  select(FKM3_FEc = mp_Cluster3_FEc.FKM_3_mb_tot_FEcT,
         FKM3_FeC = mp_Cluster3_FeC.FKM_3_mb_tot_FeCT,
         FKM3_fEC = mp_Cluster3_fEC.FKM_3_mb_tot_fECT)  %>%  
  mutate_at(vars(starts_with("FKM")), funs(scale_this(.))) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year))

# Dimensional Clusters

LogRATIOS_eco_dim_raw = LR(dmx_trade_cluster %>%  
                         select_at(vars(starts_with("mp_Cluster4"))) %>%
                         mutate(FKM_4_mb_dim_c = mp_Cluster4_fEc + mp_Cluster4_Fec,
                                FKM_4_mb_dim_C = mp_Cluster4_fEC + mp_Cluster4_FeC,
                                FKM_4_mb_dim_E = mp_Cluster4_fEc + mp_Cluster4_fEC,
                                FKM_4_mb_dim_e = mp_Cluster4_Fec + mp_Cluster4_FeC) %>% 
                         select_at(vars(matches("_dim_"))) %>% 
                         mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))

LogRATIOS_eco_dim = data.frame(LogRATIOS_eco_dim_raw$LR) %>% 
  select_at(vars(matches("_dim_")))  %>% 
  select(FKM4_c = FKM_4_mb_dim_c.FKM_4_mb_dim_C,
         FKM4_E = FKM_4_mb_dim_E.FKM_4_mb_dim_e) %>%  
  # mutate(FKM4_C = inverser(FKM4_c)) %>% 
  # select(-FKM4_c) %>% 
  mutate_at(vars(starts_with("FKM")), funs(scale_this(.))) %>% 
  # select(-FKM4_c) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year)) 

# 5 Clusters
LogRATIOS_eco_total_raw = LR(dmx_trade_cluster %>%  
                           select_at(vars(starts_with("mp_Cluster5"))) %>% 
                           mutate(
                             FKM_5_mb_tot_fEcT = mp_Cluster5_Fec + mp_Cluster5_fEC + mp_Cluster5_FeC + mp_Cluster5_FEC,
                             FKM_5_mb_tot_FecT = mp_Cluster5_fEc + mp_Cluster5_fEC + mp_Cluster5_FeC + mp_Cluster5_FEC,
                             FKM_5_mb_tot_FECT = mp_Cluster5_fEc + mp_Cluster5_fEC + mp_Cluster5_FeC + mp_Cluster5_Fec,
                             FKM_5_mb_tot_FeCT = mp_Cluster5_fEc + mp_Cluster5_fEC + mp_Cluster5_Fec + mp_Cluster5_FEC,
                             FKM_5_mb_tot_fECT = mp_Cluster5_fEc + mp_Cluster5_Fec + mp_Cluster5_FeC + mp_Cluster5_FEC)%>% 
                           mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_eco_total = data.frame(LogRATIOS_eco_total_raw$LR) %>% 
  select_at(vars(matches("_tot_"))) %>% 
  select(FKM5_fEc = mp_Cluster5_fEc.FKM_5_mb_tot_fEcT,
         FKM5_Fec = mp_Cluster5_Fec.FKM_5_mb_tot_FecT,
         FKM5_FEC = mp_Cluster5_FEC.FKM_5_mb_tot_FECT,
         FKM5_FeC = mp_Cluster5_FeC.FKM_5_mb_tot_FeCT,
         FKM5_fEC = mp_Cluster5_fEC.FKM_5_mb_tot_fECT)  %>%  
  mutate_at(vars(starts_with("FKM")), funs(scale_this(.))) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year))

# Cleaning
rm(LogRATIOS_3_eco_total_raw)
rm(LogRATIOS_eco_dim_raw)
rm(LogRATIOS_eco_total_raw)
