# Calculate LogRatios

# dmx_trade_cluster = fread(file="Datasets/performance_data/dmx_trade_cluster_v8.csv", encoding = "UTF-8")


# 3 Cluster
LogRATIOS_3_eco_total = LR(dmx_trade_cluster %>%  
                             select_at(vars(starts_with("FKM_3"), -matches("_cluster"))) %>% 
                             mutate(
                               FKM_3_mb_tot_FEcT = FKM_3_mb_FeC + FKM_3_mb_fEC,
                               FKM_3_mb_tot_FeCT = FKM_3_mb_FEc + FKM_3_mb_fEC,
                               FKM_3_mb_tot_fECT = FKM_3_mb_FeC + FKM_3_mb_FEc)%>% 
                             mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_3_eco_total = data.frame(LogRATIOS_3_eco_total$LR) %>% 
  select_at(vars(matches("_tot_")))  %>% 
  select(FKM_3_mb_FEc.FKM_3_mb_tot_FEcT,
         FKM_3_mb_FeC.FKM_3_mb_tot_FeCT,
         FKM_3_mb_fEC.FKM_3_mb_tot_fECT) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year))

# Dimensional Clusters

LogRATIOS_eco_dim = LR(dmx_trade_cluster %>%  
                         select_at(vars(starts_with("FKM_5"), -matches("_cluster"))) %>%
                         mutate(FKM_5_mb_dim_c = FKM_5_mb_fEc + FKM_5_mb_Fec,
                                FKM_5_mb_dim_C = FKM_5_mb_fEC + FKM_5_mb_FeC,
                                FKM_5_mb_dim_E = FKM_5_mb_fEc + FKM_5_mb_fEC,
                                FKM_5_mb_dim_e = FKM_5_mb_Fec + FKM_5_mb_FeC) %>% 
                         select_at(vars(matches("_dim_"))) %>% 
                         mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_eco_dim = data.frame(LogRATIOS_eco_dim$LR) %>% 
  select_at(vars(matches("_dim_")))  %>% 
  select(FKM_5_mb_dim_c.FKM_5_mb_dim_CT = FKM_5_mb_dim_c.FKM_5_mb_dim_C,
         FKM_5_mb_dim_E.FKM_5_mb_dim_eT = FKM_5_mb_dim_E.FKM_5_mb_dim_e) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year))

# 5 Clusters
LogRATIOS_eco_total = LR(dmx_trade_cluster %>%  
                           select_at(vars(starts_with("FKM_5"), -matches("_cluster"))) %>% 
                           mutate(
                             FKM_5_mb_tot_fEcT = FKM_5_mb_Fec + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_FEC,
                             FKM_5_mb_tot_FecT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_FEC,
                             FKM_5_mb_tot_FECT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_Fec,
                             FKM_5_mb_tot_FeCT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_Fec + FKM_5_mb_FEC,
                             FKM_5_mb_tot_fECT = FKM_5_mb_fEc + FKM_5_mb_Fec + FKM_5_mb_FeC + FKM_5_mb_FEC)%>% 
                           mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_eco_total = data.frame(LogRATIOS_eco_total$LR) %>% 
  select_at(vars(matches("_tot_"))) %>% 
  select(FKM_5_mb_fEc.FKM_5_mb_tot_fEcT,
         FKM_5_mb_Fec.FKM_5_mb_tot_FecT,
         FKM_5_mb_FEC.FKM_5_mb_tot_FECT,
         FKM_5_mb_FeC.FKM_5_mb_tot_FeCT,
         FKM_5_mb_fEC.FKM_5_mb_tot_fECT) %>% 
  bind_cols(dmx_trade_cluster %>% 
              select(country_text_id, year))