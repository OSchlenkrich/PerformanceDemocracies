# Auxiliary Variables
# QoC

mean_NA = function(x) {
  if(all(is.na(x)) == F) {
    return(mean(x, na.rm=T))
  } else {
    return(NA)
  }
  
}



aux_vars_qoc = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_wdi_gen_num_aux = wdi_gdpcapcur,
         
         Renewable_wdi_pr_env_num_aux = wdi_elerenew,
         Renewable_oecd_pr_env_num_aux = oecd_rnewable_t1,
         
         psocial_exp_oecd_pr_soc_num_aux = oecd_socexpnd_t1a,
         # too small sample
         # poverty_ds_num_aux = lis_pr9010,
         # gini_pr_ds_num_aux =  wdi_gini,
         
         life_expectancy_eco_num_aux = wdi_lifexp,
         
         legal_dp_ord_aux = lp_legor,
         parties_dp_num_aux = gol_enpp,
         pop_size_dp_num_aux = wdi_pop
         ) %>%
  mutate(
    Renewable_wdi_pr_env_num_aux = Renewable_wdi_pr_env_num_aux/100,
    #gini_pr_ds_num_aux = gini_pr_ds_num_aux/100,
    psocial_exp_oecd_pr_soc_num_aux = psocial_exp_oecd_pr_soc_num_aux/100,
    ) %>% 
  mutate(
    Renewable_wdi_pr_env_num_aux = 0.005 + 0.99 * Renewable_wdi_pr_env_num_aux,
    Renewable_oecd_pr_env_num_aux = 0.005 + 0.99 * Renewable_oecd_pr_env_num_aux,
    psocial_exp_oecd_pr_soc_num_aux = 0.005 + 0.99 * psocial_exp_oecd_pr_soc_num_aux,
         ) %>% 
  filter(country_text_id %in% dmx_cluster_names) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  group_by(country_text_id, year) %>% 
  summarise_all(mean_NA) %>% 
  ungroup() %>% 
  mutate_at(vars("GDP_capita_wdi_gen_num_aux", 
                 #"poverty_ds_num_aux", 
                 "life_expectancy_eco_num_aux",
                 "parties_dp_num_aux",
                 "pop_size_dp_num_aux"), funs(ladder_fun)) %>% 
  mutate_at(vars(matches("_pr_")), funs(folded_ladder_fun))  %>% 
  dplyr::arrange(country_text_id, year)  %>% 
  mutate(
    legal_dp_ord_aux = ifelse(legal_dp_ord_aux == 2, 0,
                                           ifelse(legal_dp_ord_aux == 3, 0,
                                                  ifelse(legal_dp_ord_aux == 4, 0,
                                                         ifelse(legal_dp_ord_aux == 5, 0, legal_dp_ord_aux))))
  )



aux_vars_qoc %>% 
  select_at(vars(ends_with("num_aux"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() + 
  facet_wrap(variable~., scales="free")

# V-Dem Aux
aux_vars_vdem = V_dem_all %>% 
  select(country_text_id, year,
         educ_equal_vdem_gen_num_aux = v2peedueq,
         health_vdem_soc_num_aux = v2pehealth,
         electoral_sys_vdem_dp_ord_aux = v2elparlel,
         
         universalism_ds_num_aux = v2dlunivl,
         sc_civequality_ds_num_aux = v2clacjust,
         sc_powequality_ds_num_aux = v2pepwrses,
         
         v2x_elecreg
         ) %>% 
  group_by(country_text_id) %>%
  mutate_at(vars(starts_with("electoral")), funs(fill_elections(., v2x_elecreg))) %>%
  ungroup() %>% 
  select(-v2x_elecreg)  %>% 
  mutate(
    electoral_sys_vdem_dp_ord_aux = ifelse(electoral_sys_vdem_dp_ord_aux == 3, 0,
                                           ifelse(electoral_sys_vdem_dp_ord_aux == 2, 1, electoral_sys_vdem_dp_ord_aux))
  )


aux_vars_vdem %>% 
  select_at(vars(ends_with("num_aux"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() + 
  facet_wrap(variable~., scales="free")

# Combine auxiliary variables ####
aux_vars = aux_vars_qoc %>% 
  left_join(aux_vars_vdem, by=c("country_text_id", "year"))




#DMX
scaler_perc = function(x)  {
  0.005 + 0.99 * x
}

aux_vars_dmx_env = dmx_data %>% 
  select(country, year,
         freedom_dim_index_context_num_aux = freedom_dim_index_context,
         equality_dim_index_context_num_aux = equality_dim_index_context,
         control_dim_index_context_num_aux = control_dim_index_context,
         classification_core,
  ) %>%
  mutate_at(vars(ends_with("_num_aux")), funs(scaler_perc)) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  filter(country_text_id %in% dmx_cluster_names) %>% 
  filter(year > 1960) %>% 
  mutate_at(vars(ends_with("_num_aux")), funs(folded_ladder_fun))  %>% 
  select(-country) %>%
  select(country_text_id, year, everything()) %>%
  dplyr::arrange(country_text_id, year)


aux_vars_dmx_env %>% 
  select_at(vars(ends_with("num_aux"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() + 
  facet_wrap(variable~., scales="free")

# ANALYSE VARS
if (exists("dmx_trade_cluster") == T ){
  # Main Independent Var: Log Ratios Democracy Profiles
 source("Analyse/Cluster/LogRatios_v2.R")

 # Other Analyse vars
  
  V_dem_control = V_dem_all %>% 
    select(country_text_id, year, 
           corruption_vdem_pr_ctl = v2x_corr, 
           corporatism_vdem_pr_ctl = v2csstruc_1,
           statehoodr_vdem_pr_ctl = v2svstterr)  %>%
    mutate_at(vars(statehoodr_vdem_pr_ctl), funs(./100)) %>% 
    # transformation and scaling
    mutate_at(vars(matches("pr")), funs(folded_ladder_fun)) %>% 
    mutate_at(vars(matches("pr")), funs(scale)) 
  
  # V_dem_control %>%
  #   select_at(vars(ends_with("ctl"))) %>%
  #   melt() %>%
  #   ggplot(aes(x=value)) +
  #   geom_histogram()  +
  #   facet_wrap(variable~., scales = "free")

  
  #QoC
  QoC_control = QoC_data %>% 
    dplyr::select(country_text_id, year,
                  
                  pop_over65_wdi_pr_ctl = wdi_pop65,
                  
                  unions_vi_num_ctl = vi_udr,

                  trade_wdi_num_ctl = wdi_trade,
                  
    ) %>% 
    
    filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
    filter_if(is.double, any_vars(!is.na(.))) %>%
    mutate_at(vars(matches("pr")), funs(./100)) %>% 
    arrange(country_text_id, year)  %>% 
    # transformation and scaling
    mutate_at(vars(matches("pr")), funs(folded_ladder_fun)) %>% 
    mutate_at(vars(matches("num")), funs(ladder_fun))  %>% 
    mutate_at(vars(matches("pr")), funs(scale))
  
  # QoC_control %>%
  #   select_at(vars(ends_with("ctl"))) %>%
  #   melt() %>%
  #   ggplot(aes(x=value)) +
  #   geom_histogram()  +
  #  facet_wrap(variable~., scales = "free")
  
    
  analyse_vars = LogRATIOS_3_eco_total %>% 
    left_join(LogRATIOS_eco_dim, by=c("country_text_id", "year")) %>% 
    left_join(LogRATIOS_eco_total,by=c("country_text_id", "year")) %>% 
    tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
    left_join(V_dem_control,by=c("country_text_id", "year")) %>% 
    left_join(QoC_control,by=c("country_text_id", "year")) %>% 
    dplyr::arrange(country_text_id, year) 
}

