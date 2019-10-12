# Auxiliary Variables
# QoC
aux_vars_qoc = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_wdi_gen_num_aux = wdi_gdpcapcur,
         
         Renewable_wdi_pr_env_num_aux = wdi_elerenew,
         Renewable_oecd_pr_env_num_aux = oecd_rnewable_t1,
         
         psocial_exp_oecd_pr_soc_num_aux = oecd_socexpnd_t1a,
         poverty_ds_num_aux = lis_pr9010,
         
         gini_pr_ds_num_aux =  wdi_gini
         ) %>%
  mutate(
    Renewable_wdi_pr_env_num_aux = Renewable_wdi_pr_env_num_aux/100,
    gini_pr_ds_num_aux = gini_pr_ds_num_aux/100,
    psocial_exp_oecd_pr_soc_num_aux = psocial_exp_oecd_pr_soc_num_aux/100,
    ) %>% 
  mutate(
    Renewable_wdi_pr_env_num_aux = 0.005 + 0.99 * Renewable_wdi_pr_env_num_aux,
    Renewable_oecd_pr_env_num_aux = 0.005 + 0.99 * Renewable_oecd_pr_env_num_aux,
    psocial_exp_oecd_pr_soc_num_aux = 0.005 + 0.99 * psocial_exp_oecd_pr_soc_num_aux,
         ) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  mutate_at(vars("GDP_capita_wdi_gen_num_aux", "poverty_ds_num_aux"), funs(ladder_fun)) %>% 
  mutate_at(vars(matches("_pr_")), funs(folded_ladder_fun))  %>% 
  dplyr::arrange(country_text_id, year)

aux_vars_qoc %>% 
  select_at(vars(ends_with("num_aux"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() + 
  facet_wrap(variable~., scales="free")

# V-Dem Aux
aux_vars_vdem = V_dem %>% 
  select(country_text_id, year,
         educ_equal_vdem_gen_num_aux = educ_equal,
         health_vdem_soc_num_aux = v2pehealth
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
         classification_context,
  ) %>%
  mutate_at(vars(ends_with("_num_aux")), funs(scaler_perc)) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter(year > 1950) %>% 
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
analyse_vars = dmx_trade_cluster %>%
  select(country_text_id, 
         year, 
         cluster_label_1st_fact_anal = cluster_1st) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  dplyr::arrange(country_text_id, year) 
  

