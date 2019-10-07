# Auxiliary Variables
# QoC
aux_vars_qoc_env = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_wdi_num_aux = wdi_gdpcapcur,
         Renewable_wdi_num_aux = wdi_elerenew,
         Renewable_oecd_num_aux = oecd_rnewable_t1
         ) %>%
  mutate(Renewable_wdi_num_aux = Renewable_wdi_num_aux/100,
         Renewable_wdi_num_aux = 0.005 + 0.99 * Renewable_wdi_num_aux,
         Renewable_oecd_num_aux = 0.005 + 0.99 * Renewable_oecd_num_aux) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  mutate_at(vars("GDP_capita_wdi_num_aux"), funs(ladder_fun)) %>% 
  mutate_at(vars("Renewable_wdi_num_aux", "Renewable_oecd_num_aux"), funs(folded_ladder_fun))  %>% 
  dplyr::arrange(country_text_id, year)


aux_vars_env %>% 
  select_at(vars(ends_with("num_aux"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() + 
  facet_wrap(variable~., scales="free")

#DMX
aux_vars_dmx_env = dmx_data %>% 
  select(country, year,
         total_index_context_num_aux = total_index_context,
  ) %>%
  mutate(total_index_context_num_aux = 0.005 + 0.99 * total_index_context_num_aux) %>% 
  right_join(QoC_data %>%  select(country, country_text_id, year), by=c("country", "year")) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  mutate_at(vars(total_index_context_num_aux), funs(folded_ladder_fun))  %>% 
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
         cluster_label_1st_fact_anal = cluster_label_1st) %>% 
  dplyr::arrange(country_text_id, year) 


