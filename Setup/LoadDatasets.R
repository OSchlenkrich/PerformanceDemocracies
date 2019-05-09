# Load Datasets

# Democracy Matrix
dmx_data = fread("Datasets/DemocracyMatrix_v1_1.csv")

dmx_data_trade = dmx_data %>% 
  select_at(vars(country, year, regions, classification_context, matches("dim_index_trade_off"))) %>% 
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>% 
  filter(classification_context == "Deficient Democracy" | classification_context == "Working Democracy" ) %>% 
  na.omit()

