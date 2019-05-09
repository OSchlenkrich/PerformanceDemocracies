# Load Datasets

# Democracy Matrix ----
dmx_data = fread("Datasets/DemocracyMatrix_v1_1.csv")

dmx_data_trade = dmx_data %>% 
  select_at(vars(country, year, regions, classification_context, matches("dim_index_trade_off"))) %>% 
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>% 
  filter(classification_context == "Deficient Democracy" | classification_context == "Working Democracy" ) %>% 
  na.omit()

# VDEM ----


V_dem = fread("C:/RTest/V-Dem-CY+Others-v8.csv") %>% 
  select(country = country_name, year, v2peedueq, e_wbgi_rqn)

# OECD ----
oecd_social_data = fread("Datasets/OECD_Social_Protection.csv") %>% 
  filter(Measure == "In percentage of Gross Domestic Product",
         Source == "Public",
         Branch == "Total",
         `Type of Expenditure` == "Total") %>% 
  select(country = Country, year = Year, SOCX = Value) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea"  = "Korea",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
                              ),
         country = as.character(country)
         ) %>% 
  arrange(country, year)

oecd_poverty_data = fread("Datasets/OECD_Poverty.csv") %>% 
  filter(Measure == "Poverty rate after taxes and transfers, Poverty line 50%",
         `Age group` == "Total population",
         Methodology == "New income definition since 2012") %>% 
  select(country = Country, year = Year, PovertyRate60 = Value) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea"  = "Korea",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  ) %>% 
  arrange(country, year)

table(oecd_poverty_data$Measure)
