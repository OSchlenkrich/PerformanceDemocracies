# Load Datasets

# Democracy Matrix ----
dmx_data = fread("unzip -p Datasets/DemocracyMatrix_v1_1.zip")

dmx_data_trade = dmx_data %>% 
  select_at(vars(country, year, regions, classification_context, matches("dim_index_trade_off"))) %>% 
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>% 
  filter(classification_context == "Deficient Democracy" | classification_context == "Working Democracy" ) %>% 
  na.omit()


##


dmx_data_context = dmx_data %>% 
  select_at(vars(country, year, regions, matches("total_index_context"))) 

# VDEM ----

V_dem = fread("C:/RTest/V-Dem-CY+Others-v8.csv") %>% 
  select(country = country_name, year, cso = v2csstruc_1, 
         educ_equal = v2peedueq, 
         wgi_rq = e_wbgi_rqe,
         gini_vdem = e_peginiwi,
         e_population,
         region = e_regionpol) %>% 
  mutate(log_pop = log(e_population))

# OECD ----


oecd_social_data = fread("unzip -p Datasets/OECD_Social_Protection.zip") %>% 
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


oecd_poverty_data = fread("unzip -p Datasets/OECD_Poverty.zip") %>% 
  filter(Measure == "Gini (disposable income, post taxes and transfers)",
         `Age group` == "Total population",
         Definition == "Current definition") %>% 
  select(country = Country, year = Year, Gini = Value) %>% 
  group_by(country, year) %>%
  slice(1) %>% 
  ungroup( ) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea"  = "Korea",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  ) %>% 
  arrange(country, year)


###
WB_inflation = fread("Datasets/WB_inflation.csv", header=T) %>% 
  rename(country = "Country Name") %>% 
  select(-"Country Code", -"Indicator Name", -"Indicator Code", -V64) %>% 
  melt(id.vars = c("country"), variable.name = "year", value.name = "inflation") %>% 
  mutate(year = as.numeric(levels(year))[year]) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea" = "Korea, Rep.",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  )


###
WB_gdp = fread("Datasets/WB_gdp_capita.csv", header=T) %>% 
  rename(country = "Country Name") %>% 
  select(-"Country Code", -"Indicator Name", -"Indicator Code", -V64) %>% 
  melt(id.vars = c("country"), variable.name = "year", value.name = "gdp_capita") %>% 
  mutate(year = as.numeric(levels(year))[year]) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea" = "Korea, Rep.",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  )

##
WB_export = fread("Datasets/WB_export.csv", header=T) %>% 
  rename(country = "Country Name") %>% 
  select(-"Country Code", -"Indicator Name", -"Indicator Code", -V64) %>% 
  melt(id.vars = c("country"), variable.name = "year", value.name = "gdp_export") %>% 
  mutate(year = as.numeric(levels(year))[year]) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea" = "Korea, Rep.",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  )

###
frame = fread("C:/RTest/V-Dem-CY+Others-v8.csv") %>% 
  select(country = country_name, ccy_code = country_text_id, year)


Unemployment_percent = fread("Datasets/unemployment.csv", header=T) %>% 
  filter(SUBJECT == "TOT", FREQUENCY == "A") %>% 
  select(ccy_code = LOCATION, year = TIME, unempl = Value) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(frame, by=c("ccy_code", "year")) %>% 
  select(-ccy_code)


##
Age65_percent = fread("Datasets/Age65_percent.csv", header=T) %>% 
  rename(country = "Country Name") %>% 
  select(-"Country Code", -"Indicator Name", -"Indicator Code", -V64) %>% 
  melt(id.vars = c("country"), variable.name = "year", value.name = "age65") %>% 
  mutate(year = as.numeric(levels(year))[year]) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea" = "Korea, Rep.",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  )


##

Trade_union = fread("Datasets/ILO_trade_union_density.csv", header=T) %>% 
  select(country = ref_area.label, year = time, union_density = obs_value) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea" = "Korea, Republic of",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  ) %>% 
  group_by(country) %>% 
  summarise(union_density = mean(union_density))

##

ParlGov_Cabinet = fread("Datasets/ParlGov_Cabinet.csv", header=T)
ParlGov_Party = fread("Datasets/ParlGov_Party.csv", header=T)  %>% 
  select(party_id, family_name_short)

ParlGov_Cabinet_yearly = fread("Datasets/ParlGov_Frame.csv", header=T) %>% 
  filter(id_type == "cabinet") %>%
  rename(cabinet_id = id) %>% 
  left_join(ParlGov_Cabinet, by="cabinet_id") %>% 
  filter(cabinet_party == 1) %>% 
  select(country = country_name, year, party_id, left_right) %>% 
  group_by(country, year) %>%
  summarise(left_right = mean(left_right))
  
ParlGov_Family_yearly = fread("Datasets/ParlGov_Frame.csv", header=T) %>% 
  filter(id_type == "cabinet") %>%
  rename(cabinet_id = id) %>% 
  left_join(ParlGov_Cabinet, by="cabinet_id") %>% 
  filter(prime_minister == 1) %>% 
  select(country = country_name, year, party_id, seats) %>% 
  left_join(ParlGov_Party, by="party_id") %>% 
  select(-party_id) %>% 
  mutate(family_name_short = fct_recode(family_name_short, 
                                        NULL = "none",
                                        "Left" = "com",
                                        "Left" = "soc",
                                        "eco" = "eco",
                                        "Right" = "con",
                                        "Right" = "agr",
                                        "far" = "right",
                                        "Right" = "lib",
                                        "chr" = "chr",
                                        )) %>% 
  group_by(country, year) %>% 
  arrange(-seats) %>% 
  slice(1)
  
