# Environment Performance
library(OECD)


if (exists("oecd_ghg_data") == F) {
  oecd_ghg_data = get_dataset("AIR_GHG")
}
if (exists("oecd_air_data") == F) {
  oecd_air_data = get_dataset("AIR_EMISSIONS")
}


oecd_co2 = oecd_ghg_data %>% 
  filter(POL %in% c("CO2", "CH4", "N2O", "GHG"),
         VAR == "TOTAL") %>% 
  dplyr::select(POL, country_text_id = COU, year = obsTime, obsValue)  %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(values_from = obsValue, names_from = POL) %>% 
  rename(CO2_ugdp_oecd = CO2, CH4_ugdp_oecd = CH4, N2O_ugdp_oecd = N2O, GHG_ugdp_oecd = GHG)




oecd_air = oecd_air_data %>% 
  filter(POL %in% c("CO", "NMVOC", "NOX", "SOX"),
         VAR == "TOT") %>% 
  dplyr::select(POL, country_text_id = COU, year = obsTime, obsValue)  %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(values_from = obsValue, names_from = POL) %>% 
  rename(CO_ugdp_oecd = CO, SOX_ugdp_oecd = SOX, NOX_ugdp_oecd = NOX, NMVOC_ugdp_oecd = NMVOC)

imf_gdp= fread("Datasets/imf_gdpppp.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "gdp_imf") %>% 
  mutate(year = as.numeric(year),
         country = fct_recode(country, 
                              "South Korea" = "Korea, Republic of",
                              "Kyrgyzstan" = "Kyrgyz Republic",
                              "Macedonia" = "North Macedonia",
                              "Russia" = "Russian Federation",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States",
                              "Taiwan" = "Taiwan Province of China",
                              "Sao Tome and Principe = São Tomé and Príncipe",
                              "Ivory Coast"	= "Côte d'Ivoire",
                              "Cape Verde" = "Cabo Verde",
                              "The Gambia" = "Gambia, The")) %>% 
  group_by(country) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

Environment_Performance = QoC_data %>% 
  select(country_text_id, year,
         #greenhouse_oecd = oecd_greenhouse_t1,
         #sulphur_oecd = oecd_soxnox_t1a,
         #nitrogen_oecd = oecd_soxnox_t1b,
         #co2_oecd = oecd_airqty_t1,
         water_ugdp_cap_oecd = oecd_water_t1b,
         waste_ugdp_cap_oecd = oecd_waste_t1b,
         population = oecd_evopop_t1,
         greenhouse_ugdp_wdi = wdi_co2,
         GDP_capita = oecd_sizegdp_t1,
         
         population_wdi = wdi_pop,
         GDP_capita_wdi = wdi_gdpcapcur
  ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  group_by(country_text_id) %>% 
  filter(year >= 1950) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  
  left_join(oecd_co2, by=c("country_text_id", "year")) %>% 
  left_join(oecd_air, by=c("country_text_id", "year")) %>% 
  left_join(imf_gdp, by=c("country_text_id", "year")) %>% 
  
  
  mutate_at(vars(ends_with("ugdp_oecd")), funs((./gdp_imf ) * 1000 ))  %>%
  mutate_at(vars(ends_with("cap_oecd")), funs((./population_wdi) * 10000 ))  %>%
  
  mutate(greenhouse_ugdp_wdi = ((greenhouse_ugdp_wdi*population_wdi)/gdp_imf) * 1000) %>% 
  
  #Senegal has a negative number
  mutate(greenhouse_ugdp_wdi = abs(greenhouse_ugdp_wdi)) %>% 
  # Iceland's water abstraction rises from 267.8157 to 2551.7594	in 5 years. This does not make sense
  mutate(water_ugdp_cap_oecd = ifelse(country_text_id == "ISL" & year >= 2010, NA, water_ugdp_cap_oecd)) %>% 
  
  
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
 
  left_join(dmx_trade_cluster %>%  select(-country, -regions), by=c("country_text_id", "year"))  %>%
  
  # add country and regions
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country_text_id"))  %>%
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  
  select(country, country_text_id, regions, year, everything())  %>% 
  select(-population_wdi, -GDP_capita_wdi) %>% 
  ungroup() %>% 
  dplyr::arrange(country_text_id, year)  


##### NA-Plots ####


Environment_Performance %>% 
  group_by(year) %>% 
  select_at(vars(GHG_ugdp_oecd,
                 SOX_ugdp_oecd,
                 NOX_ugdp_oecd,
                 CO2_ugdp_oecd,
                 water_ugdp_cap_oecd,
                 waste_ugdp_cap_oecd)) %>% 
  summarise_all(pMiss_01) %>% 
  pivot_longer(cols=-year) %>% 
  ggplot(aes(x=year, y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(name~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")


#### Linear Interpolation ####
# disabled
Environment_Performance_IP = Environment_Performance %>%
  group_by(country_text_id) %>% 
  # mutate_at(vars(ends_with("oecd_per_capita")), .funs = list(~na_interpol(.))) %>% 
  # mutate_at(vars(ends_with("wdi_per_capita")), .funs = list(~na_interpol(.))) %>% 
  ungroup()


# Environment_Performance_IP %>% 
#   group_by(year) %>% 
#   select_at(vars(ends_with("wdi_per_capita"), ends_with("oecd_per_capita"))) %>% 
#   summarise_all(pMiss) %>% 
#   melt(id.vars="year") %>% 
#   ggplot(aes(x=year, y=value, fill=variable)) +
#   geom_bar(stat="identity", width=1) +
#   facet_wrap(variable~.) +
#   scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
#   scale_x_continuous(breaks=seq(1950,2020, 10)) +
#   theme_bw()  +
#   theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
#   ggtitle("Missings in Democracy Profile Sample -)")



######



Environment_Performance_IP %>% 
  select_at(vars(ends_with("oecd"),ends_with("wdi"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 

Environment_Performance_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("wdi"))) %>% 
  select(water_ugdp_cap_oecd,
         waste_ugdp_cap_oecd, 
         GHG_ugdp_oecd,
         SOX_ugdp_oecd,   
         NOX_ugdp_oecd,
         CO_ugdp_oecd) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")  +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Raw Sample")





# Transformation ####


Environment_Performance_IP_norm = Environment_Performance_IP  %>% 
  select_at(vars(ends_with("oecd"), ends_with("wdi"))) %>% 
  mutate_at(vars(CH4_ugdp_oecd,
                 N2O_ugdp_oecd,
                 CO2_ugdp_oecd,
                 water_ugdp_cap_oecd,
                 SOX_ugdp_oecd,
                 NOX_ugdp_oecd,
                 NMVOC_ugdp_oecd), funs(trim(., 0.01, minimum=T))) %>% 
  mutate_at(vars(greenhouse_ugdp_wdi), funs(trim(., 0.025, minimum=T))) %>% 
  mutate_at(vars(waste_ugdp_cap_oecd), funs(trim(., 0.02, minimum=T))) %>%
  mutate_at(vars(GHG_ugdp_oecd, CO_ugdp_oecd), funs(trim(., 0.01, minimum=T, only=T))) %>%
  mutate_all(funs(ladder_fun(.))) %>% 
  mutate_all(scale)



Environment_Performance_IP_norm %>% 
  select_at(vars(ends_with("oecd"), ends_with("wdi"))) %>% 
  select(water_ugdp_cap_oecd,
         waste_ugdp_cap_oecd, 
         GHG_ugdp_oecd,
         SOX_ugdp_oecd,   
         NOX_ugdp_oecd,
         CO_ugdp_oecd) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()   +
  facet_wrap(variable~., scales = "free") +
  theme_bw()  +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Transformed Sample")


### NA Frame: OECD
NA_frame_env_oecd = Environment_Performance_IP_norm %>% 
  select(-greenhouse_ugdp_wdi) %>% 
  mutate_all(funs(is_na = ifelse(is.na(.)==T, 1,0))) %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>% 
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select_at(vars(country_text_id, year, ends_with("is_na"))) %>% 
  mutate(missing_SUM = rowSums(select(., matches("oecd"))))


#### Factor Analysis: Transfer to MI
# OECD
fa_data_oecd_frame = Environment_Performance_IP_norm %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country, country_text_id, year, ends_with("oecd"))) %>% 
  right_join(NA_frame_env_oecd, by=c("country_text_id", "year"))
  

# WDI
fa_data_wdi_frame = Environment_Performance_IP_norm %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country_text_id, year, ends_with("wdi"))) %>% 
  rename(air_wdi_env = greenhouse_ugdp_wdi) %>% 
  mutate(air_wdi_env = inverser(air_wdi_env))


write.csv(fa_data_wdi_frame, file="Datasets/performance_data/ImputedDatasets/performance_wdi_env.csv", row.names = F, fileEncoding ="UTF-8")


