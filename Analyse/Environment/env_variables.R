# Environment Performance


per_capita_maker = function(x, pop) {
  # no values which are exactly 0
  (x+(1/2))/(pop+1)
}
library(OECD)

oecd_ghg_data = get_dataset("AIR_GHG")

oecd_air_data = get_dataset("AIR_EMISSIONS")


oecd_ghg_data %>% 
  filter(Measure == "In percentage of Gross Domestic Product",
         Source == "Public",
         Branch == "Total",
         `Type of Expenditure` == "Total") %>% 
  dplyr::select(country = Country, year = Year, SOCX = Value) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea"  = "Korea",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  ) %>% 
  arrange(country, year)


  
Environment_Performance = QoC_data %>% 
  select(country_text_id, year,
         greenhouse_oecd = oecd_greenhouse_t1,
         sulphur_oecd = oecd_soxnox_t1a,
         nitrogen_oecd = oecd_soxnox_t1b,
         co2_oecd = oecd_airqty_t1,
         water_oecd = oecd_water_t1b,
         waste_oecd = oecd_waste_t1b,
         population = oecd_evopop_t1,
         greenhouse_wdi_per_capita = wdi_co2,
         GDP_capita = oecd_sizegdp_t1,
         
         population_wdi = wdi_pop,
         GDP_capita_wdi = wdi_gdpcapcur,
         realgdp_pwt = pwt_rgdp
  ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  group_by(country_text_id) %>% 
  filter(year >= 1950) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  
  mutate_at(vars(ends_with("oecd")), funs(int_oecd_per_capita = ./realgdp_pwt))  %>%
  mutate(greenhouse_wdi_per_capita = (greenhouse_wdi_per_capita*population_wdi)/realgdp_pwt) %>% 
  
  #Senegal has a negative number
  mutate(greenhouse_wdi_per_capita = abs(greenhouse_wdi_per_capita)) %>% 
  
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
 
  left_join(dmx_trade_cluster %>%  select(-country, -regions), by=c("country_text_id", "year"))  %>%
  
  # add country and regions
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country_text_id"))  %>%
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  
  select(country, country_text_id, regions, year, everything())  %>% 
  ungroup() %>% 
  dplyr::arrange(country_text_id, year)  


##### NA-Plots ####


Environment_Performance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd_per_capita"), ends_with("wdi_per_capita"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample - Environmental")



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
  select_at(vars(ends_with("oecd_per_capita"),ends_with("wdi_per_capita"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 

Environment_Performance_IP %>% 
  select_at(vars(ends_with("oecd_per_capita"), ends_with("wdi_per_capita"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")  +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Raw Sample")





####


Environment_Performance_IP_norm = Environment_Performance_IP  %>% 
  select_at(vars(ends_with("oecd_per_capita"), ends_with("wdi_per_capita"))) %>% 
  mutate_at(vars("greenhouse_wdi_per_capita",
                 "co2_oecd_int_oecd_per_capita"), funs(trim(., 0.01, minimum=T))) %>%
  mutate_all(funs(folded_ladder_fun(., plotting =F))) %>% 
  mutate_all(scale)



Environment_Performance_IP_norm %>% 
  select_at(vars(ends_with("oecd_per_capita"), ends_with("wdi_per_capita"))) %>% 
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
  select(-greenhouse_wdi_per_capita) %>% 
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
  select_at(vars(country, country_text_id, year, ends_with("oecd_per_capita"))) %>% 
  right_join(NA_frame_env_oecd, by=c("country_text_id", "year"))
  

# WDI
fa_data_wdi_frame = Environment_Performance_IP_norm %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country, country_text_id, year, ends_with("wdi_per_capita"))) %>% 
  rename(environment_wdi_index = greenhouse_wdi_per_capita) %>% 
  mutate(environment_wdi_index = inverser(environment_wdi_index))



