# Environment Performance


per_capita_maker = function(x, pop) {
  # no values which are exactly 0
  (x+(1/2))/(pop+1)
}


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
         GDP_capita_wdi = wdi_gdpcapcur
  ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  group_by(country_text_id) %>% 
  filter(year >= 1950) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  
  mutate_at(vars(ends_with("oecd")), funs(int_oecd_per_capita = per_capita_maker(., GDP_capita_wdi*population_wdi)))  %>%
  mutate(greenhouse_wdi_per_capita = (greenhouse_wdi_per_capita*population_wdi)/(GDP_capita_wdi*population_wdi)) %>% 
  
  #Senegal as negative number
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
  select_at(vars(ends_with("wdi_per_capita"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - WDI")

Environment_Performance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd_per_capita"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - OECD")



#### Linear Interpolation ####

Environment_Performance_IP = Environment_Performance %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("oecd_per_capita")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("wdi_per_capita")), .funs = list(~na_interpol(.))) %>% 
  ungroup()


Environment_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("wdi_per_capita"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - WDI (After Linear Interpolation)")

Environment_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd_per_capita"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - OECD (After Linear Interpolation)")



######



Environment_Performance_IP %>% 
  select_at(vars(ends_with("oecd_per_capita"),ends_with("wdi_per_capita"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 


Environment_Performance_IP %>% 
  select_at(vars(ends_with("oecd_per_capita"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


Environment_Performance_IP %>% 
  select_at(vars(ends_with("wdi_per_capita"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

####


Environment_Performance_IP_norm = Environment_Performance_IP  %>% 
  select_at(vars(ends_with("oecd_per_capita"), ends_with("wdi_per_capita"))) %>%
  mutate_all(funs(folded_ladder_fun(., plotting =T))) %>% 
  mutate_all(scale)



Environment_Performance_IP_norm %>% 
  select_at(vars(ends_with("oecd_per_capita"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


Environment_Performance_IP_norm %>% 
  select_at(vars(ends_with("wdi_per_capita"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


### NA Frame: OECD
NA_frame_env_oecd = Environment_Performance_IP_norm %>% 
  select(-greenhouse_wdi_per_capita) %>% 
  mutate(non_na_perc = rowSums(is.na(.)==F)/dim(.)[2]) %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>%
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select(country_text_id, year, non_na_perc)
  


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



