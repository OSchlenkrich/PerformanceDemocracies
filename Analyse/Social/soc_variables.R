# Social Performance

Integration_Performance = QoC_data %>% 
  select(country_text_id, year,
         #Combined_GI = sc_tgen,
         Unemployment_t_GI = sc_uegen,
         Pension_t_GI = sc_pgen,
         Sickness_t_GI = sc_skgen,
         
         Unemployment_s_rr_GI = sc_ue,
         Unemployment_f_rr_GI = sc_uef,
         
         Sickness_s_rr_GI = sc_sick,
         Sickness_f_rr_GI = sc_sickf,
         
         Pension_s_rr_GI = sc_sp,
         Pension_c_rr_GI = sc_spc,
         
         gini_lis = lis_gini,
         poverty9010_lis = lis_pr9010,
         poverty8020_lis = lis_pr8020,
         
         gini_wdi = wdi_gini
  )  %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  mutate(gini_wdi = gini_wdi/100) %>% 
  left_join(V_dem %>%  select( country_text_id, year, v2dlunivl_vdem = v2dlunivl), by=c("country_text_id", "year"))  %>%
  # Sample
  filter(year > 1950) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  
  left_join(dmx_trade_cluster %>%  select(-country, -regions), by=c("country_text_id", "year"))  %>%
  
  # add country and regions
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country_text_id"))  %>%
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  
  select(country, country_text_id, regions, year, everything())  %>% 
  ungroup() %>% 
  arrange(country_text_id, year) 


##### NA-Plots ####

Integration_Performance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_GI") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - CWED 2")


Integration_Performance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_lis") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - LIS")

Integration_Performance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_wdi") )) %>% 
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

Integration_Performance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_vdem") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - VDEM")


#### Linear Interpolation ####


Integration_Performance_IP = Integration_Performance %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("GI")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("lis")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol(.))) %>%
  mutate_at(vars(ends_with("vdem")), .funs = list(~na_interpol(.))) %>% 
  ungroup()



Integration_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_GI") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - CWED 2")


Integration_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_lis") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - LIS")

Integration_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_wdi") )) %>% 
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

Integration_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_vdem") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - VDEM")

###

Integration_Performance_IP %>% 
  select_at(vars(ends_with("_GI"),ends_with("_lis"),ends_with("_wdi"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 


Integration_Performance_IP %>% 
  select_at(vars(ends_with("_GI"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 

Integration_Performance_IP %>% 
  select_at(vars(ends_with("_lis"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 

Integration_Performance_IP %>% 
  select_at(vars(ends_with("_wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


####

Integration_Performance_IP_norm = Integration_Performance_IP %>% 
  mutate_at(vars(ends_with("_GI")), ~ladder_fun(.)) %>% 
  mutate_at(vars(poverty8020_lis, poverty9010_lis), ~ladder_fun(.)) %>% 
  mutate_at(vars(gini_lis), ~folded_ladder_fun(.)) %>% 
  mutate_at(vars(ends_with("_wdi")), ~folded_ladder_fun(.)) %>% 
  mutate_at(vars(ends_with("_GI"), ends_with("_lis"), ends_with("_wdi"), ends_with("_vdem")), ~scale(.))



Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_GI"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_lis"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_vdem"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

### NA Frame: OECD
NA_frame_soc_oecd = Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("t_GI"), ends_with("_lis"))) %>% 
  mutate(non_na_perc = rowSums(is.na(.)==F)/dim(.)[2]) %>% 
  bind_cols(Integration_Performance %>%  dplyr::select(country, country_text_id, year)) %>%
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  dplyr::select(country_text_id, year, non_na_perc)


#### Factor Analysis: Transfer to MI

fa_data_soc_frame = Integration_Performance_IP_norm %>% 
  bind_cols(Integration_Performance %>%  dplyr::select(country, country_text_id, year)) %>%
  select_at(vars(-ends_with("rr_GI"))) %>% 
  select_at(vars(country, country_text_id, year, 
                 ends_with("t_GI"), 
                 ends_with("_lis"), 
                 ends_with("_wdi"),
                 ends_with("_vdem"))) %>% 
  right_join(NA_frame_soc_oecd, by=c("country_text_id", "year"))

###


