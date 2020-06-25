# Social Performance

V_dem_equal = V_dem_all_v9 %>% 
  select(country_text_id, year, 
         pub_serv_gender_vdem = v2peapsgen, 
         pub_serv_social_vdem = v2peapssoc,
         v2dlunivl_vdem = v2dlunivl) %>% 
  filter(year >= 1900)

Integration_Performance = QoC_data %>% 
  select(country_text_id, year,
         Combined_t_GI = sc_tgen,
         # Unemployment_t_GI = sc_uegen,
         # Pension_t_GI = sc_pgen,
         # Sickness_t_GI = sc_skgen,
         
         # Unemployment_s_rr_GI = sc_ue,
         # Unemployment_f_rr_GI = sc_uef,
         # 
         # Sickness_s_rr_GI = sc_sick,
         # Sickness_f_rr_GI = sc_sickf,
         # 
         # Pension_s_rr_GI = sc_sp,
         # Pension_c_rr_GI = sc_spc,
         
         gini_lis = lis_gini,
         poverty9010_lis = lis_pr9010,
         poverty8020_lis = lis_pr8020,
         
         femlabor_wdi = wdi_lfpf, 
         gini_wdi = wdi_gini,
         schools_gender_wdi = wdi_gersf,
         # schoolt_gender_wdi = wdi_gertf,
         
  )  %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  
  mutate(gini_wdi = gini_wdi/100) %>% 

  # Sample
  filter(year > 1950) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
   
  left_join(V_dem_equal, by=c("country_text_id", "year")) %>% 
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
  select_at(vars(ends_with("_GI"), ends_with("_vdem"), ends_with("_lis"), ends_with("_wdi") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - Social Outcomes")




#### Linear Interpolation ####

Integration_Performance_IP = Integration_Performance %>%
  group_by(country_text_id) %>% 
  # mutate_at(vars(ends_with("GI")), .funs = list(~na_interpol2(.))) %>% 
  mutate_at(vars(ends_with("lis")), .funs = list(~na_interpol2(., 5))) %>% 
  mutate_at(vars(gini_wdi), .funs = list(~na_interpol2(., 5))) %>%
  # mutate_at(vars(ends_with("vdem")), .funs = list(~na_interpol2(.))) %>% 
  ungroup()


Integration_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_GI"), ends_with("_vdem"), ends_with("_lis"), ends_with("_wdi") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - Social Outcomes")


###

Integration_Performance_IP %>% 
  select_at(vars(ends_with("_GI"),ends_with("_lis"),ends_with("_wdi"),ends_with("_vdem"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 


Integration_Performance_IP %>% 
  select_at(vars(ends_with("_GI"),ends_with("_lis"),ends_with("_wdi"),ends_with("_vdem"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 

Integration_Performance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_GI"), ends_with("_vdem"), ends_with("_lis"), ends_with("_wdi") )) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - Social Outcomes")

####

Integration_Performance_IP_norm = Integration_Performance_IP %>% 
  mutate_at(vars(femlabor_wdi, gini_lis), funs(trim(., 0.01, minimum=T))) %>% 
#  mutate_at(vars(Sickness_t_GI, Unemployment_t_GI, femlabor_wdi, gini_lis), funs(trim(., 0.01, minimum=T))) %>% 
  mutate_at(vars(ends_with("_GI"), poverty8020_lis, poverty9010_lis, femlabor_wdi), ~ladder_fun(.)) %>% 
  mutate_at(vars(gini_lis, gini_wdi), ~folded_ladder_fun(.)) %>% 
  mutate_at(vars(ends_with("_GI"), ends_with("_lis"), ends_with("_wdi"), ends_with("_vdem")), ~scale(.))



Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_GI"),ends_with("_lis"),ends_with("_wdi"),ends_with("_vdem"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")



### NA Frame: OECD
NA_frame_soc_oecd = Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("t_GI"), ends_with("_lis"), ends_with("_wdi"), ends_with("_vdem"))) %>% 
  mutate_all(funs(is_na = ifelse(is.na(.)==T, 1,0))) %>% 
  bind_cols(Integration_Performance %>%  dplyr::select(country, country_text_id, year)) %>%
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select_at(vars(country_text_id, year, ends_with("is_na"))) %>% 
  mutate(missing_SUM = rowSums(select(., matches("t_GI"))) + rowSums(select(., matches("gini_wdi")))  )


#### Factor Analysis: Transfer to MI

fa_data_soc_frame = Integration_Performance_IP_norm %>% 
  select_at(vars(-ends_with("rr_GI"))) %>% 
  select_at(vars(country, country_text_id, year, 
                 ends_with("t_GI"), 
                 ends_with("_lis"), 
                 ends_with("_wdi"),
                 ends_with("_vdem"))) %>% 
  right_join(NA_frame_soc_oecd, by=c("country_text_id", "year"))

###


