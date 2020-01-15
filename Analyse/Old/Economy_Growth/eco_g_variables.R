# Economic Performance
if (clustersetup == T) {
source("Analyse/Cluster_v3.R")
}


Economy_Perfomance = QoC_data %>% 
  select(country_text_id, year,
         GDP_growth_oecd = oecd_evogdp_t1,
         Inflation_oecd = oecd_cpi_t1a,
         Interest_oecd = oecd_ltintrst_t1,
         Balance_oecd = oecd_bop_t1,
         Unemployment_pr_oecd = oecd_unemplrt_t1c,
         Invest_oecd = oecd_invrates_t1,
         
         GDP_growth_wdi = wdi_gdpgr,
         Debt_wdi = wdi_debt,
         Inflation_wdi = wdi_inflation,
         Interest_wdi = wdi_intrate,
         Unemployment_pr_wdi = wdi_unempilo
  ) %>% 
  mutate_at(vars(matches("_pr_")), funs(./100)) %>% 
  # Contra Deflation 
  mutate_at(vars(starts_with("Inflation")), funs(abs(.))) %>% 

  filter_if(is.double, any_vars(!is.na(.))) %>% 
  group_by(country_text_id) %>% 
  filter(year >= 1950) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster %>%  select(-country, -regions), by=c("country_text_id", "year"))  %>%
  
  # add country and regions
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country_text_id"))  %>%
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  
  select(country, country_text_id, regions, year, everything())  %>% 
  ungroup() %>%  
  dplyr::arrange(country_text_id, year)  


##### NA-Plots ####


Economy_Perfomance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("wdi"))) %>% 
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

Economy_Perfomance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd"))) %>% 
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

Economy_Perfomance_IP = Economy_Perfomance %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("oecd")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()


Economy_Perfomance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("wdi"))) %>% 
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

Economy_Perfomance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd"))) %>% 
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


Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"),ends_with("wdi"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 


Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

####

Economy_Perfomance_IP_norm = Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("wdi"))) %>%
  mutate_at(vars("Inflation_oecd",
                 "Invest_oecd",
                 "Balance_oecd",
                 "GDP_growth_oecd",
                 "Debt_wdi",
                 "GDP_growth_wdi",
                 "Inflation_wdi", 
                 "Interest_wdi"), funs(trim(., 0.01, minimum=T))) %>% 
  mutate_at(vars(ends_with("oecd"), ends_with("wdi"), -matches("_pr")), funs(ladder_fun(.))) %>% 
  mutate_at(vars(matches("_pr_")), funs(folded_ladder_fun(., plotting =T))) %>% 
  mutate_all(scale)

Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("oecd"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")



### NA Frame: OECD
NA_frame_oecd = Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("oecd"))) %>%
  mutate(non_na_perc = rowSums(is.na(.)==F)/dim(.)[2]) %>% 
  bind_cols(Economy_Perfomance %>%  select(country, country_text_id, year)) %>%
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select(country_text_id, year, non_na_perc)
  


#### Factor Analysis: Transfer to MI
# OECD
fa_data_oecd_frame = Economy_Perfomance_IP_norm %>% 
  bind_cols(Economy_Perfomance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country, country_text_id, year, ends_with("oecd"), ends_with("wdi"))) %>% 
  right_join(NA_frame_oecd, by=c("country_text_id", "year"))
  



