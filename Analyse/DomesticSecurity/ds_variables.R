### Performance Domestic Security ####
source("Analyse/Cluster_v3.R")


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)




cty_identifier = V_dem %>% 
  select(country, country_text_id) %>% 
  group_by(country) %>% 
  slice(1)


# Getting the Data 

library(readstata13)
protest_data = read.table("Datasets/mmALL_020619_v15.tab", sep="\t", header=T)

test = protest_data %>% 
  group_by(country, year) %>% 
  summarise(sum(protesterviolence, na.rm=T), sum(protest, na.rm=T))


UNODC_burglary = fread("Datasets/UNODC_burglary.csv") %>% 
  rename(burg_count = Count, 
         burg_rate = Rate,
         year =Year, 
         country = Country) %>% 
  arrange(country, year) %>% 
  select(-Region, -"Sub Region") %>% 
  mutate(country = fct_recode(country,
                              "Tanzania" = "United Republic of Tanzania",
                              "Hong Kong" = "Hong Kong Special Administrative Region of China",
                              "Ivory Coast" = "Cote d'Ivoire",
                              "Macedonia" = "The former Yugoslav Rep. of Macedonia",
                              "Syria" = "Syrian Arab Republic",
                              "Russia" = "Russian Federation",
                              "Moldova" = "Republic of Moldova",
                              "South Korea" = "Republic of Korea",
                              "Burma/Myanmar"	= "Myanmar",
                              "Kosovo" = "Kosovo under UNSCR 1244",
                              "Iraq" = "Iraq (Central Iraq)")) %>% 
  filter(!grepl("United Kingdom",country)) %>% 
  bind_rows(fread("Datasets/UNODC_burglary.csv") %>% 
              rename(burg_count = Count, 
                     burg_rate = Rate,
                     year =Year, 
                     country = Country) %>% 
              filter(grepl("United Kingdom",country)) %>% 
              group_by(year) %>% 
              summarise(burg_count = mean(burg_count)) %>% 
              mutate(country = "United Kingdom") ) %>% 
  left_join(cty_identifier, by=c("country")) %>% 
  filter(is.na(country_text_id) == F) %>% 
  arrange(country, year) %>% 
  select(-country)




UNODC_prison = fread("Datasets/UNODC_prison.csv") %>% 
  rename(incarcer_count = Count, 
         incarcer_rate = Rate,
         year =Year, 
         country = Countries)  %>% 
  arrange(country, year) %>% 
  select(-Region, -"Sub Region") %>% 
  mutate(country = fct_recode(country,
                              "Tanzania" = "United Republic of Tanzania",
                              "Hong Kong" = "China, Hong Kong Special Administrative Region",
                              "Ivory Coast" = "Côte d'Ivoire",
                              "Macedonia" = "North Macedonia",
                              "Syria" = "Syrian Arab Republic",
                              "Russia" = "Russian Federation",
                              "Moldova" = "Republic of Moldova",
                              "South Korea" = "Republic of Korea",
                              "Burma/Myanmar"	= "Myanmar",
                              "Kosovo" = "Kosovo under UNSCR 1244",
                              "Laos" = "Lao People's Democratic Republic",
                              "Iraq" = "Iraq (Central Iraq)",
                              "Iran" = "Iran (Islamic Republic of)",
                              "Czech Republic" = "Czech republic",
                              "Republic of Vietnam" = "Viet Nam",
                              "Venezuela" = "Venezuela (Bolivarian Republic of)",
                              "Democratic Republic of Congo"= "Democratic Republic of the Congo",
                              "Bolivia" = "Bolivia (Plurinational State of)",
                              "Cape Verde" = "Cabo Verde",
                              "Republic of the Congo" = "Congo",
                              "São Tomé and Príncipe" = "Sao Tome and Principe")) %>% 
  filter(!grepl("United Kingdom",country)) %>% 
  bind_rows(fread("Datasets/UNODC_prison.csv") %>% 
              rename(incarcer_count = Count, 
                     incarcer_rate = Rate,
                     year =Year, 
                     country = Countries) %>% 
              filter(grepl("United Kingdom",country)) %>% 
              group_by(year) %>% 
              summarise(incarcer_count = mean(incarcer_count, na.rm=T)) %>% 
              mutate(country = "United Kingdom") ) %>% 
  left_join(cty_identifier, by=c("country")) %>% 
  filter(is.na(country_text_id) == F) %>% 
  arrange(country, year) %>% 
  select(-country)


UNODC_cjs_process = fread("Datasets/UNODC_cjs_process.csv") %>% 
  filter(Status == "Persons convicted", Group=="Total") %>% 
  rename(convictions_count = Value, 
         convictions_rate = Rate,
         year =Year, 
         country = Country)  %>% 
  arrange(country, year) %>% 
  select(-Region, -"Sub Region", -Group, -Status) %>% 
  mutate(country = fct_recode(country,
                              "Hong Kong" = "China, Hong Kong Special Administrative Region",
                              "Macedonia" = "The former Yugoslav Republic of Macedonia",
                              "Syria" = "Syrian Arab Republic",
                              "Russia" = "Russian Federation",
                              "Moldova" = "Republic of Moldova",
                              "South Korea" = "Republic of Korea",
                              "Kosovo" = "Kosovo under UNSCR 1244",
                              "Iraq" = "Iraq (Central Iraq)",
                              "Bolivia" = "Bolivia (Plurinational State of)",
                              "Czech Republic" = "Czechia"
  )) %>% 
  filter(!grepl("United Kingdom",country)) %>% 
  bind_rows(fread("Datasets/UNODC_cjs_process.csv") %>% 
              filter(Status == "Persons convicted", Group=="Total") %>% 
              rename(convictions_count = Value, 
                     convictions_rate = Rate,
                     year =Year, 
                     country = Country) %>% 
              filter(grepl("United Kingdom",country)) %>% 
              group_by(year) %>% 
              summarise(convictions_count = mean(convictions_count, na.rm=T)) %>% 
              mutate(country = "United Kingdom") ) %>% 
  left_join(cty_identifier, by=c("country")) %>% 
  filter(is.na(country_text_id) == F) %>% 
  arrange(country, year) %>% 
  select(-country)


UNODC_robbery = fread("Datasets/UNODC_robbery.csv") %>% 
  rename(rob_count = Count, 
         rob_rate = Rate,
         year =Year, 
         country = Country) %>% 
  select(-Region, -"Sub Region") %>% 
  mutate(country = fct_recode(country,
                              "Tanzania" = "United Republic of Tanzania",
                              "Hong Kong" = "Hong Kong Special Administrative Region of China",
                              "Ivory Coast" = "Cote d'Ivoire",
                              "Macedonia" = "North Macedonia",
                              "Syria" = "Syrian Arab Republic",
                              "Russia" = "Russian Federation",
                              "Moldova" = "Republic of Moldova",
                              "South Korea" = "Republic of Korea",
                              "Burma/Myanmar"	= "Myanmar",
                              "Kosovo" = "Kosovo under UNSCR 1244")) %>% 
  filter(!grepl("United Kingdom",country)) %>% 
  bind_rows(fread("Datasets/UNODC_robbery.csv") %>% 
              rename(rob_count = Count, 
                     rob_rate = Rate,
                     year =Year, 
                     country = Country) %>% 
              filter(grepl("United Kingdom",country)) %>% 
              group_by(year) %>% 
              summarise(rob_count = mean(rob_count)) %>% 
              mutate(country = "United Kingdom") ) %>% 
  left_join(cty_identifier, by=c("country")) %>% 
  filter(is.na(country_text_id) == F) %>% 
  arrange(country, year)  %>% 
  select(-country)


UNODC_homicide = fread("Datasets/UNODC_homicide.csv", header=T) %>% 
  gather(year, homicide, "1990":"2017") %>% 
  spread(Indicator, homicide) %>% 
  rename(hom_count = "Homicide Total Count", 
         hom_rate = "Homicide Rate", 
         country = Country) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(country = fct_recode(country,
                              "Tanzania" = "United Republic of Tanzania",
                              "Hong Kong" = "China, Hong Kong Special Administrative Region",
                              "Ivory Coast" = "Cote d'Ivoire",
                              "Macedonia" = "The former Yugoslav Republic of Macedonia",
                              "Syria" = "Syrian Arab Republic",
                              "Russia" = "Russian Federation",
                              "Moldova" = "Republic of Moldova",
                              "South Korea" = "Republic of Korea",
                              "Burma/Myanmar"	= "Myanmar",
                              "Kosovo" = "Kosovo under UNSCR 1244",
                              "Republic of Vietnam" = "Viet Nam",
                              "Venezuela" = "Venezuela (Bolivarian Republic of)",
                              "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
                              "São Tomé and Príncipe" = "Sao Tome and Principe",
                              "Laos" = "Lao People's Democratic Republic",
                              "Iraq" = "Iraq (Central Iraq)",
                              "Iran" = "Iran (Islamic Republic of)",
                              "The Gambia" = "Gambia",
                              "Democratic Republic of Congo"= "Democratic Republic of the Congo",
                              "Bolivia" = "Bolivia (Plurinational State of)",
                              "Cape Verde" = "Cabo Verde",
                              "Republic of the Congo" = "Congo")) %>% 
  filter(!grepl("United Kingdom \\(",country)) %>% 
  left_join(cty_identifier, by=c("country")) %>% 
  filter(is.na(country_text_id) == F) %>% 
  arrange(country, year) 


UNODC_data = UNODC_homicide %>% 
  left_join(UNODC_burglary, by=c("country_text_id", "year")) %>% 
  left_join(UNODC_robbery, by=c("country_text_id", "year")) %>% 
  left_join(UNODC_prison, by=c("country_text_id", "year")) %>% 
  left_join(UNODC_cjs_process, by=c("country_text_id", "year")) %>% 
  left_join(QoC_data %>% select(country_text_id, year, total_pop_wdi = wdi_pop) %>%  na.omit(), by=c("country_text_id", "year")) %>% 
  mutate(total_pop_wdi = total_pop_wdi/100000) %>% 
  mutate(hom_rate_unodc = (hom_count+0.5)/(total_pop_wdi+1),
         burg_rate_unodc = (burg_count+0.5)/(total_pop_wdi+1),
         rob_rate_unodc = (rob_count+0.5)/(total_pop_wdi+1),
         incarc_ratio_unodc = (incarcer_count+0.5)/(convictions_count+1)) %>% 
  select(country_text_id, year, hom_rate_unodc, burg_rate_unodc, rob_rate_unodc,incarc_ratio_unodc)




OECD_gov = fread("Datasets/oecd_generalgov.csv") %>% 
  filter(SUBJECT == "PUBORD",
         MEASURE == "PC_GDP") %>% 
  select(country_text_id = LOCATION,
         year = TIME,
         order_safety_gdp_oecd = Value)  %>% 
  mutate(order_safety_gdp_oecd = order_safety_gdp_oecd/100)


### 

domestic_security =  QoC_data %>% 
  select(country_text_id, year, ccode) %>% # ccode??
  group_by(country_text_id, year) %>% 
  slice(1) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year"))  %>%
  left_join(OECD_gov, by=c("country_text_id", "year")) %>% 
  left_join(UNODC_data, by=c("country_text_id", "year")) %>%
  left_join(QoC_data %>% select(country_text_id, year, 
                                internalwar_ucdp = ucdp_type3, 
                                psnv_wgi = wbgi_pve) %>% 
              filter_at(vars(psnv_wgi, internalwar_ucdp), any_vars(is.na(.)==F)), by=c("country_text_id", "year")) %>% 
  select(country, country_text_id, regions, year, everything())  %>% 
  filter(year > 1950) %>% 
  group_by(country_text_id) %>% 
  mutate(country = unique(na.omit(country)),
         regions = unique(na.omit(regions))) %>%
  ungroup() %>% 
  arrange(country_text_id, year)  %>% 
  mutate(internalwar_ucdp = ifelse(is.na(internalwar_ucdp) == T, 0, internalwar_ucdp),
         internalwar_bin_ucdp = ifelse(internalwar_ucdp >= 1, 1, 0))


##### NA-Plots ####


domestic_security %>% 
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
  ggtitle("Missings in Democracy Profile Sample - WDI")

domestic_security %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("unodc"))) %>% 
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


domestic_security %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("ucdp"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - UCDP")


domestic_security %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("wgi"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - WGI")


##### Linear Interpolation


domestic_security_IP = domestic_security %>%
  group_by(country_text_id)  %>% 
  mutate_at(vars(ends_with("_unodc")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("_oecd")), .funs = list(~na_interpol(.))) %>% 
  #mutate_at(vars(ends_with("_ucdp")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("_wgi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()


domestic_security_IP %>% 
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
  ggtitle("Missings in Democracy Profile Sample - WDI")


domestic_security_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("unodc"))) %>% 
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

domestic_security_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("ucdp"))) %>% 
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

domestic_security_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("wgi"))) %>% 
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


###



domestic_security_IP %>% 
  select_at(vars(ends_with("oecd"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 

domestic_security_IP %>% 
  select_at(vars(ends_with("unodc"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

domestic_security_IP %>% 
  select_at(vars(ends_with("ucdp"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


domestic_security_IP %>% 
  select_at(vars(ends_with("wgi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


####

# WGI: its normalized already
domestic_security_IP_norm = domestic_security_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("unodc"), ends_with("ucdp"), ends_with("wgi"))) %>%
  mutate_at(vars(ends_with("oecd")), ~folded_ladder_fun(., plotting=T)) %>% 
  mutate_at(vars(ends_with("unodc")), ladder_fun) %>% 
  mutate_at(vars(ends_with("oecd"), ends_with("unodc"), ends_with("wgi")), scale)
  

domestic_security_IP_norm %>% 
  select_at(vars(ends_with("oecd"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 

domestic_security_IP_norm %>% 
  select_at(vars(ends_with("ucdp"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

domestic_security_IP_norm %>% 
  select_at(vars(ends_with("unodc"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

domestic_security_IP_norm %>% 
  select_at(vars(ends_with("wgi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


### NA Frame: OECD
NA_frame_ds_oecd = domestic_security_IP_norm %>% 
  select_at(vars(-burg_rate_unodc, -ends_with("ucdp"), -ends_with("wgi"))) %>% 
  mutate(non_na_perc = rowSums(is.na(.)==F)/dim(.)[2]) %>% 
  bind_cols(domestic_security %>%  select(country, country_text_id, year)) %>%
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select(country_text_id, year, non_na_perc)


#### Factor Analysis: Transfer to MI

fa_data_ds_frame = domestic_security_IP_norm %>% 
  bind_cols(domestic_security %>%  select(country, country_text_id, year)) %>% 
  select(-burg_rate_unodc) %>% 
  select_at(vars(-ends_with("ucdp"), -ends_with("wgi"))) %>% 
  select(country, country_text_id, year, everything()) %>% 
  right_join(NA_frame_ds_oecd, by=c("country_text_id", "year"))
  

wgi_data_ds_frame = domestic_security_IP_norm %>% 
  bind_cols(domestic_security %>%  select(country, country_text_id, year)) %>% 
  select(country_text_id, year, ds_order_index = psnv_wgi) 

###



