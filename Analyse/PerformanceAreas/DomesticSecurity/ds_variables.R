### Performance Domestic Security ####


cty_identifier = V_dem %>% 
  select(country, country_text_id) %>% 
  group_by(country) %>% 
  slice(1) %>% 
  ungroup()


# Getting the Data 
missingvalues = function(x) {
  x = ifelse(x == -9 | x == 9999999, NA, x)
  return(x)
}
  
recode_unocd = . %>% 
  mutate(country = fct_recode(country,
                              "bosnia and herzegovina" = "bosnia&herzegovina",
                              "central african republic"	= "central african rep",
                              "colombia" = "columbia",
                              "ivory coast" = "cote d'ivoire",
                              "united states of america" = "usa",
                              "dominican republic"	= "dominican rep",
                              "dominican republic" = "dominican rep.",
                              "the gambia" = "gambia",
                              "hong kong" = "hong kong, china (sar)",
                              "kazakhstan" = "kazakstan",
                              "south korea" = "rep of korea",
                              "south korea" = "korea, rep. of",
                              "south korea" = "korea, republic of",
                              "laos" = "lao people dem rep",
                              "macedonia" = "macedonia, fyr",
                              "macedonia" = "rep of macedonia",
                              "moldova" = "moldova, rep. of",
                              "moldova" = "moldova, republic of",
                              "moldova" = "rep of moldova",
                              "burma/myanmar"	= "myanmar",
                              "namibia" = "nambia",
                              "são tomé and príncipe" = "sao tome&principe",
                              "slovakia" = "slovak republic",
                              "trinidad and tobago" = "trinidad&tobago",
                              "russia" = "russian federation",
                              "seychelles" = "seychilles",
                              "syria" = "syrian arab rep",
                              "syria" = "syrian arab republic",
                              "tajikistan" = "tajikstan",
                              "tanzania" = "tanzania, u. rep. of",
                              "tanzania" = "united rep of tanzania",
                              "United Kingdom (England and Wales)" = "england & wales",
                              "United Kingdom (Northern Ireland)" = "uk - northern ireland",
                              "United Kingdom (Northern Ireland)" = "northern ireland",
                              "United Kingdom (Scotland)" = "scotland",
                              "United Kingdom (Scotland)" = "uk - scotland")  
  )

unocd_w4 = read_dta("Datasets/26462-0004-Data.dta")

unocd_w6 = read_dta("Datasets/26462-0006-Data.dta") %>% 
  select(country = CON_NAME,
         TFTPOL90,
         TFTPOL91,
         TFTPOL92,
         TFTPOL93,
         TFTPOL94,
         BURPOL90,
         BURPOL91,
         BURPOL92,
         BURPOL93,
         BURPOL94
         ) %>% 
  mutate(country = tolower(country)) %>% 
  recode_unocd %>% 
  mutate_if(is.numeric, funs(missingvalues(.))) %>% 
  pivot_longer(cols=matches("POL")) %>% 
  mutate(variable = name,
         variable = ifelse(grepl("TFT", name), "theft", variable),
         variable = ifelse(grepl("BUR", name), "burglarly", variable),
         name = gsub("TFTPOL", "19", name),
         name = gsub("BURPOL", "19", name),
         name = as.numeric(name)) 


unocd_w7 = read_dta("Datasets/26462-0007-Data.dta") %>% 
  select(country = COUNTRY,
         TFTPOL95,
         TFTPOL96,
         TFTPOL97,
         BURPOL95,
         BURPOL96,
         BURPOL97) %>% 
  mutate(country = tolower(country)) %>% 
  recode_unocd %>% 
  mutate_if(is.numeric, funs(missingvalues(.))) %>% 
  pivot_longer(cols=matches("POL")) %>% 
  mutate(variable = name,
         variable = ifelse(grepl("TFT", name), "theft", variable),
         variable = ifelse(grepl("BUR", name), "burglarly", variable),
         name = gsub("TFTPOL", "19", name),
         name = gsub("BURPOL", "19", name),
         name = as.numeric(name)) 



unocd_w8 = read_dta("Datasets/26462-0008-Data.dta") %>% 
  select(country = COUNTRY,
         TFTPOL98,
         TFTPOL99,
         TFTPOL2000,
         BURPOL98,
         BURPOL99,
         BURPOL2000) %>% 
  mutate(country = tolower(country))  %>% 
  recode_unocd  %>% 
  mutate_if(is.numeric, funs(missingvalues(.))) %>% 
  pivot_longer(cols=matches("POL")) %>% 
  mutate(variable = name,
         variable = ifelse(grepl("TFT", name), "theft", variable),
         variable = ifelse(grepl("BUR", name), "burglarly", variable),
         
         name = gsub("TFTPOL9", "199", name),
         name = gsub("BURPOL9", "199", name),
         
         name = gsub("TFTPOL2000", "2000", name),
         name = gsub("BURPOL2000", "2000", name),
         
         name = as.numeric(name)) 

unocd_w9 = read_dta("Datasets/26462-0009-Data.dta")  %>% 
  select(country = COUNTRY,
         TFTPOL2001,
         TFTPOL2002,
         BURPOL2001,
         BURPOL2002) %>% 
  mutate(country = tolower(country)) %>% 
  recode_unocd   %>% 
  mutate_if(is.numeric, funs(missingvalues(.))) %>% 
  pivot_longer(cols=matches("POL")) %>% 
  mutate(variable = name,
         variable = ifelse(grepl("TFT", name), "theft", variable),
         variable = ifelse(grepl("BUR", name), "burglarly", variable),
         
         name = gsub("TFTPOL", "", name),
         name = gsub("BURPOL", "", name),
         name = as.numeric(name)) 



unocd_theft_1990_2002 =  unocd_w6 %>% 
  bind_rows(unocd_w7) %>% 
  bind_rows(unocd_w8)%>% 
  bind_rows(unocd_w9) %>% 
  filter(variable == "theft") %>% 
  left_join(cty_identifier %>% 
              mutate(country = tolower(country)), by="country") %>% 
  select(-country, -variable) %>% 
  rename(year = name, theft_count = value )

unocd_burg_1990_2002 =  unocd_w6 %>% 
  bind_rows(unocd_w7) %>% 
  bind_rows(unocd_w8)%>% 
  bind_rows(unocd_w9) %>% 
  filter(variable == "burglarly") %>% 
  left_join(cty_identifier %>% 
              mutate(country = tolower(country)), by="country") %>% 
  select(-country, -variable) %>% 
  rename(year = name, burg_count = value )

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
  select(-country) %>% 
  bind_rows(unocd_burg_1990_2002)




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

UNODC_theft = fread("Datasets/UNODC_theft.csv") %>% 
  rename(theft_count = Count, 
         theft_rate = Rate,
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
  bind_rows(fread("Datasets/UNODC_theft.csv") %>% 
              rename(theft_count = Count, 
                     theft_rate = Rate,
                     year =Year, 
                     country = Country) %>% 
              filter(grepl("United Kingdom",country)) %>% 
              group_by(year) %>% 
              summarise(theft_count = mean(theft_count)) %>% 
              mutate(country = "United Kingdom") ) %>% 
  left_join(cty_identifier, by=c("country")) %>% 
  filter(is.na(country_text_id) == F) %>% 
  arrange(country, year)  %>% 
  select(-country) %>% 
  bind_rows(unocd_theft_1990_2002)


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
  #left_join(UNODC_prison, by=c("country_text_id", "year")) %>% 
  #left_join(UNODC_cjs_process, by=c("country_text_id", "year")) %>% 
  left_join(UNODC_theft, by=c("country_text_id", "year")) %>% 
  left_join(QoC_data %>% select(country_text_id, year, total_pop_wdi = wdi_pop) %>%  na.omit(), by=c("country_text_id", "year")) %>% 
  mutate(total_pop_wdi = total_pop_wdi/100000) %>% 
  mutate(hom_rate_unodc = (hom_count+0.5)/(total_pop_wdi+1),
         burg_rate_unodc = (burg_count+0.5)/(total_pop_wdi+1),
         rob_rate_unodc = (rob_count+0.5)/(total_pop_wdi+1),
         theft_rate_unodc = (theft_count+0.5)/(total_pop_wdi+1),
         crime_rate_unodc = (theft_count + burg_count +0.5)/(total_pop_wdi+1),
         #incarc_ratio_unodc = (incarcer_count+0.5)/(convictions_count+1)
         ) %>% 
  select(country_text_id, year, hom_rate_unodc, theft_rate_unodc, burg_rate_unodc, crime_rate_unodc)


imf_gdp= fread("Datasets/imf_gdpppp.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "gdp_imf") %>% 
  mutate(year = as.numeric(year),
         country = fct_recode(country, 
                              "South Korea" = "Korea, Republic of",
                              "Kyrgyzstan" = "Kyrgyz Republic",
                              "Macedonia" = "North Macedonia",
                              "Russia" = "Russian Federation",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States")) %>% 
  group_by(country) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

imf_conversion= fread("Datasets/imf_conversionrate.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "gdpconversion_imf") %>% 
  mutate(year = as.numeric(year),
         country = fct_recode(country, 
                              "South Korea" = "Korea, Republic of",
                              "Kyrgyzstan" = "Kyrgyz Republic",
                              "Macedonia" = "North Macedonia",
                              "Russia" = "Russian Federation",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States")) %>% 
  group_by(country) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

OECD_gov = fread("Datasets/oecd_generalgov.csv") %>% 
  filter(SUBJECT == "PUBORD",
         MEASURE == "PC_GDP") %>% 
  select(country_text_id = LOCATION,
         year = TIME,
         order_safety_gdp_perc_oecd = Value)  %>% 
  left_join(imf_gdp, by=c("country_text_id", "year")) %>% 
  left_join(imf_conversion, by=c("country_text_id", "year"))  %>% 
  left_join(QoC_data %>% select(country_text_id, year,
                                total_pop_wdi = wdi_pop) %>%  na.omit(), by=c("country_text_id", "year"))%>% 
  mutate(order_safety_gdp_perc_oecd = order_safety_gdp_perc_oecd/100,
         gdp_national = gdp_imf * gdpconversion_imf,
         order_safety_gdp_oecd = order_safety_gdp_perc_oecd * (gdp_national*1000000),
         order_safety_gdp_national_oecd = order_safety_gdp_oecd/gdpconversion_imf,
         order_safety_gdpcapita_oecd = order_safety_gdp_national_oecd/(total_pop_wdi/10000)
         ) %>%
  select(country_text_id, year, order_safety_gdp_perc_oecd, order_safety_gdpcapita_oecd)


GWP_RL = fread("Datasets/gwp_rl.csv") %>% 
  select_at(vars(country_text_id, ends_with("RL"))) %>% 
  pivot_longer(cols=ends_with("RL")) %>% 
  mutate(name = gsub("GWP", "", name),
         name = gsub("RL", "", name),
         name = paste("20", name, sep = ""),
         name = as.numeric(name)) %>% 
  rename(year = name, trust_gwp = value) %>% 
  filter(country_text_id != "")

GCS_Police = fread("Datasets/gcs_rps.csv", fill = T) %>% 
  rename(Indicatortype = "Subindicator Type", country_text_id = "Country ISO3") %>% 
  mutate(Indicator = gsub("GCI 4.0: ", "", Indicator)) %>% 
  filter(Indicator == "Organized crime"	| Indicator == "Reliability of police services") %>% 
  filter(Indicatortype == "1-7 Best") %>% 
  select_at(vars(country_text_id, Indicator, starts_with("20"))) %>% 
  pivot_longer(cols=starts_with("20")) %>% 
  pivot_wider(names_from = Indicator, values_from = value) %>% 
  mutate(name = gsub("-.*", "", name),
         name = as.numeric(name)) %>% 
  rename(year = name, orgacrime_gcr = `Organized crime`, reliab_police_gcr = `Reliability of police services`)

### Create Main Dataset ####
domestic_security =  QoC_data %>% 
  select(country_text_id, year, ccode) %>% # ccode??
  group_by(country_text_id, year) %>% 
  slice(1) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster %>% select(-country, -regions), by=c("country_text_id", "year"))  %>%
  left_join(OECD_gov, by=c("country_text_id", "year")) %>% 
  left_join(UNODC_data, by=c("country_text_id", "year")) %>%
  left_join(QoC_data %>% select(country_text_id, year, 
                                internalwar_ucdp = ucdp_type3, 
                                psnv_wgi = wbgi_pve) %>% 
              filter_at(vars(psnv_wgi, internalwar_ucdp), any_vars(is.na(.)==F)), by=c("country_text_id", "year")) %>%
  left_join(GWP_RL, by=c("country_text_id", "year")) %>%
  left_join(GCS_Police, by=c("country_text_id", "year")) %>% 
  filter(year > 1950) %>% 
  group_by(country_text_id) %>% 
  # add country and regions
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country_text_id"))  %>%
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  ungroup() %>% 
  select(country, country_text_id, regions, year, everything())  %>% 
  arrange(country_text_id, year)  %>% 
  mutate(internalwar_ucdp = ifelse(is.na(internalwar_ucdp) == T, 0, internalwar_ucdp),
         internalwar_bin_ucdp = ifelse(internalwar_ucdp >= 1, 1, 0))


##### NA-Plots ####
test = domestic_security %>% 
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy")  %>%
  group_by(year) %>% 
  select_at(vars(order_safety_gdpcapita_oecd,
                 hom_rate_unodc,
                 theft_rate_unodc, 
                 reliab_police_gcr)) %>% 
  summarise_all(pMiss_01)

domestic_security %>% 
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy")  %>%
  group_by(year) %>% 
  select_at(vars(order_safety_gdpcapita_oecd,
                 hom_rate_unodc,
                 theft_rate_unodc, 
                 reliab_police_gcr)) %>% 
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


domestic_security %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("ucdp"), ends_with("wgi"))) %>% 
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


##### Linear Interpolation
# disabled

domestic_security_IP = domestic_security %>%
  group_by(country_text_id)  %>% 
  # mutate_at(vars(ends_with("_unodc")), .funs = list(~na_interpol(.))) %>% 
  # mutate_at(vars(ends_with("_oecd")), .funs = list(~na_interpol(.))) %>% 
  # #mutate_at(vars(ends_with("_ucdp")), .funs = list(~na_interpol(.))) %>% 
  # mutate_at(vars(ends_with("_wgi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()

domestic_security_IP %>% 
  group_by(year) %>% 
  select_at(vars(order_safety_gdpcapita_oecd,
                 hom_rate_unodc,
                 theft_rate_unodc, 
                 reliab_police_gcr, ends_with("gwp"))) %>% 
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


domestic_security_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("ucdp"), ends_with("wgi"))) %>% 
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



###

domestic_security_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("unodc"), ends_with("gcr"), ends_with("gwp"))) %>% 
  select(order_safety_gdpcapita_oecd,
         hom_rate_unodc,
         theft_rate_unodc,
         reliab_police_gcr,
         #trust_gwp
         ) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")  +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Raw Sample")


domestic_security_IP %>% 
  select_at(vars(ends_with("ucdp"), ends_with("wgi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Raw Sample")



####

# WGI: its normalized already
# UCDP: almost binary variable
domestic_security_IP_norm = domestic_security_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("unodc"), ends_with("ucdp"), ends_with("wgi"), ends_with("gcr"), ends_with("gwp"))) %>% 
  
  mutate_at(vars(order_safety_gdp_perc_oecd), funs(trim(., 0.01, minimum=T))) %>%
  
  mutate_at(vars(matches("_perc")), ~folded_ladder_fun(., plotting=F)) %>% 
  mutate_at(vars(ends_with("unodc"), ends_with("gcr"), ends_with("gwp")), ladder_fun) %>% 
  mutate_at(vars(ends_with("oecd"), ends_with("unodc"), ends_with("wgi"), ends_with("gcr"), ends_with("gwp")), scale)
  
domestic_security_IP_norm %>% 
  select_at(vars(ends_with("oecd"), ends_with("unodc"), ends_with("gcr"), ends_with("gwp"))) %>% 
  select(order_safety_gdpcapita_oecd,
         hom_rate_unodc,
         theft_rate_unodc,
         reliab_police_gcr,
         #trust_gwp
         ) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") + 
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Transformed Sample")


domestic_security_IP_norm %>% 
  select_at(vars(ends_with("ucdp"), ends_with("wgi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Transformed Sample")




### NA Frame: OECD
NA_frame_ds_oecd = domestic_security_IP_norm %>% 
  select_at(vars(-ends_with("ucdp"), -ends_with("wgi"), ends_with("gcr"), ends_with("gwp"))) %>% 
  mutate_all(funs(is_na = ifelse(is.na(.)==T, 1,0))) %>% 
  bind_cols(domestic_security %>%  select(country, country_text_id, year)) %>%
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select_at(vars(country_text_id, year, ends_with("is_na"))) %>% 
  mutate(missing_SUM = rowSums(select(., matches("oecd"))) + rowSums(select(., matches("unodc"))))


#### Factor Analysis: Transfer to MI

fa_data_ds_frame = domestic_security_IP_norm %>% 
  bind_cols(domestic_security %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(-ends_with("ucdp"), -ends_with("wgi"))) %>% 
  select(country, country_text_id, year, everything()) %>% 
  right_join(NA_frame_ds_oecd, by=c("country_text_id", "year"))
  

wgi_data_ds_frame = domestic_security_IP_norm %>% 
  bind_cols(domestic_security %>%  select(country, country_text_id, year)) %>% 
  select(country_text_id, year, ds_order_index = psnv_wgi) 

###



