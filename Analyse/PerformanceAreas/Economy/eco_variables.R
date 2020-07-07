# Economic Performance
if (clustersetup == T) {
  source("Analyse/Cluster/Cluster_v4.R")
}

###
WB_current = fread("Datasets/WB_current_account.csv", header=T) %>% 
  rename(country = "Country Name") %>% 
  dplyr::select(-"Country Code", -"Indicator Name", -"Indicator Code") %>% 
  melt(id.vars = c("country"), variable.name = "year", value.name = "Balance_wdi") %>% 
  mutate(year = as.numeric(levels(year))[year]) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea" = "Korea, Rep.",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  ) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

WB_consumption = fread("Datasets/WB_consumption_capita.csv", header=T, sep=";") %>% 
  rename(country = "Country Name") %>% 
  dplyr::select(-"Country Code", -"Indicator Name", -"Indicator Code") %>% 
  melt(id.vars = c("country"), variable.name = "year", value.name = "consumption_cap_wdi") %>% 
  mutate(year = as.numeric(levels(year))[year]) %>% 
  mutate(country = as.factor(country),
         country = fct_recode(country,
                              "South Korea" = "Korea, Rep.",
                              "Slovakia" = "Slovak Republic",
                              "United States of America" = "United States"
         ),
         country = as.character(country)
  ) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)


imf_debt  = fread("Datasets/imf_globaldebt.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "generaldebt_imf") %>% 
  mutate(year = as.numeric(year),
         generaldebt_imf = generaldebt_imf/100,
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
  #mutate(generaldebt_pr_imf = generaldebt_pr_imf- dplyr::lag(generaldebt_pr_imf, 1)) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

imf_interestpaid  = fread("Datasets/imf_interestpaid.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "interestpaid_imf") %>% 
  mutate(year = as.numeric(year),
         interestpaid_imf = interestpaid_imf/100,
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
  #mutate(generaldebt_pr_imf = generaldebt_pr_imf- dplyr::lag(generaldebt_pr_imf, 1)) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country) %>% 
  filter(year >= 1950)

imf_primarybalance  = fread("Datasets/imf_primarybalance.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "primarybalance_imf") %>% 
  mutate(year = as.numeric(year),
         primarybalance_imf = primarybalance_imf/100,
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
  #mutate(generaldebt_pr_imf = generaldebt_pr_imf- dplyr::lag(generaldebt_pr_imf, 1)) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country) %>% 
  filter(year >= 1950)

imf_unempl= fread("Datasets/imf_unemployment.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "unemployment_pr_imf") %>% 
  mutate(year = as.numeric(year),
         unemployment_pr_imf = unemployment_pr_imf/100,
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
  #mutate(generaldebt_pr_imf = generaldebt_pr_imf- dplyr::lag(generaldebt_pr_imf, 1)) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

imf_gdpppp= fread("Datasets/imf_gdpcapitappp.csv", header = T, encoding = "UTF-8") %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "gdp_cap_ppp_imf") %>% 
  mutate(year = as.numeric(year),
         #unemployment_imf = unemployment_imf/100,
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
  #mutate(generaldebt_pr_imf = generaldebt_pr_imf- dplyr::lag(generaldebt_pr_imf, 1)) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

imf_inflation= fread("Datasets/imf_inflation.csv", header = T) %>% 
  pivot_longer(cols=-"country", names_to = "year", values_to = "inflation_imf") %>% 
  mutate(year = as.numeric(year),
         inflation_imf = abs(inflation_imf),
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
  #mutate(generaldebt_pr_imf = generaldebt_pr_imf- dplyr::lag(generaldebt_pr_imf, 1)) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country")) %>% 
  select(-country)

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

# Create Main Dataset
Economy_Perfomance = QoC_data %>% 
  select(country_text_id, year,

         GDP_capita_gen_num_wdi = wdi_gdpcapcur,
         # Inflation_wdi = wdi_inflation,
         # Unemployment_pr_wdi = wdi_unempilo,
         # tax_rev_wdi = wdi_taxrev,
         investment_wdi = wdi_fdiin,
         grosscapitalformation_pwt = pwt_sgcf,
         # realgdp_pwt = pwt_rgdp
  )  %>% 
  
  filter_if(is.double, any_vars(!is.na(.)))  %>% 
  group_by(country_text_id) %>% 
  filter(year >= 1950) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  
  # add IMF source
  left_join(WB_current, by=c("country_text_id", "year")) %>% 
  # left_join(imf_debt, by=c("country_text_id", "year")) %>%  
  left_join(imf_unempl, by=c("country_text_id", "year")) %>%  
  left_join(imf_gdpppp, by=c("country_text_id", "year")) %>%  
  left_join(imf_inflation, by=c("country_text_id", "year")) %>%  
  left_join(WB_consumption, by=c("country_text_id", "year")) %>% 
  left_join(imf_gdp, by=c("country_text_id", "year")) %>% 

  filter(year >= 1950) %>%  
  dplyr::arrange(country_text_id, year) %>% 
  group_by(country_text_id) %>% 
  mutate(
         # Inflation_wdi = abs(Inflation_wdi),
         # Inflation_oecd = abs(Inflation_oecd),
         # Balance_oecd = abs(Balance_oecd),
         Balance_wdi = abs(Balance_wdi)
         ) %>% 
  ungroup() %>%
  # mutate_at(vars(matches("_pr_")), funs(./100)) %>% 
  # Contra Deflation 
  mutate_at(vars(starts_with("Inflation")), funs(abs(.))) %>% 
  
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster %>%  select(-country, -regions), by=c("country_text_id", "year"))  %>%
  
  # add country and regions
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by=c("country_text_id"))  %>%
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  
  select(country, country_text_id, regions, year, everything()) %>% 
  ungroup() %>%  
  dplyr::arrange(country_text_id, year)  




##### NA-Plots ####


Economy_Perfomance %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd"), ends_with("imf"), ends_with("pwt"), ends_with("wdi"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")



#### Linear Interpolation ####
# Disabled

Economy_Perfomance_IP = Economy_Perfomance %>% 
  group_by(country_text_id) %>% 
  # mutate_at(vars(ends_with("oecd")), .funs = list(~na_interpol(.))) %>% 
  # mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()


Economy_Perfomance_IP %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd"), ends_with("imf"), ends_with("pwt"), ends_with("wdi"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")



######


Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("imf"), ends_with("pwt"), ends_with("wdi"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 


Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("imf"), ends_with("pwt"), ends_with("wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")  +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Raw Sample")


#### Transformation
Economy_Perfomance_IP_norm = Economy_Perfomance_IP  %>% 
  select_at(vars(ends_with("oecd"), ends_with("imf"), ends_with("pwt"), ends_with("wdi"))) %>% 
  mutate_at(vars("consumption_cap_wdi",
                 "grosscapitalformation_pwt",
                 "unemployment_pr_imf",
                 "inflation_imf",
                 "Balance_wdi"), funs(trim(., 0.01, minimum=T))) %>% 
  mutate_at(vars("investment_wdi",
                 GDP_capita_gen_num_wdi), funs(trim(., 0.025, minimum=T, only=T))) %>%
  mutate_at(vars(ends_with("oecd"), ends_with("imf"), ends_with("pwt"), ends_with("wdi"), -matches("_pr")), funs(ladder_fun(.))) %>% 
  mutate_at(vars(matches("_pr_")), funs(folded_ladder_fun(., plotting =F))) %>% 
  mutate_all(scale)

Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("oecd"), ends_with("imf"), ends_with("pwt"), ends_with("wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") +
  theme_bw()  +
  theme(legend.position = "none") +
  scale_y_continuous(name=NULL)  +
  scale_x_continuous(name=NULL) +
  ggtitle("Transformed Sample")



### NA Frame: OECD
NA_frame_oecd = Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("imf"), ends_with("wdi"), ends_with("pwt"),-matches("gen_num"))) %>% 
  mutate_all(funs(is_na = ifelse(is.na(.)==T, 1,0))) %>% 
  bind_cols(Economy_Perfomance %>%  select(country, country_text_id, year)) %>%
  filter(year>=1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select_at(vars(country_text_id, year, ends_with("is_na"))) %>% 
  mutate(missing_SUM = rowSums(select(., matches("wdi"))) + rowSums(select(., matches("imf"))) + rowSums(select(., matches("pwt"))))

#### Factor Analysis: Transfer to MI
# OECD
fa_data_eco_frame = Economy_Perfomance_IP_norm %>% 
  bind_cols(Economy_Perfomance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country, country_text_id, year, ends_with("wdi"), ends_with("imf"), ends_with("pwt"))) %>% 
  right_join(NA_frame_oecd, by=c("country_text_id", "year"))


