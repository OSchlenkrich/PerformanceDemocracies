source("Analyse/Cluster_v3.R")


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)


test = QoC_data %>% 
  select(country_text_id, year,
         CO2_oecd = oecd_greenhouse_t1,
         CO2_wdi = wdi_co2,
         Sec_oecd = oecd_gengovdistri_t1c)


NA_plot(test, "Emissions")


Economy_perc = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_oecd = oecd_sizegdp_t1,
         #GDP_growth_oecd = oecd_evogdp_t1,
         Inflation_oecd = oecd_cpi_t1a,
         Interest_oecd = oecd_ltintrst_t1,
         Unemployment_oecd = oecd_unemplrt_t1c,
         Debt_oecd = oecd_govdebt_t1,
         Researchers_oecd = oecd_research_t1,
         SpendingRD_oecd = oecd_gerd_t1,
         
         GDP_capita_wdi = wdi_gdpcappppcon2011,
         GDP_growth_wdi = wdi_gdpgr,
         Inflation_wdi = wdi_inflation,
         Interest_wdi = wdi_intrate,
         Unemployment_wdi = wdi_unempilo,
         Unemployment_youth_wdi = wdi_unempyilo,
         Debt_wdi = wdi_debt
         ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year")) %>%
  select(country, country_text_id, everything())  %>%
  group_by(country_text_id) %>%
  mutate(country = unique(na.omit(country))) %>% 
  ungroup() %>% 
  dplyr::arrange(country_text_id, year) %>% 
  mutate_if(is.double, ntile_fun) %>%
  mutate(Inflation_oecd = 11 - Inflation_oecd,
         Interest_oecd = 11 - Interest_oecd,
         Unemployment_oecd = 11 - Unemployment_oecd,
         Debt_oecd = 11 - Debt_oecd,
         
         Inflation_wdi = 11 - Inflation_wdi,
         Unemployment_wdi = 11 - Unemployment_wdi,
         Interest_wdi = 11 - Interest_wdi,
         Unemployment_youth_wdi = 11 - Unemployment_youth_wdi,
         Debt_wdi = 11 - Debt_wdi) %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("oecd")), .funs = list(~na_interpol_perc(.))) %>% 
  mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol_perc(.))) %>% 
  ungroup()

na_interpol_perc = function(x) {
  if (length(na.omit(x)) >= 2) {
    y = na_locf(x, maxgap = 5)
    return(y)
  } else {
    return(x) 
  }
}

hist(Economy_IQR$Inflation_wdi)


trim <- function(x,prop=.05) {
  max_trimmed_value = which(x < quantile(x,prob=(1-prop), na.rm=T))
  max_end = max(x[max_trimmed_value], na.rm=T)
  
  totrim = which(x >= quantile(x,prob=(1-prop), na.rm=T))
  
  x[totrim] = max_end
  return(x)
}

library(DescTools)
hist(log(Economy_IQR$GDP_capita_wdi))
Economy_IQR$Inflation_wditr = ntile(trim(Economy_IQR$Inflation_wdi, 0.05), 5)




#####
Economy_IQR = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_oecd = oecd_sizegdp_t1,
         GNI_capita_oecd = oecd_natinccap_t1,
         #Trade_Balance_oecd = oecd_bop_t1,
         #GDP_growth_oecd = oecd_evogdp_t1,
         Inflation_oecd = oecd_cpi_t1a,
         Interest_oecd = oecd_ltintrst_t1,
         Unemployment_oecd = oecd_unemplrt_t1c,
         # Debt_oecd = oecd_govdebt_t1,
         # Researchers_oecd = oecd_research_t1,
         # SpendingRD_oecd = oecd_gerd_t1,
         
         GDP_capita_wdi = wdi_gdpcapcur,
         # GDP_growth_wdi = wdi_gdpgr,
         Inflation_wdi = wdi_inflation,
         Interest_wdi = wdi_intrate,
         Unemployment_wdi = wdi_unempilo,
         GNI_capita_wdi = wdi_gniatlcur,
         # Debt_wdi = wdi_debt
  ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year"))  %>%
  select(country, country_text_id, everything())  %>% 
  group_by(country_text_id) %>%
  mutate(country = unique(na.omit(country))) %>% 
  ungroup() %>% 
  dplyr::arrange(country_text_id, year) %>% 
  mutate_at(vars(ends_with("oecd")), .funs = list(sgi = ~SGI_fun(.)))  %>%  
  mutate(GDP_capita_wdi_sgi = SGI_fun_WDI(GDP_capita_wdi, GDP_capita_oecd),
         Inflation_wdi_sgi = SGI_fun_WDI(Inflation_wdi, Inflation_oecd),
         Interest_wdi_sgi = SGI_fun_WDI(Interest_wdi, Interest_oecd),
         Unemployment_wdi_sgi = SGI_fun_WDI(Unemployment_wdi, Unemployment_oecd),
         #Unemployment_youth_wdi_sgi = SGI_fun_WDI(Unemployment_youth_wdi, Unemployment_youth_wdi)
         ) %>% 
  mutate(Inflation_oecd_sgi = max(Inflation_oecd_sgi, na.rm=T) - Inflation_oecd_sgi,
         Interest_oecd_sgi = max(Interest_oecd_sgi, na.rm=T) - Interest_oecd_sgi,
         Unemployment_oecd_sgi = max(Unemployment_oecd_sgi, na.rm=T) - Unemployment_oecd_sgi,
         #Debt_oecd_sgi = max(Debt_oecd_sgi, na.rm=T) - Debt_oecd_sgi,
         
         Inflation_wdi_sgi = max(Inflation_wdi_sgi, na.rm=T) - Inflation_wdi_sgi,
         Interest_wdi_sgi = max(Interest_wdi_sgi, na.rm=T) - Interest_wdi_sgi,
         Unemployment_wdi_sgi = max(Unemployment_wdi_sgi, na.rm=T) - Unemployment_wdi_sgi,
         #Unemployment_youth_wdi_sgi = max(Unemployment_youth_wdi_sgi, na.rm=T) - Unemployment_youth_wdi_sgi,
         # Debt_wdi_sgi = max(Debt_wdi_sgi, na.rm=T) - Debt_wdi_sgi,
  ) %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("oecd")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()


na_interpol = function(x) {
  if (length(na.omit(x)) >= 2) {
    y = na_interpolation(x,  option = "linear", maxgap = 5)
    return(y)
  } else {
    return(x) 
  }
}
na_interpol2 = function(x,maxgap) {
  if (length(na.omit(x)) >= 2) {
    y = na_interpolation(x,  option = "linear", maxgap = maxgap)
    return(y)
  } else {
    return(x) 
  }
}
na_interpolation(c(2, 3, 4, 5, 6, NA, 7, 8))


IQR_min_fun = function(x) {
  iqrange = IQR(x, na.rm=T)
  minimum = quantile(x, 0.25, na.rm=T) - 1.5*iqrange
  
  minimum_0 = ifelse(minimum < 0, 0, minimum)
  return(minimum_0)
}

IQR_max_fun = function(x) {
  iqrange = IQR(x, na.rm=T)
  maximum = quantile(x, 0.75, na.rm=T) + 1.5*iqrange
  return(maximum)
}

SGI_fun = function(x) {
  minimum = IQR_min_fun(x)
  maximum = IQR_max_fun(x)
  scale = maximum - minimum
  
  y = ifelse(x > maximum, 10,
         ifelse(x < minimum, 1, 1 + ((x - minimum)/scale)*9))
  
  return(y)
}

SGI_fun_WDI = function(x, x_oecd) {
  minimum = IQR_min_fun(x_oecd)
  maximum = IQR_max_fun(x_oecd)
  scale = maximum - minimum
  
  y = ifelse(x > maximum, 10,
             ifelse(x < minimum, 1, 1 + ((x - minimum)/scale)*9))
  
  return(y)
}



quantile(Economy_IQR$Unemployment_oecd, 0.25, na.rm=T)
IQR(Economy_IQR$Unemployment_oecd,  na.rm=T) * 1.5



fa_data = Economy_IQR %>%
  #filter(year > 1990) %>% 
  select_at(vars(country_text_id, year, ends_with("wdi"))) %>% 
  select(-Interest_wdi ) -


Economy_IQR_NA = Economy_IQR %>%  filter(is.na(cluster_label_1st) == F)

NA_plot(Economy_IQR %>%  filter(is.na(cluster_label_1st) == F), "Economy")


test = fa.parallel(fa_data, fm="ml", plot=F)
fa.parallel(fa_data %>%  select(-country_text_id, -year), fm="ml", plot=T, sim=T, n.iter=100, quant=0.95)

fa.parallel(Economy_IQR %>% select(GDP_capita_wdi_sgi, Unemployment_wdi_sgi, Inflation_wdi_sgi), fm="ml")
fa_solution = fa(Economy_IQR %>% select(GDP_capita_wdi_sgi, Unemployment_wdi_sgi, Inflation_wdi_sgi) %>% na.omit(), 1, fm="ml")


psych::scree(Economy_IQR %>% select_at(vars(ends_with("wdi_sgi"))))


fa_solution = fa(fa_data  %>%  select(-country_text_id, -year),2, fm="ml", rotate="oblimin")
fa_solution
fa.diagram(fa_solution, cut=.1)


fa_solution = pca(fa_data %>%  select(-country_text_id, -year), 2)
fa_solution
fa.diagram(fa_solution, cut=.1)
biplot.psych(fa_solution)

fa_solution$scores %>% 
  as.data.frame() %>% 
  rename(Money = RC1, Unemp = RC2) %>% 
  bind_cols(fa_data) %>% 
  group_by(year) %>% 
  summarise(Money = mean(Money, na.rm=T), Unemp = mean(Unemp, na.rm=T)) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, col=variable)) +
  geom_line(size=1)

fa_solution$scores %>% 
  as.data.frame() %>% 
  rename(Money = RC1, Unemp = RC2) %>% 
  bind_cols(fa_data) %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year,  Money, Unemp)) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=variable)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(country_text_id ~ .)
  

Economy_IQR %>% 
  group_by(year) %>% 
  summarise(GDP_capita_oecd_sgi = mean(GDP_capita_oecd_sgi, na.rm=T), 
            GNI_capita_oecd_sgi =  mean(GNI_capita_oecd_sgi, na.rm=T),
            Inflation_oecd_sgi = mean(Inflation_oecd_sgi, na.rm=T),
            Interest_oecd_sgi = mean(Interest_oecd_sgi, na.rm=T),
            Unemployment_oecd_sgi = mean(Unemployment_oecd_sgi, na.rm=T),
            GDP_growth_oecd_sgi = mean(GDP_growth_oecd_sgi, na.rm=T),
            ) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, col=variable)) +
  geom_line(size=1)



samples = c("AUS","AUT", "DEU", "USA", "LBR", "TTO")

Economy_IQR %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, ends_with("oecd_sgi"), ends_with("wdi_sgi"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) +
  ylim(0,10)


# Prisonbrief
library(prisonbrief)
test = wpb_list()
germany <- wpb_series(country = "trinidad-and-tobago")
germany


#############
# Trust
QoC_data$wvs_psdem
Trust = QoC_data %>% 
  select(country_text_id, year,
         idea_democracy = wvs_imppol,
  ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year"))  %>%
  select(country, country_text_id, everything())

Trust %>% 
  group_by(cluster_label_1st) %>% 
  summarise(mean(idea_democracy))

Trust %>% 
  ggplot(aes(x=X1, y=idea_democracy)) +
  geom_point()
summary(lm(idea_democracy ~ cluster_label_1st, Trust))



#### Environmental Performance ####

oecd_greenhouse_t1
oecd_soxnox_t1a
oecd_soxnox_t1b
oecd_waste_t1b
oecd_airqty_t1


QoC_data %>% 
  filter(country_text_id == "USA") %>% 
  select(oecd_greenhouse_t1, oecd_evopop_t1, wdi_pop) %>% 
  mutate(oecd_evopop_t1 = oecd_evopop_t1 * 1000)



per_capita_maker = function(x, pop) {
  x/pop
}



Environment_IQR = QoC_data %>% 
  select(country_text_id, year, 
         greenhouse_oecd = oecd_greenhouse_t1,
         sulphur_oecd = oecd_soxnox_t1a,
         nitrogen_oecd = oecd_soxnox_t1b,
         co2_oecd = oecd_airqty_t1,
         water_oecd = oecd_water_t1a,
         waste_oecd = oecd_waste_t1b,
         population = oecd_evopop_t1,
         GDP_capita_oecd = oecd_sizegdp_t1,
         
         greenhouse_wdi = wdi_co2

  ) %>% 
  mutate(population = population) %>% 
  mutate_at(vars(ends_with("oecd")), funs(per_captita = per_capita_maker(., GDP_capita_oecd*population)))  %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year"))  %>%
  select(country, country_text_id, everything())  %>% 
  filter(is.na(cluster_label_1st) == F) %>% 
  dplyr::arrange(country_text_id, year)  %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("oecd")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()

Environment_IQR %>% 
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

Environment_IQR %>% 
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



fa.parallel(Environment_IQR %>%  select_at(vars(ends_with("oecd_per_captita"))) %>% na.omit(), fm="ml", plot=T, sim=T, n.iter=100, quant=0.95)


fa_solution = pca(Environment_IQR %>%  select_at(vars(ends_with("_oecd"))) %>% na.omit(), 2)
fa_solution
fa.diagram(fa_solution, cut=.1)
biplot.psych(fa_solution)


Environment_IQR %>%  
  select_at(vars(country_text_id, year, ends_with("_oecd"))) %>% 
  na.omit() %>% 
  bind_cols(data.frame(EV_scores = fa_solution$scores)) %>% 
  left_join(Environment_IQR %>%  select(country_text_id, year, cluster_label_1st), by=c("country_text_id", "year")) %>% 
  group_by(cluster_label_1st) %>% 
  summarise(mean(PC1))

Environment_IQR %>%
  filter(year >= 1960, year <= 1980) %>%  
  group_by(cluster_label_1st) %>% 
  summarise(mean(greenhouse_wdi, na.rm=T))

summary(lm(greenhouse_wdi ~ cluster_label_1st, Environment_IQR))



#### Integration

integration = QoC_data %>% 
  select(country_text_id, year, 
         Combined_GI = sc_tgen,
         Unemployment_GI = sc_uegen,
         Pension_GI = sc_pgen,
         Sickness_GI = sc_skgen,
         
         gini_lis = lis_gini,
         poverty9010_lis = lis_pr9010,
         
         gini_wdi = wdi_gini
  )  %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year"))  %>%
  filter(is.na(cluster_label_1st) == F) %>% 
  select(country, country_text_id, everything())  %>% 
  dplyr::arrange(country_text_id, year) %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("lis")), .funs = list(~na_interpol2(., 5))) %>% 
  mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol2(., 5))) %>% 
  ungroup()


integration  %>% 
  group_by(cluster_label_1st) %>% 
  summarise(mean(Combined_GI, na.rm=T),
            mean(Unemployment_GI, na.rm=T),
            mean(Sickness_GI, na.rm=T),
            mean(Pension_GI, na.rm=T)
  )


integration %>% 
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


integration %>% 
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

integration %>% 
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

cor(integration$gini_wdi, integration$gini_lis, use="pairwise")

### Domestic Security ####
library(tidyr)
cty_identifier = V_dem %>% 
  select(country, country_text_id) %>% 
  group_by(country) %>% 
  slice(1)



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
  mutate(hom_rate_unodc = hom_count/total_pop_wdi,
         burg_rate_unodc = burg_count/total_pop_wdi,
         rob_rate_unodc = rob_count/total_pop_wdi,
         incarc_ratio_unodc = incarcer_count/convictions_count) %>% 
  select(country_text_id, year, hom_rate_unodc, burg_rate_unodc, rob_rate_unodc,incarc_ratio_unodc)




OECD_gov = fread("Datasets/oecd_generalgov.csv") %>% 
  filter(SUBJECT == "PUBORD",
         MEASURE == "PC_GDP") %>% 
  select(country_text_id = LOCATION,
         year = TIME,
         order_safety_gdp_oecd = Value)  



domestic_security =  QoC_data %>% 
  select(country_text_id, year) %>% 
  group_by(country_text_id, year) %>% 
  slice(1) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster %>% select(country, country_text_id, year, cluster_label_1st), by=c("country_text_id", "year")) %>% 
  left_join(OECD_gov, by=c("country_text_id", "year")) %>% 
  left_join(UNODC_data, by=c("country_text_id", "year")) %>%
  left_join(QoC_data %>% select(country_text_id, year, internalwar_ucdp = ucdp_type3, psnv_wgi = wbgi_pve) %>% filter_at(vars(psnv_wgi, internalwar_ucdp), any_vars(is.na(.)==F)), by=c("country_text_id", "year")) %>% 
  filter(is.na(cluster_label_1st) == F) %>% 
  arrange(country_text_id, year) %>% 
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("_unodc")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("_oecd")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("_ucdp")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("_wgi")), .funs = list(~na_interpol(.))) %>% 
  ungroup() %>% 
  mutate(internalwar_ucdp = if_else(is.na(internalwar_ucdp) == T, 0, internalwar_ucdp),
         internalwar_bin_ucdp = if_else(internalwar_ucdp >= 1, 1, 0))



domestic_security %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_unodc"))) %>% 
  select(-burg_rate_unodc, -rob_rate_unodc) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - UNODC")


domestic_security %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_OECD"))) %>% 
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
  select_at(vars(ends_with("_ucdp"))) %>% 
  select(-internalwar_bin_ucdp) %>% 
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
  select_at(vars(ends_with("_wgi"))) %>% 
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


domestic_security %>% 
  ggplot(aes(x=internalwar_bin_ucdp, y=burg_rate_unodc)) + 
  geom_point()
domestic_security %>% 
  ggplot(aes(x=internalwar_bin_ucdp, y=rob_rate_unodc)) + 
  geom_point()


my_data = domestic_security %>% select_at(vars(ends_with("_unodc"), ends_with("oecd"), ends_with("bin_ucdp"), ends_with("WGI"))) %>% 
  select(-burg_rate_unodc, -rob_rate_unodc) 
fa.parallel(my_data, fm="ml")
my_sol = pca(my_data, 2, rotate = "varimax")
my_sol
fa.diagram(my_sol)
biplot.psych(my_sol)

#### Political Culture ####
QoC_data$wvs_confgov

QoC_data %>% 
  select(
    conf_civil_service_wvs = wvs_confcs,
    #conf_government_wvs = wvs_confgov,
    conf_judicial_wvs = wvs_confjs,
    conf_police_wvs = wvs_confpol,
    conf_parliament_wvs = wvs_confpar,
    conf_parties_wvs = wvs_confpp,
    
    trust_politices = asd,
  )

mycountries = dmx_trade_cluster %>% 
  filter(year >= 2000) %>% 
  select(country, cluster_label_1st) %>% 
  na.omit() %>% 
  group_by(country) %>% 
  summarise(years = n()) %>% 
  arrange(-years)

# write.csv2(mycountries, file="wvs_cty.csv", row.names = F)

library(haven)

IVS_ctry_id = read.csv("C:/RTest/IVS/ctry_id.csv", header=T, encoding = "UTF-8") %>% 
  rename(S003 = V2) %>% 
  mutate(country = fct_recode(country,
                              "United Kingdom" = "Great Britain",
                              "Czech Republic" = "Czech Rep.",
                              "Bosnia and Herzegovina" = "Bosnian Federation",
                              "Republic of Vietnam" = "Viet Nam",
                              "Bosnia and Herzegovina" = "Bosnia",
                              "United States of America" = "United States"))

library(tidyr)
cty_identifier = V_dem %>% 
  select(country, country_text_id) %>% 
  group_by(country) %>% 
  slice(1)

# IVS_cases = IVS %>% 
#   distinct(country) %>% 
#   left_join(cty_identifier, by="country")


EVS = read_spss("C:/RTest/IVS/ZA4804_v3-0-0.sav", user_na = T)
WVS = readRDS("C:/RTest/IVS/F00008390-WVS_Longitudinal_1981_2016_r_v20180912.rds")

IVS = WVS %>% 
  bind_rows(EVS) %>% 
  left_join(IVS_ctry_id, by="S003") %>% 
  left_join(cty_identifier, by="country")



IVS_missings = IVS %>%
  rename(year_study = S020) %>% 
  group_by(country, year_study) %>% 
  summarise(cases = n()) %>% 
  arrange(country, year_study) %>% 
  group_by()%>% 
  complete(country, year_study = full_seq(year_study, 1)) %>% 
  group_by(country) %>% 
  mutate(cases_lag1 = dplyr::lag(cases,1),
         cases_lag2 = dplyr::lag(cases,2),
         cases_lag3 = dplyr::lag(cases,3),
         cases_lead1 = dplyr::lead(cases,1),
         cases_lead2 = dplyr::lead(cases,2),
         cases_lead3 = dplyr::lead(cases,3)
         ) %>% 
  filter_at(vars(starts_with("cases")), any_vars(is.na(.)==F)) %>% 
  select(country, year = year_study) %>% 
  mutate(helper_ivs = 1)

test = dmx_trade_cluster %>% 
  select(country, country_text_id, year, cluster_label_1st) %>% 
  left_join(IVS_missings, by=c("country", "year")) %>% 
  filter(is.na(cluster_label_1st) == F) 

test %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_ivs"))) %>% 
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


confidence = IVS %>% 
  select(
    survey = S001,
    country,
    country_text_id,
    year_study = S020,
    conf_parliament_ivs = E069_07,
    conf_civilservice_ivs = E069_08,
    conf_judiciary_ivs = E069_17,
    conf_parties_ivs = E069_12,
    conf_govt_ivs = E069_11,
    
    
    # democracy_ivs = E117,
    # leader_ivs = E114,
    # experts_ivs = E115,
    # army_ivs = E116

  ) %>% 
  mutate_at(vars(ends_with("_ivs")),  ~if_else(. < 0, NA_real_, .))


confidence_missings = confidence %>% 
  na.omit() %>% 
  group_by(country, year_study) %>% 
  summarise(cases = n()) %>% 
  arrange(country, year_study) %>% 
  group_by()%>% 
  complete(country, year_study = full_seq(year_study, 1)) %>% 
  group_by(country) %>% 
  mutate(cases_lag1 = dplyr::lag(cases,1),
         cases_lag2 = dplyr::lag(cases,2),
         cases_lag3 = dplyr::lag(cases,3),
         cases_lead1 = dplyr::lead(cases,1),
         cases_lead2 = dplyr::lead(cases,2),
         cases_lead3 = dplyr::lead(cases,3)
  ) %>% 
  filter_at(vars(starts_with("cases")), any_vars(is.na(.)==F)) %>% 
  select(country, year = year_study) %>% 
  mutate(helper_ivs = 1)



test = dmx_trade_cluster %>% 
  select(country, country_text_id, year, cluster_label_1st) %>% 
  left_join(confidence_missings, by=c("country", "year")) %>% 
  filter(is.na(cluster_label_1st) == F) 


test %>% 
  filter(year >= 1950) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_ivs"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - IVS (WVS/EVS)")




fa.parallel(confidence %>%  select_at(vars(ends_with("_ivs"))))
pca_wvs = pca(confidence %>%  select_at(vars(ends_with("_ivs"))), 1)
fa.diagram(pca_wvs)


## Goal-Attainment

library(tidyr)
dmx_trade_cluster_ccp = dmx_trade_cluster %>% 
  left_join(V_dem %>% select(country_text_id, year, COWcode), by=c("country_text_id", "year")) %>% 
  mutate(COWcode = if_else(COWcode == 255, as.integer(260), COWcode))
  

ccp = fread("C:/RTest/ccp_csv.csv")

# Reg_data = fread("C:/RTest/RegressionData.txt")
# 
ADData = fread("C:/RTest/ADData.txt") %>%
  rename(country_text_id = cabbr,
         COWcode = cowcode)
# 
ARData = fread("C:/RTest/ARData.txt") %>%
  rename(country_text_id = cabbr,
         COWcode = cowcode)

ccp_endure = fread("C:/RTest/design_variables.txt") %>% 
  rename(country_text_id = cabbr,
         COWcode = cowcode) 

names(ARData)
ccp_yearly = ccp_endure %>% 
  select(systid = system_num, amend_ccp = amend_rate) %>% 
  right_join(ccp %>%  select(COWcode = cowcode, country, year, systid), by="systid") %>% 
  arrange(COWcode, year)

ccp_yearly = ADData %>% 
  select(systid = systid, amend_15_ccp = ad_ccp) %>% 
  right_join(ccp_yearly, by="systid") %>% 
  arrange(COWcode, year)

cor(ccp_yearly$amend_15_ccp, ccp_yearly$amend_ccp, use="pairwise")

ccp_yearly = ARData %>% 
  select(systid = systid, amend_ccp = ar_ccpy, amend_lutz = ar_lutz) %>% 
  right_join(ccp %>%  select(COWcode = cowcode, country, year, systid), by="systid") %>% 
  arrange(COWcode, year)

ccp_yearly = ADData %>% 
  select(systid = systid, amend_ccp = ad_ccp, amend_lutz = ad_lutz, adiff_lijphart = ad_lijphart) %>% 
  right_join(ccp %>%  select(COWcode = cowcode, country, year, systid), by="systid") %>% 
  arrange(COWcode, year)




AR_dmx = ccp_yearly %>% 
  select(-country) %>% 
  right_join(dmx_trade_cluster_ccp, by=c("COWcode", "year")) %>% 
  mutate(amend_rate_thresh = cut(amend_ccp, breaks=c(0, 0.35, 0.45, 0.65, 0.75,1), labels = c("sub", "mod", "opt", "mod", "sub")))
  
AR_dmx %>% 
  group_by(cluster_label_1st) %>% 
  summarise(mean(amend_ccp, na.rm=T), mean(amend_lutz, na.rm=T), mean(adiff_lijphart, na.rm=T))



AR_dmx %>% 
  filter(year >= 1950) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_ccp"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - CCP")



AR_dmx %>% 
  group_by(cluster_label_1st) %>% 
  summarise(mean(amend_ccp, na.rm=T), mean(amend_lutz, na.rm=T))


AR_dmx %>% 
  group_by(amend_rate_thresh, cluster_label_1st) %>% 
  summarise(n())


prop.table(table(AR_dmx$amend_rate_thresh, AR_dmx$cluster_label_1st),1)

# VoC + Welfare

VoC_Welfare_types = read.csv("Datasets/VoC_welfare.csv", sep=";", quote="") %>% 
  rename(country = X.Country,
         VoC_HS = Varieties.of.capitalism.,
         country_text_id = Country.code,
         welfare_E = Three.worlds,
         VoC_Kitschelt = Types.of.capitalism
         ) %>% 
  mutate(country = gsub("\"", "", country),
         VoC_HS = gsub("\"", "", VoC_HS),
  )

#write.csv2(VoC_Welfare_types, "C:/RTest/types.csv")

modes_cluster = Economy_Perfomance_final %>% 
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st))


VoC_welfare_dmx = dmx_trade_cluster %>% 
  left_join(modes_cluster, by="country_text_id") %>% 
  left_join(VoC_Welfare_types %>% select(-country), by="country_text_id")


tbl = table(VoC_welfare_dmx$cluster_label_1st, VoC_welfare_dmx$welfare_E)
prop.table(table(VoC_welfare_dmx$cluster_label_1st, VoC_welfare_dmx$welfare_E), 1)
tbl = table(VoC_welfare_dmx$cluster_label_1st_mode, VoC_welfare_dmx$VoC_HS)
prop.table(tbl, 1)
tbl = table(VoC_welfare_dmx$cluster_label_1st_mode, VoC_welfare_dmx$VoC_Kitschelt)
prop.table(tbl, 1)
chisq = chisq.test(tbl)


library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)
