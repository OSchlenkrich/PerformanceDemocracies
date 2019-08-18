QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)


test = QoC_data %>% 
  select(country_text_id, year,
         CO2 = oecd_greenhouse_t1)

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
         #GDP_growth_wdi = wdi_gdpgr,
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


#####
Economy_IQR = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_oecd = oecd_sizegdp_t1,
         #GDP_growth_oecd = oecd_evogdp_t1,
         Inflation_oecd = oecd_cpi_t1a,
         Interest_oecd = oecd_ltintrst_t1,
         Unemployment_oecd = oecd_unemplrt_t1c,
         # Debt_oecd = oecd_govdebt_t1,
         # Researchers_oecd = oecd_research_t1,
         # SpendingRD_oecd = oecd_gerd_t1,
         
         GDP_capita_wdi = wdi_gdpcappppcon2011,
         # GDP_growth_wdi = wdi_gdpgr,
         Inflation_wdi = wdi_inflation,
         Interest_wdi = wdi_intrate,
         Unemployment_wdi = wdi_unempilo,
         Unemployment_youth_wdi = wdi_unempyilo,
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
         Unemployment_youth_wdi_sgi = SGI_fun_WDI(Unemployment_youth_wdi, Unemployment_youth_wdi)
         ) %>% 
  mutate(Inflation_oecd_sgi = max(Inflation_oecd_sgi, na.rm=T) - Inflation_oecd_sgi,
         Interest_oecd_sgi = max(Interest_oecd_sgi, na.rm=T) - Interest_oecd_sgi,
         Unemployment_oecd_sgi = max(Unemployment_oecd_sgi, na.rm=T) - Unemployment_oecd_sgi,
         #Debt_oecd_sgi = max(Debt_oecd_sgi, na.rm=T) - Debt_oecd_sgi,
         
         Inflation_wdi_sgi = max(Inflation_wdi_sgi, na.rm=T) - Inflation_wdi_sgi,
         Interest_wdi_sgi = max(Interest_wdi_sgi, na.rm=T) - Interest_wdi_sgi,
         Unemployment_wdi_sgi = max(Unemployment_wdi_sgi, na.rm=T) - Unemployment_wdi_sgi,
         Unemployment_youth_wdi_sgi = max(Unemployment_youth_wdi_sgi, na.rm=T) - Unemployment_youth_wdi_sgi,
         # Debt_wdi_sgi = max(Debt_wdi_sgi, na.rm=T) - Debt_wdi_sgi,
  ) %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("oecd_sgi")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("wdi_sgi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()


na_interpol = function(x) {
  if (length(na.omit(x)) >= 2) {
    y = na_interpolation(x,  option = "linear", maxgap = 5)
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



NA_plot(Economy_IQR, "Economy")
fa.parallel(Economy_IQR %>% select_at(vars(ends_with("oecd_sgi"))), fm="ml")

fa.parallel(Economy_IQR %>% select(GDP_capita_wdi_sgi, Unemployment_wdi_sgi, Inflation_wdi_sgi), fm="ml")
fa_solution = fa(Economy_IQR %>% select(GDP_capita_wdi_sgi, Unemployment_wdi_sgi, Inflation_wdi_sgi) %>% na.omit(), 1, fm="ml")


fa_solution = fa(Economy_IQR %>% select_at(vars(ends_with("oecd_sgi"))) %>% na.omit(), 2, fm="ml")
fa_solution = pca(Economy_IQR %>% select_at(vars(ends_with("wdi_sgi"))), 2, rotate="varimax")
fa_solution
fa.diagram(fa_solution, cut=.1)



Economy %>% 
  ggplot(aes(x=as.factor(Interest_wdi), y=Debt_wdi)) +
  geom_boxplot()


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

