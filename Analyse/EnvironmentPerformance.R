# Economic Performance
source("Analyse/Cluster_v3.R")


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)



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
         greenhouse_wdi = wdi_co2
         
  ) %>% 
  mutate(population = population) %>% 
  mutate_at(vars(ends_with("oecd")), funs(per_captita = per_capita_maker(., population)))  %>% 
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



fa.parallel(Environment_IQR %>%  select_at(vars(ends_with("_oecd"))) %>% na.omit(), fm="ml", plot=T, sim=T, n.iter=100, quant=0.95)


fa_solution = pca(Environment_IQR %>%  select_at(vars(ends_with("_oecd"))) %>% na.omit(), 1)
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
