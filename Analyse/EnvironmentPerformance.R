# Economic Performance
source("Analyse/Cluster_v3.R")


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)



per_capita_maker = function(x, pop) {
  x/pop
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
  mutate_at(vars(ends_with("oecd")), funs(int_oecd_per_capita = per_capita_maker(., GDP_capita_wdi*population_wdi)))  %>%
  mutate(greenhouse_wdi_per_capita = (greenhouse_wdi_per_capita*population_wdi)/(GDP_capita_wdi*population_wdi),
         test = GDP_capita*population) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year"))  %>%
  select(country, country_text_id, everything())  %>% 
  filter(year > 1950, is.na(cluster_label_1st) == F) %>% 
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

gladder(Environment_Performance_IP$greenhouse_oecd_per_capita)
gladder(Environment_Performance_IP$sulphur_oecd_per_capita)
gladder(Environment_Performance_IP$nitrogen_oecd_per_capita)
gladder(Environment_Performance_IP$co2_oecd_per_capita)
gladder(Environment_Performance_IP$water_oecd_per_capita)
gladder(Environment_Performance_IP$waste_oecd_per_capita)



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


Environment_Performance_IP_norm = Environment_Performance_IP %>% 
  select_at(vars(ends_with("oecd_per_capita"), ends_with("wdi_per_capita"))) %>%
  mutate_all(ladder_fun) %>% 
  #mutate_all(~trim(., 0.01, minimum = T)) %>%
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


#### Factor Analysis

fa_data_oecd_frame = Environment_Performance_IP_norm %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>% 
  mutate(non_na_count = rowSums(is.na(Environment_Performance_IP_norm %>%  select(-greenhouse_wdi_per_capita))==F)) %>% 
  filter(non_na_count >= 3, year>=1990) %>% 
  select_at(vars(country, country_text_id, year, ends_with("oecd_per_capita"))) 


fa_data_oecd = fa_data_oecd_frame %>% 
  select_at(vars(ends_with("oecd_per_capita"))) 


fa.parallel(fa_data_oecd, fm="ml", n.iter=100 )
fa_oecd_env = fa(fa_data_oecd, 1, rotate="varimax", missing=F, fm="ml")
fa.diagram(fa_oecd_env, cut=0)
biplot.psych(fa_oecd_env)

nb = estim_ncpPCA(fa_data_oecd,ncp.max=5)
res.comp = imputePCA(data.frame(fa_data_oecd),ncp=1)
res.pca = fa(res.comp$completeObs, 1, rotate="varimax", fm="ml")
fa.diagram(res.pca, cut=0)




#### Combining
Environment_Performance_final = Environment_Performance %>% 
  left_join(bind_cols(fa_data_oecd_frame, environmental_1_index_oecd = res.pca$scores)%>% 
              select_at(vars(country_text_id, year, matches("index")))
            , by=c("country_text_id", "year")) %>% 
  bind_cols(environmental_1_index_wdi = Environment_Performance_IP_norm$greenhouse_wdi_per_capita) %>% 
  mutate_at(vars(matches("environmental_1")), ~inverser(.)) %>% 
  mutate_at(vars(matches("index")), ~EPI_fun(.))


samples = c("CAN","IND")

Environment_Performance_final %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, greenhouse_wdi_per_capita, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  facet_wrap(variable ~ ., scales="free_y") 



Environment_Performance_final %>% 
  select_at(vars(matches("index"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


Environment_Performance_final %>% 
  select_at(vars(year, matches("index"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean, Q1 = fun_quantile25, Q3=fun_quantile75), na.rm=T) %>% 
  melt(id.vars=c("year")) %>% 
  mutate(source = if_else(grepl("oecd", variable), "oecd", "wdi")) %>% 
  ggplot(aes(x=year, y=value, col=variable)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(source ~ .) +
  ylim(0,100)




samples = c("LUX", "IND", "SWE", "FRA", "DNK", "EST", "USA")

Environment_Performance_final %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) +
  ylim(0,100)

###

modes_cluster = Environment_Performance_final %>% 
  filter(year < 1990) %>% 
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st))

modes_cluster %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n())

Environment_Performance_final %>% 
  left_join(modes_cluster, by="country_text_id") %>% 
  group_by(cluster_label_1st_mode, year) %>% 
  select_at(vars(cluster_label_1st_mode, year, matches("index"))) %>% 
  summarise_all(mean, na.rm=T) %>% 
  filter(is.na(cluster_label_1st_mode)==F) %>% 
  melt(id.vars=c("cluster_label_1st_mode", "year")) %>% 
  ggplot(aes(x=year, y=value, col=cluster_label_1st_mode)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) 



Environment_Performance_final %>% 
  left_join(modes_cluster, by="country_text_id") %>% 
  group_by(cluster_label_1st, year) %>% 
  select_at(vars(cluster_label_1st, year, matches("index"))) %>% 
  summarise_all(mean, na.rm=T) %>% 
  filter(is.na(cluster_label_1st)==F) %>% 
  melt(id.vars=c("cluster_label_1st", "year")) %>% 
  ggplot(aes(x=year, y=value, col=cluster_label_1st)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) 
