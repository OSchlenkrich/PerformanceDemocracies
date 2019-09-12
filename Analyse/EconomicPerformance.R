# Economic Performance
source("Analyse/Cluster_v3.R")


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)


Economy_Perfomance = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_oecd = oecd_sizegdp_t1,
         GNI_capita_oecd = oecd_natinccap_t1,
         Inflation_oecd = oecd_cpi_t1a,
         #Interest_oecd = oecd_ltintrst_t1,
         Unemployment_oecd = oecd_unemplrt_t1c,

         GDP_capita_wdi = wdi_gdpcapcur,
         GNI_capita_wdi = wdi_gniatlcur,
         Inflation_wdi = wdi_inflation,
         #Interest_wdi = wdi_intrate,
         Unemployment_wdi = wdi_unempilo,
  ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_trade_cluster, by=c("country_text_id", "year"))  %>%
  filter(year > 1950, is.na(cluster_label_1st) == F) %>% 
  select(country, country_text_id, everything())  %>% 
  group_by(country_text_id) %>%
  mutate(country = unique(na.omit(country))) %>% 
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
  select_at(vars(ends_with("oecd"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
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


set0_fun = function(x) {
  x = x - min(x, na.rm=T) + 0.001
}

Economy_Perfomance_IP_norm = Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("wdi"))) %>%
  mutate_at(vars(matches("inflation")), ~set0_fun(.)) %>% 
  mutate_at(vars(matches("capita"), matches("inflation")), ~log10_fun(.)) %>% 
  mutate_at(vars(matches("inflation"), matches("interest"), matches("unemployment")), ~trim(., 0.05, minimum = T)) %>% 
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


#### Factor Analysis

fa_data_oecd_thin = Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("oecd")))  %>% 
  select(-Unemployment_oecd)

fa.parallel(fa_data_oecd_thin, fm="ml")
fa_oecd_thin = pca(fa_data_oecd_thin, 1, rotate="varimax")
fa.diagram(fa_oecd_thin, cut=0)
biplot.psych(fa_oecd_thin)

fa_data_oecd_thick = Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("oecd")))  

fa.parallel(fa_data_oecd_thick, fm="ml")
fa_oecd_thick = pca(fa_data_oecd_thick, 2, rotate="varimax")
fa.diagram(fa_oecd_thick, cut=0)
biplot.psych(fa_oecd_thick)

###


fa_data_wdi_thin = Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("wdi")))  %>% 
  select(-Unemployment_wdi)

fa.parallel(fa_data_wdi_thin, fm="ml")
fa_wdi_thin = pca(fa_data_wdi_thin, 1, rotate="varimax")
fa.diagram(fa_wdi_thin, cut=0)
biplot.psych(fa_wdi_thin)

fa_data_wdi_thick = Economy_Perfomance_IP_norm %>% 
  select_at(vars(ends_with("wdi")))  

fa.parallel(fa_data_wdi_thick, fm="ml")
fa_wdi_thick = pca(fa_data_wdi_thick, 2, rotate="varimax")
fa.diagram(fa_wdi_thick, cut=0)
biplot.psych(fa_wdi_thick)


#### Combining
inverser = function(x) {
  x = x *-1
  return(x)
}

Economy_Perfomance_final = Economy_Perfomance %>% 
  bind_cols(economy_thin_index_oecd = fa_oecd_thin$scores) %>%
  bind_cols(data.frame(fa_oecd_thick$scores) %>%  rename(economy_1_thick_index_oecd = RC1, economy_2_thick_index_oecd = RC2)) %>% 
  bind_cols(economy_thin_index_wdi = fa_wdi_thin$scores) %>% 
  bind_cols(data.frame(fa_wdi_thick$scores) %>%  rename(economy_1_thick_index_wdi = RC1, economy_2_thick_index_wdi = RC2)) %>%
  mutate_at(vars(matches("2_thick")), ~inverser(.)) %>% 
  mutate_at(vars(matches("index")), ~EPI_fun(.))



Economy_Perfomance_final %>% 
  select_at(vars(matches("index"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 



samples = c("AUS","AUT", "DEU", "USA", "LBR", "GBR")

Economy_Perfomance_final %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) 


###

modes_cluster = Economy_Perfomance_final %>% 
  filter(year <= 1990) %>% 
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st))

modes_cluster %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n())

Economy_Perfomance_final %>% 
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



Economy_Perfomance_final %>% 
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
