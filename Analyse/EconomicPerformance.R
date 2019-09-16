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

library(describedata)

gladder(Economy_Perfomance_IP$GDP_capita_oecd)
gladder(Economy_Perfomance_IP$GNI_capita_oecd)
gladder(Economy_Perfomance_IP$Inflation_oecd)
gladder(Economy_Perfomance_IP$Unemployment_oecd+0.001)
gladder(Economy_Perfomance_IP$Unemployment_wdi+0.001)

hist(sqrt(Economy_Perfomance_IP$Unemployment_oecd))

sqrt_fun = function(x) {
  sqrt(x)
}

install.packages("rcompanion")
library(rcompanion)

ladder_fun = function(x) {
  constant = min(x, na.rm=T)
  constant = sqrt(constant^2)
  y = transformTukey(x+constant)
  return(y)
}



Economy_Perfomance_IP_norm = Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("wdi"))) %>%
  mutate_at(vars(matches("inflation"), matches("unemployment")), ~set0_fun(.)) %>% 
  mutate_at(vars(matches("inflation")), ~trim(., 0.01, minimum = T))  %>% 
  mutate_at(vars(matches("capita"), matches("inflation"), matches("Unemployment_oecd")), ~log10_fun(.)) %>% 
  mutate_at(vars(matches("Unemployment_oecd")), ~sqrt_fun(.)) %>%
  mutate_all(scale)

Economy_Perfomance_IP_norm = Economy_Perfomance_IP %>% 
  select_at(vars(ends_with("oecd"), ends_with("wdi"))) %>%
  mutate_at(vars(matches("inflation")), ~trim(., 0.05, minimum = T))  %>% 
  mutate_all(ladder_fun) %>% 
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

fa_data_economy_oecd_frame = Economy_Perfomance_IP_norm %>% 
  bind_cols(Economy_Perfomance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country_text_id, year, ends_with("oecd")))  %>%
  filter(is.na(GDP_capita_oecd) == F) 


fa_data_economy_oecd_thin_frame = fa_data_economy_oecd_frame %>% 
  select(-Unemployment_oecd) %>% 
  mutate(non_na_count = rowSums(is.na(fa_data_economy_oecd_frame %>%  select_at(vars(ends_with("oecd"))))==F)) %>% 
  filter(non_na_count >= 2)


fa_data_economy_oecd_thin = fa_data_economy_oecd_thin_frame %>% 
  select_at(vars(ends_with("oecd")))



fa.parallel(fa_data_economy_oecd_thin, fm="ml", n.iter=100)
fa_oecd_eco_thin = fa(fa_data_economy_oecd_thin, 1, rotate="varimax", missing=F, fm="ml")
fa.diagram(fa_oecd_eco_thin, cut=0)
biplot.psych(fa_oecd_eco_thin)

i_eco_thin = imputePCA(data.frame(fa_data_economy_oecd_thin),ncp=1)
fa_oecd_eco_thin_i = fa(i_eco_thin$completeObs, 1, rotate="varimax", missing=F, fm="ml")
fa.diagram(fa_oecd_eco_thin_i, cut=0)



fa_data_economy_oecd_thick_frame = fa_data_economy_oecd_frame %>% 
  mutate(non_na_count = rowSums(is.na(fa_data_economy_oecd_frame %>%  select_at(vars(ends_with("oecd"))))==F)) %>%
  filter(is.na(Unemployment_oecd) == F) %>% 
  filter(non_na_count >= 2)


fa_data_economy_oecd_thick = fa_data_economy_oecd_thick_frame %>% 
  select_at(vars(ends_with("oecd")))  

fa.parallel(fa_data_economy_oecd_thick, fm="ml", n.iter=100)
fa_oecd_eco_thick = fa(fa_data_economy_oecd_thick, 1, rotate="varimax", missing=F, fm="ml")
fa.diagram(fa_oecd_eco_thick, cut=0)
biplot.psych(fa_oecd_eco_thick)

i_eco_thick = imputePCA(data.frame(fa_data_economy_oecd_thick),ncp=1)
fa_oecd_eco_thick_i = fa(i_eco_thick$completeObs, 1, rotate="varimax", missing=F, fm="ml")
fa.diagram(fa_oecd_eco_thick_i, cut=0)



fa_data_economy_oecd_thin_frame_scores = fa_data_economy_oecd_thin_frame %>% 
  bind_cols(data.frame(fa_oecd_eco_thin_i$scores) %>%  rename(economy_thin_index_oecd = ML1)) %>% 
  select_at(vars(country_text_id, year, matches("index")))

fa_data_economy_oecd_thick_frame_scores = fa_data_economy_oecd_thick_frame %>% 
  bind_cols(data.frame(fa_oecd_eco_thick_i$scores) %>%  rename(economy_1_thick_index_oecd = ML1)) %>% 
  select_at(vars(country_text_id, year, matches("index")))




#### WDI ####

fa_data_economy_wdi_frame = Economy_Perfomance_IP_norm %>% 
  bind_cols(Economy_Perfomance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country_text_id, year, ends_with("wdi")))  %>%
  filter(is.na(GDP_capita_wdi) == F) 


fa_data_wdi_thin_frame = fa_data_economy_wdi_frame %>% 
  select(-Unemployment_wdi) %>% 
  mutate(non_na_count = rowSums(is.na(fa_data_economy_wdi_frame %>%  select_at(vars(ends_with("wdi"))))==F)) %>% 
  filter(non_na_count >= 2)

fa_data_economy_wdi_thin = fa_data_wdi_thin_frame %>% 
  select_at(vars(ends_with("wdi")))


fa.parallel(fa_data_economy_wdi_thin, fm="ml", n.iter=100)
fa_wdi_thin = fa(fa_data_economy_wdi_thin, 1, rotate="varimax", fm="ml")
fa.diagram(fa_wdi_thin, cut=0)
biplot.psych(fa_wdi_thin)

i_eco_wdi_thin = imputePCA(data.frame(fa_data_economy_wdi_thin),ncp=1)
fa_wdi_eco_thin_i = fa(i_eco_wdi_thin$completeObs, 1, rotate="varimax", fm="ml")
fa.diagram(fa_wdi_eco_thin_i, cut=0)



fa_data_wdi_thick_frame = fa_data_economy_wdi_frame %>% 
  filter(year > 1990) %>% 
  filter(is.na(Unemployment_wdi) == F)

fa_data_wdi_thick = fa_data_wdi_thick_frame %>% 
  select_at(vars(ends_with("wdi")))


fa.parallel(fa_data_wdi_thick, fm="ml", n.iter=100)
fa_wdi_thick = fa(fa_data_wdi_thick, 1, rotate="varimax", fm="ml")
fa.diagram(fa_wdi_thick, cut=0)
biplot.psych(fa_wdi_thick)

i_eco_wdi_thick = imputePCA(data.frame(fa_data_wdi_thick),ncp=1)
fa_wdi_eco_thick_i = fa(i_eco_wdi_thick$completeObs, 1, rotate="varimax", fm="ml")
fa.diagram(fa_wdi_eco_thick_i, cut=0)




fa_data_economy_wdi_thin_frame_scores = fa_data_wdi_thin_frame %>% 
  bind_cols(data.frame(fa_wdi_eco_thin_i$scores) %>%  rename(economy_thin_index_wdi = ML1))%>% 
  select_at(vars(country_text_id, year, matches("index")))

fa_data_economy_wdi_thick_frame_scores = fa_data_wdi_thick_frame %>% 
  bind_cols(data.frame(fa_wdi_eco_thick_i$scores) %>%  rename(economy_1_thick_index_wdi = ML1)) %>% 
  select_at(vars(country_text_id, year, matches("index")))





#### Combining ####

Economy_Perfomance_final = Economy_Perfomance %>% 
  left_join(fa_data_economy_wdi_thin_frame_scores, by=c("country_text_id", "year")) %>% 
  left_join(fa_data_economy_oecd_thin_frame_scores, by=c("country_text_id", "year")) %>%  
  left_join(fa_data_economy_wdi_thick_frame_scores, by=c("country_text_id", "year")) %>%  
  left_join(fa_data_economy_oecd_thick_frame_scores, by=c("country_text_id", "year")) %>% 
  #mutate_at(vars(matches("2_thick")), ~inverser(.)) %>% 
  mutate_at(vars(matches("index")), ~EPI_fun(.))


Economy_Perfomance_final %>% 
  select_at(vars(matches("index"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


Economy_Perfomance_final %>% 
  select_at(vars(year, matches("index"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean, Q1 = fun_quantile25, Q3=fun_quantile75), na.rm=T) %>% 
  melt(id.vars=c("year")) %>% 
  mutate(source = if_else(grepl("oecd", variable), "oecd", "wdi")) %>%
  mutate(concept = if_else(grepl("1_thick", variable), "thick_1", if_else(grepl("2_thick", variable), "thick_2", "thin"))) %>% 
  tidyr::unite("comb", source, concept) %>% 
  ggplot(aes(x=year, y=value, col=variable)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(comb ~ .) +
  ylim(0,100)


samples = c("CZE", "DEU")

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
  filter(year < 1990) %>% 
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
