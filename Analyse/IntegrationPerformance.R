# IntegrationPerformance
source("Analyse/Cluster_v3.R")



QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)


Integration_Performance = QoC_data %>% 
  select(country_text_id, year, 
         Combined_GI = sc_tgen,
         Unemployment_t_GI = sc_uegen,
         Pension_t_GI = sc_pgen,
         Sickness_t_GI = sc_skgen,
         
         Unemployment_s_rr_GI = sc_ue,
         Unemployment_f_rr_GI = sc_uef,
         
         Sickness_s_rr_GI = sc_sick,
         Sickness_f_rr_GI = sc_sickf,
         
         Pension_s_rr_GI = sc_sp,
         Pension_c_rr_GI = sc_spc,
         
         gini_lis = lis_gini,
         poverty9010_lis = lis_pr9010,
         
         gini_wdi = wdi_gini
  )  %>% 
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

Integration_Performance %>% 
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


Integration_Performance %>% 
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

Integration_Performance %>% 
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





#### Linear Interpolation ####


Integration_Performance_IP = Integration_Performance %>%
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("GI")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("lis")), .funs = list(~na_interpol(.))) %>% 
  mutate_at(vars(ends_with("wdi")), .funs = list(~na_interpol(.))) %>% 
  ungroup()



Integration_Performance_IP %>% 
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


Integration_Performance_IP %>% 
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

Integration_Performance_IP %>% 
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



###


gladder(Integration_Performance_IP$Unemployment_f_rr_GI)
gladder(Integration_Performance_IP$poverty9010_lis)
gladder(Integration_Performance_IP$gini_lis)
gladder(Integration_Performance_IP$gini_wdi)


Integration_Performance_IP %>% 
  select_at(vars(ends_with("_GI"),ends_with("_lis"),ends_with("_wdi"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 


Integration_Performance_IP %>% 
  select_at(vars(ends_with("_GI"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 

Integration_Performance_IP %>% 
  select_at(vars(ends_with("_lis"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 

Integration_Performance_IP %>% 
  select_at(vars(ends_with("_wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


####
square_fun = function(x) {
  1/(x^2)
}

Integration_Performance_IP_norm = Integration_Performance_IP %>% 
  select_at(vars(ends_with("_GI"),ends_with("_lis"),ends_with("_wdi"))) %>%
  mutate_all(ladder_fun) %>%
  mutate_all(scale)

  mutate_at(vars(matches("_lis"), matches("_wdi")), ~square_fun(.)) %>% 
  mutate_all(~trim(., 0.01, minimum = T)) %>%
  mutate_all(scale)

Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_GI"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_wdi"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

Integration_Performance_IP_norm %>% 
  select_at(vars(ends_with("_lis"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")



#### Factor Analysis

fa_data_GI_frame = Integration_Performance_IP_norm %>% 
  bind_cols(Integration_Performance %>%  select(country, country_text_id, year)) %>% 
  mutate(non_na_count = rowSums(is.na(Integration_Performance_IP_norm %>%  select_at(vars(ends_with("t_GI"))))==F)) %>% 
  filter(non_na_count >= 2, year>=1967) %>% 
  select_at(vars(country, country_text_id, year, ends_with("t_GI"), -ends_with("Combined_GI"))) %>% 
  filter(is.na(Pension_t_GI) == F)


fa_data_GI = fa_data_GI_frame %>% 
  select_at(vars(ends_with("_GI"))) 


fa.parallel(fa_data_GI, fm="ml", n.iter = 100)
fa_GI_env = fa(fa_data_GI, 2, rotate="varimax", missing=F, fm="ml")
fa.diagram(fa_GI_env, cut=0)
biplot.psych(fa_GI_env)


# no missings
# GI.comp = imputePCA(fa_data_GI, ncp=2)
# GI.pca = fa(GI.comp$completeObs, 2, rotate="varimax", missing=F, fm="ml")
# fa.diagram(GI.pca, cut=0)


fa_data_GI_frame_scores = fa_data_GI_frame %>% 
  bind_cols(
    data.frame(fa_GI_env$scores) %>% 
      rename(integration_1_index_GI = ML1,
             integration_2_index_GI = ML2)
  ) %>% 
  select_at(vars(country_text_id, year, matches("index")))


####


#### Combining

Integration_Performance_final = Integration_Performance %>% 
  select_at(vars(-ends_with("wdi"), -ends_with("lis"), -ends_with("GI"))) %>% 
  bind_cols(Integration_Performance_IP_norm) %>% 
  left_join(fa_data_GI_frame_scores, by=c("country_text_id", "year")) %>% 
  mutate(
    gini_lis_index = gini_lis,
    poverty9010_lis_index = poverty9010_lis,
    gini_wdi_index = gini_wdi
  ) %>% 
  mutate_at(vars(ends_with("lis_index"), ends_with("lis_index"), ends_with("wdi_index")), inverser)%>% 
  mutate_at(vars(matches("_index")), ~EPI_fun(.))

names(Integration_Performance_final)

samples = c("USA","DEU","SWE")

Integration_Performance_final %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  facet_wrap(variable ~ ., scales="free_y") +
  ylim(0,100)



Integration_Performance_final %>% 
  select_at(vars(matches("index"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


Integration_Performance_final %>% 
  select_at(vars(year, matches("index"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean, Q1 = fun_quantile25, Q3=fun_quantile75), na.rm=T) %>% 
  melt(id.vars=c("year")) %>% 
  mutate(source = if_else(grepl("GI", variable), "gi_index", if_else(grepl("lis", variable), "lis", "wdi_gini"))) %>%
  mutate(concept = if_else(grepl("gini_lis", variable), "gini_lis", if_else(grepl("9010_lis", variable), "9010_lis", "thin"))) %>% 
  tidyr::unite("comb", source, concept) %>% 
  ggplot(aes(x=year, y=value, col=variable)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(comb ~ .) +
  ylim(0,100)




samples = c("LUX", "DEU", "SWE", "FRA", "DNK", "EST", "USA")

Integration_Performance_final %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) +
  ylim(0,100)


###

modes_cluster = Integration_Performance_final %>% 
  filter(year < 1990) %>% 
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st))

modes_cluster %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n())

Integration_Performance_final %>% 
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



Integration_Performance_final %>% 
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
