# Economic Performance
source("Analyse/Cluster_v3.R")


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)



per_capita_maker = function(x, pop) {
  # no values which are exactly 0
  (x+(1/2))/(pop+1)
}


dmx_trade_cluster_framed = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  group_by(country) %>% 
  tidyr::complete(country, year = min(year):max(year), fill = list(NA))



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
  filter(country_text_id %in% unique(dmx_trade_cluster_framed$country_text_id)) %>% 
  left_join(dmx_trade_cluster_framed, by=c("country_text_id", "year"))  %>%
  select(country, country_text_id, everything())  %>% 
  group_by(country_text_id) %>% 
  mutate(country = unique(na.omit(country))) %>% 
  filter(year >= 1950) %>% 
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
  #mutate_all(ladder_fun) %>% 
  mutate_all(funs(folded_ladder_fun(., plotting =T))) %>% 
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


### NA Frame
# Data range: 1990 - 2016
NA_frame_env_oecd = Environment_Performance_IP_norm %>% 
  select(-greenhouse_wdi_per_capita) %>% 
  mutate(non_na_perc = rowSums(is.na(.)==F)/dim(.)[2]) %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>%
  filter(year>=1990, year <= 2016) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = min(year):max(year), fill = list(NA)) %>% 
  ungroup() %>% 
  select(country_text_id, year, non_na_perc)
  
dim(fa_data_oecd_frame)

#### Factor Analysis: MI

fa_data_oecd_frame = Environment_Performance_IP_norm %>% 
  bind_cols(Environment_Performance %>%  select(country, country_text_id, year)) %>% 
  select_at(vars(country, country_text_id, year, ends_with("oecd_per_capita"))) %>% 
  right_join(NA_frame_env_oecd, by=c("country_text_id", "year"))
  

### KOM-Test
KMO(fa_data_oecd_frame %>% 
      select_at(vars(ends_with("oecd_per_capita"))) ) 
cor(fa_data_oecd_frame %>% 
      select_at(vars(ends_with("oecd_per_capita"), ends_with("wdi_per_capita"))) , use="pairwise")



# MICE

fa_data_oecd_frame_mice = fa_data_oecd_frame %>% 
  mutate(id = group_indices(., country_text_id)) %>% 
  left_join(aux_vars_env, by=c("country", "year")) %>% 
  select(-country, -country_text_id) %>% 
  # mutate(year = year - min(year),
  #        year1 = poly(year,1)[,1]
  # ) %>%
  # select(-year) %>% 
  rename_all(funs(sub("_oecd_int_oecd_per_capita", "", .))) %>% 
  select(-co2) 



pdf(file="Plots/m_pattern_env.pdf",width=20,height=8)
md.pattern(fa_data_oecd_frame_mice, rotate.names = T)
dev.off()

pred_matrix = make.predictorMatrix(fa_data_oecd_frame_mice)


pred_matrix[,"id"] = -2


nr_immputations = 2
fa_data_oecd_frame_mice_result = mice(fa_data_oecd_frame_mice, pred=pred_matrix, meth="2l.pmm",
     m = nr_immputations, maxit = 10)
head(fa_data_oecd_frame_mice_result$loggedEvents, 2)



densityplot(fa_data_oecd_frame_mice_result)
plot(fa_data_oecd_frame_mice_result)


fa_data_oecd_frame_mice %>%
  data.frame() %>% 
  mutate(.imp = "observed",
         .id = 1)  %>% 
  bind_rows(complete(fa_data_oecd_frame_mice_result, "long")%>% 
              mutate(.imp = as.factor(.imp))) %>% 
  select(-".id", -"id", -year1, -year2) %>% 
  filter(.imp == "observed" | .imp  %in% sample(nr_immputations,3)) %>% 
  melt(id.vars=".imp") %>% 
  ggplot(aes(x=variable, y=value, fill=as.factor(.imp))) +
  geom_boxplot() +
  coord_flip()

fa.parallel(my_imputet_data[[3]][,1:5], fm="mle", n.iter=100, quant=0.95)
vss(my_imputet_data[[3]][,1:5], fm="mle", rotate="none")
fa_oecd_env = fa(my_imputet_data[[3]][,1:5], 1, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_env, cut=0)



produce_fa_scores = function(mice_data, nr_immputations, nr_factors) {
  scores_data = data.frame(matrix(NA, dim(my_imputet_data[[1]])[1], nr_immputations)) %>% 
    rename_all(funs(sub("X", "imp_", .)))
  
  for (i in 1:nr_immputations) {
    stack_data = my_imputet_data[[i]][,1:5]

    fa_stack = fa(stack_data, nr_factors, rotate="promax", missing=F, fm="mle")
    scores_data[,i] = as.numeric(fa_stack$scores)
  }
  return(scores_data)
}


env_scores = fa_data_oecd_frame %>% 
  select(country, country_text_id, year) %>% 
  bind_cols(produce_fa_scores(fa_data_oecd_frame_mice_result, 10, 1))

samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")


env_scores %>% 
  select(-country) %>%
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_score = mean(value, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_score, col=country_text_id)) +
  geom_line(size=1)
  
### KOM-Test
KMO(fa_data_oecd) 
cor(fa_data_oecd, use="pairwise")



fa.parallel(fa_data_oecd, fm="mle", n.iter=100, quant=0.95)
vss(fa_data_oecd, fm="mle", rotate="none")


fa_oecd_env = fa(fa_data_oecd, 1, rotate="oblimin", missing=F, fm="mle")
fa.diagram(fa_oecd_env, cut=0)
biplot.psych(fa_oecd_env)

nb = estim_ncpPCA(fa_data_oecd,ncp.max=5)
res.comp = imputePCA(data.frame(fa_data_oecd),ncp=1)
res.pca = fa(res.comp$completeObs, 1, rotate="varimax", fm="ml")
fa.diagram(res.pca, cut=0)




#### Combining

env_scores_mean = env_scores %>% 
  select(-country) %>%
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(environment_index = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  select(environment_index)

Environment_Performance_final = Environment_Performance %>% 
  left_join(bind_cols(fa_data_oecd_frame, env_scores_mean) %>% 
              select_at(vars(country_text_id, year, matches("index")))
            , by=c("country_text_id", "year")) %>% 
  mutate_at(vars(matches("environment_index")), ~inverser(.)) %>% 
  mutate_at(vars(matches("index")), ~EPI_fun(.))


samples = c("CAN","IND", "DEU", "USA")


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
