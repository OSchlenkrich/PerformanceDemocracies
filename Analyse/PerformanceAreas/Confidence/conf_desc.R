# Confidence Descirptive Analyse of Factor Scores

source("Analyse/Confidence/conf_FA.R")


samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")


conf_scores %>% 
  filter(country_text_id %in% samples) %>% 
  dplyr::select(-survey, -weights) %>%
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_score = mean(value, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_score, col=country_text_id)) +
  geom_line(size=1) +
  theme_bw()



#### 


confidence_final = confidence_IVS %>%
  dplyr::select(survey, country, country_text_id, regions, year = year_study, classification_context, cluster_label_1st) %>%
  filter(year >= 1990) %>% 
  bind_cols(conf_scores %>% dplyr::select(-survey)) %>% 
  ungroup() 



confidence_final_agg = confidence_final %>% 
  dplyr::select(country_text_id, year, conf_index) %>%
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(conf_index = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  mutate_at(vars(matches("index")), ~EPI_fun(.))  %>%  
  left_join(dmx_trade_cluster %>%  dplyr::select(-country, -regions), 
            by=c("country_text_id", "year"))
  

samples = c("GBR","NZL", "SWE", "USA", "DEU", "FRA")

confidence_final_agg %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  xlim(1990, 2020) +
  ylim(0,100) +
  facet_wrap(variable ~ ., scales="free_y") +
  theme_bw() 



confidence_final_agg %>% 
  select_at(vars(matches("index"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") +
  theme_bw()



confidence_final_agg %>% 
  select_at(vars(year, matches("index"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean, min = fun_quantile25, max=fun_quantile75), na.rm=T) %>% 
  ggplot(aes(x=year, y=mean)) +
  geom_line(size=1) +
  geom_line(aes(x=year, y=max)) +
  geom_line(aes(x=year, y=min)) +
  geom_point() +
  ylim(0,100) +
  theme_bw() 


samples = c("LUX", "IND", "SWE", "FRA", "DNK", "EST", "USA", "DEU")

confidence_final_agg %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) +
  ylim(0,100) +
  theme_bw()



###

modes_cluster = confidence_final_agg %>% 
  filter(year < 1990) %>% 
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st))

modes_cluster %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n())

confidence_final_agg %>% 
  left_join(modes_cluster, by="country_text_id") %>% 
  group_by(cluster_label_1st_mode, year) %>% 
  select_at(vars(cluster_label_1st_mode, year, matches("index"))) %>% 
  summarise_all(mean, na.rm=T) %>% 
  filter(is.na(cluster_label_1st_mode)==F) %>% 
  melt(id.vars=c("cluster_label_1st_mode", "year")) %>% 
  ggplot(aes(x=year, y=value, col=cluster_label_1st_mode)) +
  geom_line(size=1) +
  ylim(0,100) +
  geom_point() +
  facet_wrap(variable ~ .) 



confidence_final_agg %>% 
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


print("End of Confidence Script")