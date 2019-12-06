# Economic Descriptive Analyse of Factor Scores

source("Analyse/Economy/eco_FA.R")


samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")


eco_scores %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_score = mean(value, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_score, col=country_text_id)) +
  geom_line(size=1) +
  theme_bw()




#### Combining MIs

eco_scores_oecd_mean = eco_scores %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(eco_oecd_index = mean(value, na.rm=T)) %>% 
  ungroup() 


#### 


Economy_Performance_final = Economy_Perfomance %>%
  select(country, country_text_id, regions, year, classification_context, cluster_label_1st) %>% 
  left_join(eco_scores_oecd_mean, by=c("country_text_id", "year")) %>% 
  left_join(fa_data_wdi_frame, by=c("country_text_id", "year")) %>%
  left_join(fa_data_oecd_frame %>% select(-country), by=c("country_text_id", "year")) %>%
  ungroup() %>% 
  mutate_at(vars(matches("index")), ~EPI_fun(.))



samples = c("GBR","NZL", "SWE", "USA", "DEU", "FRA", "IND")
samples = c("GBR","JPN", "USA", "IND", "ESP", "TTO")

Economy_Performance_final %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  xlim(1970, 2020) +
  facet_wrap(variable ~ ., scales="free_y") +
  theme_bw()



Economy_Performance_final %>% 
  select_at(vars(matches("index"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") +
  theme_bw()



Economy_Performance_final %>% 
  select_at(vars(year, matches("index"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean, Q1 = fun_quantile25, Q3=fun_quantile75), na.rm=T) %>% 
  melt(id.vars=c("year")) %>% 
  mutate(source = if_else(grepl("oecd", variable), "oecd", "wdi")) %>% 
  ggplot(aes(x=year, y=value, col=variable)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(source ~ .) +
  ylim(0,100) +
  theme_bw()




samples = c("LUX", "IND", "SWE", "FRA", "DNK", "EST", "USA")

Economy_Performance_final %>% 
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

modes_cluster = Economy_Performance_final %>% 
  filter(year < 1990) %>% 
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st))

modes_cluster %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n())

Economy_Performance_final %>% 
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



Economy_Performance_final %>% 
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

print("End Economic Performance Script")