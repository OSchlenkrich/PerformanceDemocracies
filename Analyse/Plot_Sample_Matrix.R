dmx_trade_cluster_ext %>% 
  select_at(vars(country, year, cluster_1st, matches("context")))


Cluster_Summarized = dmx_trade_cluster %>% 
  left_join(dmx_data %>%  
              select(-classification_context), by=c("country", "year")) %>% 
  select_at(vars(country, year, cluster_label_1st, cluster_label_2nd, matches("trade_off"), -classification_context)) %>% 
  #filter(year >= 2000) %>% 
  group_by(cluster_label_1st) %>% 
  summarise_at(vars(matches("trade_off")), mean, na.rm=T) %>% 
  rename(country_name = cluster_label_1st) %>% 
  mutate(year = 2012)
  

grid.arrange(vis_15_Felder("Fec", 2012, "Trade-Off Measurement",Cluster_Summarized) + ggtitle("Fec"),
             vis_15_Felder("fEc", 2012, "Trade-Off Measurement",Cluster_Summarized) + ggtitle("fEc"),
             vis_15_Felder("FeC", 2012, "Trade-Off Measurement",Cluster_Summarized) + ggtitle("FeC"),
             vis_15_Felder("fEC", 2012, "Trade-Off Measurement",Cluster_Summarized) + ggtitle("fEC"),
             vis_15_Felder("FEC", 2012, "Trade-Off Measurement",Cluster_Summarized) + ggtitle("FEC"),
             vis_15_Felder("6", 2012, "Trade-Off Measurement",Cluster_Summarized) + ggtitle("6"),
             vis_15_Felder("7", 2012, "Trade-Off Measurement",Cluster_Summarized) + ggtitle("7")
)

##

Random_Sample = dmx_trade_cluster %>% 
  left_join(dmx_data %>%  
              select(-classification_context), by=c("country", "year")) %>% 
  select_at(vars(country, year, cluster_label_1st, cluster_label_2nd, matches("trade_off"), -classification_context)) %>% 
  sample_n(1)  %>% 
  rename(country_name = country)


vis_15_Felder(Random_Sample$country, Random_Sample$year, "Trade-Off Measurement", Random_Sample) + 
  ggtitle(paste(Random_Sample$country, Random_Sample$year, Random_Sample$cluster_label_1st))

