# CLUSTER ANALYSE ####
library(clusterSim)
library(fclust)
library(fpc)
source("Analyse/CreateDatasets.R")


scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


dimensions_plot = function(dataset, clustering, title) {
  dataset %>%
    mutate(cluster = as.factor(clustering)) %>%
    mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>%
    pivot_longer(cols=c(ends_with("_eco"), 
                        ends_with("_env"), 
                        ends_with("_ga"), 
                        ends_with("_soc"), 
                        ends_with("_ds"), 
                        ends_with("_pc")))   %>% 
    mutate(name = as.factor(name),
           name = fct_relevel(name, 
                              "wealth_eco",
                              "productivity_eco",
                              "air_env",
                              "resources_env",
                              "arate_ccp_ga",
                              "eco_inequal_soc",
                              "soc_inequal_soc",
                              "domsec_ds",
                              "conf_pc")) %>% 
    ggplot(aes(x=cluster, y=value, fill=name)) +
    geom_boxplot() +
    scale_fill_grey(start = 0.5, end = 1) +
    geom_hline(yintercept = 0, alpha=0.5, linetype="dashed") +
    theme_bw() +
    xlab("") +
    ylab("Performance") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggtitle(title)
}


principal_plot = function(cluster, label) {
  data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
    bind_cols(data.frame(Cluster = as.factor(cluster))) %>%  
    bind_cols(performance_data_noNAS %>% select(country_text_id)) %>%
    mutate(label = paste(country_text_id, "\n",cluster),
           cluster = as.factor(cluster)) %>%
    ggplot(aes(x=PC1, y=PC2, col=cluster)) +
    geom_text(size=3, aes(label=label), show.legend = FALSE) +  
    scale_color_grey(name="", start = 0, end = 0.65) +
    ggtitle(label) +
    theme_bw()  +
    #theme(legend.position = c("none")) +
    geom_hline(yintercept = 0, alpha=0.5, linetype="dashed") +
    geom_vline(xintercept = 0, alpha=0.5, linetype="dashed")
}

create_world_map_cat_perf= function(dataset, label = NULL, cat_label) {
  require(RColorBrewer)
  
  # dmy_year$country[dmy_year$country=="Burma/Myanmar"] = "Burma"
  # dmy_year$country[dmy_year$country=="Republic of Vietnam"] = "Vietnam"
  # dmy_year$country[dmy_year$country=="São Tomé and Príncipe"] = "Sao Tome and Principe"
  
  merged_map_data <- joinCountryData2Map(dataset,
                                         joinCode = "NAME",
                                         nameJoinColumn = "country",
                                         verbose = TRUE)
  
  
  
  cnt = as.character(merged_map_data$NAME[merged_map_data$NAME != "Antarctica"])
  cnt = as.character(cnt[cnt != "Greenland"])
  
  merged_map_data <- subset(merged_map_data, NAME  %in%  cnt)
  
  
  colourPalette <- brewer.pal(length(unique(dataset$cluster)), "Set3")
  
  if (is.null(label) == T) {
    mapParams = mapCountryData(merged_map_data,
                               nameColumnToPlot="cluster",
                               colourPalette=colourPalette,
                               catMethod="categorical", 
                               addLegend = F, 
                               #lwd=1,
                               mapTitle = "")    
  } else {
    mapParams = mapCountryData(merged_map_data,
                               nameColumnToPlot="cluster",
                               colourPalette=colourPalette,
                               catMethod="categorical", 
                               addLegend = F, 
                               #lwd=1,
                               mapTitle = label)
  }
  
  
  do.call( addMapLegendBoxes, c(mapParams, title=cat_label))
  
}



# Load Dataset ####
performance_cluster_data = performance_all %>% 
  filter(year >= 1990) %>% 
  select(country_text_id, year, 
         wealth_eco,
         productivity_eco,
         air_env,
         resources_env,
         arate_ccp_ga,
         eco_inequal_soc,
         soc_inequal_soc,
         domsec_ds,
         conf_pc
  ) %>% 
  group_by(country_text_id) %>% 
  mutate(conf_pc = na.locf0(conf_pc)) %>% 
  # 10 year interval#
  mutate(year = floor(year/10)*10) %>%
  group_by(country_text_id, year)%>%
  summarise_all(funs(mean(., na.rm=T))) %>% 
  ungroup() 


performance_cluster_data %>%
  select(-country_text_id) %>% 
  group_by(year) %>% 
  summarise_all(funs(pMiss_01(.))) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")


performance_data_noNAS = performance_cluster_data %>% 
  na.omit() %>%
  mutate_at(vars(-country_text_id,-year), funs(scale_this))


cluster_data = performance_data_noNAS %>%
  select(-country_text_id, -year )

length(unique(performance_data_noNAS$country_text_id))


# Benchmark ####

clustermethod=c("kmeansCBI", "pamkCBI")
methodname <- c("kmeans","pam")

clustermethodpars <- list()
clustermethodpars[[1]] <- list()
clustermethodpars[[1]]$runs <- 20
clustermethodpars[[2]] <- list()


bench_results = fpc::clusterbenchstats(cluster_data, G=2:10,
                       diss = F,
                       clustermethod = clustermethod, 
                       clustermethodpars = clustermethodpars,
                       methodname = methodname,
                       nnruns = 100,
                       fnruns = 100,
                       avenruns = 100,
                       kmruns = 100,
                       useallg = F, 
                       trace = T)

# saveRDS(bench_results, file="Analyse/Descriptive Analysis Performance/bench_results_ga.RDS")
bench_results = readRDS(file="Analyse/Descriptive Analysis Performance/bench_results_ga.RDS")

aggregated_indizes = print(bench_results$sstat,
                           aggregate=TRUE,
                           weights=c(1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0))

bench_results_df = as.data.frame(do.call(rbind, bench_results$sstat[[1]])) %>% 
  mutate_all(funs(unlist)) %>% 
  mutate(method = "kmeans",
         cluster = 1:10,
         nr = "Solution",
         aggregate = c(NA, unlist(aggregated_indizes[[17]][1,-1]))
         ) %>% 
  bind_rows(as.data.frame(do.call(rbind, bench_results$sstat[[2]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "pam",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[[17]][2,-1])))) 




a1 = bench_results_df %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>%
  filter(method!="average") %>% 
  ggplot(aes(x=cluster, y=aggregate, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  geom_point(size=3) +
  #scale_color_grey(end = 0.3) +
  ylab("Aggregated Indices") +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a2 = bench_results_df %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="average") %>% 
  ggplot(aes(x=cluster, y=avewithin, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  geom_point(size=3) +
  #scale_color_grey(end = 0.3) +
  ylab("Average Within Distance") +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a3 = bench_results_df %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="average") %>% 
  ggplot(aes(x=cluster, y=pearsongamma, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  geom_point(size=3) +
  #scale_color_grey(end = 0.3) +
  ylab(expression("Pearson" * Gamma)) +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a4 = bench_results_df %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>%
  filter(method!="average") %>% 
  ggplot(aes(x=cluster, y=asw, col=method, shape=method)) +
  geom_line() +
  geom_point(size=3) +
  #scale_color_grey(end = 0.3) +
  ylab("Average Silhoutte Width") +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()

ggarrange(a1,a2,a3, common.legend = T)  %>% 
  annotate_figure(top="Z-score calibration based on the same K")


# Stability ####
cboot_kmeans_2_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                     c("boot"),clustermethod=kmeansCBI,
                                   k=2, seed=15555)

cboot_kmeans_2_boot
cboot_kmeans_3_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                  c("boot"),clustermethod=kmeansCBI,
                                k=3, seed=15555)

cboot_kmeans_3_boot
cboot_kmeans_4_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                     c("boot"),clustermethod=kmeansCBI,
                                   k=4, seed=15555)

cboot_kmeans_4_boot


cboot_PAM_5_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                  c("boot"),clustermethod=pamkCBI,
                                k=5, seed=15555)
cboot_PAM_5_boot

cboot_PAM_6_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                  c("boot"),clustermethod=pamkCBI,
                                k=6, seed=15555)
cboot_PAM_6_boot
cboot_PAM_7_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                  c("boot"),clustermethod=pamkCBI,
                                k=7, seed=15555)
cboot_PAM_7_boot

# Solutions

PAM_6 = pamk(cluster_data, 6, seed=1234)
PAM_7 = pamk(cluster_data, 7, seed=1234)

kmeans_2 = kmeans(cluster_data, 2, nstart =20)
kmeans_3 = kmeans(cluster_data, 3, nstart =20)

table(PAM_6$pamobject$clustering)
table(PAM_7$pamobject$clustering)

performance_cluster = performance_data_noNAS %>% 
  bind_cols(cluster2 = as.factor(kmeans_2$cluster)) %>% 
  bind_cols(cluster3 = as.factor(kmeans_3$cluster)) %>% 
  bind_cols(cluster6 = as.factor(PAM_6$pamobject$clustering)) %>% 
  bind_cols(cluster7 = as.factor(PAM_7$pamobject$clustering)) %>% 
  mutate(cluster2 = fct_recode(cluster2, "Top Performer" = "2", "Laggard" = "1"),
         cluster2 = fct_relevel(cluster2, "Top Performer", "Laggard"),
         cluster3 = fct_recode(cluster3, "Top Performer" = "1", "Middle Performer" = "3", "Laggard" = "2"),
         cluster3 = fct_relevel(cluster3, "Top Performer", "Middle Performer", "Laggard"),
         cluster6 = fct_recode(cluster6, 
                               "Top Performer (high AR)" = "4", "Top Performer (low AR)" = "6",
                               "Middle Performer" = "5", "Middle Performer (low SP)" = "3",
                               "Laggard" = "2", "Extreme Laggard" = "1"),
         cluster6 = fct_relevel(cluster6, "Top Performer (high AR)", "Top Performer (low AR)","Middle Performer", "Middle Performer (low SP)", "Laggard"),
         cluster7 = fct_recode(cluster7, 
                               "Top Performer (high AR)" = "4", "Top Performer (low AR)" = "7",
                               "Middle Performer" = "5", "Middle Performer (low SP)" = "3",
                               "Laggard" = "2", "Extreme Laggard (low CP)" = "1", "Extreme Laggard (high CP)" = "6"),
         cluster7 = fct_relevel(cluster7, "Top Performer (high AR)", "Top Performer (low AR)","Middle Performer", "Middle Performer (low SP)", "Laggard"))

# Principal Component Plot ####


p1_pc = principal_plot(kmeans_2$cluster, "")
p2_pc = principal_plot(kmeans_3$cluster, "")
p3_pc = principal_plot(PAM_6$pamobject$clustering, "")
p4_pc = principal_plot(PAM_7$pamobject$clustering, "")

ggarrange(p1_pc,p2_pc, p3_pc, p4_pc, ncol=2,nrow=2, common.legend = F)


p1_dim = dimensions_plot(performance_data_noNAS, kmeans_2$cluster, "")
p2_dim = dimensions_plot(performance_data_noNAS, kmeans_3$cluster, "")
p3_dim = dimensions_plot(performance_data_noNAS, PAM_6$pamobject$clustering, "")
p4_dim = dimensions_plot(performance_data_noNAS, PAM_7$pamobject$clustering, "")
ggarrange(p1_dim,p2_dim, p3_dim, p4_dim, ncol=2,nrow=2, common.legend = T, legend ="bottom")


# Describing Cluster Solutions ####
ggarrange(p1_pc,p1_dim, ncol=1,nrow=2, common.legend = F, legend ="bottom") 
create_world_map_cat_perf(performance_cluster %>%  
                            group_by(country_text_id) %>% 
                            top_n(1, year) %>% 
                            ungroup() %>% 
                            rename(cluster = cluster2,
                                   country = country_text_id), label = "", cat_label = "Cluster")


ggarrange(p2_pc,p2_dim, ncol=1,nrow=2, common.legend = F, legend ="bottom")
create_world_map_cat_perf(performance_cluster %>%  
                            group_by(country_text_id) %>% 
                            top_n(1, year) %>% 
                            ungroup() %>% 
                            rename(cluster = cluster3,
                                   country = country_text_id), label = "", cat_label = "Cluster")


ggarrange(p3_pc,p3_dim, ncol=1,nrow=2, common.legend = F, legend ="bottom")
create_world_map_cat_perf(performance_cluster %>%  
                            group_by(country_text_id) %>% 
                            top_n(1, year) %>% 
                            ungroup() %>% 
                            rename(cluster = cluster6,
                                   country = country_text_id), label = "", cat_label = "Cluster")

ggarrange(p4_pc,p4_dim, ncol=1,nrow=2, common.legend = F, legend ="bottom")
create_world_map_cat_perf(performance_cluster %>%  
                            group_by(country_text_id) %>% 
                            top_n(1, year) %>% 
                            ungroup() %>% 
                            rename(cluster = cluster7,
                                   country = country_text_id), label = "", cat_label = "Cluster")
# World Map ####

mapdata1990 = performance_data_noNAS %>% 
  bind_cols(cluster = PAM_5$pamobject$clustering) %>% 
  filter(year == 1990) %>% 
  rename(country = country_text_id)
mapdata2010 = performance_data_noNAS %>% 
  bind_cols(cluster = PAM_5$pamobject$clustering) %>% 
  filter(year == 2010) %>% 
  rename(country = country_text_id)


create_world_map_cat_perf(mapdata1990, label = "1990", cat_label = "Cluster")
create_world_map_cat_perf(mapdata2010, label = "2010", cat_label = "Cluster")


