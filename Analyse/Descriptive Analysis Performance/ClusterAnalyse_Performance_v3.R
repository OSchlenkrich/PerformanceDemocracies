# CLUSTER ANALYSE ####
library(clusterSim)
library(fclust)
library(fpc)
source("Analyse/CreateDatasets.R")


scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


dimensions_plot = function(dataset, clustering) {
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
                              "eco_inequal_soc",
                              "soc_inequal_soc",
                              "domsec_ds",
                              "conf_pc")) %>% 
    ggplot(aes(x=cluster, y=value, fill=name)) +
    geom_boxplot() +
    scale_fill_grey(start = 0.5, end = 1) +
    theme_bw() +
    xlab("") +
    ylab("") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggtitle("Boxplot")
}


plot_countries_overtime = function(No_countries = 5) {
  
  if (is.numeric(No_countries)==T) {
    random_countries = sample(n_distinct(cluster_time$country_text_id), No_countries)
    selected_countries = unique(cluster_time$country_text_id)[random_countries]
  } else {
    selected_countries = No_countries
  }
  
  
  plotted_country = cluster_time %>%
    filter(country_text_id %in% selected_countries) %>%
    filter(year >= 1945) %>% 
    mutate(country_text_id = fct_relevel(country_text_id, selected_countries)) %>% 
    select(country_text_id, year, cluster5) %>% 
    pivot_longer(cols="cluster5") %>% 
    mutate(value = as.factor(value),
           value = fct_relevel(value, levels(cluster_time$cluster5))
    ) %>% 
    mutate(y=1)
  
  levels(plotted_country$country_text_id) <- gsub(" ", "\n", levels(plotted_country$country_text_id))
  
  gap_years = unique(plotted_country$year)[order(unique(plotted_country$year))]
  
  gap_years = gap_years[2] - gap_years[1]
  
  p1 = ggplot(plotted_country, aes(x=year,  y=y, fill=value)) + 
    geom_rect(aes(xmin=year, xmax = year + gap_years, ymin=0, ymax=1)) + 
    facet_wrap(country_text_id~. , nrow=length(selected_countries), strip.position="left") + 
    theme_bw() +
    scale_fill_brewer(name="Cluster", type="qual", palette="Paired") +
    scale_y_continuous(breaks=c(0,1)) +
    scale_x_continuous(limits=c(1940, 2020), breaks=seq(1900, 2020, 10)) + 
    theme(axis.text.x = element_text(angle=90, size=10), strip.text = element_text(size=12, face="bold"),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank()) + 
    xlab("") + 
    ylab("")
  return(p1)
}


# Load Dataset ####
performance_cluster_data = performance_all %>% 
  filter(year >= 1990) %>% 
  select(country_text_id, year, 
         wealth_eco,
         productivity_eco,
         air_env,
         resources_env,
         #GA_ccp_ga,
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
  group_by(year) %>% 
  select_at(vars(ends_with("_eco"), 
                 ends_with("_env"), 
                 #ends_with("_ga"), 
                 ends_with("_soc"), 
                 ends_with("_ds"), 
                 ends_with("_pc"))) %>% 
  summarise_all(funs(1-pMiss_01(.))) %>% 
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

# saveRDS(bench_results, file="Analyse/Descriptive Analysis Performance/bench_results.RDS")
bench_results = readRDS(file="Analyse/Descriptive Analysis Performance/bench_results.RDS")

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

ggarrange(a1,a2,a3, a4, common.legend = T)  %>% 
  annotate_figure(top="Z-score calibration based on the same K")


# Stability ####
cboot_kmeans_3_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                  c("boot"),clustermethod=kmeansCBI,
                                k=3, seed=15555)

cboot_kmeans_3_boot


cboot_PAM_3_boot <- clusterboot(cluster_data,B=100, bootmethod=
                                  c("boot"),clustermethod=pamkCBI,
                                k=8, seed=15555)
cboot_PAM_3_boot

# Solutions

PAM_6 = pamk(cluster_data, 6, seed=1234)
kmeans_2 = kmeans(cluster_data, 2, nstart =20)
kmeans_3 = kmeans(cluster_data, 3, nstart =20)



# Principal Component Plot ####
principal_plot = function(cluster, label) {
  data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
    bind_cols(data.frame(Cluster = as.factor(cluster))) %>% 
    ggplot(aes(x=PC1, y=PC2, col=Cluster)) +
    geom_point() +
    ggtitle(label) +
    theme_bw()  +
    theme(legend.position = c("none")) 
}


p1 = principal_plot(kmeans_2$cluster, "KMEANS 2")
p2 = principal_plot(kmeans_3$cluster, "KMEANS 3")
p3 = principal_plot(PAM_6$pamobject$clustering, "PAM 6")

ggarrange(p1,p2, p3, ncol=1,nrow=2, common.legend = F)

dimensions_plot(performance_data_noNAS, kmeans_2$cluster)
dimensions_plot(performance_data_noNAS, kmeans_3$cluster)
dimensions_plot(performance_data_noNAS, PAM_6$pamobject$clustering)



