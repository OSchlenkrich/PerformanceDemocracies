# Democracy Profiles Cluster #####
source("Setup/Packages.R")
source("Setup/Base_Functions.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
library(fpc)
library(fclust)
library(clusterSim)
library(tidyr)


interface_FKM = function(data, k, method) {
  FKM_results = FKM(X = data, k, maxit=2000, stand=0, RS=25)
  hc_classes = as.integer(FKM_results$clus[,1])
  #print(FKM_results$iter)
  
  cluster_sol = matrix(hc_classes, nrow=length(hc_classes), ncol=k)
  clusterlist =list()
  
  for (kk in 1:k) {
    clusterlist[[kk]] = if_else(cluster_sol[,kk]==kk, T, F)
  }
  
  make_list = list(
    result = hc_classes,
    nc=k,
    clusterlist = clusterlist, 
    partition=hc_classes,
    clustermethod="FKM"
  )
  
  return(make_list)
}

interface_FKMmed = function(data, k, method) {
  
  FKMmed_results = FKM.med(X = data, k, maxit=30, stand=0, RS=2)
  hc_classes = as.integer(FKMmed_results$clus[,1])
  print(FKMmed_results$iter)
  
  cluster_sol = matrix(hc_classes, nrow=length(hc_classes), ncol=k)
  
  clusterlist =list()
  
  for (kk in 1:k) {
    clusterlist[[kk]] = if_else(cluster_sol[,kk]==kk, T, F)
  }
  
  make_list = list(
    result = hc_classes,
    nc=k,
    clusterlist = clusterlist, 
    partition=hc_classes,
    clustermethod="FKMmed"
  )
  
  return(make_list)
}
# used by ggplot: N per cluster
stat_box_count <- function(y, upper_limit = 0.3) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n =', length(y), '\n')
    )
  )
}

# Imputation ####
Plot_Impu = F
#source("Analyse/Cluster/ImputeCluster.R")
# Democracy Matrix V1.1
dmx_data_trade = fread("Datasets/performance_data/ImputedDatasets/dmx_data_trade.csv", encoding = "UTF-8")



# Descriptive Analysis ####

# Correlation
dmx_data_trade %>% 
  select_at(vars(matches("dim_index_trade_off"))) %>% 
  cor() %>% 
  corrplot(method="number")


dmx_data_trade %>% 
  select_at(vars(matches("trade_off"), -matches("index"))) %>% 
  rename_all(funs(gsub("_trade_off","",.))) %>% 
  cor() %>% 
  corrplot()



# Included Countries:

dmx_data_trade %>% 
  select(country) %>% 
  na.omit() %>% 
  n_distinct()

dmx_data_trade %>% 
  select(country, year) %>% 
  group_by(country) %>% 
  summarise(No = n()) %>% 
  summarise(mean = mean(No),
            min = min(No),
            max = max(No))


year_country = dmx_data_trade %>%
  na.omit() %>% 
  select(country, year) %>% 
  group_by(country) %>% 
  summarise(No = n()) %>% 
  pull(No) 
string_countries = paste(enc2utf8(year_country$country), " (", year_country$No, ")", sep="")
# write.csv(paste(string_countries, collapse = ", "), "Analyse/Cluster/Text/countries_cluster.csv", row.names = F, fileEncoding = "UTF-8")




# Create Cluster Data ####

cluster_data = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>% 
  rowwise() %>%
  mutate(mean_dim =  mean(c(freedom, equality, control)),
         freedom = (freedom - mean_dim),
         equality = (equality - mean_dim),
         control = (control - mean_dim),
         
  ) %>%
  ungroup() %>% 
  select(mean_dim, freedom, equality, control) %>% 
  mutate_all(funs(uv = (. - min(.))/(max(.)-min(.)))) %>% 
  mutate(mean_dim_uv = mean_dim_uv * 0.5)  %>% 
  select(-mean_dim_uv) %>% 
  select_at(vars(ends_with("_uv")))

cluster_data_dist = dist(cluster_data, method = "euclidean")


# Cluster Benchmark ####
# I use the modified version
source("Analyse/Cluster/Clusterbenchstats_mod.R")


clustermethodpars <- list()
clustermethodpars[[4]] <- list()
clustermethodpars[[4]]$method <- "average"
clustermethodpars[[1]] <- list()
clustermethodpars[[1]]$method <- ""
clustermethodpars[[2]] <- list()
clustermethodpars[[2]]$runs <- 20
clustermethodpars[[3]] <- list()


distmethod <- c(F,F,F,F,F)

methodname <- c("FKM", "kmeans", "pam", "average")
library(parallel)
set.seed(1234)

benchmark_results = clusterbenchstats_mod(cluster_data, G=2:10,
                         diss = F,
                  clustermethod =  c("interface_FKM","kmeansCBI", "pamkCBI", "hclustCBI"),
                  clustermethodpars = clustermethodpars,
                  distmethod = distmethod,
                  methodnames = methodname,
                  scaling=F,
                  multicore = T,
                  cores = 10,
                  nnruns = 70,
                  fnruns = 00,
                  avenruns = 00,
                  kmruns = 70,
                  useallg = F)


# save(benchmark_results, file = "Analyse/Cluster/RObjects/benchmark_results_nomean_nomed.Rdata")
# write.csv(my_data, "Analyse/Cluster/RObjects//bench_data_nomean_nomed.csv", fileEncoding = "UTF-8", row.names=F)
load(file = "Analyse/Cluster/RObjects/benchmark_results_nomean_nomed.Rdata")
# my_data = fread("Analyse/Cluster/RObjects//bench_data_nomean_nomed.csv", encoding = "UTF-8") %>% select(-V1)

# Extract Fits Separate K ####
# Use average-within % pearson gamma

# when using sindex, inverse values (*-1)
poled_results = benchmark_results
for (i in 1:4) {
  for (ii in 2:10) {
    poled_results$sstat[[i]][[ii]]$sindex = poled_results$sstat[[i]][[ii]]$sindex * -1
  }
}

aggregated_indizes = print(poled_results$sstat,
                           aggregate=TRUE,
                           weights=c(1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0))

bench_results_df = as.data.frame(do.call(rbind, poled_results$sstat[[1]])) %>% 
  mutate_all(funs(unlist)) %>% 
  mutate(method = "FKM",
         cluster = 1:10,
         nr = "Solution",
         aggregate = c(NA, unlist(aggregated_indizes[[17]][1,-1]))) %>% 
  bind_rows(as.data.frame(do.call(rbind, poled_results$sstat[[2]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "kmeans",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[[17]][2,-1])))) %>% 
  bind_rows(as.data.frame(do.call(rbind, poled_results$sstat[[3]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "pam",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[[17]][3,-1]))) ) %>% 
  bind_rows(as.data.frame(do.call(rbind, poled_results$sstat[[4]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "average",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[[17]][4,-1]))) )  

# sim_df = as.data.frame(do.call(rbind, benchmark_results$sim$km)) %>% 
#   mutate(cluster = rep(2:10, each = 50),
#          nr = rep(1:50, 9),
#          nr = as.factor(nr),
#          method = "simkm") %>% 
#   bind_rows(as.data.frame(do.call(rbind, benchmark_results$sim$nn)) %>% 
#               mutate(cluster = rep(2:10, each = 50),
#                      nr = rep(1:50, 9),
#                      nr = as.factor(nr),
#                      method = "simnn"))

# Analyse
a1 = bench_results_df %>% 
  filter(method!="average") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=aggregate, col=method, shape=method)) +
  geom_line(size=1.1) +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  ylab("Aggregated Indices") +
  theme_bw()
a2 = bench_results_df %>% 
  filter(method!="average") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=avewithin, col=method, shape=method)) +
  geom_line(size=1.1) +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  ylab("Average Within Distance") +
  geom_hline(yintercept = 0) +
  theme_bw()  +
  xlab("")
a3 = bench_results_df %>% 
  filter(method!="average") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=pearsongamma, col=method, shape=method)) +
  geom_line(size=1.1) +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  ylab(expression("Pearson" * Gamma)) +
  geom_hline(yintercept = 0) +
  theme_bw()  +
  xlab("")
a4 = bench_results_df %>% 
  filter(method!="average") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=asw, col=method, shape=method)) +
  geom_line(size=1.1) +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("")

ggarrange(a1,a2,a3, common.legend = T, legend = "bottom")  %>% 
  annotate_figure(top="Z-score calibration based on the same K")


# No Calibration ####

bench_results_df_nc = as.data.frame(do.call(rbind, benchmark_results$stat[[1]])) %>% 
  mutate_all(funs(unlist)) %>% 
  mutate(method = "FKM",
         cluster = 1:10,
         nr = "Solution") %>% 
  bind_rows(as.data.frame(do.call(rbind, benchmark_results$stat[[2]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "kmeans",
                     cluster = 1:10,
                     nr = "Solution")) %>% 
  bind_rows(as.data.frame(do.call(rbind, benchmark_results$stat[[3]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "pam",
                     cluster = 1:10,
                     nr = "Solution") ) %>% 
  bind_rows(as.data.frame(do.call(rbind, benchmark_results$stat[[4]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "average",
                     cluster = 1:10,
                     nr = "Solution") )  

# Analyse
a1 = bench_results_df_nc %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=asw, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a2 = bench_results_df_nc %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=avewithin, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a3 = bench_results_df_nc %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=pearsongamma, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(a1,a2,a3)


# All K ####

allG_calibrated = cgrestandard(benchmark_results$stat, benchmark_results$sim, 2:10,percentage=F,
                               useallmethods = F, useallg = T, benchmark_results$cm$othernc)


aggregated_indizes_allk = print(allG_calibrated,
      aggregate=TRUE,
      weights=c(1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0))



bench_results_df_allk = as.data.frame(do.call(rbind, allG_calibrated[[1]])) %>% 
  mutate_all(funs(unlist)) %>% 
  mutate(method = "FKM",
         cluster = 1:10,
         nr = "Solution",
         aggregate = c(NA, unlist(aggregated_indizes_allk[1,-1]))) %>% 
  bind_rows(as.data.frame(do.call(rbind, allG_calibrated[[2]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "kmeans",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes_allk[2,-1])))) %>% 
  bind_rows(as.data.frame(do.call(rbind, allG_calibrated[[3]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "pam",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes_allk[3,-1]))) ) %>% 
  bind_rows(as.data.frame(do.call(rbind, allG_calibrated[[4]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "average",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes_allk[4,-1]))) ) 

# Analyse
a1_ak = bench_results_df_allk %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=aggregate, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a2_ak = bench_results_df_allk %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=avewithin, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a3_ak = bench_results_df_allk %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=pearsongamma, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a4_ak = bench_results_df_allk %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=asw, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(a1_ak,a2_ak,a3_ak,a4_ak)


# Stability ####
# Fuzzy K Means
cboot_FKM_2 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=interface_FKM,
                           k=2, seed=15555)
# save(cboot_FKM_2, file = "Analyse/Cluster/RObjects/cboot_FKM_2_nomean_nomed.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_2_nomean_nomed.Rdata")
print(cboot_FKM_2)


cboot_FKM_3 <- clusterboot(cluster_data,B=50,bootmethod=
                          c("boot"),clustermethod=interface_FKM,
                        k=3, seed=15555)
# save(cboot_FKM_3, file = "Analyse/Cluster/RObjects/cboot_FKM_3_nomean_nomed.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_3_nomean_nomed.Rdata")
print(cboot_FKM_3)

cboot_FKM_4 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=interface_FKM,
                           k=4, seed=15555)
# save(cboot_FKM_4, file = "Analyse/Cluster/RObjects/cboot_FKM_4_nomean_nomed.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_4_nomean_nomed.Rdata")
print(cboot_FKM_4)

cboot_FKM_5 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=interface_FKM,
                           k=5, seed=15555)
# save(cboot_FKM_5, file = "Analyse/Cluster/RObjects/cboot_FKM_5_nomean_nomed.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_5_nomean_nomed.Rdata")
print(cboot_FKM_5)

cboot_FKM_6 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=interface_FKM,
                           k=6, seed=15555)
# save(cboot_FKM_6, file = "Analyse/Cluster/RObjects/cboot_FKM_6_nomean_nomed.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_6_nomean_nomed.Rdata")
print(cboot_FKM_6)

# PAM
cboot_PAM_2 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=pamkCBI,
                           k=2, seed=15555)
cboot_PAM_2

cboot_PAM_3 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=pamkCBI,
                           k=3, seed=15555)
cboot_PAM_3
cboot_PAM_4 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=pamkCBI,
                           k=4, seed=15555)
cboot_PAM_4
cboot_PAM_5 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=pamkCBI,
                           k=5, seed=15555)
cboot_PAM_5
cboot_PAM_6 <- clusterboot(cluster_data,B=50,bootmethod=
                             c("boot"),clustermethod=pamkCBI,
                           k=6, seed=15555)
cboot_PAM_6


# Cluster the Whole Dataset ####
FKM_2 = FKM(X = cluster_data, 2, maxit=1400, stand=0, RS=30, seed=1234)
FKM_3 = FKM(X = cluster_data, 3, maxit=1400, stand=0, RS=30, seed=1234)
FKM_4 = FKM(X = cluster_data, 4, maxit=1400, stand=0, RS=30, seed=1234)
FKM_5 = FKM(X = cluster_data, 5, maxit=1400, stand=0, RS=30, seed=1234)
FKM_6 = FKM(X = cluster_data, 6, maxit=1400, stand=0, RS=30, seed=1234)
# save(FKM_2, file = "Analyse/Cluster/RObjects/FKM_2_nomean_nomed.Rdata")
# save(FKM_3, file = "Analyse/Cluster/RObjects/FKM_3_nomean_nomed.Rdata")
# save(FKM_4, file = "Analyse/Cluster/RObjects/FKM_4_nomean_nomed.Rdata")
# save(FKM_5, file = "Analyse/Cluster/RObjects/FKM_5_nomean_nomed.Rdata")
# save(FKM_6, file = "Analyse/Cluster/RObjects/FKM_6_nomean_nomed.Rdata")
load((file = "Analyse/Cluster/RObjects/FKM_2_nomean_nomed.Rdata"))
load((file = "Analyse/Cluster/RObjects/FKM_3_nomean_nomed.Rdata"))
load((file = "Analyse/Cluster/RObjects/FKM_4_nomean_nomed.Rdata"))
load((file = "Analyse/Cluster/RObjects/FKM_5_nomean_nomed.Rdata"))
load((file = "Analyse/Cluster/RObjects/FKM_6_nomean_nomed.Rdata"))

# Visualize Cluster Solutions ####

summary(prcomp(cluster_data))
# FKM
FKM_2_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster = as.factor(FKM_2$clus[,1]))) %>% 
  ggplot(aes(x=PC1, y=PC2, col=Cluster)) +
  geom_point() +
  ggtitle("FKM 2") +
  theme_bw()  +
  theme(legend.position = c("none")) 

FKM_3_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster =  as.factor(FKM_3$clus[,1]))) %>% 
  ggplot(aes(x=PC1, y=PC2, col=Cluster)) +
  geom_point() +
  ggtitle("FKM 3") +
  theme_bw()  +
  theme(legend.position = c("none")) 

FKM_4_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster =  as.factor(FKM_4$clus[,1]))) %>% 
  ggplot(aes(x=PC1, y=PC2, col=Cluster)) +
  geom_point() +
  ggtitle("FKM 4") +
  theme_bw()  +
  theme(legend.position = c("none")) 

FKM_5_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster =  as.factor(FKM_5$clus[,1]))) %>% 
  ggplot(aes(x=PC1, y=PC2, col=Cluster)) +
  geom_point() +
  ggtitle("FKM 5") +
  theme_bw()  +
  theme(legend.position = c("none")) 

FKM_6_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster =  as.factor(FKM_6$clus[,1]))) %>% 
  ggplot(aes(x=PC1, y=PC2, col=Cluster)) +
  geom_point() +
  ggtitle("FKM 6") +
  theme_bw() +
  theme_bw()  +
  theme(legend.position = c("none")) 

grid.arrange(FKM_2_plot, FKM_3_plot, FKM_4_plot, FKM_5_plot)


# Compare to Clusterboot
data.frame(predict(prcomp(my_data))[,1:2]) %>% 
  bind_cols(data.frame(Cluster = cboot_FKM_5$partition)) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("FKM 5")

data.frame(predict(prcomp(cluster_data))[,1:2]) %>% 
  bind_cols(data.frame(Cluster = cboot_FKM_6$partition)) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("FKM 6")

##
dmx_data_trade_cluster = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster2 = as.factor(FKM_2$clus[,1]))) %>%
  bind_cols(data.frame(Cluster3 = as.factor(FKM_3$clus[,1]))) %>%
  bind_cols(data.frame(Cluster4 = as.factor(FKM_4$clus[,1]))) %>%
  bind_cols(data.frame(Cluster5 = as.factor(FKM_5$clus[,1]))) %>% 
  mutate(Cluster2 = fct_recode(Cluster2, "fEC" = "1", "FeC" = "2")) %>% 
  mutate(Cluster3 = fct_recode(Cluster3, "fEC" = "2", "FeC" = "1", "FEc" = "3")) %>% 
  mutate(Cluster4 = fct_recode(Cluster4, "fEC" = "3", "FeC" = "4", "Fec" = "2", "fEc" = "1")) %>% 
  mutate(Cluster5 = fct_recode(Cluster5, "FEC" = "1", "fEC" = "5", "FeC" = "4", "Fec" = "3", "fEc" = "2"))

FKM_2_dim_plot = dmx_data_trade_cluster %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster2, y=value, fill=name))+
  geom_boxplot() +
  stat_summary(fun.data = stat_box_count, geom = "text", hjust = 0.5, vjust = 0.4, size=4) +
  theme_bw() +
  ggtitle("FKM 2") +
  scale_fill_discrete(name = "") +
  xlab("") +
  ylab("Democratic Quality")

FKM_3_dim_plot = dmx_data_trade_cluster %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster3, y=value, fill=name))+
  geom_boxplot() +
  stat_summary(fun.data = stat_box_count, geom = "text", hjust = 0.5, vjust = 0.4, size=4) +
  theme_bw() +
  ggtitle("FKM 3") +
  scale_fill_discrete(name = "") +
  xlab("") +
  ylab("Democratic Quality")

FKM_4_dim_plot = dmx_data_trade_cluster %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster4, y=value, fill=name))+
  geom_boxplot() +
  stat_summary(fun.data = stat_box_count, geom = "text", hjust = 0.5, vjust = 0.4, size=4) +
  theme_bw() +
  ggtitle("FKM 4") +
  scale_fill_discrete(name = "") +
  xlab("") +
  ylab("Democratic Quality")

FKM_5_dim_plot = dmx_data_trade_cluster %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster5, y=value, fill=name))+
  geom_boxplot() +
  stat_summary(fun.data = stat_box_count, geom = "text", hjust = 0.5, vjust = 0.4, size=4) +
  theme_bw() +
  ggtitle("FKM 5") +
  scale_fill_discrete(name = "")+
  xlab("") +
  ylab("Democratic Quality")



ggarrange(FKM_2_dim_plot, FKM_3_dim_plot, FKM_4_dim_plot, FKM_5_dim_plot, nrow=2, ncol=2, common.legend = T, legend = "bottom")


PAM_3_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(cboot_PAM_3$partition))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw() +
  ggtitle("FKM 3")

PAM_4_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(cboot_PAM_4$partition))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw() +
  ggtitle("FKM 4")

PAM_5_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(cboot_PAM_5$partition))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw() +
  ggtitle("FKM 5")


PAM_6_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(cboot_PAM_6$partition))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() + 
  theme_bw() +
  ggtitle("FKM 6")

grid.arrange(PAM_3_dim_plot, PAM_4_dim_plot, PAM_5_dim_plot, PAM_6_dim_plot, nrow=4)

# Create Dataset ####

dmx_trade_cluster = dmx_data_trade %>%
  select(country, year, regions,
         classification_core,
         freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>% 
  bind_cols(data.frame(FKM_3_cluster = as.factor(FKM_3$clus[,1]))) %>% 
  bind_cols(data.frame(round(FKM_3$U,3))) %>% 
  mutate(
    FKM_3_cluster = as.factor(FKM_3_cluster),
    FKM_3_cluster = fct_recode(FKM_3_cluster, 
                               "FeC" = "1",  
                               "fEC" = "2",  
                               "FEc" = "3")
    ) %>% 
  rename(
    FKM_3_mb_FeC = Clus.1,
    FKM_3_mb_fEC = Clus.2,
    FKM_3_mb_FEc = Clus.3
  )  %>% 
  bind_cols(data.frame(FKM_4_cluster = as.factor(FKM_4$clus[,1]))) %>% 
  bind_cols(data.frame(round(FKM_4$U,3))) %>% 
  mutate(
    FKM_4_cluster = as.factor(FKM_4_cluster),
    FKM_4_cluster = fct_recode(FKM_4_cluster, 
                               "fEc" = "1",  
                               "FEc" = "2",  
                               "fEC" = "3",  
                               "FeC" = "4")
  ) %>% 
  rename(
    FKM_4_mb_fEc = Clus.1,
    FKM_4_mb_FEc = Clus.2,
    FKM_4_mb_fEC = Clus.3,
    FKM_4_mb_FeC = Clus.4
  )   %>% 
  bind_cols(data.frame(FKM_5_cluster = as.factor(FKM_5$clus[,1]))) %>% 
  bind_cols(data.frame(round(FKM_5$U,3))) %>% 
  mutate(
    FKM_5_cluster = as.factor(FKM_5_cluster),
    FKM_5_cluster = fct_recode(FKM_5_cluster, 
                               "FEC" = "1",  
                               "fEc" = "2",  
                               "Fec" = "3",  
                               "FeC" = "4",  
                               "fEC" = "5")
  ) %>% 
  rename(
    FKM_5_mb_FEC = Clus.1,
    FKM_5_mb_fEc = Clus.2,
    FKM_5_mb_Fec = Clus.3,
    FKM_5_mb_FeC = Clus.4,
    FKM_5_mb_fEC = Clus.5
  )  %>% 
  bind_cols(data.frame(FKM_6_cluster = as.factor(FKM_6$clus[,1]))) %>% 
  bind_cols(data.frame(round(FKM_6$U,3))) %>% 
  mutate(
    FKM_6_cluster = as.factor(FKM_6_cluster),
    FKM_6_cluster = fct_recode(FKM_6_cluster, 
                               "FeC" = "1",  
                               "FEC" = "2",  
                               "Fec" = "3",  
                               "fEC" = "4",  
                               "fEc" = "5",  
                               "FEc" = "6")
  ) %>% 
  rename(
    FKM_6_mb_FeC = Clus.1,
    FKM_6_mb_FEC = Clus.2,
    FKM_6_mb_Fec = Clus.3,
    FKM_6_mb_fEC = Clus.4,
    FKM_6_mb_fEc = Clus.5,
    FKM_6_mb_FEc = Clus.6
  ) %>% 
  left_join(V_dem %>% select(country, year, country_text_id), by=c("country", "year")) %>%
  left_join(dmx_data_trade %>% select(country, year, na_count), by=c("country", "year")) %>% 
  select(country, country_text_id, year, regions, na_count, everything())


lab_FKM_3_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKM_3_cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw()

lab_FKM_4_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKM_4_cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw()
lab_FKM_5_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKM_5_cluster, y=value, fill=name)) +
  geom_boxplot() +
  theme_bw()
lab_FKM_6_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKM_6_cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw()



grid.arrange(lab_FKM_3_dim_plot, lab_FKM_4_dim_plot, lab_FKM_5_dim_plot, lab_FKM_6_dim_plot, nrow=4)

# CSV Save
write.csv(dmx_trade_cluster, file="Datasets/performance_data/dmx_trade_cluster_v8.csv", row.names = F, fileEncoding ="UTF-8")


# Basic information about clusters:
countries_cluster = dmx_trade_cluster %>% 
  select(FKMmed_6_cluster, country) %>% 
  group_by(FKMmed_6_cluster) %>% 
  distinct(country) %>% 
  summarise(No = n())


cluster_year = dmx_trade_cluster %>% 
  select(FKMmed_6_cluster, country, year) %>% 
  group_by(FKMmed_6_cluster, country) %>% 
  summarise(No = n()) %>% 
  summarise(mean = round(mean(No), 0),
            min = min(No),
            max = max(No))

string_clusters = paste(countries_cluster$FKMmed_6_cluster,
                         ": ",
                         countries_cluster$No, 
                         " countries",
                         " (average years: ", cluster_year$mean,
                         ")", sep="")


write.csv(paste(string_clusters, collapse = "; "), "Analyse/Cluster/Text/cluster_info.csv", row.names = F, fileEncoding = "UTF-8")

string_clusters_year = dmx_trade_cluster %>% 
  select(FKMmed_6_cluster, country) %>% 
  group_by(FKMmed_6_cluster) %>% 
  distinct(country) %>% 
  arrange(FKMmed_6_cluster) %>% 
  left_join(dmx_trade_cluster %>% 
              select(FKMmed_6_cluster, country) %>% 
              group_by(FKMmed_6_cluster, country) %>%
              summarise(No = n()), 
            by=c("FKMmed_6_cluster", "country")) %>% 
  mutate(string = paste(country, " (", No, ")", sep=""))
write.csv(paste(string_clusters_year$string, collapse = ", "), "Analyse/Cluster/Text/cluster_country_year.csv", row.names = F, fileEncoding = "UTF-8")

# Split by Classification

dmx_trade_cluster %>%
  mutate(cluster = as.factor(FKMmed_6_cluster)) %>%
  dplyr::select(freedom, equality, control, cluster, classification_core) %>%
  melt(id.vars=c("classification_core", "cluster")) %>% 
  ggplot(aes(x=cluster, y=value, fill=variable)) + geom_boxplot()  + theme_bw() +
  ylim(0.25,1) + 
  stat_summary(fun.data = stat_box_count, geom = "text", hjust = 0.5, vjust = 0.9) +
  ylab("") + 
  xlab("Cluster")  + 
  scale_fill_discrete(name = "Dimensions", labels=c("Freedom", "Equality", "Control")) +
  ggtitle(paste("Boxplot ", "PAM (5 Clusters)", sep="")) + 
  theme(plot.title = element_text(hjust=0.5))  + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) +
  facet_wrap(classification_core ~ .)


### Visualization of Democracy Profiles

# Plotting: Random Countries
# plot_random_countries_dim_improved(dmx_trade_cluster, 5)
# plot_random_countries_dim_improved(dmx_trade_cluster, c("Germany", "Sweden","United Kingdom", "New Zealand"))
# plot_random_countries_dim_improved(dmx_trade_cluster, c("United States of America", "Switzerland", "Venezuela"))

plot_random_countries_dim_improved(dmx_trade_cluster, c("United Kingdom", "Netherlands", "United States of America", "Germany", "Denmark"))
plot_random_countries_dim_improved(dmx_trade_cluster, c("Cape Verde","Ghana","New Zealand", "Austria", "Switzerland", "Turkey"))
plot_random_countries_dim_improved(dmx_trade_cluster, c("United Kingdom", "Germany", "New Zealand", "Switzerland"))


# Plotting: Time Development ####

plot_types_N = dmx_trade_cluster %>%
  dplyr::select(FKMmed_6_cluster, year) %>%
  group_by(year) %>%
  summarise(n_total=n())

complete_data_cluster = data.frame(FKMmed_6_cluster = rep(unique(dmx_trade_cluster$FKMmed_6_cluster), each=length(unique(dmx_trade_cluster$year))),
                                   year = rep(unique(dmx_trade_cluster$year), length(unique(dmx_trade_cluster$FKMmed_6_cluster))))

plot_types_yearly = dmx_trade_cluster %>%
  dplyr::select(FKMmed_6_cluster, year) %>%
  group_by(year, FKMmed_6_cluster) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  full_join(complete_data_cluster, by=c("year", "FKMmed_6_cluster")) %>%
  arrange(year, FKMmed_6_cluster) %>%
  mutate(
    n = ifelse(is.na(n) == T, 0, n)
  ) %>%
  left_join(plot_types_N, by="year") %>%
  mutate(percent = n/n_total)



ggplot(plot_types_yearly, aes(x=year, y=n, fill=FKMmed_6_cluster)) + 
  geom_area(stat="identity", col="black") + 
  theme_classic() +
  scale_x_continuous(breaks=seq(1900, 2020, 20)) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, size=10), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + 
  xlab("") +
  coord_cartesian(expand=0) + 
  scale_fill_brewer(name="", type="qual", palette="Paired") + 
  ggtitle("Temporal Distribution of Democracy Profiles", subtitle = "Count") + ylab("Count") 


ggplot(plot_types_yearly, aes(x=year, y=percent, fill=FKMmed_6_cluster)) + geom_area(stat="identity", col="black", size=0.8) + theme_classic() +
  scale_x_continuous(breaks=seq(1900, 2000, 20), limits=c(1900, 2020)) + 
  theme(legend.position = "bottom", axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, size=12), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + xlab("") +
  coord_cartesian(expand=0) + 
  scale_fill_brewer(name="", type="qual", palette="Paired") + 
  ggtitle("Temporal Distribution of Democracy Profiles", subtitle = "Percent") + 
  scale_y_continuous(labels=percent, name="")


# Plotting World Map ####
create_world_map(dmx_trade_cluster, "FKMmed_3_cluster", "1974-2017", 
                 "Spatial Distribution of Democracy Profiles \n", mode=T)
create_world_map(dmx_trade_cluster, "FKMmed_3_cluster", "2017", 
                 "Spatial Distribution of Democracy Profiles \n", mode=F)

create_world_map(dmx_trade_cluster, "FKMmed_6_cluster", "1974-2017", 
                 "Spatial Distribution of Democracy Profiles \n", mode=T)
create_world_map(dmx_trade_cluster, "FKMmed_6_cluster", "2017", 
                 "Spatial Distribution of Democracy Profiles \n", mode=F)


# Cleaning
rm(dmx_trade_dimension_unequal_w_outlier)
rm(hc_classes)
rm(correlation_distance)
rm(dmx_trade_dimension_unequal_outlier)
rm(dmx_trade_dimension_prep)
rm(hc1_color)
rm(hc1)
rm(hc_outlier)
rm(dmx_trade_dimension_same)
rm(dmx_trade_dimension_unequal)
rm(DIANA_med)
rm(complete_data_cluster)
rm(plot_types_N)
rm(string_countries)
rm(year_country)