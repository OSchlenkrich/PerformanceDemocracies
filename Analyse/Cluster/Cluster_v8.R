# Democracy Profiles Cluster #####
source("Setup/Packages.R")
source("Setup/Base_Functions.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
library(fpc)
library(fclust)
library(clusterSim)
library(tidyr)



# Imputation ####
Plot_Impu = F
#source("Analyse/Cluster/ImputeCluster.R")
dmx_data_trade = fread("Datasets/performance_data/ImputedDatasets/dmx_data_trade.csv", encoding = "UTF-8")


which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(FALSE,diff(x)>0,TRUE))>0)
    }else {
      which(diff(diff(x)>0)>0)+1
    }
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)+1
    }
  }
}

make_fit_indices = function(data = cluster_data, method="kmeans", nruns = 10, dist_measure="euclidean") {
  
  names_validity = c("method", "clusters", 
                     "CH_fi", 
                     "DB2_fi",
                     "pG_fi",
                     "DI1_fi",
                     "DI2_fi",
                     "ASW_fi",
                     "cdbw_fi"
  )
  
  fit_table = data.frame(matrix(NA, nrow = nruns-1, ncol=length(names_validity)))
  colnames(fit_table) = names_validity
  


  if (method == "fkmeans" | method == "fkmed") {
    fit_table %>% 
      mutate(PE_fi = NA,
             XB_fi = NA)
  }
  
  for (k in 2:nruns) {
    print(paste("nrun = ", k))
    
    #KMEANS#
    if (method == "kmeans") {
      fit_table$method = "kMeans"
      cl_result = kmeans(data, k, nstart = 100, iter.max = 30)
      cluster_solution = cl_result$cluster
    }
    
    #FKMEANS#
    if (method == "fkmeans") {
      fit_table$method = "fkMeans"
      cl_result = FKM(data, RS=10, k=k)
      cluster_solution = as.integer(cl_result$clus[,1])
      
      fit_table$PE_fi[k-1] = PE(U=cl_result$U)
      
      # minimum value of index = best cluster solution 
      fit_table$XB_fi[k-1] = XB(Xca=cl_result$Xca, 
                                U=cl_result$U, H = cl_result$H)
    }
    
    #PAM#
    if (method == "pam") {
      fit_table$method = "pam"
      cl_result = pam(dist(data, dist_measure), k)
      cluster_solution  = cl_result$clustering
    }
    
    #clara#
    if (method == "clara") {
      fit_table$method = "clara"
      cl_result = clara(data, k)

      cluster_solution  = cl_result$clustering
    }
    
    #FKMED#
    if (method == "fkmed") {
      cluster_data_dist = dist(data, dist_measure )
      
      
      fit_table$method = "fkmed"
      cl_result = FKM.med(as.matrix(cluster_data_dist), RS =1, k=k)
      
      cluster_solution = as.integer(cl_result$clus[,1])
      
      fit_table$PE_fi[k-1] = PE(U=cl_result$U)
      
      # minimum value of index = best cluster solution 
      fit_table$XB_fi[k-1] = XB(Xca=cl_result$Xca, 
                                U=cl_result$U, H = cl_result$H)
    }
    
    #FKMED#
    if (method == "fanny") {
      cluster_data_dist = dist(data, dist_measure )
      
      
      fit_table$method = "fanny"
      cl_result = fanny(cluster_data_dist, k=k, memb.exp=1.4, maxit=1000)
      
      cluster_solution = as.integer(cl_result$clustering)
      
      fit_table$PE_fi[k-1] = PE(U=cl_result$membership)
      fit_table$XB_fi[k-1] = NA
      # minimum value of index = best cluster solution 
      # fit_table$XB_fi[k-1] = XB(Xca=cluster_data_dist, 
      #                           U=cl_result$membership, H = cl_result$membership)
    }
    
    cl_stats = cluster.stats(dist(data, dist_measure), cluster_solution)
    
    # maximum value of index = best cluster solution
    fit_table$clusters[k-1] = k
    fit_table$CH_fi[k-1] = cl_stats$ch
    fit_table$DI1_fi[k-1] = cl_stats$dunn
    fit_table$DI2_fi[k-1] = cl_stats$dunn2
    fit_table$ASW_fi[k-1] = cl_stats$avg.silwidth
    fit_table$pG_fi[k-1] = cl_stats$pearsongamma
    # minimum value of index = best cluster solution
    fit_table$cdbw_fi[k-1] = cdbw(data, cluster_solution)$cdbw
    fit_table$DB2_fi[k-1] = index.DB(data, cluster_solution)$DB
    
  }
  return(fit_table) 
}

# Descriptive Analysis ####

dmx_data_trade %>% 
  select_at(vars(matches("dim_index_trade_off"))) %>% 
  cor() %>% 
  corrplot(method="number")


dmx_data_trade %>% 
  select(decision_freedom_trade_off, 
         intermediate_freedom_trade_off,
         communication_freedom_trade_off,
         rule_settlement_freedom_trade_off) %>% 
  cor() %>% 
  corrplot(method="number")

dmx_data_trade %>% 
  select(decision_equality_trade_off, 
         intermediate_equality_trade_off,
         communication_equality_trade_off,
         rule_settlement_equality_trade_off) %>% 
  cor() %>% 
  corrplot(method="number")

dmx_data_trade %>% 
  select_at(vars(matches("trade_off"), -matches("index"))) %>% 
  cor() %>% 
  corrplot()



# With NAs: 3427
# Without NAs: 2906

# Cluster Analysis

# Included Countries:

dmx_data_trade %>% 
  select(country) %>% 
  n_distinct()

dmx_data_trade %>% 
  select(country, year) %>% 
  group_by(country) %>% 
  summarise(No = n()) %>% 
  summarise(mean = mean(No),
            min = min(No),
            max = max(No))


year_country = dmx_data_trade %>% 
  select(country, year) %>% 
  group_by(country) %>% 
  summarise(No = n()) %>% 
  pull(No) 
string_countries = paste(unique(enc2utf8(dmx_data_trade$country)), " (", year_country, ")", sep="")
# write.csv(paste(string_countries, collapse = ", "), "Results/countries_cluster.csv", row.names = F, fileEncoding = "UTF-8")


# dmx_data_trade %>% 
#   filter(year >= 1900) %>% 
#   group_by(year) %>% 
#   select_at(vars(matches("freedom"))) %>% 
#   summarise_all(pMiss) %>% 
#   melt(id.vars="year") %>% 
#   ggplot(aes(x=year, y=value, fill=variable)) +
#   geom_bar(stat="identity", width=1) +
#   facet_wrap(variable~.) +
#   scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100), name=NULL)  +
#   scale_x_continuous(breaks=seq(1900,2020, 10), name=NULL) +
#   theme_bw()  +
#   theme(axis.text.x = element_text(angle=90), legend.position = "none") +
#   ggtitle("Percentage of NA-Values For Each Performance Areas")


# Create Distance Matrix ####

# dmx_data_trade2 = dmx_data_trade %>% 
#   sample_frac(0.2)

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

interface_FKM = function(data, k, method) {
  FKM_results = FKM(X = data, k, maxit=2000, stand=0, RS=20)
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


clustermethodpars <- list()
clustermethodpars[[5]] <- list()
clustermethodpars[[5]]$method <- "average"
clustermethodpars[[1]] <- list()
clustermethodpars[[1]]$method <- ""
clustermethodpars[[2]] <- list()
clustermethodpars[[2]]$method <- ""

distmethod <- c(F,F,F,F,F)

methodname <- c("FKM","FKM.med", "kmeans", "pam", "average")

set.seed(1234)
my_data = cluster_data %>%  sample_frac(0.26)
benchmark_results = clusterbenchstats(cluster_data, G=2:3,
                         diss = F,
                  clustermethod =  c("kmeansCBI", "pamkCBI", "hclustCBI"),
                  clustermethodpars = clustermethodpars,
                  distmethod = distmethod,
                  methodnames = methodname,
                  scaling=F,
                  multicore = F,
                  cores = 1,
                  nnruns = 10,
                  fnruns = 00,
                  avenruns = 00,
                  kmruns = 10,
                  useallg = F)



# save(benchmark_results, file = "Analyse/Cluster/RObjects/benchmark_results_nomean.Rdata")
# write.csv(my_data, "Analyse/Cluster/RObjects//bench_data_nomean.csv", fileEncoding = "UTF-8", row.names=F)
load(file = "Analyse/Cluster/RObjects/benchmark_results_nomean.Rdata")
my_data = fread("Analyse/Cluster/RObjects//bench_data_nomean.csv", encoding = "UTF-8") %>% select(-V1)

# Extract Fits ####
# Separate K ####
# Use average-within % pearson gamma

# when using sindex, inverse values (*-1)
poled_results = benchmark_results
for (i in 1:5) {
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
         aggregate = c(NA, unlist(aggregated_indizes[1,-1]))) %>% 
  bind_rows(as.data.frame(do.call(rbind, poled_results$sstat[[2]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "FKM.med",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[2,-1])))) %>% 
  bind_rows(as.data.frame(do.call(rbind, poled_results$sstat[[3]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "kmeans",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[3,-1]))) ) %>% 
  bind_rows(as.data.frame(do.call(rbind, poled_results$sstat[[4]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "pam",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[4,-1]))) ) %>% 
  bind_rows(as.data.frame(do.call(rbind, poled_results$sstat[[5]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "average",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes[5,-1]))) ) %>% 
  na.omit()

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
  #filter(method!="average", method!="kmeans", method!="pam") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=aggregate, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a2 = bench_results_df %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=avewithin, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a3 = bench_results_df %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>% 
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=pearsongamma, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()
a4 = bench_results_df %>% 
  #filter(method!="average", method!="kmeans", method!="pam") %>%
  filter(method!="FKM.med") %>% 
  ggplot(aes(x=cluster, y=asw, col=method, shape=method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2,10,1)) +
  geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(a1,a2,a3,a4)


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
              mutate(method = "FKM.med",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes_allk[2,-1])))) %>% 
  bind_rows(as.data.frame(do.call(rbind, allG_calibrated[[3]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "kmeans",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes_allk[3,-1]))) ) %>% 
  bind_rows(as.data.frame(do.call(rbind, allG_calibrated[[4]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "pam",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes_allk[4,-1]))) ) %>% 
  bind_rows(as.data.frame(do.call(rbind, allG_calibrated[[5]])) %>% 
              mutate_all(funs(unlist)) %>% 
              mutate(method = "average",
                     cluster = 1:10,
                     nr = "Solution",
                     aggregate = c(NA, unlist(aggregated_indizes_allk[5,-1]))) ) %>% 
  na.omit()


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
cboot_FKM_3 <- clusterboot(my_data,B=50,bootmethod=
                          c("boot"),clustermethod=interface_FKM,
                        k=3, seed=15555)
# save(cboot_FKM_3, file = "Analyse/Cluster/RObjects/cboot_FKM_3_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_3_nomean.Rdata")
print(cboot_FKM_3)

cboot_FKM_4 <- clusterboot(my_data,B=50,bootmethod=
                             c("boot"),clustermethod=interface_FKM,
                           k=4, seed=15555)
# save(cboot_FKM_4, file = "Analyse/Cluster/RObjects/cboot_FKM_4_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_4_nomean.Rdata")
print(cboot_FKM_4)

cboot_FKM_5 <- clusterboot(my_data,B=50,bootmethod=
                             c("boot"),clustermethod=interface_FKM,
                           k=5, seed=15555)
# save(cboot_FKM_5, file = "Analyse/Cluster/RObjects/cboot_FKM_5_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_5_nomean.Rdata")
print(cboot_FKM_5)

cboot_FKM_6 <- clusterboot(my_data,B=50,bootmethod=
                             c("boot"),clustermethod=interface_FKM,
                           k=6, seed=15555)
# save(cboot_FKM_6, file = "Analyse/Cluster/RObjects/cboot_FKM_6_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKM_6_nomean.Rdata")
print(cboot_FKM_6)

# Fuzzy K Medoid
cboot_FKMmed_3 <- clusterboot(my_data,B=50,bootmethod=
                          c("boot"),clustermethod=interface_FKMmed,
                        distances = F,
                        k=3, seed=15555)
# save(cboot_FKMmed_3, file = "Analyse/Cluster/RObjects/cboot_FKMmed_3_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKMmed_3_nomean.Rdata")
print(cboot_FKMmed_3)

cboot_FKMmed_4 <- clusterboot(my_data,B=50,bootmethod=
                                c("boot"),clustermethod=interface_FKMmed,
                              distances = F,
                              k=4, seed=15555)
# save(cboot_FKMmed_4, file = "Analyse/Cluster/RObjects/cboot_FKMmed_4_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKMmed_4_nomean.Rdata")
print(cboot_FKMmed_4)

cboot_FKMmed_5 <- clusterboot(my_data,B=50,bootmethod=
                                c("boot"),clustermethod=interface_FKMmed,
                              distances = F,
                              k=5, seed=15555)
# save(cboot_FKMmed_5, file = "Analyse/Cluster/RObjects/cboot_FKMmed_5_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKMmed_5_nomean.Rdata")
print(cboot_FKMmed_5)


cboot_FKMmed_6 <- clusterboot(my_data,B=50,bootmethod=
                          c("boot"),clustermethod=interface_FKMmed,
                        distances = F,
                        k=6, seed=15555)
save(cboot_FKMmed_6, file = "Analyse/Cluster/RObjects/cboot_FKMmed_6_nomean.Rdata")
load(file = "Analyse/Cluster/RObjects/cboot_FKMmed_6_nomean.Rdata")
print(cboot_FKMmed_6)


mytest = nselectboot(cluster_data,
                     B=50,
                     distances=F,
                     clustermethod=kmeansCBI,
                     classification="centroid",
                     krange=2:10,
                     count=T)


# Cluster the Whole Dataset ####


FKM_3 = FKM(X = cluster_data, 3, maxit=1400, stand=0, RS=30, seed=1234)
FKM_4 = FKM(X = cluster_data, 4, maxit=1400, stand=0, RS=30, seed=1234)
FKM_5 = FKM(X = cluster_data, 5, maxit=1400, stand=0, RS=30, seed=1234)
FKM_6 = FKM(X = cluster_data, 6, maxit=1400, stand=0, RS=30, seed=1234)
# save(FKM_3, file = "Analyse/Cluster/RObjects/FKM_3_nomean.Rdata")
# save(FKM_4, file = "Analyse/Cluster/RObjects/FKM_4_nomean.Rdata")
# save(FKM_5, file = "Analyse/Cluster/RObjects/FKM_5_nomean.Rdata")
# save(FKM_6, file = "Analyse/Cluster/RObjects/FKM_6_nomean.Rdata")
load((file = "Analyse/Cluster/RObjects/FKM_3_nomean.Rdata"))
load((file = "Analyse/Cluster/RObjects/FKM_4_nomean.Rdata"))
load((file = "Analyse/Cluster/RObjects/FKM_5_nomean.Rdata"))
load((file = "Analyse/Cluster/RObjects/FKM_6_nomean.Rdata"))


# Visualize Cluster Solutions ####

# FKM
FKM_3_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster = FKM_3$clus[,1])) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("PAM 3")

FKM_4_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster = FKM_4$clus[,1])) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("PAM 4")

FKM_5_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster = FKM_5$clus[,1])) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("PAM 5")

FKM_6_plot = data.frame(predict(prcomp(cluster_data))[,1:2]) %>%  
  bind_cols(data.frame(Cluster = FKM_6$clus[,1])) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("PAM 6")

grid.arrange(FKM_3_plot, FKM_4_plot, FKM_5_plot, FKM_6_plot)


# Compare to Clusterboot
data.frame(predict(prcomp(my_data))[,1:2]) %>% 
  bind_cols(data.frame(Cluster = cboot_FKM_5$partition)) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("FKM 5")

data.frame(predict(prcomp(my_data))[,1:2]) %>% 
  bind_cols(data.frame(Cluster = cboot_FKM_6$partition)) %>% 
  ggplot(aes(x=PC1, y=PC2, col=as.factor(Cluster))) +
  geom_point() +
  ggtitle("FKM 6")

#



FKM_3_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(FKM_3$clus[,1]))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw() +
  ggtitle("FKM 3")

FKM_4_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(FKM_4$clus[,1]))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw() +
  ggtitle("FKM 4")

FKM_5_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(FKM_5$clus[,1]))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw() +
  ggtitle("FKM 5")


FKM_6_dim_plot = dmx_data_trade %>%
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) %>%
  bind_cols(data.frame(Cluster = as.factor(FKM_6$clus[,1]))) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=Cluster, y=value, fill=name))+
  geom_boxplot() + 
  theme_bw() +
  ggtitle("FKM 6")

grid.arrange(FKM_3_dim_plot, FKM_4_dim_plot, FKM_5_dim_plot, FKM_6_dim_plot, nrow=4)


# Create Dataset ####

# Medoids
medoids = dmx_data_trade %>%
  select(country, year, regions,
         classification_core,
         freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off)
medoids[FKMmed_3$medoid,]
medoids[FKMmed_4$medoid,]
medoids[FKMmed_5$medoid,]
medoids[FKMmed_6$medoid,]


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
                               "fEC" = "1",  
                               "FeC" = "2",  
                               "FEc" = "3")
    ) %>% 
  rename(
    FKM_3_mb_fEC = Clus.1,
    FKM_3_mb_FeC = Clus.2,
    FKM_3_mb_FEc = Clus.3
  )  %>% 
  bind_cols(data.frame(FKM_6_cluster = as.factor(FKM_6$clus[,1]))) %>% 
  bind_cols(data.frame(round(FKM_6$U,3))) %>% 
  mutate(
    FKM_6_cluster = as.factor(FKM_6_cluster),
    FKM_6_cluster = fct_recode(FKM_6_cluster, 
                               "fEC" = "1",  
                               "Fec" = "2",  
                               "FEc" = "3",  
                               "fEc" = "4",  
                               "FEC" = "5",  
                               "FeC" = "6")
  ) %>% 
  rename(
    FKM_6_mb_fEC = Clus.1,
    FKM_6_mb_Fec = Clus.2,
    FKM_6_mb_FEc = Clus.3,
    FKM_6_mb_fEc = Clus.4,
    FKM_6_mb_FEC = Clus.5,
    FKM_6_mb_FeC = Clus.6
  )%>% 
  bind_cols(data.frame(FKMmed_4_cluster = as.factor(FKMmed_4$clus[,1]))) %>% 
  bind_cols(data.frame(round(FKMmed_4$U,3))) %>% 
  mutate(
         FKMmed_4_cluster = fct_recode(FKMmed_4_cluster, 
                                    "FeC" = "1",
                                    "fEC" = "2", 
                                    "FEc" = "3", 
                                    "fEc" = "4")
         ) %>% 
  rename(
      FKMmed_4_mb_FeC = Clus.1,
      FKMmed_4_mb_fEC = Clus.2,
      FKMmed_4_mb_FEc = Clus.3,
      FKMmed_4_mb_fEc = Clus.4,
    )  %>% 
  left_join(V_dem %>% select(country, year, country_text_id), by=c("country", "year")) %>%
  left_join(dmx_data_trade %>% select(country, year, na_count), by=c("country", "year")) %>% 
  select(country, country_text_id, year, regions, na_count, everything())


lab_FKM_3_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKM_3_cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw()

lab_FKM_6_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKM_6_cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw()

lab_fkmmed_3_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKMmed_3_cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw()

lab_fkmmed_6_dim_plot = dmx_trade_cluster %>%
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=FKMmed_6_cluster, y=value, fill=name))+
  geom_boxplot() +
  theme_bw()

grid.arrange(lab_FKM_3_dim_plot, lab_FKM_6_dim_plot, lab_fkmmed_3_dim_plot, lab_fkmmed_6_dim_plot, nrow=4)



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