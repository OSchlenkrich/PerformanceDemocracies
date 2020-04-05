# Democracy Profiles Cluster #####
source("Setup/Packages.R")
source("Setup/Base_Functions.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
library(fpc)
library(fclust)
library(clusterSim)
library(pixiedust)


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
      print(cl_result$silinfo)
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
         
         # weighting
         mean_dim = mean_dim * 0.5
  ) %>%
  ungroup() %>% 
  select(mean_dim, freedom, equality, control) 


cluster_data_dist = dist(cluster_data, method = "euclidean")


# MDS ####
library(smacof)

mds_obj = mds(cluster_data_dist)
plot(mds_obj, plot.type = "confplot")
permtest(mds_obj, nrep=100)


pam_2 = FKM.med(cluster_data_dist, 5)
pam_2 = Fclust(cluster_data_dist, distance=T)

pam_2 = pam(cluster_data, 4, metric="manhattan")
clusterboot(dist(cluster_data, "manhattan"), clustermethod = pamkCBI, k=9)

fpam = fanny(dist(cluster_data, "manhattan"), k=4, memb.exp=1.1)

adjustedRandIndex(pam_2$clustering, fpam$clustering)

fmed = FKM.med(dist(cluster_data, "manhattan"), k=4, m = 1.3)

dmx_data_trade %>%
  bind_cols(data.frame(mds_obj$conf)) %>% 
  #mutate(cluster = as.factor(pam_2$clus[,1])) %>%
  mutate(cluster = as.factor(pam_2$clustering)) %>%
  mutate(label = paste(country_text_id, "\n",cluster)) %>%
  rename("Dimension 1" = D1, "Dimension 2" = D2) %>% 
  ggplot(aes(x=`Dimension 1`, y=`Dimension 2`, label=label, col=cluster, labels=label)) +
  geom_text(size=3) +
  #scale_color_grey(start = 0, end = 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("MDS-Plot with Clusters")


dmx_data_trade %>%
  #mutate(cluster = as.factor(pam_2$clus[,1])) %>% 
  mutate(cluster = as.factor(pam_2$clustering)) %>%
  
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=cluster, y=value, fill=name))+
  geom_boxplot()

test = dmx_data_trade %>%
  #mutate(cluster = as.factor(pam_2$clus[,1])) %>% 
  mutate(cluster = as.factor(pam_2$clustering)) 
##

cluster_solut = fanny(cluster_data%>% 
                        sample_frac(0.2), 
                      k=3, diss=F, maxit=2000, metric ="manhattan",  memb.exp = 1.9)

cluster_solut$membership



cluster_solut = pamk(cluster_data_dist, krange=2:10, diss=T, usepam=F)
FKM.med(cluster_data_dist, 5)

# Cluster Analysis ####
#fit_table_fkmed = make_fit_indices(data = cluster_data,method="fkmed", nruns = 10)
fit_table_fkmed = make_fit_indices(data = cluster_data, method="pam", nruns = 10, dist_measure = "manhattan")

#Pixiedust Table
fit_table_fkmed %>% 
  pivot_longer(cols=ends_with("_fi")) %>%
  mutate(value = round(value, 3)) %>% 
  mutate(name = gsub("_fi","",name)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  select(-method) %>% 
  dust() %>% 
  sprinkle(rows = c(1), border = "top", border_color = "black", part=c("body"), border_thickness = 2) %>%
  sprinkle(cols = c(2,3,4), border = "left", border_color = "black", part=c("head")) %>%
  sprinkle(cols = c(2,3,4), border = "left", border_color = "black", part=c("body")) %>%
  
  # LocalMaxima = BOLD
  sprinkle(rows = which.peaks(fit_table_fkmed$CH_fi), cols=2, bold=T) %>%
  sprinkle(rows = which.peaks(fit_table_fkmed$DB2_fi, decreasing = T), cols=3, bold=T) %>%
  sprinkle(rows = which.peaks(fit_table_fkmed$pG_fi), cols=4, bold=T) %>% 
  sprinkle(rows = which.peaks(fit_table_fkmed$DI1_fi), cols=5, bold=T) %>%
  sprinkle(rows = which.peaks(fit_table_fkmed$DI2_fi), cols=6, bold=T) %>%
  sprinkle(rows = which.peaks(fit_table_fkmed$ASW_fi), cols=7, bold=T) %>%
  sprinkle(rows = which.peaks(fit_table_fkmed$cdbw_fi, decreasing = T), cols=8, bold=T) %>%
  # sprinkle(rows = which.peaks(fit_table_fkmed$PE_fi, decreasing = T), cols=9, bold=T) %>%
  # sprinkle(rows = which.peaks(fit_table_fkmed$XB_fi, decreasing = T), cols=10, bold=T) %>%
  
  # font size
  sprinkle(font_size = 11, font_size_units = "pt", part="head") %>% 
  sprinkle(font_size = 10, font_size_units = "pt", part="body") %>% 
  # NAs
  sprinkle_na_string(na_string = "") %>% 
  sprinkle_print_method("html")

# Lineplot
fit_table_fkmed %>% 
  pivot_longer(cols=ends_with("_fi")) %>%
  mutate(name = gsub("_fi","",name)) %>% 
  ggplot(aes(x=clusters, y=value, grp=name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(1,100, 1)) +
  facet_wrap(name ~ ., scales="free_y") +
  theme_bw()



###
library(mvtnorm)

resultsfinal = data.frame()
for (i in 1:100) {
  print(paste("iter", i))

  sample = rmvnorm(dim(cluster_data)[1], mean = colMeans(cluster_data), sigma = cov(cluster_data))
  results = data.frame(method = "clara", clusters = 2:10, ASW_fi = rep(NA, 9))
  #results = make_fit_indices(data = sample, method="clara", nruns = 10)
  for (k in 2:10) {
    clara_obj = clara(sample, k=k, pamLike=T, samples=1000, metric="euclidean")
    results$ASW_fi[k-1] = clara_obj$silinfo$avg.width
  }
  results = results %>% 
    cbind(data.frame(sample = i))
  
  resultsfinal = resultsfinal %>% 
    bind_rows(results)
}

results = data.frame(method = "clara", clusters = 2:10, ASW_fi = rep(NA, 9))
#results = make_fit_indices(data = sample, method="clara", nruns = 10)
for (k in 2:10) {
  clara_obj = clara(cluster_data, k=k, pamLike=T, samples=1000, metric="euclidean")
  results$ASW_fi[k-1] = clara_obj$silinfo$avg.width
}

#fit_table_fkmed = make_fit_indices(data = cluster_data, method="clara", nruns = 10)


resultsfinal %>% 
  group_by(clusters) %>% 
  summarise(median = quantile(ASW_fi, probs=0.5),
            lower = quantile(ASW_fi, probs=0.025),
            higher = quantile(ASW_fi, probs=0.975),
  ) %>% 
  ggplot(aes(x=clusters, y=median, ymin=lower, ymax=higher)) +
  geom_line() +
  geom_errorbar() +
  geom_line(inherit.aes = F, data=results, aes(x=clusters, y=ASW_fi), color="red")

resultsfinal %>% 
  ggplot(aes(x=clusters, y=ASW_fi, grp=as.factor(sample))) +
  geom_line() +
  geom_line(inherit.aes = F, data=results, aes(x=clusters, y=ASW_fi), color="red")

fkm_2 = FKM.med(cluster_data, RS =1, 2)
fkm_5 = FKM.med(cluster_data, RS =1, 5)
fkm_6 = FKM.med(cluster_data, RS =1, 6)
fkm_8 = clara(cluster_data, k=3, pamLike=T, samples=1000, metric="euclidean")
fkm_6 = pam(cluster_data, RS =1, 4)

# fkm_2 = pam(cluster_data, 4)
dmx_data_trade %>%
  mutate(cluster = as.factor(fkm_8$clustering)) %>% 
  pivot_longer(cols=c("freedom", "equality", "control")) %>% 
  mutate(name = fct_relevel(name, "freedom","equality","control")) %>% 
  ggplot(aes(x=cluster, y=value, fill=name))+
  geom_boxplot()

table(fkm_8$clustering)

test = dmx_data_trade %>%
  mutate(cluster = as.factor(pamk(cluster_data_dist, krange=5, diss=T, usepam=F)$pamobject$clustering)) 

# outlier detection
correlation_distance_out = as.dist(1-cor(t(dmx_trade_dimension_unequal %>% dplyr::select(freedom, equality, control))))
hc_outlier = hclust(correlation_distance_out, "single")
plot(hc_outlier, label=F)
nr_cuts_out = 1

table(stats::cutree(hc_outlier, nr_cuts_out))

hc_out_color=color_branches(as.dendrogram(hc_outlier),k=nr_cuts_out) 
hc_out_color %>% set("labels_cex", 0.001) %>% set("branches_lwd", 2) %>% plot(main = "HC - Single Linkage (Correlation Distance)")



dmx_trade_dimension_unequal_w_outlier = dmx_trade_dimension_unequal %>% 
  dplyr::select(country, year, regions, classification_core, freedom, equality, control) %>% 
  mutate(outlier = stats::cutree(hc_outlier, nr_cuts_out)) %>%
  filter(outlier == 1)
dmx_trade_dimension_unequal_outlier = dmx_trade_dimension_unequal %>% 
  dplyr::select(country, year, regions, classification_core, freedom, equality, control) %>% 
  mutate(outlier = stats::cutree(hc_outlier, nr_cuts_out)) %>%
  filter(outlier !=1)

##

# DIANA Clustering
correlation_distance = as.dist(1-cor(t(dmx_trade_dimension_unequal_w_outlier %>% dplyr::select(freedom, equality, control))))
dim(dmx_trade_dimension_unequal_w_outlier)

hc1 = diana(correlation_distance, diss=T)
hc1_color=color_branches(as.dendrogram(hc1),k=4) 
hc1_color %>% set("labels_cex", 0.001) %>% set("branches_lwd", 2) %>% plot(main = "DIANA (Correlation Distance)")


# Three or Four Clusters
hc_classes = stats::cutree(hc1, 4)
table(hc_classes)


# Boxplot
boxplot_dim(dmx_trade_dimension_unequal_w_outlier, hc_classes, "A: DIANA (4 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)

# Some Validation
# Silhouette
sil_hc1 = silhouette(hc_classes, correlation_distance)
plot_silhoutte(sil_hc1, "DIANA")


# Six Clusters
hc_classes_6 = stats::cutree(hc1, 6)
table(hc_classes_6)


# Boxplot
boxplot_dim(dmx_trade_dimension_unequal_w_outlier, hc_classes_6, "A: DIANA (5 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)

# Silhouette
sil_hc16 = silhouette(hc_classes_6, correlation_distance)
plot_silhoutte(sil_hc16, "DIANA")

## Validation ----

hclust_average = hclust(correlation_distance, "average")
hclust_average_classes = stats::cutree(hclust_average, 4)
adjustedRandIndex(hc_classes, hclust_average_classes)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier, hclust_average_classes, "A: HClust - average linkage (4 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)


# pam as best solutions
# extract medoids from DIANA
DIANA_med = dmx_trade_dimension_unequal_w_outlier %>% 
  mutate(DIANA_class = hc_classes) %>% 
  group_by(DIANA_class) %>% 
  summarise(freedom_med = mean(freedom), equality_med = mean(equality), control_med = mean(control)) %>% 
  right_join(dmx_trade_dimension_unequal_w_outlier %>%  mutate(DIANA_class = hc_classes, row_id = row_number()), by="DIANA_class") %>% 
  mutate(diff_f = abs(freedom - freedom_med),
         diff_e = abs(equality - equality_med),
         diff_c = abs(control - control_med),
         difference = diff_f + diff_e + diff_c
  ) %>% 
  group_by(DIANA_class) %>% 
  arrange(difference) %>% 
  slice(1)


pam_cluster = fanny(correlation_distance, 4, diss=T, iniMem.p = unmap(hc_classes))$clustering
fanny_every = fanny(correlation_distance, 4, diss=T, iniMem.p = unmap(hc_classes))

adjustedRandIndex(hc_classes, pam_cluster)
adjustedRandIndex(hclust_average_classes, pam_cluster)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier, pam_cluster, "A: FANNY (4 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)

sil_hc1 = silhouette(pam_cluster, correlation_distance)
plot_silhoutte(sil_hc1, "FANNY", c("fEC", "fEc", "FeC", "Fec"))



# Robustness ----
# all Jaccards: > 0.95
# robust_cluster = clusterboot(correlation_distance, B=100, bootmethod=c("boot", "subset"),
#                              clustermethod=interface_diana, k=4, seed=1234)
# print(robust_cluster)



# Create Dataset

dmx_trade_cluster = dmx_trade_dimension_unequal_w_outlier %>% 
                                mutate(cluster_1st = pam_cluster) %>% 
                                bind_cols(data.frame(fanny_every$membership)) %>% 
                                dplyr::select(-outlier) %>% 
  mutate(cluster_label_1st = as.factor(cluster_1st),
         cluster_label_1st = fct_recode(cluster_label_1st, 
                                    "fEC" = "1", # egalitarian + control
                                    "fEc" = "2", # egalitarian 
                                    "FeC" = "3", # liberal + control
                                    "Fec" = "4", # liberal
                                   # "FEC" = "5", # balanced
         ),
         cluster_label_1st = fct_relevel(cluster_label_1st,
                                     "Fec",
                                     "fEc",
                                     "FeC",
                                     "fEC",
                                     #"FEC"
         )
  )   %>% 
  left_join(V_dem %>% select(country, year, country_text_id), by=c("country", "year")) %>%
  left_join(dmx_data_trade %>% select(country, year, na_count), by=c("country", "year")) %>%
  select(country, country_text_id, year, regions, na_count, everything())



# prop.table(table(dmx_trade_cluster$classification_context,  dmx_trade_cluster$cluster_label_1st), 2)
# Boxplot


boxplot_dim(dmx_trade_cluster, dmx_trade_cluster$cluster_label_1st, "5 Clusters") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) 


# Basic information about clusters:
countries_cluster = dmx_trade_cluster %>% 
  select(cluster_label_1st, country) %>% 
  group_by(cluster_label_1st) %>% 
  distinct(country) %>% 
  summarise(No = n())


cluster_year = dmx_trade_cluster %>% 
  select(cluster_label_1st, country, year) %>% 
  group_by(cluster_label_1st, country) %>% 
  summarise(No = n()) %>% 
  summarise(mean = round(mean(No), 0),
            min = min(No),
            max = max(No))

string_clusters = paste(countries_cluster$cluster_label_1st,
                         ": ",
                         countries_cluster$No, 
                         " countries",
                         " (average years: ", cluster_year$mean,
                         ")", sep="")


write.csv(paste(string_clusters, collapse = "; "), "Results/cluster_info.csv", row.names = F, fileEncoding = "UTF-8")

string_clusters_year = dmx_trade_cluster %>% 
  select(cluster_label_1st, country) %>% 
  group_by(cluster_label_1st) %>% 
  distinct(country) %>% 
  arrange(cluster_label_1st) %>% 
  left_join(dmx_trade_cluster %>% 
              select(cluster_label_1st, country) %>% 
              group_by(cluster_label_1st, country) %>%
              summarise(No = n()), 
            by=c("cluster_label_1st", "country")) %>% 
  mutate(string = paste(country, " (", No, ")", sep=""))
write.csv(paste(string_clusters_year$string, collapse = ", "), "Results/cluster_country_year.csv", row.names = F, fileEncoding = "UTF-8")

# Split by Classification

dmx_trade_cluster %>%
  mutate(cluster = as.factor(cluster_label_1st)) %>%
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

# Plotting: Time Development Cluster

plot_types_N = dmx_trade_cluster %>%
  dplyr::select(cluster_label_1st, year) %>%
  group_by(year) %>%
  summarise(n_total=n())

complete_data_cluster = data.frame(cluster_label_1st = rep(unique(dmx_trade_cluster$cluster_label_1st), each=length(unique(dmx_trade_cluster$year))),
                                   year = rep(unique(dmx_trade_cluster$year), length(unique(dmx_trade_cluster$cluster_label_1st))))

plot_types_yearly = dmx_trade_cluster %>%
  dplyr::select(cluster_label_1st, year) %>%
  group_by(year, cluster_label_1st) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  full_join(complete_data_cluster, by=c("year", "cluster_label_1st")) %>%
  arrange(year, cluster_label_1st) %>%
  mutate(
    n = ifelse(is.na(n) == T, 0, n)
  ) %>%
  left_join(plot_types_N, by="year") %>%
  mutate(percent = n/n_total)



ggplot(plot_types_yearly, aes(x=year, y=n, fill=cluster_label_1st)) + 
  geom_area(stat="identity", col="black") + 
  theme_classic() +
  scale_x_continuous(breaks=seq(1900, 2020, 20)) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, size=10), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + 
  xlab("") +
  coord_cartesian(expand=0) + 
  scale_fill_brewer(name="", type="qual", palette="Paired") + 
  ggtitle("Temporal Distribution of Democracy Profiles", subtitle = "Count") + ylab("Count") 


ggplot(plot_types_yearly, aes(x=year, y=percent, fill=cluster_label_1st)) + geom_area(stat="identity", col="black", size=0.8) + theme_classic() +
  scale_x_continuous(breaks=seq(1900, 2000, 20), limits=c(1900, 2020)) + 
  theme(legend.position = "bottom", axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, size=12), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + xlab("") +
  coord_cartesian(expand=0) + 
  scale_fill_brewer(name="", type="qual", palette="Paired") + 
  ggtitle("Temporal Distribution of Democracy Profiles", subtitle = "Percent") + 
  scale_y_continuous(labels=percent, name="")


# Plotting World Map

create_world_map(dmx_trade_cluster, "cluster_label_1st", "1974-2017", 
                 "Spatial Distribution of Democracy Profiles \n", mode=T)
create_world_map(dmx_trade_cluster, "cluster_label_1st", "2017", 
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