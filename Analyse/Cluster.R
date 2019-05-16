source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")

# With NAs: 3427
# Without NAs: 2906


# Cluster Analysis
# Extract Same Level
dmx_trade_dimension_prep = dmx_data_trade %>%
  rowwise() %>%
  mutate(minimum =  min(c(freedom, equality, control)),
         freedom = freedom - minimum,
         equality = equality - minimum,
         control = control - minimum
  ) %>%
  select(-minimum) %>%
  ungroup()

dmx_trade_dimension_prep$class = apply(dmx_trade_dimension_prep %>% 
                                         select(freedom, equality, control), 1, 
                                       FUN=function(x) length(which(x >= 0 & x < 0.05 | x <= 0 & x > -0.05)))

dmx_trade_dimension_prep = dmx_trade_dimension_prep %>%
  mutate(class = if_else(class==3, 1, 0))

dmx_trade_dimension_same = dmx_data_trade %>%
  mutate(class = dmx_trade_dimension_prep$class) %>%
  filter(class==1) %>%
  select(-class) 

dmx_trade_dimension_unequal = dmx_data_trade %>%
  mutate(class = dmx_trade_dimension_prep$class) %>%
  filter(class==0) %>%
  select(-class) 

dim(dmx_trade_dimension_same)
dim(dmx_trade_dimension_unequal)

###

# outlier detection
correlation_distance_out = as.dist(1-cor(t(dmx_trade_dimension_unequal %>% select(freedom, equality, control))))
hc_outlier = hclust(correlation_distance_out, "single")
plot(hc_outlier, label=F)
nr_cuts_out = 2

table(stats::cutree(hc_outlier, nr_cuts_out))

hc_out_color=color_branches(as.dendrogram(hc_outlier),k=2) 
hc_out_color %>% set("labels_cex", 0.001) %>% set("branches_lwd", 2) %>% plot(main = "HC - Single Linkage (Correlation Distance)")



dmx_trade_dimension_unequal_w_outlier = dmx_trade_dimension_unequal %>% 
  select(country, year, regions, classification_context, freedom, equality, control) %>% 
  mutate(outlier = stats::cutree(hc_outlier, nr_cuts_out)) %>%
  filter(outlier == 1)
dmx_trade_dimension_unequal_outlier = dmx_trade_dimension_unequal %>% 
  select(country, year, regions, classification_context, freedom, equality, control) %>% 
  mutate(outlier = stats::cutree(hc_outlier, nr_cuts_out)) %>%
  filter(outlier !=1)

##

# DIANA Clustering
correlation_distance = as.dist(1-cor(t(dmx_trade_dimension_unequal_w_outlier %>% select(freedom, equality, control))))
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



## Validation ----

hclust_average = hclust(correlation_distance, "average")
hclust_average_classes = stats::cutree(hclust_average, 4)
adjustedRandIndex(hc_classes, hclust_average_classes)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier, hclust_average_classes, "A: HClust - average linkage (4 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)

# pam as best solutions
pam_cluster = pam(correlation_distance, 4)$clustering


adjustedRandIndex(hc_classes, pam_cluster)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier, pam_cluster, "A: PAM (4 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)

sil_hc1 = silhouette(pam_cluster, correlation_distance)
plot_silhoutte(sil_hc1, "DIANA")

# Robustness ----
# all Jaccards: > 0.95
# robust_cluster = clusterboot(correlation_distance, B=100, bootmethod=c("boot"), 
#                              clustermethod=interface_diana, k=4, seed=1234)
# print(robust_cluster)


dmx_trade_cluster = dmx_trade_dimension_unequal_w_outlier %>% 
                                mutate(cluster_1st = hc_classes) %>% 
  bind_cols(data.frame(pam_cluster6$membership))


# Create Dataset
dmx_trade_cluster = bind_rows(dmx_trade_dimension_unequal_w_outlier %>% 
                                mutate(cluster_1st = pam_cluster,
                                       cluster_2nd = hclust_average_classes) %>% 
                                select(-outlier), 
                              dmx_trade_dimension_same %>% 
                                mutate(cluster_1st = max(pam_cluster) + 1,
                                       cluster_2nd = max(hclust_average_classes) + 1)
) %>% 
  mutate(cluster_label_1st = as.factor(cluster_1st),
         cluster_label_1st = fct_recode(cluster_label_1st, 
                                    "fEC" = "1", # egalitarian + control
                                    "fEc" = "2", # egalitarian 
                                    "FeC" = "3", # liberal + control
                                    "Fec" = "4", # liberal
                                    "FEC" = "5", # balanced
         ),
         cluster_label_1st = fct_relevel(cluster_label_1st,
                                     "Fec",
                                     "fEc",
                                     "FeC",
                                     "fEC",
                                     "FEC"
         )
  )  %>% 
  mutate(cluster_label_2nd = as.factor(cluster_2nd),
         cluster_label_2nd = fct_recode(cluster_label_2nd, 
                                    "fEC" = "2", # egalitarian + control
                                    "fEc" = "1", # egalitarian 
                                    "FeC" = "3", # liberal + control
                                    "Fec" = "4", # liberal
                                    "FEC" = "5", # balanced
         ),
         cluster_label_2nd = fct_relevel(cluster_label_2nd,
                                     "Fec",
                                     "fEc",
                                     "FeC",
                                     "fEC",
                                     "FEC"
         )
  )




# prop.table(table(dmx_trade_cluster$classification_context,  dmx_trade_cluster$cluster_label_1st), 2)
# Boxplot
boxplot_dim(dmx_trade_cluster, dmx_trade_cluster$cluster_label_1st, "A: DIANA (5 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) 
boxplot_dim(dmx_trade_cluster, dmx_trade_cluster$cluster_label_2nd, "A: HC - Average Linkage (5 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) 


# Plotting
plot_random_countries_dim(5)

plot_random_countries_dim(c("Germany", "Sweden","United Kingdom", "New Zealand"))
plot_random_countries_dim(c("United States of America"))

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


