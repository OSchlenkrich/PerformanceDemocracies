source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")

# With NAs: 3427
# Without NAs: 2906
dim(dmx_data_trade)

# Cluster Analysis
# Extract Same Level
dmx_trade_dimension_prep = dmx_data_trade %>%
  rowwise() %>%
  mutate(minimum =  min(c(freedom, equality, control)),
         freedom = freedom - minimum,
         equality = equality - minimum,
         control = control - minimum
  ) %>%
  dplyr::select(-minimum) %>%
  ungroup()

dmx_trade_dimension_prep$class = apply(dmx_trade_dimension_prep %>% 
                                         dplyr::select(freedom, equality, control), 1, 
                                       FUN=function(x) length(which(x >= 0 & x < 0.05 | x <= 0 & x > -0.05)))

table(dmx_trade_dimension_prep$class)


# BALANCED
dmx_trade_dimension_same = dmx_data_trade %>%
  mutate(class = dmx_trade_dimension_prep$class) %>%
  filter(class==3) %>%
  dplyr::select(-class) 

# 2 Unequal Dimensions 
dmx_trade_dimension_unequal_2dim = dmx_data_trade %>%
  mutate(class = dmx_trade_dimension_prep$class) %>%
  filter(class==1) %>%
  dplyr::select(-class) 

# 1 Unequal Dimensions 
dmx_trade_dimension_unequal_1dim = dmx_data_trade %>%
  mutate(class = dmx_trade_dimension_prep$class) %>%
  filter(class==2) %>%
  dplyr::select(-class) 

dim(dmx_trade_dimension_same)
dim(dmx_trade_dimension_unequal_2dim)
dim(dmx_trade_dimension_unequal_1dim)



#### 1 Unequal Dimensions Cluster Analysis ####

# outlier detection
correlation_distance_out = as.dist(1-cor(t(dmx_trade_dimension_unequal_1dim %>% dplyr::select(freedom, equality, control))))
hc_outlier = hclust(correlation_distance_out, "single")
plot(hc_outlier, label=F)
nr_cuts_out = 2

table(stats::cutree(hc_outlier, nr_cuts_out))

hc_out_color=color_branches(as.dendrogram(hc_outlier),k=2) 
hc_out_color %>% set("labels_cex", 0.001) %>% set("branches_lwd", 2) %>% plot(main = "HC - Single Linkage (Correlation Distance)")



dmx_trade_dimension_unequal_w_outlier = dmx_trade_dimension_unequal_1dim %>% 
  dplyr::select(country, year, regions, classification_context, freedom, equality, control) %>% 
  mutate(outlier = stats::cutree(hc_outlier, nr_cuts_out)) %>%
  filter(outlier == 1)
dmx_trade_dimension_unequal_outlier = dmx_trade_dimension_unequal_1dim %>% 
  dplyr::select(country, year, regions, classification_context, freedom, equality, control) %>% 
  mutate(outlier = stats::cutree(hc_outlier, nr_cuts_out)) %>%
  filter(outlier !=1)

##

# DIANA Clustering
correlation_distance_1U = as.dist(1-cor(t(dmx_trade_dimension_unequal_w_outlier %>% dplyr::select(freedom, equality, control))))
dim(dmx_trade_dimension_unequal_w_outlier)

hc1 = diana(correlation_distance_1U, diss=T)
hc1_color=color_branches(as.dendrogram(hc1),k=4) 
hc1_color %>% set("labels_cex", 0.001) %>% set("branches_lwd", 2) %>% plot(main = "DIANA (Correlation Distance)")


# Two or Four Clusters
hc_classes = stats::cutree(hc1, 2)
table(hc_classes)


# Boxplot
boxplot_dim(dmx_trade_dimension_unequal_w_outlier, hc_classes, "A: DIANA (2 Clusters)") + 
  geom_vline(xintercept = 1.5) 

# Some Validation
# Silhouette
sil_hc1 = silhouette(hc_classes, correlation_distance_1U)
plot_silhoutte(sil_hc1, "DIANA")


# Four Clusters
hc_classes_4 = stats::cutree(hc1, 4)
table(hc_classes_4)


# Boxplot
boxplot_dim(dmx_trade_dimension_unequal_w_outlier, hc_classes_4, "A: DIANA (4 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5)

# Silhouette Worse Silhouette
sil_hc14 = silhouette(hc_classes_4, correlation_distance_1U)
plot_silhoutte(sil_hc14, "DIANA")


## Validation ----

hclust_average = hclust(correlation_distance_1U, "average")
hclust_average_classes = stats::cutree(hclust_average, 2)
adjustedRandIndex(hc_classes, hclust_average_classes)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier, hclust_average_classes, "A: HClust - average linkage (4 Clusters)") + 
  geom_vline(xintercept = 1.5)


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


pam_cluster_2 = pam(correlation_distance_1U, medoids = DIANA_med$row_id, 2)$clustering
dmx_trade_dimension_unequal_w_outlier[pam(correlation_distance_1U, medoids = DIANA_med$row_id, 2)$medoids,]

adjustedRandIndex(hc_classes, pam_cluster_2)
adjustedRandIndex(hclust_average_classes, pam_cluster_2)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier, pam_cluster_2, "A: PAM (2 Clusters)") + 
  geom_vline(xintercept = 1.5)

sil_hc1 = silhouette(pam_cluster_2, correlation_distance_1U)
plot_silhoutte(sil_hc1, "PAM")


### 2 Dimensions

# outlier detection
correlation_distance_out = as.dist(1-cor(t(dmx_trade_dimension_unequal_2dim %>% dplyr::select(freedom, equality, control))))
hc_outlier = hclust(correlation_distance_out, "single")
plot(hc_outlier, label=F)
nr_cuts_out = 2

table(stats::cutree(hc_outlier, nr_cuts_out))

hc_out_color=color_branches(as.dendrogram(hc_outlier),k=2) 
hc_out_color %>% set("labels_cex", 0.001) %>% set("branches_lwd", 2) %>% plot(main = "HC - Single Linkage (Correlation Distance)")

# no outliers
dmx_trade_dimension_unequal_w_outlier_2dim = dmx_trade_dimension_unequal_2dim %>% 
  dplyr::select(country, year, regions, classification_context, freedom, equality, control) 

##

# DIANA Clustering
correlation_distance = as.dist(1-cor(t(dmx_trade_dimension_unequal_w_outlier_2dim %>% dplyr::select(freedom, equality, control))))
dim(dmx_trade_dimension_unequal_w_outlier_2dim)

hc1 = diana(correlation_distance, diss=T)
hc1_color=color_branches(as.dendrogram(hc1),k=4) 
hc1_color %>% set("labels_cex", 0.001) %>% set("branches_lwd", 2) %>% plot(main = "DIANA (Correlation Distance)")


# Three or Six Clusters
hc_classes = stats::cutree(hc1, 3)
table(hc_classes)


# Boxplot
boxplot_dim(dmx_trade_dimension_unequal_w_outlier_2dim, hc_classes, "A: DIANA (3 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) 

# Some Validation
# Silhouette
sil_hc1 = silhouette(hc_classes, correlation_distance)
plot_silhoutte(sil_hc1, "DIANA")


## Validation ----

hclust_average = hclust(correlation_distance, "average")
hclust_average_classes = stats::cutree(hclust_average, 3)
adjustedRandIndex(hc_classes, hclust_average_classes)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier_2dim, hclust_average_classes, "A: HClust - average linkage (3 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) 


# pam as best solutions
# extract medoids from DIANA
DIANA_med = dmx_trade_dimension_unequal_w_outlier_2dim %>% 
  mutate(DIANA_class = hc_classes) %>% 
  group_by(DIANA_class) %>% 
  summarise(freedom_med = mean(freedom), equality_med = mean(equality), control_med = mean(control)) %>% 
  right_join(dmx_trade_dimension_unequal_w_outlier_2dim %>%  mutate(DIANA_class = hc_classes, row_id = row_number()), by="DIANA_class") %>% 
  mutate(diff_f = abs(freedom - freedom_med),
         diff_e = abs(equality - equality_med),
         diff_c = abs(control - control_med),
         difference = diff_f + diff_e + diff_c
  ) %>% 
  group_by(DIANA_class) %>% 
  arrange(difference) %>% 
  slice(1)


pam_cluster_3 = pam(correlation_distance, medoids = DIANA_med$row_id, 3)$clustering

dmx_trade_dimension_unequal_w_outlier_2dim[pam(correlation_distance, medoids = DIANA_med$row_id, 3)$medoids,]


adjustedRandIndex(hc_classes, pam_cluster_3)
adjustedRandIndex(hclust_average_classes, pam_cluster_3)

boxplot_dim(dmx_trade_dimension_unequal_w_outlier_2dim, pam_cluster_3, "A: PAM (3 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) 

sil_hc1 = silhouette(pam_cluster_3, correlation_distance)
plot_silhoutte(sil_hc1, "PAM")



# Robustness ----
# all Jaccards: > 0.95
# robust_cluster = clusterboot(correlation_distance, B=100, bootmethod=c("boot"),
#                              clustermethod=interface_diana, k=4, seed=1234)
# print(robust_cluster)


# Create Dataset


dmx_trade_cluster = bind_rows(dmx_trade_dimension_unequal_w_outlier %>% 
                                mutate(cluster_1st = pam_cluster_2) %>% 
                                dplyr::select(-outlier),
                              dmx_trade_dimension_unequal_w_outlier_2dim %>% 
                                mutate(cluster_1st = pam_cluster_3 + max(pam_cluster_2))) %>% 
                    bind_rows(dmx_trade_dimension_same %>% 
                                mutate(cluster_1st = max(pam_cluster_2) + max(pam_cluster_3) + 1)
                    ) %>% 
  mutate(cluster_label_1st = as.factor(cluster_1st),
         cluster_label_1st = fct_recode(cluster_label_1st, 
                                    "fEc" = "1", # egalitarian
                                    "Fec" = "2", # libertarian 
                                    "fEC" = "3", # egalitarian + control
                                    "FeC" = "4", # libertarian + control
                                    "FEc" = "5", # libertarian + egalitarian
                                    "FEC" = "6", # balanced
         ),
         cluster_label_1st = fct_relevel(cluster_label_1st,
                                     "Fec",
                                     "fEc",
                                     "FeC",
                                     "FEc",
                                     "fEC",
                                     "FEC"
         )
  )


# prop.table(table(dmx_trade_cluster$classification_context,  dmx_trade_cluster$cluster_label_1st), 2)
# Boxplot

boxplot_dim(dmx_trade_cluster, dmx_trade_cluster$cluster_1st, "A: PAM (6 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5)  +  geom_vline(xintercept = 5.5) 

boxplot_dim(dmx_trade_cluster, dmx_trade_cluster$cluster_label_1st, "A: PAM (6 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5)  +  geom_vline(xintercept = 5.5) 




# Plotting: Random Countries
plot_random_countries_dim(5)

plot_random_countries_dim(c("Germany", "Sweden","United Kingdom", "New Zealand"))
plot_random_countries_dim(c("United States of America"))

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
  ggtitle("Temporal Distribution of Subtypes of Democracy", subtitle = "Count") + ylab("Count") 


ggplot(plot_types_yearly, aes(x=year, y=percent, fill=cluster_label_1st)) + geom_area(stat="identity", col="black", size=0.8) + theme_classic() +
  scale_x_continuous(breaks=seq(1900, 2000, 20), limits=c(1900, 2020)) + 
  theme(legend.position = "bottom", axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, size=12), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + xlab("") +
  coord_cartesian(expand=0) + 
  scale_fill_brewer(name="", type="qual", palette="Paired") + 
  ggtitle("Temporal Distribution of Subtypes of Democracy", subtitle = "Percent") + 
  scale_y_continuous(labels=percent, name="")




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
