source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")


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


###

# outlier detection
correlation_distance_out = as.dist(1-cor(t(dmx_trade_dimension_unequal %>% select(freedom, equality, control))))
hc_outlier = hclust(correlation_distance_out, "single")
plot(hc_outlier, label=F)
nr_cuts_out = 2

table(stats::cutree(hc_outlier, nr_cuts_out))


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
hc1_color=color_branches(as.dendrogram(hc1),k=3) 
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




dmx_trade_cluster = bind_rows(dmx_trade_dimension_unequal_w_outlier %>% 
                                mutate(cluster = hc_classes) %>% 
                                select(-outlier), 
                              dmx_trade_dimension_same %>% 
                                mutate(cluster = max(hc_classes) + 1)
                              ) %>% 
  mutate(cluster_label = as.factor(cluster),
         cluster_label = fct_recode(cluster_label, 
                                    "fEC" = "1", # egalitarian + control
                                    "fEc" = "2", # egalitarian 
                                    "FeC" = "3", # liberal + control
                                    "Fec" = "4", # liberal
                                    "FEC" = "5", # balanced
                                    ),
         cluster_label = fct_relevel(cluster_label,
                                     "fEC",
                                     "Fec"
                                     )
         )

# Boxplot
boxplot_dim(dmx_trade_cluster, dmx_trade_cluster$cluster_label, "A: DIANA (5 Clusters)") + 
  geom_vline(xintercept = 1.5) + geom_vline(xintercept = 2.5) + geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) 

# Plotting
plot_random_countries_dim(5)

plot_random_countries_dim(c("Germany", "Sweden","United Kingdom", "New Zealand"))


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


