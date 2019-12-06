
source("Analyse/Cluster/ImputeCluster.R")


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

dmx_trade_dimension_prep = dmx_trade_dimension_prep %>%
  mutate(class = if_else(class==3, 1, 0))

dmx_trade_dimension_same = dmx_data_trade %>%
  mutate(class = dmx_trade_dimension_prep$class) %>%
  filter(class==1) %>%
  dplyr::select(-class, -na_count) 

dmx_trade_dimension_unequal = dmx_data_trade %>%
  mutate(class = dmx_trade_dimension_prep$class) %>%
  filter(class==0) %>%
  dplyr::select(-class) 

dim(dmx_trade_dimension_same)
dim(dmx_trade_dimension_unequal)



###

# outlier detection
correlation_distance_out = as.dist(1-cor(t(dmx_trade_dimension_unequal %>% dplyr::select(freedom, equality, control))))
hc_outlier = hclust(correlation_distance_out, "single")
plot(hc_outlier, label=F)
nr_cuts_out = 3

table(stats::cutree(hc_outlier, nr_cuts_out))

hc_out_color=color_branches(as.dendrogram(hc_outlier),k=2) 
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

dmx_trade_cluster = bind_rows(dmx_trade_dimension_unequal_w_outlier %>% 
                                mutate(cluster_1st = pam_cluster) %>% 
                                bind_cols(data.frame(fanny_every$membership)) %>% 
                                dplyr::select(-outlier), 
                              dmx_trade_dimension_same %>% 
                                mutate(cluster_1st = max(pam_cluster) + 1)
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
  )   %>% 
  select(-country_text_id) %>% 
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
  dplyr::select(freedom, equality, control, cluster, classification_context) %>%
  melt(id.vars=c("classification_context", "cluster")) %>% 
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
  facet_wrap(classification_context ~ .)


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