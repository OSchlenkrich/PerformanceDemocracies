# CLUSTER ANALYSE ####
library(clusterSim)
library(fclust)
source("Analyse/CreateDatasets.R")

# source for function localMaxima: https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}


scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

make_fit_indices = function(method="kmeans", nruns = 10) {
  
  names_validity = c("method", "clusters", 
                     "CH_fi", 
                     #"DB2_fi",
                     "PH_fi",
                     #"DI1_fi",
                     #"DI2_fi",
                     "ASW_fi"
                     #"cdbw_fi"
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
      cl_result = kmeans(cluster_data, k, nstart = 100)
      cluster_solution = cl_result$cluster
    }
    
    #FKMEANS#
    if (method == "fkmeans") {
      fit_table$method = "fkMeans"
      cl_result = FKM(cluster_data, RS =100, k=k)
      cluster_solution = as.integer(cl_result$clus[,1])
      
      fit_table$PE_fi[k-1] = PE(U=cl_result$U)
      
      # minimum value of index = best cluster solution 
      fit_table$XB_fi[k-1] = XB(Xca=cl_result$Xca, 
                                U=cl_result$U, H = cl_result$H)
    }
    
    #PAM#
    if (method == "pam") {
      fit_table$method = "pam"
      cl_result = pam(cluster_data_dist, k)
      cluster_solution  = cl_result$clustering
    }
    
    #FKMED#
    if (method == "fkmed") {
      fit_table$method = "fkmed"
      cl_result = FKM.med(as.matrix(cluster_data), RS =1, k=k)
      
      cluster_solution = as.integer(cl_result$clus[,1])
      
      fit_table$PE_fi[k-1] = PE(U=cl_result$U)
      
      # minimum value of index = best cluster solution 
      fit_table$XB_fi[k-1] = XB(Xca=cl_result$Xca, 
                                U=cl_result$U, H = cl_result$H)
    }
    
    
    cl_stats = cluster.stats(cluster_data_dist, cluster_solution)
    
    # maximum value of index = best cluster solution
    fit_table$clusters[k-1] = k
    fit_table$CH_fi[k-1] = cl_stats$ch
    #fit_table$DI1_fi[k-1] = cl_stats$dunn
    #fit_table$DI2_fi[k-1] = cl_stats$dunn2
    fit_table$ASW_fi[k-1] = cl_stats$avg.silwidth
    fit_table$PH_fi[k-1] = cl_stats$pearsongamma
    # minimum value of index = best cluster solution 
    #fit_table$cdbw_fi[k-1] = cdbw(cluster_data, cluster_solution)$cdbw
    #fit_table$DB2_fi[k-1] = index.DB(cluster_data, cluster_solution)$DB
    
  }
  return(fit_table) 
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
  
  p1 = ggplot(plotted_country, aes(x=year,  y=y, fill=value)) + 
    geom_rect(aes(xmin=year, xmax = year + 10, ymin=0, ymax=1)) + 
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


# LOAD DATA ####
performance_cluster_data = performance_all %>% 
  filter(year >= 1990) %>% 
  select(country_text_id, year, 
         wealth_eco,
         #productivity_eco,
         air_env,
         #abstraction_env,
         #GA_ccp_ga,
         eco_inequal_soc,
         soc_inequal_soc,
         pubsafe_ds,
         conf_pc
         ) %>% 
  group_by(country_text_id) %>% 
  mutate(conf_pc = na.locf0(conf_pc)) %>% 
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


cluster_data_dist = dist(cluster_data, method = "euclidean")

# MDS

library(smacof)

mds_obj = mds(cluster_data_dist)
plot(mds_obj, plot.type = "confplot")
permtest(mds_obj, nrep=100)


# Cluster Validity Indices ####
fit_table_pam = make_fit_indices(method="pam", nruns = 10)

#Pixiedust Table
fit_table_pam %>% 
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
  sprinkle(rows = localMaxima(fit_table_pam$CH_fi), cols=2, bold=T) %>%
  sprinkle(rows = localMaxima(fit_table_pam$PH_fi), cols=3, bold=T) %>%
  sprinkle(rows = localMaxima(fit_table_pam$ASW_fi), cols=4, bold=T) %>%

  # font size
  sprinkle(font_size = 11, font_size_units = "pt", part="head") %>% 
  sprinkle(font_size = 10, font_size_units = "pt", part="body") %>% 
  # NAs
  sprinkle_na_string(na_string = "") %>% 
  sprinkle_print_method("html")

# Lineplot
fit_table_pam %>% 
  pivot_longer(cols=ends_with("_fi")) %>%
  mutate(name = gsub("_fi","",name)) %>% 
  ggplot(aes(x=clusters, y=value, grp=name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(1,100, 1)) +
  facet_wrap(name ~ ., scales="free_y") +
  theme_bw()




# 2 Cluster Solution  ####

pam_2 = pam(cluster_data, 2)


p2_1 = performance_data_noNAS %>%
  bind_cols(data.frame(mds_obj$conf)) %>% 
  mutate(cluster = as.factor(pam_2$clustering)) %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>% 
  mutate(label = paste(country_text_id, "\n",cluster)) %>%
  rename("Dimension 1" = D1, "Dimension 2" = D2) %>% 
  ggplot(aes(x=`Dimension 1`, y=`Dimension 2`, label=label, col=cluster, labels=label)) +
  geom_text(size=3) +
  scale_color_grey(start = 0, end = 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("MDS-Plot with Clusters")


p2_2 = performance_data_noNAS %>%
  mutate(cluster = as.factor(pam_2$clustering)) %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>%
  pivot_longer(cols=c(ends_with("_eco"), 
                                ends_with("_env"), 
                                ends_with("_ga"), 
                                ends_with("_soc"), 
                                ends_with("_ds"), 
                                ends_with("_pc")))   %>% 
  ggplot(aes(x=cluster, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Boxplot")

grid.arrange(p2_1, p2_2)

# 4 Cluster Solution  ####

pam_4 = pam(cluster_data, 4)

p4_1 = performance_data_noNAS %>%
  bind_cols(data.frame(mds_obj$conf)) %>% 
  mutate(cluster = as.factor(pam_4$clustering))  %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>% 
  mutate(label = paste(country_text_id, "\n",cluster)) %>%
  rename("Dimension 1" = D1, "Dimension 2" = D2) %>% 
  ggplot(aes(x=`Dimension 1`, y=`Dimension 2`, label=label, col=cluster, labels=label)) +
  geom_text(size=3) +
  scale_color_grey(start = 0, end = 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("MDS-Plot with Clusters")


p4_2 = performance_data_noNAS %>%
  mutate(cluster = as.factor(pam_4$clustering)) %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>%
  pivot_longer(cols=c(ends_with("_eco"), 
                      ends_with("_env"), 
                      ends_with("_ga"), 
                      ends_with("_soc"), 
                      ends_with("_ds"), 
                      ends_with("_pc")))  %>% 
  ggplot(aes(x=cluster, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Boxplot")

grid.arrange(p4_1, p4_2)


test = performance_data_noNAS %>%
  mutate(cluster = pam_4$clustering)



# 5 Cluster Solution ####

pam_5 = pam(cluster_data, 5)

p5_1 = performance_data_noNAS %>%
  bind_cols(data.frame(mds_obj$conf)) %>% 
  mutate(cluster = as.factor(pam_5$clustering))  %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>% 
  mutate(label = paste(country_text_id, "\n",cluster)) %>%
  rename("Dimension 1" = D1, "Dimension 2" = D2) %>% 
  ggplot(aes(x=`Dimension 1`, y=`Dimension 2`, label=label, col=cluster, labels=label)) +
  geom_text(size=3) +
  scale_color_grey(start = 0, end = 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("MDS-Plot with Clusters")


p5_2 = performance_data_noNAS %>%
  mutate(cluster = as.factor(pam_5$clustering)) %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>%
  pivot_longer(cols=c(ends_with("_eco"), 
                      ends_with("_env"), 
                      ends_with("_ga"), 
                      ends_with("_soc"), 
                      ends_with("_ds"), 
                      ends_with("_pc")))  %>% 
  ggplot(aes(x=cluster, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Boxplot")

grid.arrange(p5_1, p5_2)


test = performance_data_noNAS %>%
  mutate(cluster = pam_5$clustering)



# 7 Cluster Solution ####

pam_7 = pam(cluster_data, 7)


p7_1 = performance_data_noNAS %>%
  bind_cols(data.frame(mds_obj$conf)) %>% 
  mutate(cluster = as.factor(pam_7$clustering))  %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>% 
  mutate(label = paste(country_text_id, "\n",cluster)) %>%
  rename("Dimension 1" = D1, "Dimension 2" = D2) %>% 
  ggplot(aes(x=`Dimension 1`, y=`Dimension 2`, label=label, col=cluster, labels=label)) +
  geom_text(size=3) +
  scale_color_grey(start = 0, end = 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("MDS-Plot with Clusters")


p7_2 = performance_data_noNAS %>%
  mutate(cluster = as.factor(pam_7$clustering)) %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>%
  pivot_longer(cols=c(ends_with("_eco"), 
                      ends_with("_env"), 
                      ends_with("_ga"), 
                      ends_with("_soc"), 
                      ends_with("_ds"), 
                      ends_with("_pc")))  %>% 
  ggplot(aes(x=cluster, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Boxplot")

grid.arrange(p7_1, p7_2)



test = performance_data_noNAS %>%
  mutate(cluster = pam_7$clustering)

# 9 Cluster Solution ####

pam_9 = pam(cluster_data, 9)


p9_1 = performance_data_noNAS %>%
  bind_cols(data.frame(mds_obj$conf)) %>% 
  mutate(cluster = as.factor(pam_9$clustering))  %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>% 
  mutate(label = paste(country_text_id, "\n",cluster)) %>%
  rename("Dimension 1" = D1, "Dimension 2" = D2) %>% 
  ggplot(aes(x=`Dimension 1`, y=`Dimension 2`, label=label, col=cluster, labels=label)) +
  geom_text(size=3) +
  scale_color_grey(start = 0, end = 0.65) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("MDS-Plot with Clusters")


p9_2 = performance_data_noNAS %>%
  mutate(cluster = as.factor(pam_9$clustering)) %>%
  mutate(cluster = fct_reorder(cluster,  wealth_eco)) %>%
  pivot_longer(cols=c(ends_with("_eco"), 
                      ends_with("_env"), 
                      ends_with("_ga"), 
                      ends_with("_soc"), 
                      ends_with("_ds"), 
                      ends_with("_pc")))  %>% 
  ggplot(aes(x=cluster, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Boxplot")

grid.arrange(p9_1, p9_2)


# Complete Cluster Dataset ####

complete_cluster = performance_data_noNAS %>%
  mutate(cluster2 = pam_2$clustering) %>%
  mutate(cluster4 = pam_4$clustering) %>%
  mutate(cluster5 = pam_5$clustering) %>%
  mutate(cluster7 = pam_7$clustering) %>%
  mutate(cluster9 = pam_9$clustering)

complete_cluster %>% 
  group_by(cluster7, cluster9) %>% 
  summarise(n())

# Development over Time ####

cluster_time = performance_data_noNAS %>%
  mutate(cluster5 = as.factor(pam_5$clustering),
         cluster5 = fct_recode(cluster5,
            "acepsw"=  "1",
            "ACEPSW" = "4",
            "ACePsW" = "3",
            "high equality" = "2",
            "aCepsw" = "5"
         )) 

sample = c("NZL", "GBR", "USA", "SWE", "CHE", "DEU", "BRA")
plot_countries_overtime(sample)
plot_countries_overtime(5)



plot_types_N = cluster_time %>%
  dplyr::select(cluster5, year) %>%
  group_by(year) %>%
  summarise(n_total=n())

complete_data_cluster = data.frame(cluster5 = rep(unique(cluster_time$cluster5), each=length(unique(cluster_time$year))),
                                   year = rep(unique(cluster_time$year), length(unique(cluster_time$cluster5))))

plot_types_yearly = cluster_time %>%
  dplyr::select(cluster5, year) %>%
  group_by(year, cluster5) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  full_join(complete_data_cluster, by=c("year", "cluster5")) %>%
  arrange(year, cluster5) %>%
  mutate(
    n = ifelse(is.na(n) == T, 0, n)
  ) %>%
  left_join(plot_types_N, by="year") %>%
  mutate(percent = n/n_total)



p1 = ggplot(plot_types_yearly, aes(x=year, y=n, fill=cluster5)) + 
  geom_area(stat="identity", col="black") + 
  theme_classic() +
  scale_x_continuous(breaks=seq(1990, 2020, 10), limits=c(1990, 2015)) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, size=10), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + 
  xlab("") +
  coord_cartesian(expand=0) + 
  scale_fill_brewer(name="", type="qual", palette="Paired") + 
  ggtitle("Temporal Distribution of Democracy Profiles", subtitle = "Count") + ylab("Count") 


p2 = ggplot(plot_types_yearly, aes(x=year, y=percent, fill=cluster5)) + geom_area(stat="identity", col="black", size=0.8) + theme_classic() +
  scale_x_continuous(breaks=seq(1990, 2020, 10), limits=c(1990, 2015)) + 
  theme(legend.position = "bottom", axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, size=12), plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + xlab("") +
  coord_cartesian(expand=0) + 
  scale_fill_brewer(name="", type="qual", palette="Paired") + 
  ggtitle("Temporal Distribution of Democracy Profiles", subtitle = "Percent") + 
  scale_y_continuous(labels=percent, name="")

grid.arrange(p1,p2)

