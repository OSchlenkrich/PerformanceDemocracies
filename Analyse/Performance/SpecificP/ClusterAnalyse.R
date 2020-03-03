# CLUSTER ANALYSE ####
library(clusterSim)

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

make_fit_indices = function(method="kmeans", nruns = 10) {
  
  fit_table = data.frame(matrix(NA, nrow = nruns-1, ncol=9))
  colnames(fit_table) = c("method", "clusters", 
                          "CH_fi", 
                          "DB2_fi",
                          "pG_fi",
                          "DI1_fi",
                          "DI2_fi",
                          "ASW_fi",
                          "cdbw_fi"
  )
  
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
    fit_table$DI1_fi[k-1] = cl_stats$dunn
    fit_table$DI2_fi[k-1] = cl_stats$dunn2
    fit_table$ASW_fi[k-1] = cl_stats$avg.silwidth
    fit_table$pG_fi[k-1] = cl_stats$pearsongamma
    # minimum value of index = best cluster solution 
    fit_table$cdbw_fi[k-1] = cdbw(cluster_data, cluster_solution)$cdbw
    fit_table$DB2_fi[k-1] = index.DB(cluster_data, cluster_solution)$DB
    
  }
  return(fit_table) 
}


# LOAD DATA ####
performance_data = performance_all %>% 
  filter(year >= 1970) %>% 
  select(country_text_id, year, 
         wealth_eco,
         #productivity_eco,
         air_env,
         #abstraction_env,
         #GA_ccp_ga,
         eco_inequal_soc,
         soc_inequal_soc,
         pubsafe_ds
         #conf_pc
         ) 
  # mutate(year5 = floor(year/5)*5) %>% 
  # group_by(country_text_id, year5) %>% 
  # summarise_all(funs(mean(., na.rm=T))) %>% 
  # ungroup() %>% 
  # select(-year) 


performance_data %>% 
  group_by(year5) %>% 
  select_at(vars(ends_with("_eco"), 
                 ends_with("_env"), 
                 #ends_with("_ga"), 
                 ends_with("_soc"), 
                 ends_with("_ds"), 
                 ends_with("_pc"))) %>% 
  summarise_all(funs(1-pMiss_01(.))) %>% 
  melt(id.vars="year5") %>% 
  ggplot(aes(x=year5, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")


performance_data_noNAS = performance_data %>% 
  na.omit()

cluster_data = performance_data_noNAS %>%
  na.omit() %>% 
  rowwise() %>%
  mutate(mean_dim =  mean(c(wealth_eco , 
                            #productivity_eco , 
                            air_env ,
                            #abstraction_env , 
                            eco_inequal_soc , 
                            #GA_ccp_ga, 
                            soc_inequal_soc ,
                            pubsafe_ds
                            #conf_pc 
                            ))) %>% 
  mutate_at(vars(ends_with("_eco"),
                 ends_with("_env"),
                 ends_with("_ga"),
                 ends_with("_soc"),
                 ends_with("_ds"),
                 ends_with("_pc")), funs(.-mean_dim)) %>%
  mutate(# weighting
         mean_dim = mean_dim * 1
  ) %>%
  ungroup() %>% 
  select(-country_text_id, -year )


cluster_data_dist = dist(cluster_data, method = "euclidean")

# MDS

library(smacof)
mds_obj = mds(cluster_data %>% dist())
plot(mds_obj, plot.type = "confplot")
abline(h=0, lty=2); abline(v=0, lty=2)



# PAM Cluster ####
fit_table_pam = make_fit_indices(method="pam", nruns = 10)

fit_table_pam %>% 
  pivot_longer(cols=ends_with("_fi")) %>%
  ggplot(aes(x=clusters, y=value, col=name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(1,100, 1)) +
  facet_wrap(name ~ ., scales="free_y") 

fit_table_pam %>% 
  mutate_at(vars(ends_with("_fi"), -DB2_fi), funs(ifelse(. == max(.), 1, 0))) %>% 
  mutate_at(vars(DB2_fi), funs(ifelse(. == min(.), 1, 0))) %>% 
  mutate(best = CH_fi + DB2_fi + pG_fi + DI1_fi + DI2_fi + ASW_fi + cdbw_fi)



pam_7 = pam(cluster_data_dist,5)

performance_data_noNAS %>%
  mutate(cluster = as.factor(pam_7$clustering)) %>%
  pivot_longer(cols=c(ends_with("_eco"), 
                                ends_with("_env"), 
                                ends_with("_ga"), 
                                ends_with("_soc"), 
                                ends_with("_ds"), 
                                ends_with("_pc"))) %>% 
  ggplot(aes(x=cluster, y=value, fill=name))+
  geom_boxplot()


test = performance_data_noNAS %>%
  mutate(cluster = as.factor(pam_7$clustering))
