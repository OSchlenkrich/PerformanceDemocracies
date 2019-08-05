# Packages

library(dplyr)
library(data.table)
library(cluster)
library(dendextend)
library(ggplot2)
library(gridExtra)
library(forcats)
library(lmtest)
library(plm)
library(panelAR)
library(mclust)
library(fpc)
library(zoo)
library(imputeTS)
library(scales)
library(bit64)
library(psych)
library(effects)
library(corrplot)
library(rworldmap)

# for correct display of tibbles
options(crayon.enabled = FALSE)

# 

interface_diana = function(matrix, k) {
  diana_results = diana(matrix, k, diss=T)
  hc_classes = stats::cutree(diana_results, k)
  
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
    clustermethod="diana"
  )
  
  return(make_list)
}
