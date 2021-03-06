# Packages
library(paran)
library(dplyr)
library(data.table)
library(cluster)
library(dendextend)
library(ggplot2)
library(gridExtra)
library(forcats)
library(lmtest)
library(plm)
#library(panelAR)
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
library(missMDA)
library(describedata)
library(tidyr)
library(rcompanion)
library(mice)
library(miceadds)
library(Amelia)
library(haven)
library(umx)
library(margins)
library(DirichletReg)
library(betareg)
library(ggrepel)
library(semPlot)
library(pixiedust)
library(ggpubr)
library(tidybayes)
library(brms)
library(easyCODA)
library(broom)
library(rstan)

# for correct display of tibbles
options(crayon.enabled = FALSE)

# STAN and BRMS parallel
options(mc.cores = parallel::detectCores())

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
