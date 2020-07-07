# Base Functions

select <- dplyr::select

#VDem: Fill between elections
fill_elections = function(x, v2x_elecreg) {
  loadedvalue <- NA
  for (ii in 1:length(x)) {
    if (is.na(v2x_elecreg[ii]) == F) {
      if (v2x_elecreg[ii] == 1) {
        if (is.na(x[ii]) == F) {
          loadedvalue <- x[ii]
        }
        x[ii] <- loadedvalue      }
    }
  } 
  return(x)
}


# Missing Data Pattern Plot

missd_pattern = function(data) {
 col_NR = data %>% 
  dim()
 
 x_annotate = col_NR[2] + 0.6
 
 cols_n = data %>% 
  mutate_all(is.na) %>% 
  mutate_all(funs(if_else(. == T, 1,0))) %>%
  summarise_all(sum) %>% 
  melt()


 rows_n = data %>% 
   mutate_all(is.na) %>% 
   mutate_all(funs(if_else(. == T, 1,0))) %>%
   unite("sum_n", sep="_") %>% 
   group_by(sum_n) %>% 
   summarise(rows_n=n())
 
 rows_n_frame = data %>% 
   mutate_all(is.na) %>% 
   mutate_all(funs(if_else(. == T, 1,0))) %>%
   distinct() %>%
   unite("sum_n", sep="_", remove=T) %>%
   mutate(id = 1:dim(.)[1]) %>% 
   left_join(rows_n, by="sum_n") %>%
   separate(sum_n, into=paste("C_Miss",c(1:col_NR[2]), sep="_"), sep="_", convert=T) %>% 
   mutate(missing_vars = rowSums(select(., starts_with("C_Miss"))),
          id = as.factor(id),
          id = fct_reorder(id, -missing_vars)) %>% 
   dplyr::select(id, rows_n, missing_vars) %>% 
   arrange(id)
 
 miss_plot = data %>% 
   mutate_all(is.na) %>%
   distinct() %>% 
   mutate_all(funs(if_else(. == T, "missing","observed"))) %>%
   mutate(id = 1:dim(.)[1]) %>% 
   mutate(id = as.factor(id)) %>% 
   left_join(rows_n_frame, by="id") %>% 
   mutate(id = fct_reorder(id, -missing_vars)) %>% 
   dplyr::select(-missing_vars) %>% 
   melt(id.vars=c("rows_n", "id")) %>% 
   mutate(variable = as.factor(variable),
          variable = fct_reorder(variable, value)) %>% 
   ggplot(aes(x = variable,
              y = id,
              fill = value)) +
   geom_tile(color="black") +
   scale_x_discrete(position = "top", name=NULL, expand = c(0.05,0.5)) +
   scale_y_discrete(expand = c(0.1,0), labels = rows_n_frame$rows_n) +
   ylab("No. of Missings") +
   theme_bw() +
   theme(axis.text.x = element_text(angle=90), legend.position = "right") +
   annotate("text", x = cols_n$variable, label= cols_n$value, y=0.2) +
   annotate("text", x = x_annotate, label= rows_n_frame$missing_vars, y=rows_n_frame$id) 
 
 return(miss_plot)
}

# SGI
IQR_min_fun = function(x) {
  iqrange = IQR(x, na.rm=T)
  minimum = quantile(x, 0.25, na.rm=T) - 1.5*iqrange
  
  minimum_0 = ifelse(minimum < 0, 0, minimum)
  return(minimum_0)
}

IQR_max_fun = function(x) {
  iqrange = IQR(x, na.rm=T)
  maximum = quantile(x, 0.75, na.rm=T) + 1.5*iqrange
  return(maximum)
}

SGI_fun = function(x) {
  minimum = IQR_min_fun(x)
  maximum = IQR_max_fun(x)
  scale = maximum - minimum
  
  y = ifelse(x > maximum, 10,
             ifelse(x < minimum, 1, 1 + ((x - minimum)/scale)*9))
  
  return(y)
}

log10_fun = function(x) {
  log(x)
}
# reverse

reverse_sgi_fun = function(x) {
  x = 11-x
}

transformTukeyown = function (x, start = -10, end = 10, int = 0.025, plotit = TRUE, 
                              verbose = FALSE, quiet = FALSE, statistic = 1, returnLambda = FALSE) 
{
  library(nortest)
  
  n = (end - start)/int
  lambda = as.numeric(rep(0, n))
  W = as.numeric(rep(0, n))
  Shapiro.p.value = as.numeric(rep(0, n))
  if (statistic == 2) {
    A = as.numeric(rep(1000, n))
    Anderson.p.value = as.numeric(rep(0, n))
  }
  

  for (i in (1:n)) {
    lambda[i] = signif(start + (i - 1) * int, digits = 4)
    if (lambda[i] > 0) {
      TRANS = x^lambda[i]
    }
    if (lambda[i] == 0) {
      TRANS = log(x)
    }
    if (lambda[i] < 0) {
      TRANS = -1 * x^lambda[i]
    }
    W[i] = NA
    if (statistic == 2) {
      A[i] = NA
    }
    if (any(is.infinite(TRANS)) == FALSE & any(is.nan(TRANS)) == 
        FALSE) {
    # I disabled shapiro.test due to sample limitations (only 5000)  
    
      
      # W[i] = signif(shapiro.test(TRANS)$statistic, digits = 4)
      # Shapiro.p.value[i] = signif(shapiro.test(TRANS)$p.value, 
      #                             digits = 4)
      if (statistic == 2) {
        A[i] = signif(ad.test(TRANS)$statistic, digits = 4)
        Anderson.p.value[i] = signif(ad.test(TRANS)$p.value, 
                                     digits = 4)
      }
    }
  }
  if (statistic == 2) {
    df = data.frame(lambda, W, Shapiro.p.value, A, Anderson.p.value)
  }
  if (statistic == 1) {
    df = data.frame(lambda, W, Shapiro.p.value)
  }
  if (verbose == TRUE) {
    print(df)
  }
  if (plotit == TRUE) {
    if (statistic == 1) {
      plot(lambda, W, col = "black")
    }
    if (statistic == 2) {
      plot(lambda, A, col = "blue")
    }
  }
  if (statistic == 1) {
    df2 = df[with(df, order(-W)), ]
  }
  if (statistic == 2) {
    df2 = df[with(df, order(A)), ]
  }
  if (quiet == FALSE) {
    cat("\n")
    print(df2[1, ])
    cat("\n")
    cat("if (lambda >  0){TRANS = x ^ lambda}", "\n")
    cat("if (lambda == 0){TRANS = log(x)}", "\n")
    cat("if (lambda <  0){TRANS = -1 * x ^ lambda}", "\n")
    cat("\n")
  }
  lambda = df2[1, "lambda"]
  if (lambda > 0) {
    TRANS = x^lambda
  }
  if (lambda == 0) {
    TRANS = log(x)
  }
  if (lambda < 0) {
    TRANS = -1 * x^lambda
  }
  if (plotit == TRUE) {
    plotNormalHistogram(TRANS, xlab = "Transformed variable", 
                        linecol = "red", col = "lightgray")
  }
  if (plotit == TRUE) {
    qqnorm(TRANS)
    qqline(TRANS, col = "red")
  }
  if (returnLambda == FALSE) {
    return(TRANS)
  }
  if (returnLambda == TRUE) {
    names(lambda) = "lambda"
    return(lambda)
  }
}



ladder_fun = function(x) {
  library(rcompanion)
  
  print(paste("ratio: ", max(x, na.rm=T)/min(x, na.rm=T)))
  print(paste(">5: ", (max(x, na.rm=T)/min(x, na.rm=T)) > 5 ))
  print(paste(round(min(x, na.rm=T),3)," ", round(max(x, na.rm=T),3)))
  
  minimum = min(x, na.rm=T)
  constant = abs(minimum) 
  
  if (minimum >= 0) {
    x_aligned = x - constant + 1
  } else {
    x_aligned = x + constant + 1
  }

  y = transformTukeyown(x_aligned, plotit=F, start=-2, end=2, statistic = 2)
  return(y)
}


folded_ladder_fun = function(x, plotting = F) {
  library(nortest)
  
  f_fun <- function(x, lambda) (x^lambda - (1-x)^lambda)
  
  print(paste("ratio: ", max(x, na.rm=T)/min(x, na.rm=T)))
  print(paste("0:", max(x, na.rm=T) == 0, min(x, na.rm=T) == 0))
  print(paste("1:", max(x, na.rm=T) == 1, min(x, na.rm=T) == 1))
  print(paste(round(min(x, na.rm=T),3)," ", round(max(x, na.rm=T),3)))

  
  # no 0 and 1 are allowed
  if (min(x, na.rm=T) == 0 | max(x, na.rm=T) == 1) {
    x = 0.005 + 0.99 * x
  }

  nr_iterations = 1/0.025 + 1
  
  my_results_frame = data.frame(matrix(NA, nr_iterations, 3)) %>% 
    dplyr::rename(lambda = X1, ad.value = X2, ad.p.value = X3)
  
  # special: lambda=0 is logit
  results = log(x) - log(1-x)
  #sh_test_W = stats::shapiro.test(results)
  ad_test_W = ad.test(results)
  
  my_results_frame[1,] = c(0, ad_test_W$statistic, round(ad_test_W$p.value, 3))
  
  lambda = 0.025

  for (i in 2:nr_iterations) {
    results = f_fun(x, lambda)
    #sh_test_W = shapiro.test(results)
    ad_test_W = ad.test(results)
    
    my_results_frame[i,] = c(lambda, ad_test_W$statistic, round(ad_test_W$p.value, 3))
    lambda = lambda + 0.025
  }
  
  best_lambda = my_results_frame %>% 
    top_n(n=1, wt=-ad.value) %>% 
    mutate(ad.value = round(ad.value, 3))
  
  print(best_lambda)
  
  if (best_lambda$lambda == 0) {
    y = log(x) - log(1-x)
  } else {
    y = f_fun(x, best_lambda$lambda)
  }
  
  if (plotting == T) {
    qqnorm(y)
    qqline(y)
  }
  
  return(y)
}


na_interpol2 = function(x,maxgap = 2) {
  if (length(na.omit(x)) >= 2) {
    y = na_interpolation(x,  option = "linear", maxgap = maxgap)
    return(y)
  } else {
    return(x) 
  }
}

# Quantile 25 and 75
fun_quantile25 = function(x, na.rm=T) {
  quantile(x, 0.25, na.rm=T)
}
fun_quantile75 = function(x, na.rm=T) {
  quantile(x, 0.75, na.rm=T)
}
fun_quantile10 = function(x, na.rm=T) {
  quantile(x, 0.10, na.rm=T)
}
fun_quantile90 = function(x, na.rm=T) {
  quantile(x, 0.90, na.rm=T)
}


# EPI

EPI_fun = function(x, lower = 0.025, upper=0.975) {
  best = quantile(x, upper, na.rm=T)
  minimum = quantile(x, lower, na.rm=T)
  
  x = if_else(x >= best, 100, 
              if_else(x <= minimum, 0, ((x - minimum)/(best-minimum)) * 100))
  return(x)
}



# Trimming


trim = function(x,prop=.05, minimum=F, only=F) {
  max_trimmed_value = which(x < quantile(x,prob=(1-prop), na.rm=T))
  max_end = max(x[max_trimmed_value], na.rm=T)

  
  totrim_max = which(x >= quantile(x,prob=(1-prop), na.rm=T))

  if (minimum == T) {
    min_trimmed_value = which(x > quantile(x,prob=prop, na.rm=T))
    min_end = min(x[min_trimmed_value], na.rm=T)

    
    totrim_min = which(x <= quantile(x,prob=prop, na.rm=T))
    x[totrim_min] = min_end
    #x[totrim_min] = NA
    
  } 
  
  if (only == F) {
    x[totrim_max] = max_end
    #x[totrim_max] = NA
  }
  
  return(x)
}

# Inverse

inverser = function(x) {
  x = x *-1
  return(x)
}




# interpolation
na_interpol = function(x, max_gap = 2) {
  if (length(na.omit(x)) >= 2) {

    pointer = length(x)
    flag = F
    while (flag == F) {
      if (is.na(x[pointer] == T)) {
        pointer = pointer - 1
      } else {
        flag = T
      }
    }
    
    y = na_interpolation(x,  option = "linear", maxgap = max_gap)
    if (pointer < length(x)) {
      y[(pointer+1):length(x)] = NA
    } 
    
    return(y)
  } else {
    return(x) 
  }
}

# Percentage Missings
pMiss <- function(x){sum(is.na(x))/length(x)*100}
pMiss_01 <- function(x){sum(is.na(x))/length(x)}
pMiss_Abs <- function(x){sum(is.na(x))}


# Function for Mode
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# unit root test
purtest_function = function(dataset, variable, lags, exo) {
  print(variable)
  for (i in 1:length(unique(dataset$country))) {
    dataset2check = dataset %>% 
      filter(country == unique(dataset$country)[i]) %>% 
      select(country, year_id, var2check = variable)
    
    dataset_plm <- pdata.frame(data.frame(dataset2check), index=c("country", "year_id"))
    
    parameter = round(purtest(dataset_plm$var2check, test="hadri", exo=exo, lags = lags)$statistic$p.value, 2)
    
    print(paste(unique(dataset$country)[i], 
                parameter)
    )
    
  }
}

# opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))


# detrending function
detrending = function(dataset, year, variable){
  frame = data.frame(country="test", year =as.factor(year))
  
  if (length(na.omit(dataset))!=0 ) {
    dataset = data.frame(detrend_var = dataset,
                         year = year,
                         country = "test") 
    dataset = pdata.frame(dataset, index=c("country", "year"))
    
    detrend_result = plm(detrend_var ~ as.numeric(year) + I(as.numeric(year)^2) , 
                         dataset, model="pooling")
    
    results = data.frame(year=index(detrend_result)[,2], resid = detrend_result$residuals) %>% 
      full_join(frame, by="year")
    return(results$resid)
  } else {
    return(NA)
  }
  
}



# Summary Table to Excel


results_to_excel = function(reg_obj, filename, PCSE = F) {
  if (PCSE == F) {
    print("No PCSEs")
    results_df = data.frame(varnames = rownames(round(summary(reg_obj)$coefficients, 3)),
                            round(summary(reg_obj)$coefficients, 3))
  } else {
    coef_obj = data.frame(as.matrix.data.frame(round(coeftest(re_mod_cluster_1st, vcov=vcovBK),3))) %>% 
      rename("Estimate" = 1,
             "Std..Error"  = 2,
             "t.value"  = 3,
             "Pr...t.." = 4)
    
    
    results_df = data.frame(varnames = rownames(round(summary(reg_obj)$coefficients, 3)),
                            coef_obj)
                            
  }
  results_df = results_df %>% 
    rename(prb = 5) %>% 
    mutate(Std.Er = paste("(", Std..Error, ")", sep="")) %>%  # xx must be removed in Excel
    mutate(Sign = if_else(prb < 0.001, "***", 
                          if_else(prb < 0.01, "**", 
                                  if_else(prb < 0.05, "*", " ")))) %>%
    mutate(Estimate = paste(Estimate, Sign, sep="")) %>% 
    mutate(Estimate = paste(Estimate, Std.Er, sep=" ")) %>% 
    dplyr::select(varnames, Estimate) 
  
  file = paste("Results/", filename, ".csv", sep="")
  write.csv2(results_df, file=file, row.names = T)
}


# Scaling
scale_this = function(x) {
  x_scaled = (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
  return(x_scaled)
}

# to calculate logratios > 0
zeroadjuster = function(x) {
  x = ifelse(x==0, 0.005, x) 
  return(x)
}

# First Difference
first_DF = function(x) {
  return(x - dplyr::lag(x,1))
}


# XY Plots
xyplot = function(dataset, dependent_vars_label, deselection_independent = NULL) {
  
  dependent_vars = dataset %>% 
    select_at(vars(dependent_vars_label)) 
  
  if (is.null(deselection_independent) == T) {
    independent_vars = dataset %>% 
      select_at(vars(ends_with("_ctl"), starts_with("FKM"), ends_with("_odempr"))) 
    
  } else {
    independent_vars = dataset %>% 
      select_at(vars(ends_with("_ctl"), starts_with("FKM"), ends_with("_odempr"), -deselection_independent)) 
  }
  
  plotlist = list()
  for (i in 1:dim(independent_vars)[2]) {
    xlabel = colnames(independent_vars)[i]
    xlabel = gsub("_num_ctl","", xlabel)
    xlabel = gsub("_pr_ctl","", xlabel)
    xlabel = gsub("_ctl","", xlabel)
    
    plotlist[[i]] = independent_vars %>%
      select(sel_var = i) %>%
      bind_cols(dependent_vars) %>%
      pivot_longer(cols = -sel_var) %>%
      ggplot(aes(x=sel_var, y=value, col=name)) +
      geom_point(col="black", alpha=0.25) +
      geom_smooth(se=F, size=1.1) +
      stat_cor(aes(method = "pearson", label = ..r.label..)) + 
      xlab(xlabel) +
      ylab("") +
      theme_bw() +
      theme(legend.position = "none")
    

  }
  
  return(ggarrange(plotlist=plotlist, common.legend = F) %>% 
           annotate_figure(top = text_grob(dependent_vars_label)))
}


# Make Data for TSCS Regression
make_reg_data = function(imputatedData, DV_label, naframe, vars_noimput) {
  
  vars_noimput_naid = paste(vars_noimput, "_is_na", sep="")

  data_list = list()
  
  for (i in 1:10) {
    performance_data_nas = imputatedData$imputations[[i]]
    
    for (k in 1:length(vars_noimput)) {
      performance_data_nas = performance_data_nas %>% 
        bind_cols(naframe) %>% 
        rename(LHS = vars_noimput[k], RHS_na = vars_noimput_naid[k]) %>% 
        mutate(LHS = ifelse(RHS_na == 1, NA, LHS)) %>% 
        rename(!!vars_noimput[k] := LHS) %>% 
        select_at(vars(-RHS_na, -ends_with("is_na")))
    }

    # fill in gaps for correct lags
    performance_data = performance_data_nas %>% 
      select_at(vars(-matches("lag"),-matches("lead"),-matches("_mb_"))) %>% 
      # rename DV
      rename(DV = DV_label) %>% 
      
      group_by(country_text_id) %>% 
      tidyr::complete(country_text_id, year_0 = min(year_0):max(year_0), fill = list(NA)) %>% 
      ungroup()
    
    # LDVs and FD
    performance_data_LDV = performance_data %>% 
      group_by(country_text_id) %>% 
      mutate(
        # ADL
        DV_lag = dplyr::lag(DV, 1),
        #productivity_eco_lag = dplyr::lag(productivity_eco, 1),
        
        # ECM
        DV_df = first_DF(DV),
        #productivity_eco_df = first_DF(productivity_eco),
        
      )  %>% 
      # Within Effect of LDV
      mutate_at(vars(matches("lag")), funs(DV_wi_lag = . - mean(., na.rm=T))) %>%
      ungroup() 
    
    # Control Variables
    performance_data_LDV_ctl = performance_data_LDV %>% 
      group_by(country_text_id) %>%
      
      # within effect
      mutate_at(vars(ends_with("num_ctl"), ends_with("cat_ctl"), ends_with("pr_ctl")), funs(wi = . - mean(., na.rm=T))) %>%
      mutate_at(vars(ends_with("num_ctl_wi"), ends_with("cat_ctl_wi"), ends_with("pr_ctl_wi")), funs(lag = dplyr::lag(.,1))) %>% 
      
      mutate_at(vars(ends_with("num_ctl"), ends_with("cat_ctl"), ends_with("pr_ctl")), funs(df = first_DF(.)))  %>%
      mutate_at(vars(ends_with("num_ctl_wi"), ends_with("cat_ctl_wi"), ends_with("pr_ctl_wi")), funs(df = first_DF(.)))  %>%
      
      
      #between effect
      mutate_at(vars(ends_with("num_ctl"), ends_with("cat_ctl"), ends_with("pr_ctl")), funs(bw = mean(., na.rm=T))) %>% 
      ungroup()
    
    # Democracy Profiles
    performance_data_LDV_ctl_dp = performance_data_LDV_ctl %>% 
      group_by(country_text_id) %>%
      # within effect
      mutate_at(vars(starts_with("FKM"), matches("centrip_odempr")), funs(df = first_DF(.))) %>%
      mutate_at(vars(starts_with("FKM"), matches("centrip_odempr")), funs(wi = . - mean(., na.rm=T))) %>%
      mutate_at(vars(matches("FKM"), matches("centrip_odempr"), -matches("df")), funs(lag = dplyr::lag(.,1))) %>%
      
      #between effect
      mutate_at(vars(starts_with("FKM"), centrip_odempr, -matches("wi"), -matches("df"), -matches("lag")), funs(bw = mean(., na.rm=T))) %>% 
      ungroup()
    
    performance_data_LDV_ctl_dp_trend = performance_data_LDV_ctl_dp %>% 
      mutate(trend = year_0 - median(year_0, na.rm=T))
    
    data_list[[i]] = performance_data_LDV_ctl_dp_trend
    colnames(data_list[[i]]) = gsub("DV", DV_label, colnames(data_list[[i]]))
    
  }
  
  return(data_list)
  
}
