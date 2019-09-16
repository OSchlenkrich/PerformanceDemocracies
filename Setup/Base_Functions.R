# Base Functions

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

na_interpol2 = function(x,maxgap) {
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


# EPI

EPI_fun = function(x) {
  best = quantile(x, 0.975, na.rm=T)
  minimum = quantile(x, 0.025, na.rm=T)
  
  x = if_else(x >= best, 100, 
              if_else(x <= minimum, 0, ((x - minimum)/(best-minimum)) * 100))
  return(x)
}



# Trimming
trim = function(x,prop=.05,minimum=F) {
  max_trimmed_value = which(x < quantile(x,prob=(1-prop), na.rm=T))
  max_end = max(x[max_trimmed_value], na.rm=T)

  
  totrim_max = which(x >= quantile(x,prob=(1-prop), na.rm=T))

  if (minimum == T) {
    min_trimmed_value = which(x > quantile(x,prob=prop, na.rm=T))
    min_end = min(x[min_trimmed_value], na.rm=T)

    
    totrim_min = which(x <= quantile(x,prob=prop, na.rm=T))
    x[totrim_min] = min_end
    x[totrim_min] = NA
    
  } 
  x[totrim_max] = max_end
  x[totrim_max] = NA
  
  return(x)
}

# Inverse

inverser = function(x) {
  x = x *-1
  return(x)
}




# interpolation
x = c(1,2,4,5,6)
na_interpol = function(x) {
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
    
    y = na_interpolation(x,  option = "linear", maxgap = 5)
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

