# Plotting Functions

# used by ggplot: N per cluster
stat_box_count <- function(y, upper_limit = 0.3) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n =', length(y), '\n')
    )
  )
}

boxplot_dim = function(dim_data, clustering, algorithm) {
  plotdata = dim_data %>%
    mutate(cluster = as.factor(clustering)) %>%
    dplyr::select(freedom, equality, control, cluster) %>%
    melt(id.vars="cluster")
  
  ggplot(plotdata, aes(x=cluster, y=value, fill=variable)) + geom_boxplot()  + theme_bw() +
    ylim(0.25,1) + 
    stat_summary(fun.data = stat_box_count, geom = "text", hjust = 0.5, vjust = 0.9, size=6) +
    ylab("") + 
    xlab("")  + 
    scale_fill_discrete(name = "Dimensions", labels=c("Freedom", "Equality", "Control")) +
    ggtitle(paste("Boxplot ", algorithm, sep="")) + 
    theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(size=12, face="bold"))
}


plot_silhoutte = function(silhoutte_obj, method, labels_plot = NULL) {
  plot_sil = data.frame(cluster = as.factor(silhoutte_obj[,1]), sil_width = silhoutte_obj[,3]) %>%
    arrange(cluster, sil_width) %>%
    mutate(rowNr = row(.)[,1])  %>% 
    ungroup() %>% 
    mutate(cluster = fct_recode(cluster, 
                                "fEC" = "1",
                                "fEc" = "2",
                                "FeC" = "3",
                                "Fec" = "4"),
           cluster = fct_relevel(cluster, "Fec", "fEc", "FeC", "fEC")
    )

    plot_sil_labels = plot_sil %>%
    group_by(cluster) %>%
    summarise_all(funs(median, mean)) %>%
    group_by(cluster) %>%
    mutate_all(funs(round(.,2)))
  
  if (is.null(labels_plot) == T) {
    labels_plot = seq(1, max(silhoutte_obj[,1]), 1)
  }
  
  ggplot(plot_sil, aes(x=rowNr, y=sil_width, fill=cluster)) + geom_bar(stat="identity", position="dodge", width=1) +
    scale_fill_brewer(name="Cluster:", type="qual", palette="Paired") + 
    scale_y_continuous(limits = c(-1,1), breaks=seq(-1,1,0.25)) +
    geom_hline(aes(yintercept=mean(sil_width)), color="red", linetype=8, size= 1) +
    geom_hline(yintercept=0, color="black", size= 1) + 
    geom_hline(yintercept=0.25, color="black", linetype=8) +
    annotate("text", x = plot_sil_labels$rowNr_median, y = min(plot_sil_labels$sil_width_mean)*0.95, label = plot_sil_labels$sil_width_mean, size=6, fontface="bold",) + 
    ggtitle(paste("Silhouette-Plot ", "(", method, ")", sep=""), paste("Average Silhouette Width:",round(mean(plot_sil$sil_width),2), paste("(n=", max(plot_sil$rowNr), ")", sep=""))) + xlab("Observation") + ylab("Silhouette Width") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, size=13), legend.position = "bottom", panel.background = element_blank(), panel.grid.major.x = element_line(color = "grey80"), panel.grid.major.y = element_line(color = "grey80"))  
}



plot_random_countries_dim = function(No_countries) {
  
  if (is.numeric(No_countries)==T) {
    random_countries = sample(n_distinct(dmx_trade_cluster$country), No_countries)
    selected_countries = unique(dmx_trade_cluster$country)[random_countries]
  } else {
    selected_countries = No_countries
  }
  
  plotted_country = dmx_trade_cluster %>%
    filter(country %in% selected_countries)
  
  p1 = ggplot(plotted_country, aes(x=year, y=cluster_label_1st)) + geom_point(size=4) + facet_wrap(country~.) + theme_bw() +
    scale_x_continuous(limits=c(1900, 2020), breaks=seq(1900, 2020, 10)) + theme(axis.text.x = element_text(angle=90)) + xlab("") + ylab("")
  return(p1)
}


plot_random_countries_dim_improved = function(complete_data_dimensions, No_countries, cluster_var) {
  
  if (is.numeric(No_countries)==T) {
    random_countries = sample(n_distinct(complete_data_dimensions$country), No_countries)
    selected_countries = unique(complete_data_dimensions$country)[random_countries]
  } else {
    selected_countries = No_countries
  }
  
  plotted_country = complete_data_dimensions %>%
    filter(country %in% selected_countries) %>%
    filter(year >= 1945) %>% 
    mutate(country = fct_relevel(country, No_countries)) %>% 
    select(country, year, cluster = cluster_var) %>% 
    pivot_longer(cols = -c("country", "year")) %>% 
    # mutate(value = as.factor(value),
    #        value = fct_relevel(value, levels(dmx_trade_cluster$FKMmed_6_cluster))) %>% 
    mutate(y=1)
  
  levels(plotted_country$country) <- gsub(" ", "\n", levels(plotted_country$country))

  p1 = ggplot(plotted_country, aes(x=year,  y=y, fill=value)) + geom_bar(width=1, stat="identity") + 
    facet_wrap(country~. , nrow=length(selected_countries), strip.position="left") + 
    theme_bw() +
    scale_fill_brewer(name="Cluster", type="qual", palette="Paired") +
    scale_y_continuous(breaks=c(0,1)) +
    scale_x_continuous(limits=c(1940, 2020), breaks=seq(1900, 2020, 10)) + 
    theme(axis.text.x = element_text(angle=90, size=10), strip.text = element_text(size=12, face="bold"),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = "bottom") + 
    xlab("") + 
    ylab("")
  return(p1)
}

plot_random_countries_mp_improved = function(complete_data_dimensions, No_countries, cluster_var) {
  
  if (is.numeric(No_countries)==T) {
    random_countries = sample(n_distinct(complete_data_dimensions$country), No_countries)
    selected_countries = unique(complete_data_dimensions$country)[random_countries]
  } else {
    selected_countries = No_countries
  }
  
  MPlabel = paste("mp_", cluster_var, sep="")
  
  plotted_country = complete_data_dimensions %>%
    filter(country %in% selected_countries) %>%
    filter(year >= 1945) %>% 
    mutate(country = fct_relevel(country, No_countries)) %>% 
    select_at(vars(country, year, starts_with(MPlabel))) %>% 
    pivot_longer(cols = -c("country", "year")) %>% 
    mutate(name = gsub(paste(MPlabel, "_", sep=""),"", name))
  
  levels(plotted_country$country) <- gsub(" ", "\n", levels(plotted_country$country))

  
  p1 =   ggplot(plotted_country, aes(x=year,  y=value, fill=name)) + 
    geom_area(stat="identity")  + 
    facet_wrap(country~. , nrow=length(selected_countries), strip.position="right") + 
    theme_bw() +
    scale_fill_brewer(name="Cluster", type="qual", palette="Paired") +
    scale_y_continuous(labels=percent) +
    scale_x_continuous(limits=c(1940, 2020), breaks=seq(1900, 2020, 10)) + 
    theme(axis.text.x = element_text(angle=90, size=10), strip.text = element_text(size=12, face="bold"),
          # axis.text.y = element_blank(),
          # axis.ticks.y=element_blank(),
          legend.position = "bottom") + 
    xlab("") + 
    ylab("")
  return(p1)
}



create_world_map= function(dataset, selected_var, selected_year, label, mode=F) {
  if (mode == T) {  
    dmy_year = dataset %>% 
      filter(year>=selected_year[1], year <= selected_year[2]) %>%
      select(country, variable = selected_var) %>%
      mutate(country = as.character(country)) %>% 
      group_by(country) %>% 
      summarise(variable = getmode(variable))
    
    year_label = paste(selected_year[1], "-", selected_year[2], sep="")
  } else {
    dmy_year = dataset %>% 
      filter(year == selected_year) %>%
      select(country, variable = selected_var) %>%
      mutate(country = as.character(country))
    
    year_label = selected_year
  }
  
  dmy_year$country[dmy_year$country=="Burma/Myanmar"] = "Burma"
  dmy_year$country[dmy_year$country=="Republic of Vietnam"] = "Vietnam"
  dmy_year$country[dmy_year$country=="São Tomé and Príncipe"] = "Sao Tome and Principe"
  
  merged_map_data <- joinCountryData2Map(dmy_year,
                                         joinCode = "NAME",
                                         nameJoinColumn = "country",
                                         verbose = TRUE)
  cnt = as.character(merged_map_data$NAME[merged_map_data$NAME != "Antarctica"])
  cnt = as.character(cnt[cnt != "Greenland"])
  merged_map_data <- subset(merged_map_data, NAME  %in%  cnt)
  
  library(RColorBrewer)
  
  colourPalette <- brewer.pal(length(levels(dmy_year$variable)), 'Paired')
  
  par(mai=c(2,0,0.6,0),xaxs="i",yaxs="i")
  mapParams = mapCountryData(merged_map_data, 
                             nameColumnToPlot="variable", 
                             colourPalette=colourPalette,
                             catMethod="categorical", 
                             addLegend = T,
                             borderCol= "black",
                             lwd=1, 
                             mapTitle = paste(label, year_label),
                             missingCountryCol="lightgrey",
                             #mapRegion = "Europe"
  )
  # country_coord<-data.frame(coordinates(merged_map_data),stringsAsFactors=F)
  # # label the countries
  # text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord), cex = 0.6)
}

# Plots all NAs from a dataset
NA_plot = function(data, name_data, var_selection = NULL) {
  data = data %>% 
    select_if(is.numeric) %>% 
    select(-year)
  
  if (is.null(var_selection) == F ){
    data = data %>% 
      select_at(vars(matches(var_selection)))
  }
  
  na_col = sapply(data, function(x) sum(is.na(x)))
  na_col_plot = na_col %>%
    melt() %>%
    mutate("variable" = rownames(.),
           variable= fct_reorder(variable, value, .desc = T)) 
  
  NA_plot = ggplot(na_col_plot, aes(x=variable, y=value)) + 
    geom_bar(stat="identity") + 
    theme_bw() + 
    ggtitle(paste(name_data, "- Missings for each Variable")) +
    theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5))
  
  return(NA_plot)
}

#* sd(pol_att_sub_dummy$age)
# Odds Ratio Plot

odds_ratio_plot = function(ref_model, sign_niveau=0.05, vars_sel = NULL) {
  
  nr_dep_vars = length(ref_model$varnames)
  ref_labels = gsub("mp_", "ref_", ref_model$varnames)
  
  diri_models = list()
  diri_models[[1]] = ref_model
  for (i in 2:nr_dep_vars) {
    new_formular = formula(paste(ref_labels[i], as.character(formula(ref_model))[3], sep ="~"))
    print(new_formular)

    diri_models[[i]] =  update(ref_model, formula=new_formular)
    
  }
  
  # diri_models = list(...)
  
  diri_model_ref = diri_models[[1]]
  nr_vars = diri_model_ref$n.vars[1]-1
  varnames = colnames(diri_model_ref$d)[-1] 
  
  make_Data = function(model) {
    sig = summary(model) 
    
    sd_data = as_tibble(model$d) %>% 
      pivot_longer(cols=matches("_caus"), names_to = "vars", values_to = "value") %>%
      select(vars, value) %>%  
      group_by(vars) %>% 
      summarise(sd_x = sd(value), nr = n_distinct(value)) %>% 
      ungroup() %>% 
      mutate(sd_x = ifelse(nr == 2, 1, sd_x)) %>% 
      select(-nr)
    

    mydata = data.frame(coefs = names(unlist(coef(model)$beta)), 
                        logits = unlist(coef(model)$beta),
                        row.names = NULL,
                        sig = sig$coef.mat[-dim(sig$coef.mat)[1],4])   %>% 
      mutate(vars = gsub(".*?\\.", "", coefs),
             cat = gsub("\\..*", "", coefs),
             sig = if_else(sig < sign_niveau, 1, 0)) %>% 
      filter(vars!="(Intercept)")  %>% 
      # SD
      left_join(sd_data, by="vars") %>% 
      mutate(logits =  logits * sd_x) %>% 
      select(-sd_x) %>% 
      pivot_longer(cols=logits, values_to = "logits") %>% 
      select(-coefs) %>% 
      rbind(data.frame(cat=colnames(model$Y)[1], 
                       vars = varnames, 
                       name="logits", logits=0,sig=0)) %>%
      mutate(y_jitter_gr = group_indices(., vars)) %>% 
      group_by(vars) %>% 
      mutate(y_jitter = rnorm(dim(model$Y)[2],y_jitter_gr,0.25)) %>% 
      ungroup()  %>% 
      mutate(y_jitter = ifelse(cat == colnames(model$Y)[1], y_jitter_gr, y_jitter))
    return(mydata)
  }
  
  mydata = make_Data(diri_model_ref)
  
  # Lines for insignficant results
  
  segment_data_plot = mydata %>% 
    group_by(vars) %>% 
    filter(sig!=1) %>% 
    mutate(x_end = logits[cat==colnames(diri_model_ref$Y)[1]],
           y_end = y_jitter[cat==colnames(diri_model_ref$Y)[1]])
  
  for (i in 2:length(diri_models)) {
    model = diri_models[[i]]
    
    segment_data_add = make_Data(model) %>% 
      select(sig, vars, cat) %>% 
      left_join(mydata %>% select(-sig), by=c("vars", "cat")) %>% 
      group_by(vars) %>%  
      mutate(x_end = logits[cat==colnames(model$Y)[1]],
             y_end = y_jitter[cat==colnames(model$Y)[1]]) %>% 
      filter(sig!=1)   
    
    segment_data_plot = segment_data_plot %>% 
      bind_rows(segment_data_add)
  }

  
  
  # Calculate Marginal Effects
  marg_eff = data.frame(array(NA, dim = c(length(colnames(diri_model_ref$Y)), nr_vars+1)))
  names(marg_eff)[1] = "cat"
  names(marg_eff)[-1] = varnames
  
  marg_eff[,1] = colnames(diri_model_ref$Y)
  
  for (i in 1:nr_vars) {
    make_dataset_sd = function(dataset1, dataset2) {
      dataset1 = dataset1
      dataset2 = dataset2 + sd(dataset2)
      return(list(dataset1, dataset2))
    }
    make_dataset_bin = function(dataset1, dataset2) {
      dataset1 = 0
      dataset2 = 1
      return(list(dataset1, dataset2))
    }
    
    dataset1 = dataset2 = diri_model_ref$d
    
    
    if (length(unique(dataset1[,varnames[i]])) > 2) {
      dataset1[,varnames[i]] = make_dataset_sd(dataset1[,varnames[i]], dataset2[,varnames[i]])[[1]]
      dataset2[,varnames[i]] = make_dataset_sd(dataset1[,varnames[i]], dataset2[,varnames[i]])[[2]]
    } else {
      dataset1[,varnames[i]] = make_dataset_bin(dataset1[,varnames[i]][[1]], dataset2[,varnames[i]][[1]])[[1]]
      dataset2[,varnames[i]] = make_dataset_bin(dataset1[,varnames[i]][[1]], dataset2[,varnames[i]][[1]])[[2]]
    }
    
    marg_eff[,varnames[i]] = colMeans((predict(diri_model_ref, newdata=dataset2) - predict(diri_model_ref, newdata=dataset1)))
    
  }
  
  marg_eff = marg_eff %>% 
    pivot_longer(cols=-cat, names_to = "vars", values_to = "marg_eff")
  
  
  # Plotting

  plot_data = mydata %>% 
    left_join(marg_eff, by=c("cat", "vars")) %>% 
    mutate(marg_eff_dir = if_else(marg_eff >= 0, "+","-"),
           cat = gsub("X_","",cat),
           cat = gsub("mp_Cluster4_","",cat),
           cat = gsub("mp_Cluster5_","",cat),
           #vars = gsub("_caus","",vars),
           cat = paste(cat, marg_eff_dir, sep="")) 

  if (is.null(vars_sel) == F) {
    plot_data = plot_data %>% 
      filter(vars %in% vars_sel) 
    segment_data_plot = segment_data_plot %>% 
      filter(vars %in% vars_sel)
  }
  
  var_labels = plot_data %>% 
    group_by(vars) %>% 
    dplyr::slice(1) %>% 
    pull(vars)
  print(plot_data)
  var_labels = gsub("_caus","",var_labels)
  
  if (length(var_labels) == 1) {
    plot_data %>% 
      ggplot(aes(x=logits, y=y_jitter, label=cat)) +
      geom_text(aes(size=abs(marg_eff))) +
      scale_x_continuous(name = "Logits", sec.axis = sec_axis(~ exp(.), breaks = seq(0,10,0.25), name="Odds-Ratios")) +
      scale_y_continuous(name=var_labels) +
      scale_size(range = c(3,5)) +
      theme_bw() +
      theme(legend.position = "none", axis.text.y = element_blank()) +
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_segment(data=segment_data_plot, aes(x=logits, y=y_jitter, xend=x_end, yend=y_end), alpha=0.5) +
      ggtitle(paste("Ref:", colnames(diri_model_ref$Y)[1]))
  } else {
    plot_data %>% 
      ggplot(aes(x=logits, y=y_jitter, label=cat)) +
      geom_text(aes(size=abs(marg_eff))) +
      scale_x_continuous(name = "Logits", sec.axis = sec_axis(~ exp(.), breaks = seq(0,10,0.25), name="Odds-Ratios")) +
      scale_y_continuous(name=NULL, breaks=seq(1,length(var_labels),1), labels=var_labels) +
      scale_size(range = c(3,5)) +
      theme_bw() +
      theme(legend.position = "none") +
      geom_hline(yintercept = seq(1,length(var_labels)-1,1) + 0.5) +
      geom_vline(xintercept = 0, linetype="dashed") +
      geom_segment(data=segment_data_plot, aes(x=logits, y=y_jitter, xend=x_end, yend=y_end), alpha=0.5) +
      ggtitle(paste("Ref:", colnames(diri_model_ref$Y)[1]))
  }
    
}
