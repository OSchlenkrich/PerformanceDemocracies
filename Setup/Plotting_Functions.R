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
    mutate(rowNr = row(.)[,1])
  
  plot_sil_labels = plot_sil %>%
    group_by(cluster) %>%
    summarise_all(funs(median, mean)) %>%
    group_by(cluster) %>%
    mutate_all(funs(round(.,2)))
  
  if (is.null(labels_plot) == T) {
    labels_plot = seq(1, max(silhoutte_obj[,1]), 1)
  }
  
  ggplot(plot_sil, aes(x=rowNr, y=sil_width, fill=cluster)) + geom_bar(stat="identity", position="dodge", width=1) +
    scale_fill_discrete(name="Cluster:", labels = labels_plot) + 
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


plot_random_countries_dim_improved = function(complete_data_dimensions, No_countries) {
  
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
    melt(id.vars=c("country", "year"), measure.vars="cluster_label_1st") %>% 
    mutate(value = as.factor(value),
           value = fct_relevel(value, levels(dmx_trade_cluster$cluster_label_1st))) %>% 
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
          axis.ticks.y=element_blank()) + 
    xlab("") + 
    ylab("")
  return(p1)
}



create_world_map= function(dataset, selected_var, selected_year, label, mode=F) {
  if (mode == T) {  
    dmy_year = dataset %>% 
      filter(year>=1974) %>%
      select(country, variable = selected_var) %>%
      mutate(country = as.character(country)) %>% 
      group_by(country) %>% 
      summarise(variable = getmode(variable))
  } else {
    dmy_year = dataset %>% 
      filter(year == selected_year) %>%
      select(country, variable = selected_var) %>%
      mutate(country = as.character(country))
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
                             mapTitle = paste(label, selected_year),
                             missingCountryCol="lightgrey",
                             #mapRegion = "Europe"
  )
  # country_coord<-data.frame(coordinates(merged_map_data),stringsAsFactors=F)
  # # label the countries
  # text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord), cex = 0.6)
}
