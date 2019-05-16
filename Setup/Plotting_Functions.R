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
    select(freedom, equality, control, cluster) %>%
    melt(id.vars="cluster")
  
  ggplot(plotdata, aes(x=cluster, y=value, fill=variable)) + geom_boxplot()  + theme_bw() +
    ylim(0.25,1) + 
    stat_summary(fun.data = stat_box_count, geom = "text", hjust = 0.5, vjust = 0.9) +
    ylab("") + 
    xlab("Cluster")  + 
    scale_fill_discrete(name = "Dimensions", labels=c("Freedom", "Equality", "Control")) +
    ggtitle(paste("Boxplot ", algorithm, sep="")) + 
    theme(plot.title = element_text(hjust=0.5))
  
}


plot_silhoutte = function(silhoutte_obj, method) {
  plot_sil = data.frame(cluster = as.factor(silhoutte_obj[,1]), sil_width = silhoutte_obj[,3]) %>%
    arrange(cluster, sil_width) %>%
    mutate(rowNr = row(.)[,1])
  
  plot_sil_labels = plot_sil %>%
    group_by(cluster) %>%
    summarise_all(funs(median, mean)) %>%
    group_by(cluster) %>%
    mutate_all(funs(round(.,2)))
  
  ggplot(plot_sil, aes(x=rowNr, y=sil_width, fill=cluster)) + geom_bar(stat="identity", position="dodge", width=1) +
    scale_fill_discrete(name="Cluster:") + scale_y_continuous(limits = c(-1,1), breaks=seq(-1,1,0.25)) +
    geom_hline(aes(yintercept=mean(sil_width)), color="red", linetype=8, size= 1) +
    geom_hline(yintercept=0, color="black", size= 1) + 
    geom_hline(yintercept=0.25, color="black", linetype=8) +
    annotate("text", x = plot_sil_labels$rowNr_median, y = min(plot_sil_labels$sil_width_mean)*0.95, label = plot_sil_labels$sil_width_mean, size=4, fontface="bold",) + 
    ggtitle(paste("Silhouette-Plot ", "(", method, ")", sep=""), paste("Average Silhouette Width:",round(mean(plot_sil$sil_width),2), paste("(n=", max(plot_sil$rowNr), ")", sep=""))) + xlab("Observation") + ylab("Silhouette Width") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom", panel.background = element_blank(), panel.grid.major.x = element_line(color = "grey80"), panel.grid.major.y = element_line(color = "grey80"))  
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
  
  p1 = ggplot(plotted_country, aes(x=year, y=cluster_label)) + geom_point(size=4) + facet_wrap(country~.) + theme_bw() +
    scale_x_continuous(limits=c(1900, 2020), breaks=seq(1900, 2020, 20)) + theme(axis.text.x = element_text(angle=90)) + xlab("") + ylab("")
  return(p1)
}
