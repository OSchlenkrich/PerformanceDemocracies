source("Analyse/CreateDatasets.R")

# Setup ####
OECD_member = fread("Datasets/OECD_member.csv") %>% 
  left_join(V_dem %>%  select(country_name = country, country_text_id) %>%  distinct(), by="country_name") %>% 
  select(country_text_id, OECD, OECD_founder)


# Create Table Functions
coef_variation = function(x){
  if (mean(x, na.rm=T) > 0) {
    100*(sd(x, na.rm=T)/mean(x, na.rm=T))
  } else {
    print("Error: Values are smaller than 0!")
  }
}


calculate_trend = function(variables) {

  frame_coef = performance_all %>%
    select(country_text_id, year,  variables) %>% 
    na.omit() %>% 
    distinct(country_text_id) 
  
  for (i in 1:length(variables)){
    frame_coef = frame_coef %>% 
      cbind(data.frame("trend"=NA, coefV = NA))
    # colnames(frame_coef)[i+1] = paste("trend", variables[i], sep="_")
  }
  
  for (i in 1:length(variables)){
    for (n in 1:dim(frame_coef)[1]) {
      performance_lm = performance_all %>% 
        filter(country_text_id == frame_coef$country_text_id[n]) %>%
        select(country_text_id, year,  sel_variable = variables[i]) 
      
      #performance_lm$year = scale(performance_lm$year)[,1]
      
      m1_lm = lm(sel_variable ~ year, performance_lm)
      
     if (is.na(coef(m1_lm)[2]) == F) {
      frame_coef$trend[n] = round(coef(m1_lm)[2],2)
      frame_coef$coefV[n] =  round(coef_variation(performance_lm$sel_variable),1)
      #frame_coef[ paste("trend", variables[i], sep="_")][n,] = round(coef(m1_lm)[2],2)
     } else {
       frame_coef$trend[n] = NA
       frame_coef$coefV[n] = NA
     }

      
    }
  }
  return(frame_coef)
}

get_table = function(variable) {
  table_complete = performance_all %>%
    select(country_text_id, year,  sel_variable = variable) %>% 
    na.omit() %>% 
    mutate(year5 = floor(year/5)*5) %>% 
    group_by(country_text_id, year5) %>% 
    summarise(mean_performance = mean(sel_variable, na.rm=T)) %>% 
    mutate(mean_performance = round(mean_performance, 1)) %>%
    arrange(year5) %>% 
    pivot_wider(names_from = year5, values_from = mean_performance) 
  
  
  return(table_complete)
}



get_totalvalue_table = function(variable) {
  table_data = performance_all %>%
    select(country_text_id, year,  sel_variable = variable) %>% 
    na.omit() %>% 
    filter(year >= 1950) 
  
  mtotal_lm = lm(sel_variable ~ year, table_data)
  coefV =  round(coef_variation(table_data$sel_variable),1)
  
  table_data_mean = table_data %>% 
    mutate(country_text_id = "All countries", year5 = floor(year/5)*5 ) %>% 
    group_by(country_text_id, year5) %>% 
    summarise_at(vars(sel_variable), funs(mean(., na.rm=T))) %>% 
    mutate_at(vars(sel_variable), funs(round(., 2)))  %>% 
    pivot_wider(names_from = year5, values_from = sel_variable)  %>% 
    mutate(trend = round(coef(mtotal_lm)[2],2))%>% 
    mutate(coefV = coefV)
  
  table_data_var = table_data %>% 
    mutate(country_text_id = "All countries \n Variation", year5 = floor(year/5)*5 ) %>% 
    group_by(country_text_id, year5) %>% 
    summarise_at(vars(sel_variable), funs(coef_variation(.))) %>% 
    mutate_at(vars(sel_variable), funs(round(., 1)))  %>% 
    pivot_wider(names_from = year5, values_from = sel_variable)  %>% 
    mutate(trend = NA) %>% 
    mutate(coefV = NA)
  
  
  return(rbind(table_data_mean, table_data_var))
}

get_OECDvalue_table = function(variable) {
  table_data = performance_all %>%
    select(country_text_id, year,  sel_variable = variable) %>% 
    left_join(OECD_member, by="country_text_id") %>% 
    filter(OECD_founder == 1) %>% 
    na.omit() %>% 
    filter(year >= 1950) 
  
  mtotal_lm = lm(sel_variable ~ year, table_data)
  coefV =  round(coef_variation(table_data$sel_variable),1)
  
  table_data_mean = table_data %>% 
    mutate(country_text_id = "OECD Founders", year5 = floor(year/5)*5 ) %>% 
    group_by(country_text_id, year5) %>% 
    summarise_at(vars(sel_variable), funs(mean(., na.rm=T))) %>% 
    mutate_at(vars(sel_variable), funs(round(., 2)))  %>% 
    pivot_wider(names_from = year5, values_from = sel_variable)  %>% 
    mutate(trend = round(coef(mtotal_lm)[2],2))%>% 
    mutate(coefV = coefV)
  
  table_data_var = table_data %>% 
    mutate(country_text_id = "OECD Founders \n Variation", year5 = floor(year/5)*5 ) %>% 
    group_by(country_text_id, year5) %>% 
    summarise_at(vars(sel_variable), funs(coef_variation(.))) %>% 
    mutate_at(vars(sel_variable), funs(round(., 1)))  %>% 
    pivot_wider(names_from = year5, values_from = sel_variable)  %>% 
    mutate(trend = NA)%>% 
    mutate(coefV = NA)
  
  
  return(rbind(table_data_mean, table_data_var))
}

make_dust_table = function(table_complete) {
  # Split Table for better page fit
  split1 = table_complete %>% 
    slice(1:ceiling(((dim(table_complete)[1])/2))) %>% 
    mutate(id = 1:dim(.)[1])
  split2 = table_complete %>% 
    anti_join(split1)  %>% 
    mutate(id = 1:dim(.)[1])
  
  wide_table_df = full_join(split1, split2, by="id") %>% 
    select(-id)
  
  colnames_df = split1 %>%  
    select(-id) %>%  
    rename(country = country_text_id) %>% 
    colnames()
  
  colnames_df = gsub("_.*", "", colnames_df)
  
  
  dust_table = dust(wide_table_df) 
  dust_table$head$value = colnames_df
  dust_table %>% 
    # border rows
    sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
    sprinkle(rows = 2:dim(wide_table_df)[1], border = "top", border_color = "black", border_thickness=1) %>%
    
    # border cols
    sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2) %>%
    sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
    
    sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, part="head") %>%
    sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, pad=5) %>%
    
    # thick line indicating split
    sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3) %>%
    sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
    
    # thick line after 2nd country
    sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2) %>%
    sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%

    # thick line after before "trend"
    sprinkle(col = dim(split1)[2]-3, border = "right", border_color = "black", border_thickness=2) %>%
    sprinkle(col = dim(split1)[2]-3, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
    sprinkle(col = dim(wide_table_df)[2]-2, border = "right", border_color = "black", border_thickness=2) %>%
    sprinkle(col = dim(wide_table_df)[2]-2, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
    
    # fontsize
    sprinkle(font_size = 9, font_size_units = "pt", part="head") %>%
    sprinkle(font_size = 8, font_size_units = "pt", part="body") %>%
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
  
}

development_plot = function(variable, title, OECD=F, position="top") {
  
  plot_data = performance_all %>%
    select_at(vars(country_text_id, year, selected_var = variable)) %>%
    na.omit() 
  
  if (OECD == T) {
    plot_data = plot_data %>% 
      left_join(OECD_member, by="country_text_id") %>%
      filter(OECD_founder == 1, country_text_id != "TUR") 
  }
  
  plot_data %>% 
    group_by(year) %>% 
    summarise(ymax = fun_quantile10(selected_var), ymean = median(selected_var, na.rm=T), ymin=fun_quantile90(selected_var)) %>% 
    ggplot(aes(x=year, y=ymean, ymin=ymin, ymax=ymax)) +
    geom_line(size=1) +
    geom_point() +
    geom_errorbar() +
    geom_rect(xmin = 2007, xmax=2008, ymin=100, ymax=0, alpha=0.01) +
    stat_cor(aes(label = ..r.label..), label.y.npc= position) +
    ylim(0,100) +
    xlab("") +
    #ylab(gsub(" \\(.*", "", title)) +
    ylab("Performance Index") +
    theme_bw() +
    ggtitle(title)
}


lineplot = function(variable, OECD=F, title = NULL, ownsample= NULL) {
  position = "none"
  add_plot = NULL
  plot_data = performance_all %>%
    select_at(vars(country_text_id, year, selected_var = variable)) %>%
    na.omit() 
  
  if (OECD == T) {
    plot_data = plot_data %>% 
      left_join(OECD_member, by="country_text_id") %>%
      filter(OECD_founder == 1, country_text_id != "TUR") 
  }
  
  if (is.null(ownsample) == F) {
    plot_data = plot_data %>%
      filter(country_text_id %in% ownsample) 
    position = "bottom"
    add_plot =  geom_point(aes(shape = country_text_id), size=2) 
  }
  
  plot_data %>% 
    group_by(year) %>% 
    ggplot(aes(x=year, y=selected_var, col=country_text_id )) +
    geom_line(size=1) +
    ylim(0,100) +
    xlab("") +
    ylab("Performance Index") +
    theme_bw() +
    ggtitle(title) +
    scale_color_grey(start = 0, end = .6) +
    theme(legend.position = position, legend.title = element_blank())  +
    add_plot
}

variationplot = function(variable, title = NULL, OECD=F) {
  plot_data = performance_all %>%
    select_at(vars(country_text_id, year, selected_var = variable)) %>%
    na.omit() 
  
  if (OECD == T) {
    plot_data = plot_data %>% 
      left_join(OECD_member, by="country_text_id") %>%
      filter(OECD_founder == 1, country_text_id != "TUR") 
  }
  plot_data %>% 
    group_by(year) %>%
    summarise(variation = coef_variation(selected_var)) %>% 
    ggplot(aes(x=year, y=variation)) +
    geom_line(size=1) +
    geom_point() +
    ylab("Coefficient of Variation in %") +
    xlab("") +
    ylim(0,100) +
    stat_cor(aes(label = ..r.label..)) +
    theme_bw() +
    ggtitle(title)
}

top_performer_table = function(variable) {
  get_table(variable) %>% 
    pivot_longer(cols = -country_text_id) %>% 
    group_by(name) %>% 
    arrange(name, -value) %>% 
    slice(1:5) %>%
    mutate(rank = 1:5) %>% 
    select(-value) %>% 
    pivot_wider(names_from = name, values_from = country_text_id) %>% 
    dust()  %>% 
    sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
    sprinkle(font_size = 12, font_size_units = "pt", part="head") %>%
    sprinkle(font_size = 11, font_size_units = "pt", part="body") %>%
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")  
}

create_world_map_cat= function(dataset, label = NULL, cat_label) {
  require(RColorBrewer)
  
  # dmy_year$country[dmy_year$country=="Burma/Myanmar"] = "Burma"
  # dmy_year$country[dmy_year$country=="Republic of Vietnam"] = "Vietnam"
  # dmy_year$country[dmy_year$country=="São Tomé and Príncipe"] = "Sao Tome and Principe"
  
  merged_map_data <- joinCountryData2Map(dataset,
                                         joinCode = "NAME",
                                         nameJoinColumn = "country",
                                         verbose = TRUE)
  
  
  
  cnt = as.character(merged_map_data$NAME[merged_map_data$NAME != "Antarctica"])
  cnt = as.character(cnt[cnt != "Greenland"])
  
  merged_map_data <- subset(merged_map_data, NAME  %in%  cnt)
  
  
  colourPalette <- c("grey", "black")
  
  if (is.null(label) == T) {
    mapParams = mapCountryData(merged_map_data,
                               nameColumnToPlot="value",
                               colourPalette=colourPalette,
                               catMethod="categorical", 
                               addLegend = T, 
                               #lwd=1,
                               mapTitle = "")    
  } else {
    mapParams = mapCountryData(merged_map_data,
                               nameColumnToPlot="value",
                               colourPalette=colourPalette,
                               catMethod="categorical", 
                               addLegend = T, 
                               #lwd=1,
                               mapTitle = label, selected_year)
  }
  
  
  do.call( addMapLegendBoxes, c(mapParams, title=cat_label))
  
}

create_world_map_cont= function(dataset, label = NULL) {
  require(RColorBrewer)
  
  
  merged_map_data <- joinCountryData2Map(dataset,
                                         joinCode = "NAME",
                                         nameJoinColumn = "country",
                                         verbose = TRUE)
  
  
  
  cnt = as.character(merged_map_data$NAME[merged_map_data$NAME != "Antarctica"])
  cnt = as.character(cnt[cnt != "Greenland"])
  
  merged_map_data <- subset(merged_map_data, NAME  %in%  cnt)
  
  colourPalette <- brewer.pal(10,'RdYlGn')
  
  mapParams = mapCountryData(merged_map_data,
                             nameColumnToPlot="variable",
                             colourPalette=colourPalette,
                             catMethod=seq(0,100,25), 
                             addLegend = F, 
                             lwd=1,
                             mapTitle = paste(label))
  do.call( addMapLegend, c(mapParams, legendWidth=0.5,legendMar = 4,horiz=T))
  
}



# NA values ####
test = performance_all %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_eco"), 
                 ends_with("_env"), 
                 ends_with("_ga"), 
                 ends_with("_soc"), 
                 ends_with("_ds"), 
                 ends_with("_pc"))) %>% 
  summarise_all(funs(1-pMiss_01(.)))

performance_all %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_eco"), 
                 ends_with("_env"), 
                 ends_with("_ga"), 
                 ends_with("_soc"), 
                 ends_with("_ds"), 
                 ends_with("_pc"))) %>% 
  summarise_all(funs(pMiss_01(.))) %>% 
  pivot_longer(cols=-year) %>% 
  ggplot(aes(x=year, y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(name~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")

# World Map
par(mfrow=c(2,2))

plot_Data = performance_all %>%
  filter(year > 2000) %>% 
  group_by(country) %>% 
  select_at(vars(ends_with("_eco"), 
                 ends_with("_env"), 
                 ends_with("_ga"), 
                 ends_with("_soc"), 
                 ends_with("_ds"), 
                 ends_with("_pc"))) %>% 
  summarise_all(funs(pMiss_01(.))) %>% 
  pivot_longer(cols=-country) %>% 
  #filter(name == "conf_pc") %>% 
  mutate(value = ifelse(value < 0.9, "yes", "no"))

for (i in 1:length(unique(plot_Data$name))) {
  varname = unique(plot_Data$name)[i]
  create_world_map_cat(plot_Data %>%  filter(name == varname), label = varname , cat_label = "Observed?")
}


# OECD sample
performance_all  %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  filter( OECD_founder == 1) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_eco"), 
                 ends_with("_env"), 
                 ends_with("_ga"), 
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
  ggtitle("Missings in Democracy Profile Sample (OECD founder)")


# ECONOMY ####
# Wealth ####
variable_eco = "wealth_eco_index"

# Ranking 
table_wealth = get_table(variable_eco) %>% 
  left_join(calculate_trend(variable_eco), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_eco)) %>% 
  bind_rows(get_OECDvalue_table(variable_eco))

make_dust_table(table_wealth)


# Top Performer
top_performer_table(variable_eco)
table_wealth %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>%  
  arrange(-trend) %>% 
  top_n(5, trend)
table_wealth %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(trend) %>% 
  top_n(-5, trend)

# Development

grid.arrange(development_plot(variable_eco, "Economic Performance - \nWealth (All Countries)"),
             development_plot(variable_eco, "Economic Performance - \nWealth (OECD Founder)", OECD=T),
             variationplot(variable_eco, "Economic Performance - \nWealth (All Countries)", OECD=F),
             variationplot(variable_eco, "Economic Performance - \nWealth (OECD Founder)", OECD=T)
             )
lineplot(variable_eco, OECD=F)
lineplot(variable_eco, OECD=T)


# Productivity ####
variable_prod_eco = "productivity_eco_index"

# Ranking 
table_prod = get_table(variable_prod_eco) %>% 
  left_join(calculate_trend(variable_prod_eco), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_prod_eco)) %>% 
  bind_rows(get_OECDvalue_table(variable_prod_eco))

make_dust_table(table_prod)

# Top Performer
top_performer_table(variable_prod_eco)
own_sample1 = table_prod %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(-trend) %>% 
  top_n(5, trend) %>% 
  pull(country_text_id)
own_sample2 = table_prod %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>%  
  arrange(trend) %>% 
  top_n(5, -trend) %>% 
  pull(country_text_id)

grid.arrange(lineplot(variable_prod_eco, OECD=F,  "Economic Performance - Productivity (Positive Trend)", own_sample1),
             lineplot(variable_prod_eco, OECD=F,  "Economic Performance - Productivity (Negative Trend)", own_sample2))


# Development
grid.arrange(development_plot(variable_prod_eco, "Economic Performance - \nProductivity (All Countries)", position="bottom"),
             development_plot(variable_prod_eco, "Economic Performance - \nProductivity (OECD Founder)", OECD=T, "bottom"),
             variationplot(variable_prod_eco, "Economic Performance - \nProductivity (All Countries)", OECD=F),
             variationplot(variable_prod_eco, "Economic Performance - \nProductivity (OECD Founder)", OECD=T)
)
lineplot(variable_prod_eco, OECD=F)
lineplot(variable_prod_eco, OECD=T)



# ENVIRONMENT ####
# Air ####
variable_air_env = "air_env_index"

# Ranking 
table_air = get_table(variable_air_env) %>% 
  left_join(calculate_trend(variable_air_env), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_air_env)) %>% 
  bind_rows(get_OECDvalue_table(variable_air_env))

make_dust_table(table_air)

# Top Performer
top_performer_table(variable_air_env)

table_air %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(-trend) %>% 
  top_n(5, trend)


# Development
grid.arrange(development_plot(variable_air_env, "Environmental Performance - \nAir Quality (All Countries)", position="top"),
             development_plot(variable_air_env, "Environmental Performance - \nAir Quality (OECD Founder)", OECD=T, "top"),
             variationplot(variable_air_env, "Environmental Performance - \nAir Quality (All Countries)", OECD=F),
             variationplot(variable_air_env, "Environmental Performance - \nAir Quality (OECD Founder)", OECD=T)
)

lineplot(variable_air_env, OECD=F)
lineplot(variable_air_env, OECD=T)


# Abstraction ####
variable_abs_env = "resources_env_index"

# Ranking 
table_abs = get_table(variable_abs_env) %>% 
  left_join(calculate_trend(variable_abs_env), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_abs_env)) %>% 
  bind_rows(get_OECDvalue_table(variable_abs_env))

make_dust_table(table_abs)

# Top Performer
top_performer_table(variable_abs_env)
table_abs %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(-trend) %>% 
  top_n(5, trend)
table_abs %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(trend) %>% 
  top_n(5, -trend)


# Development
grid.arrange(development_plot(variable_abs_env, "Environmental Performance - \nResources (All Countries)", position="top"),
             development_plot(variable_abs_env, "Environmental Performance - \nResources (OECD Founder)", OECD=T, "top"),
             variationplot(variable_abs_env, "Environmental Performance - \nResources (All Countries)", OECD=F),
             variationplot(variable_abs_env, "Environmental Performance - \nResources (OECD Founder)", OECD=T)
)
lineplot(variable_abs_env, OECD=F)
lineplot(variable_abs_env, OECD=T)

# GOAL-ATTAINMENT ####
# Amendmend-Rate

performance_all %>% 
  select_at(vars(country_text_id, year, matches("_ga_index"))) %>% 
  filter(year > 1990, is.na(arate_ccp_ga_index) == F) %>% 
  select(-year) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  group_by(country_text_id) %>% 
  mutate_all(funs(ifelse(is.nan(.) == T, NA, .))) %>% 
  mutate_all(funs(round(., 1))) %>% 
  ungroup() %>% 
  make_dust_table() %>% 
  sprinkle_colnames("country", "AR CCP", "AR Lutz")


performance_all %>% 
  select_at(vars(country_text_id, year, matches("_ga"))) %>% 
  #filter(year > 1990) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  mutate(country_text_id = fct_reorder2(country_text_id, arate_lutz_ga, arate_ccp_ga)) %>% 
  pivot_longer(cols=c("arate_ccp_ga", "arate_lutz_ga")) %>% 
  na.omit() %>% 
  ggplot(aes(x=country_text_id, y=value, shape=name)) +
  geom_point(size=2) +
  xlab("") +
  ylab("Amendment Rate") +
  #facet_wrap(name ~ .) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

performance_all %>% 
  select_at(vars(country_text_id, year, matches("_ga"))) %>% 
  #filter(year > 1990) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(country_text_id, arate_lutz_ga) %>% 
  arrange(-arate_lutz_ga) %>% 
  top_n(5, arate_lutz_ga)

performance_all %>% 
  select_at(vars(country_text_id, year, matches("_ga"))) %>% 
  #filter(year > 1990) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  select(country_text_id, arate_ccp_ga) %>% 
  arrange(-arate_ccp_ga) %>% 
  top_n(5, arate_ccp_ga)

test = performance_all %>% 
  select(country, year, variable = arate_lutz_ga_index) %>%
  na.omit() %>% 
  group_by(country) %>% 
  top_n(1, year) %>% 
  ungroup()

create_world_map_cont(performance_all %>% 
                        select(country, year, variable = arate_lutz_ga_index) %>%
                        na.omit() %>% 
                        group_by(country) %>% 
                        top_n(1, year) %>% 
                        ungroup(), label="Amendment Rate Lutz")
create_world_map_cont(performance_all %>% 
                        select(country, year, variable = arate_ccp_ga_index) %>%
                        na.omit() %>% 
                        group_by(country) %>% 
                        top_n(1, year) %>% 
                        ungroup(), label="Amendment Rate CCP")



# SOCIAL ####
# Eco Inequality ####
variable_equ_env = "eco_inequal_soc_index"
# Ranking 
table_equ = get_table(variable_equ_env) %>% 
  left_join(calculate_trend(variable_equ_env), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_equ_env)) %>% 
  bind_rows(get_OECDvalue_table(variable_equ_env))

make_dust_table(table_equ)

# Top Performer
top_performer_table(variable_equ_env)


# Development
grid.arrange(development_plot(variable_equ_env, "Social Performance - \nEconomic Inequality (All Countries)", position="bottom"),
             development_plot(variable_equ_env, "Social Performance - \nEconomic Inequality (OECD Founder)", OECD=T, "bottom"),
             variationplot(variable_equ_env, "Social Performance - \nEconomic Inequality (All Countries)", OECD=F),
             variationplot(variable_equ_env, "Social Performance - \nEconomic Inequality (OECD Founder)", OECD=T)
)
lineplot(variable_equ_env, OECD=F)
lineplot(variable_equ_env, OECD=T)


# Social Inequality ####
variable_squ_env = "soc_inequal_soc_index"

# Ranking 
table_squ = get_table(variable_squ_env) %>% 
  left_join(calculate_trend(variable_squ_env), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_squ_env)) %>% 
  bind_rows(get_OECDvalue_table(variable_squ_env))

make_dust_table(table_squ)

# Top Performer
top_performer_table(variable_squ_env)

# Why does the performance decline since 2010 (for the lower 10th percentile)?
# Number of cases decreases in 2001
performance_all %>% 
  group_by(year) %>% 
  select(variable_squ_env) %>% 
  na.omit() %>% 
  summarise(n()) %>% 
  filter(year >= 1995)

# Development
grid.arrange(development_plot(variable_squ_env, "Social Performance - \nSocial Inequality (All Countries)", position="bottom"),
             development_plot(variable_squ_env, "Social Performance - \nSocial Inequality (OECD Founder)", OECD=T, "bottom"),
             variationplot(variable_squ_env, "Social Performance - \nSocial Inequality (All Countries)", OECD=F),
             variationplot(variable_squ_env, "Social Performance - \nSocial Inequality (OECD Founder)", OECD=T)
)
lineplot(variable_squ_env, OECD=F)
lineplot(variable_squ_env, OECD=T)

# INTEGRATION ####
# Domestic Security ####

variable_ds = "domsec_ds_index"

# Ranking 
table_ds = get_table(variable_ds) %>% 
  left_join(calculate_trend(variable_ds), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_ds)) %>% 
  bind_rows(get_OECDvalue_table(variable_ds))

make_dust_table(table_ds)

# Top Performer
top_performer_table(variable_ds)
table_ds %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(-trend) %>% 
  top_n(5, trend)
table_ds %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(trend) %>% 
  top_n(5, -trend)


# Why does the performance rise in 1995?
# Number of cases decreases in 1995
performance_all %>% 
  group_by(year) %>% 
  select(pubsafe_ds_index) %>% 
  na.omit() %>% 
  summarise(n())



# Development
grid.arrange(development_plot(variable_ds, "Domestic Security Performance \n(All Countries)", position="bottom"),
             development_plot(variable_ds, "Domestic Security Performance \n(OECD Founder)", OECD=T, "bottom"),
             variationplot(variable_ds, "Domestic Security Performance \n(All Countries)", OECD=F),
             variationplot(variable_ds, "Domestic Security Performance \n(OECD Founder)", OECD=T)
)
lineplot(variable_ds, OECD=F)
lineplot(variable_ds, OECD=T)


# LATENT PATTERN MAINTENANCE ####
# Confidence
variable_lpm = "conf_pc_index"


table_lpm = get_table(variable_lpm) %>% 
  left_join(calculate_trend(variable_lpm), by="country_text_id") %>% 
  ungroup() %>% 
  arrange(country_text_id) %>% 
  bind_rows(get_totalvalue_table(variable_lpm)) %>% 
  bind_rows(get_OECDvalue_table(variable_lpm))
make_dust_table(table_lpm)


table_lpm %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(-trend) %>% 
  top_n(5, trend)

table_lpm %>% 
  na.omit() %>% 
  filter(country_text_id != "All countries") %>% 
  arrange(trend) %>% 
  top_n(5, -trend)


p1 = performance_all %>%
  select(country_text_id, year,  sel_variable = variable_lpm) %>% 
  na.omit() %>% 
  mutate(year5 = floor(year/5)*5) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_performance = mean(sel_variable, na.rm=T)) %>% 
  group_by(year5) %>% 
  summarise(ymax = fun_quantile10(mean_performance), 
            ymean = median(mean_performance, na.rm=T), 
            ymin=fun_quantile90(mean_performance)) %>% 
  ggplot(aes(x=year5, y=ymean, ymin=ymin, ymax=ymax)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  stat_cor(aes(label = ..r.label..), label.y.npc= "top") +
  ylim(0,100) +
  xlab("") +
  #ylab(gsub(" \\(.*", "", title)) +
  ylab("Performance Index") +
  theme_bw() +
  ggtitle("Confidence \n(All Countries)")

p2 = performance_all %>%
  select(country_text_id, year,  sel_variable = variable_lpm) %>% 
  na.omit() %>% 
  mutate(year5 = floor(year/5)*5) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_performance = mean(sel_variable, na.rm=T)) %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  filter(OECD_founder == 1) %>% 
  group_by(year5) %>% 
  summarise(ymax = fun_quantile10(mean_performance), 
            ymean = median(mean_performance, na.rm=T), 
            ymin=fun_quantile90(mean_performance)) %>% 
  ggplot(aes(x=year5, y=ymean, ymin=ymin, ymax=ymax)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  stat_cor(aes(label = ..r.label..), label.y.npc= "top") +
  ylim(0,100) +
  xlab("") +
  #ylab(gsub(" \\(.*", "", title)) +
  ylab("Performance Index") +
  theme_bw() +
  ggtitle("Confidence \n(OECD)")

p3 = performance_all %>%
  select(country_text_id, year,  sel_variable = variable_lpm) %>% 
  na.omit() %>% 
  mutate(year5 = floor(year/5)*5) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_performance = mean(sel_variable, na.rm=T)) %>% 
  group_by(year5) %>% 
  summarise(variation = coef_variation(mean_performance))  %>% 
  ggplot(aes(x=year5, y=variation)) +
  geom_line(size=1) +
  geom_point() +
  ylab("Coefficient of Variation in %") +
  xlab("") +
  ylim(0,100) +
  stat_cor(aes(label = ..r.label..)) +
  theme_bw() +
  ggtitle("Confidence \n(All Countries)")


p4 = performance_all %>%
  select(country_text_id, year,  sel_variable = variable_lpm) %>% 
  na.omit() %>% 
  mutate(year5 = floor(year/5)*5) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_performance = mean(sel_variable, na.rm=T)) %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  filter(OECD_founder == 1) %>% 
  group_by(year5) %>% 
  summarise(variation = coef_variation(mean_performance))  %>% 
  ggplot(aes(x=year5, y=variation)) +
  geom_line(size=1) +
  geom_point() +
  ylab("Coefficient of Variation in %") +
  xlab("") +
  ylim(0,100) +
  stat_cor(aes(label = ..r.label..)) +
  theme_bw() +
  ggtitle("Confidence \n(OECD)")


# Development
grid.arrange(p1,p2,p3,p4)
lineplot(variable_lpm, OECD=F)
lineplot(variable_lpm, OECD=T)
