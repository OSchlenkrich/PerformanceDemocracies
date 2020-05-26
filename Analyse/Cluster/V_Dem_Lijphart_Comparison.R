source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Analyse/Cluster_v2.R")

V_dem = fread("C:/RTest/V-Dem-CY+Others-v8.csv")

names(V_dem)

V_dem_test = V_dem %>% 
  select(country_name, year, v2jureview) %>% 
  filter(country_name == "Israel" | country_name == "Germany")

V_dem_principals = V_dem %>% 
  select(country = country_name, 
         year, 
         v2x_libdem,
         v2x_partipdem,
         v2x_delibdem,
         v2x_egaldem
         )


dmx_trade_cluster_vdem = dmx_trade_cluster %>% 
  select(country, year, cluster_label_1st, classification_context) %>% 
  left_join(V_dem_principals, by = c("country", "year")) 



dmx_trade_cluster_vdem %>% 
  filter(classification_context == "Working Democracy") %>% 
  select(-country, -year, -classification_context) %>% 
  melt(id.var="cluster_label_1st") %>% 
  mutate(variable = fct_relevel(variable, "v2x_libdem", "v2x_egaldem")) %>% 
  ggplot(aes(x=cluster_label_1st, y=value, fill=variable)) +
  geom_boxplot() +
  theme(legend.position = "bottom")


dmx_trade_cluster_vdem %>% 
  select(-country, -year, -classification_context) %>% 
  mutate_if(is.numeric, scale) %>% 
  group_by(cluster_label_1st) %>% 
  summarise_all(median)

# Lijphart

Lijphart = fread("Datasets/Lijphart.csv") %>% 
  rename(country_text_id = country)

country_bridger = V_dem %>% 
  select(country_text_id, country = country_name) %>% 
  group_by(country_text_id) %>% 
  slice(1)


country_id_Lijphart = Lijphart %>% 
  left_join(country_bridger, by="country_text_id") %>% 
  write.csv2("Datasets/country_id_Lijphart_raw.csv", row.names = F)  


Lijphart_Consensus = read.csv2("country_id_Lijphart.csv")  %>% 
  left_join(Lijphart, by="country_text_id") %>% 
  select(country, exec_parties_1945_2010, exec_parties_1981_2010, federal_unitary_1945_2010, federal_unitary_1981_2010 = federal_unitry_1981_2010) %>% 
  mutate(exec_parties_1945_2010 = exec_parties_1945_2010 * -1,
         federal_unitary_1945_2010 = federal_unitary_1945_2010 * -1,
  )


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

dmx_trade_cluster %>% 
  filter(year >= 1981, year <= 2017) %>% 
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  summarise(mode_cluster = Mode(cluster_label_1st)) %>% 
  right_join(Lijphart_Consensus, by = "country") %>% 
  na.omit() %>% 
  ggplot(aes(x =  exec_parties_1945_2010 , y=federal_unitary_1945_2010, col=mode_cluster, label=country)) +
  geom_text() + 
  geom_point(size=3.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)


## Einzelne Indices



names(Lijphart)

Lijphart_Indices = read.csv2("country_id_Lijphart.csv")  %>% 
  left_join(Lijphart, by="country_text_id") %>% 
  select_at(vars(country, starts_with("index"))) %>% 
  select_at(vars(country, matches("1981")))


dmx_trade_cluster %>% 
  filter(year >= 1945, year <= 2017) %>% 
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  summarise(mode_cluster = Mode(cluster_label_1st)) %>% 
  right_join(Lijphart_Indices, by = "country") %>% 
  select(-country) %>% 
  na.omit() %>% 
  melt(id.var="mode_cluster") %>% 
  ggplot(aes(x=mode_cluster, y=value, fill=variable)) +
  geom_boxplot() +
  geom_point() + 
  facet_wrap(variable ~., scales = "free_y")


test = dmx_trade_cluster %>% 
  filter(year >= 1945, year <= 2017) %>% 
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  summarise(mode_cluster = Mode(cluster_label_1st)) %>% 
  right_join(Lijphart_Indices, by = "country") 



dmx_trade_cluster %>% 
  filter(year >= 1945, year <= 2017) %>% 
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  summarise(mode_cluster = Mode(cluster_label_1st)) %>% 
  right_join(Lijphart_Indices, by = "country") %>% 
  select_at(vars(country, mode_cluster, bic = matches("bicameralism"), jud = matches("judicial"))) %>% 
  na.omit() %>% 
  ggplot(aes(x=bic, y=jud, col=mode_cluster)) +
  geom_jitter(size = 3, width=0.2, height = 0.2) 


test2 = dmx_trade_cluster %>% 
  filter(year >= 1945, year <= 2017) %>% 
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  summarise(mode_cluster = Mode(cluster_label_1st)) %>% 
  right_join(Lijphart_Indices, by = "country") %>% 
  select_at(vars(country, mode_cluster, bic = matches("bicameralism"), jud = matches("judicial")))




library(ggplot2)
library(gridExtra)

plot_random_countries_dim = function(complete_data_dimensions, No_countries) {
  
  if (is.numeric(No_countries)==T) {
    random_countries = sample(n_distinct(complete_data_dimensions$country), No_countries)
    selected_countries = unique(complete_data_dimensions$country)[random_countries]
  } else {
    selected_countries = No_countries
  }
  
  plotted_country = complete_data_dimensions %>%
    filter(country %in% selected_countries) %>%
    melt(id.vars=c("country", "year"), measure.vars="cluster_label_1st") %>%
    mutate(y=1)
  
  p1 = ggplot(plotted_country, aes(x=year,  y=y, fill=value)) + geom_bar(width=1, stat="identity") + 
    facet_wrap(country~. , nrow=length(selected_countries), strip.position="left") + 
    theme_bw() +
    scale_y_continuous(breaks=c(0,1)) +
    scale_x_continuous(limits=c(1900, 2020), breaks=seq(1900, 2020, 20)) + 
    theme(axis.text.x = element_text(angle=90)) + xlab("") + 
    ylab("") +
    scale_fill_brewer(name="Cluster", type="qual", palette="Accent")
   return(p1)
}

plot_random_countries_dim(dmx_trade_cluster, 15)

