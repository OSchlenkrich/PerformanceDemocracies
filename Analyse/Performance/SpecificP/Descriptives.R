
##### NA Plot #########
dim(performance_all)


performance_all %>% 
  filter(year >= 1950) %>% 
  group_by(year) %>% 
  select_at(vars(matches("index"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100), name=NULL)  +
  scale_x_continuous(breaks=seq(1950,2020, 10), name=NULL) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Percentage of NA-Values For Each Performance Areas")



## MAKE PLOTS ####

test = performance_all %>% 
  select_at(vars(year, matches("index"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean), na.rm=T) %>% 
  melt(id.vars=c("year"), value.name="mean") %>% 
  bind_cols(performance_all %>% 
              select_at(vars(year, matches("index"))) %>% 
              group_by(year) %>% 
              summarise_all(funs(fun_quantile25), na.rm=T) %>% 
              melt(id.vars=c("year")) %>% 
              select(lower25 = value)) %>% 
  bind_cols(performance_all %>% 
              select_at(vars(year, matches("index"))) %>% 
              group_by(year) %>% 
              summarise_all(funs(fun_quantile75), na.rm=T) %>% 
              melt(id.vars=c("year")) %>% 
              select(upper75 = value)) %>% 
  na.omit()


# OVERALL TREND
performance_all %>% 
  select_at(vars(year, matches("index"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(mean), na.rm=T) %>% 
  melt(id.vars=c("year"), value.name="mean") %>% 
  bind_cols(performance_all %>% 
              select_at(vars(year, matches("index"))) %>% 
              group_by(year) %>% 
              summarise_all(funs(fun_quantile25), na.rm=T) %>% 
              melt(id.vars=c("year")) %>% 
              select(lower25 = value)) %>% 
  bind_cols(performance_all %>% 
              select_at(vars(year, matches("index"))) %>% 
              group_by(year) %>% 
              summarise_all(funs(fun_quantile75), na.rm=T) %>% 
              melt(id.vars=c("year")) %>% 
              select(upper75 = value)) %>% 
  ggplot(aes(x=year, y=mean, col=variable)) +
  geom_line(size=1) +
  geom_line(aes(y=lower25), size=1) +
  geom_line(aes(y=upper75), size=1) +
  facet_wrap(variable ~ .) +
  ylim(0,100) +
  theme_bw() +
  theme(legend.position = "none")


# SINGLE COUNTRIES
samples = c("GBR","NZL", "SWE", "USA", "DEU", "FRA")

samples = sample(unique(performance_all$country_text_id), 4)
year_sel = 2011

performance_all %>% 
  filter(country_text_id %in% samples, year == year_sel) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  mutate(variable = fct_relevel(variable, 
                                "conf_index" , 
                                "ds_order_index",
                                "ds_life_index", 
                                "soc_index",
                                "GA_lutz_index",
                                "GA_ccp_index",
                                "environment_wdi_index",
                                "environment_oecd_index",
                                "eco_oecd_index",
  )) %>% 
  mutate(variable = fct_recode(variable, 
                               "4: Confidence" = "conf_index" , 
                               "3B: Order" = "ds_order_index",
                               "3B: Security" = "ds_life_index", 
                               "3A: Social P." = "soc_index",
                               "2: Goal Att L" = "GA_lutz_index",
                               "2: Goal Att CCP" = "GA_ccp_index",
                               "1B: Environment P. WDI" = "environment_wdi_index",
                               "1B: Environment P. OECD" = "environment_oecd_index",
                               "1A: Economy P." = "eco_oecd_index",
  )) %>% 
  ggplot(aes(x=country_text_id, y=variable, fill=value)) +
  geom_tile() + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "#e84434", high = "#34e851", limits=c(0,100)) +
  xlab("") +
  ylab("") + 
  ggtitle(year_sel) +
  geom_hline(yintercept = c(1.5, 4.5,6.5), size=1.5)


###
samples = sample(unique(performance_all$country_text_id), 14)
samples = c("GBR","NZL", "SWE", "USA", "DEU", "FRA")
# samples = c("CZE","POL")

lower = 2007
upper = 2017
performance_all %>% 
  filter(country_text_id %in% samples, year >= lower, year <= upper) %>% 
  select_at(vars(country, year, matches("index"))) %>% 
  select(-year) %>% 
  group_by(country) %>% 
  summarise_all(mean, na.rm=T) %>% 
  melt(id.vars=c("country")) %>% 
  mutate(variable = fct_relevel(variable, 
                                "conf_index" , 
                                "ds_order_index",
                                "ds_life_index", 
                                "soc_index",
                                "GA_lutz_index",
                                "GA_ccp_index",
                                "environment_wdi_index",
                                "environment_oecd_index",
                                "eco_oecd_index",
  )) %>% 
  mutate(variable = fct_recode(variable, 
                               "4: Confidence" = "conf_index" , 
                               "3B: Order" = "ds_order_index",
                               "3B: Security" = "ds_life_index", 
                               "3A: Social P." = "soc_index",
                               "2: Goal Att L" = "GA_lutz_index",
                               "2: Goal Att CCP" = "GA_ccp_index",
                               "1B: Environment P. WDI" = "environment_wdi_index",
                               "1B: Environment P. OECD" = "environment_oecd_index",
                               "1A: Economy P." = "eco_oecd_index",
  )) %>% 
  ggplot(aes(x=country, y=variable, fill=value)) +
  geom_tile() + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "#e84434", high = "#34e851", limits=c(0,100)) +
  xlab("") +
  ylab("") +
  ggtitle(paste("Mean Value:", lower, "-", upper)) +
  geom_hline(yintercept = c(1.5, 4.5,6.5), size=1.5)


# Cluster Analysis ####
performance_all_na = performance_all  %>% 
  select(-environment_wdi_index, -GA_lutz_index) %>% 
  filter(year >= 2000) %>% 
  filter(year <= 2010) %>% 
  group_by(country) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  na.omit()


cluster_performance = hclust(dist(performance_all_na %>% 
                                    select_at(vars(ends_with("index"))), method="euclidean"))
plot(cluster_performance)
performance_all_na$class = cutree(cluster_performance, 4)

library(factoextra)
fviz_nbclust(performance_all_na %>% 
               select_at(vars(ends_with("index"))), 
             pam, method = "silhouette", k.max = 24) + 
  theme_minimal() + 
  ggtitle("The Silhouette Plot")


pam_solution = pam(performance_all_na %>% 
                     select_at(vars(ends_with("index"))), 2)

performance_all_na[pam_solution$id.med,]


performance_all_na$class = pam_solution$clustering

# Matrix Plot
performance_all_na %>% 
  group_by(class) %>% 
  summarise_if(is.numeric, mean)  %>% 
  select(-year) %>% 
  melt(id.vars=c("class")) %>% 
  mutate(variable = fct_relevel(variable, 
                                "conf_index" , 
                                "ds_order_index",
                                "ds_life_index", 
                                "soc_index",
                                "GA_lutz_index",
                                "GA_ccp_index",
                                "environment_wdi_index",
                                "environment_oecd_index",
                                "eco_oecd_index",
  )) %>% 
  mutate(variable = fct_recode(variable, 
                               "4: Confidence" = "conf_index" , 
                               "3B: Order" = "ds_order_index",
                               "3B: Security" = "ds_life_index", 
                               "3A: Social P." = "soc_index",
                               "2: Goal Att L" = "GA_lutz_index",
                               "2: Goal Att CCP" = "GA_ccp_index",
                               "1B: Environment P. WDI" = "environment_wdi_index",
                               "1B: Environment P. OECD" = "environment_oecd_index",
                               "1A: Economy P." = "eco_oecd_index",
  )) %>% 
  ggplot(aes(x=class, y=variable, fill=value)) +
  geom_tile() + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "#e84434", high = "#34e851", limits=c(0,100)) +
  xlab("") +
  ylab("") +
  ggtitle("Mean Value") +
  geom_hline(yintercept = c(1.5, 4.5,5.5), size=1.5)


# Boxplot
performance_all_na %>% 
  group_by(class) %>% 
  select(-year, -country, -country_text_id ) %>% 
  melt(id.vars=c("class")) %>% 
  mutate(variable = fct_recode(variable, 
                               "4: Confidence" = "conf_index" , 
                               "3B: Order" = "ds_order_index",
                               "3B: Security" = "ds_life_index", 
                               "3A: Social P." = "soc_index",
                               "2: Goal Att CCP" = "GA_ccp_index",
                               "1B: Environment P. OECD" = "environment_oecd_index",
                               "1A: Economy P." = "eco_oecd_index",
  )) %>% 
  ggplot(aes(x=as.factor(class), y=value, fill=variable)) +
  geom_boxplot() + 
  xlab("") +
  ylab("") +
  ggtitle("Mean Value")



#### Democracy Profiles ####

dmx_performance = performance_all %>% 
  left_join(dmx_trade_cluster %>% 
              select(-country_text_id), by=c("country", "year"))

longdemocracies = dmx_performance %>% 
  select(country, classification_context) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(nr = n()) %>% 
  filter(nr > 10) %>% 
  pull(country)

modes_cluster = dmx_performance %>% 
  filter(country %in% longdemocracies) %>% 
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st))

modes_cluster %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n())

lower = 2007
upper = 2017
dmx_performance %>% 
  left_join(modes_cluster, by="country_text_id")  %>% 
  filter(year >= lower, year <= upper) %>% 
  select_at(vars(cluster_label_1st_mode, matches("index"))) %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise_all(mean, na.rm=T) %>% 
  na.omit() %>% 
  melt(id.vars=c("cluster_label_1st_mode")) %>% 
  mutate(variable = fct_relevel(variable, 
                                "conf_index" , 
                                "ds_order_index",
                                "ds_life_index", 
                                "soc_index",
                                "GA_lutz_index",
                                "GA_ccp_index",
                                "environment_wdi_index",
                                "environment_oecd_index",
                                "eco_oecd_index",
  )) %>% 
  mutate(variable = fct_recode(variable, 
                               "4: Confidence" = "conf_index" , 
                               "3B: Order" = "ds_order_index",
                               "3B: Security" = "ds_life_index", 
                               "3A: Social P." = "soc_index",
                               "2: Goal Att L" = "GA_lutz_index",
                               "2: Goal Att CCP" = "GA_ccp_index",
                               "1B: Environment P. WDI" = "environment_wdi_index",
                               "1B: Environment P. OECD" = "environment_oecd_index",
                               "1A: Economy P." = "eco_oecd_index",
  )) %>% 
  ggplot(aes(x=cluster_label_1st_mode, y=variable, fill=value)) +
  geom_tile() + 
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "#e84434", high = "#34e851", limits=c(0,100)) +
  xlab("") +
  ylab("") +
  ggtitle("Mean Value") +
  geom_hline(yintercept = c(1.5, 4.5,6.5), size=1.5)




dmx_performance %>% 
  left_join(modes_cluster, by="country_text_id") %>% 
  group_by(cluster_label_1st_mode, year) %>% 
  select_at(vars(cluster_label_1st_mode, year, matches("index"))) %>% 
  summarise_all(mean, na.rm=T) %>% 
  filter(is.na(cluster_label_1st_mode)==F) %>% 
  melt(id.vars=c("cluster_label_1st_mode", "year")) %>% 
  ggplot(aes(x=year, y=value, col=cluster_label_1st_mode)) +
  geom_line(size=1) +
  ylim(0,100) +
  geom_point() +
  facet_wrap(variable ~ .) 


dmx_performance %>% 
  left_join(modes_cluster, by="country_text_id") %>% 
  group_by(cluster_label_1st, year) %>% 
  select_at(vars(cluster_label_1st, year, matches("index"))) %>% 
  summarise_all(mean, na.rm=T) %>% 
  filter(is.na(cluster_label_1st)==F) %>% 
  melt(id.vars=c("cluster_label_1st", "year")) %>% 
  ggplot(aes(x=year, y=value, col=cluster_label_1st)) +
  geom_line(size=1) +
  geom_point() +
  facet_wrap(variable ~ .) 