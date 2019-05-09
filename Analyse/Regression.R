source("Setup/Packages.R")
source("Setup/Plotting_Functions.R")
source("Setup/LoadDatasets.R")
source("Setup/Cluster.R")
source("Setup/Base_Functions.R")

# Creating whole dataset

# IDV
median_cluster = dmx_trade_cluster %>% 
  group_by(country) %>% 
  summarise(mod_cluster = as.factor(getmode(cluster)),
            mod_cluster = fct_recode(mod_cluster, 
                                     "fEC" = "1", # egalitarian + control
                                     "fEc" = "2", # egalitarian 
                                     "FeC" = "3", # liberal + control
                                     "Fec" = "4", # liberal
                                     "FEC" = "5", # balanced
            ),
            mod_cluster = fct_relevel(mod_cluster,
                                      "fEC",
                                      "Fec"
            ))


# Dataset

dmx_trade_cluster_ext =  dmx_trade_cluster %>% 
  left_join(oecd_social_data, by=c("country", "year")) %>% 
  left_join(oecd_poverty_data, by=c("country", "year")) %>%  
  left_join(V_dem, by=c("country", "year")) %>% 
  left_join(median_cluster, by="country") %>% 
  mutate(mod_cluster = relevel(mod_cluster, ref="Fec")) 


unique(dmx_trade_cluster_ext$country)
unique(oecd_social_data$country)



# test = dmx_trade_cluster_ext %>% 
#   group_by(year, med_cluster) %>% 
#   summarize(n())
# 
dmx_trade_cluster_ext %>%
  group_by(med_cluster, year) %>%
  summarise(variable = mean(SOCX, na.rm=T)) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=variable, col=med_cluster)) +
  geom_line(size=1.1)



dmx_plm_data <- pdata.frame(dmx_trade_cluster_ext, index=c("country", "year"))


my_reg = plm(SOCX ~ lag(SOCX) + lag(e_wbgi_rqn) + mod_cluster, dmx_plm_data, model="pooling")
my_reg = plm(PovertyRate60 ~ lag(PovertyRate60) + lag(e_wbgi_rqn) + mod_cluster, dmx_plm_data, model="pooling")
my_reg = plm(v2peedueq ~ lag(v2peedueq) + mod_cluster, dmx_plm_data, model="pooling")
my_reg = plm(e_wbgi_rqn ~ lag(e_wbgi_rqn) + mod_cluster, dmx_plm_data, model="pooling")

summary(my_reg)
coeftest(my_reg, vcov.=function(x) vcovHC(x, type="sss", cluster="group"))



pwtest(my_reg)


