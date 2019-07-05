## Creating Final dataset

# Creating whole dataset


# year frame
year_frame = data.frame(country = rep(unique(dmx_trade_cluster$country), each=length(1900:2017)),
                        year = rep(1900:2017, length(unique(dmx_trade_cluster$country)))
)



# IDV

median_cluster_1st = dmx_trade_cluster %>%
  full_join(year_frame, by=c("country", "year")) %>%
  arrange(country, year) %>%
  filter(year >= 1940) %>%
  group_by(country) %>%
  mutate(mod_cluster_1st = rollapply(cluster_1st, 15, FUN = function(x) getmode(x),
                                     fill=NA, align="right", partial=T))


# median_cluster_1st = dmx_trade_cluster %>%
#   full_join(year_frame, by=c("country", "year")) %>%
#   arrange(country, year) %>%
#   filter(year >= 1940) %>%
#   group_by(country) %>%
#   mutate(mod_cluster_1st = getmode(cluster_1st))

median_cluster_1st$mod_cluster_1st = as.factor(median_cluster_1st$mod_cluster_1st)


median_cluster_1st = median_cluster_1st %>%
  mutate(mod_cluster_1st = fct_recode(mod_cluster_1st,
                                         "fEC" = "1", # egalitarian + control
                                         "fEc" = "2", # egalitarian
                                         "FeC" = "3", # liberal + control
                                         "Fec" = "4", # liberal
                                         "FEC" = "5", # balanced
            ),
            mod_cluster_1st = fct_relevel(mod_cluster_1st,
                                          "fEC",
                                          "Fec"
            )
         ) %>%
  dplyr::select(country, year, mod_cluster_1st, cluster_1st)

#

median_cluster_2nd = dmx_trade_cluster %>%
  full_join(year_frame, by=c("country", "year")) %>%
  arrange(country, year) %>%
  filter(year >= 1940) %>%
  group_by(country) %>%
  mutate(mod_cluster_2nd = rollapply(cluster_2nd,10, FUN = function(x) getmode(x),
                                     fill=NA, align="right", partial=T))

median_cluster_2nd$mod_cluster_2nd = as.factor(median_cluster_2nd$mod_cluster_2nd)

median_cluster_2nd = median_cluster_2nd %>%
  mutate(mod_cluster_2nd = fct_recode(mod_cluster_2nd,
                                      "fEC" = "1", # egalitarian + control
                                      "fEc" = "2", # egalitarian
                                      "FeC" = "3", # liberal + control
                                      "Fec" = "4", # liberal
                                      "FEC" = "5", # balanced
  ),
  mod_cluster_2nd = fct_relevel(mod_cluster_2nd,
                                "fEC",
                                "Fec"
  )) %>%
  dplyr::select(country, year, mod_cluster_2nd, cluster_1st)



dmx_data_complete = dmx_data %>% 
  dplyr::select(country, year, classification_context) %>% 
  mutate(classification_context = fct_recode(classification_context, 
                                             NULL = "",
                                             NULL = "Autocracy",
                                             NULL = "Hybrid Regime",
  ))


dmx_trade_cluster_ext =  dmx_trade_cluster %>% 
  dplyr::select(-classification_context) %>% 
  full_join(year_frame, by=c("country", "year")) %>%
  left_join(dmx_data_complete, by=c("country", "year")) %>% 
  left_join(oecd_social_data, by=c("country", "year")) %>% 
  left_join(oecd_poverty_data, by=c("country", "year")) %>%  
  left_join(V_dem, by=c("country", "year")) %>%
  left_join(WB_gdp, by=c("country", "year")) %>%  
  left_join(population_total, by=c("country", "year")) %>%
  left_join(WB_export, by=c("country", "year")) %>%  
  left_join(WB_inflation, by=c("country", "year")) %>%  
  left_join(Age65_percent , by=c("country", "year")) %>%   
  left_join(ParlGov_Cabinet_yearly , by=c("country", "year")) %>% 
  left_join(ParlGov_Family_yearly , by=c("country", "year")) %>% 
  left_join(Trade_union, by=c("country")) %>% 
  left_join(median_cluster_1st, by=c("country", "year")) %>%
  left_join(dmx_data_context %>%  dplyr::select(-regions), by=c("country", "year")) %>%
  left_join(median_cluster_2nd, by=c("country", "year")) %>%
  left_join(populism_dataset, by=c("country", "year")) %>% 
  mutate(mod_cluster_1st = relevel(mod_cluster_1st, ref="FeC"),
         mod_cluster_2nd = relevel(mod_cluster_2nd, ref="FeC"),
         region = relevel(as.factor(region), ref="Western Europe and North America"),
         year_factor = as.factor(year),
         gdp_capita = log(gdp_capita)) %>% 
  arrange(country, year)




# Cleaning
# rm(oecd_social_data)
# rm(oecd_poverty_data)
# rm(WB_inflation)
# rm(WB_export)
# rm(WB_gdp)
# rm(ParlGov_Family_yearly)
# rm(ParlGov_Cabinet_yearly)
# rm(ParlGov_Cabinet)
# rm(ParlGov_Party)
# rm(Trade_union)
# rm(Age65_percent)
# rm(Unemployment_percent)
# rm(year_frame)
# rm(frame)
# rm(median_cluster_2nd)
# rm(median_cluster_1st)
# rm(population_total)