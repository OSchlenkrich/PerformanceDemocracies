## Goal-Attainment Performance

dmx_trade_cluster_ccp = dmx_trade_cluster %>% 
  left_join(V_dem %>% select(country_text_id, year, COWcode), by=c("country_text_id", "year")) %>% 
  mutate(COWcode = if_else(COWcode == 255, as.integer(260), COWcode))


# Create Dataset
ccp = fread("C:/RTest/ccp_csv.csv") %>% 
  as_tibble() %>% 
  select(systid, COWcode = cowcode, country, year)

ARate_Data = fread("C:/RTest/ARData.txt") %>%
  select(
    COWcode = cowcode, country, country_text_id = cabbr, year, systid,
    arate_ccp = ar_ccpy,
    warate_ccp = ar_ccpw,
    arate_lutz = ar_lutz
  )
ADiff_Data = fread("C:/RTest/ADData.txt") %>%
  select(
    COWcode = cowcode, year,
    adiff_ccp = ad_ccp,
    adiff_lutz = ad_lutz
  )  
  
# Join and Change to country-year format

Amendment_data_yearly =  ARate_Data %>% 
  left_join(ADiff_Data, by=c("COWcode", "year")) %>% 
  select_at(vars(systid, ends_with("_ccp"), ends_with("_lutz"))) %>% 
  right_join(ccp, by="systid") %>% 
  select(COWcode, year, everything()) %>% 
  arrange(COWcode, year) %>% 
  select(-country) 


AR_dmx = V_dem %>% 
  select(country, country_text_id, year, COWcode)  %>% 
  mutate(COWcode = if_else(COWcode == 255, as.integer(260), COWcode)) %>% 
  left_join(Amendment_data_yearly, by=c("COWcode", "year")) %>% 
  left_join(dmx_trade_cluster %>% select(-country, -regions, -classification_context), by=c("country_text_id", "year")) %>% 

  # Filtering
  filter(year >= 1950) %>%
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  left_join(dmx_data %>%  select(country, year, classification_context) , by=c("country", "year")) %>% 
  filter(classification_context == "Deficient Democracy" |  classification_context == "Working Democracy")  %>%
  
  # add country and regions
  left_join(dmx_data %>%  select(country, regions) %>%  distinct(), by=c("country"))  %>%
  
  select(country, country_text_id, regions, year, everything())  %>% 
  dplyr::arrange(country_text_id, year)  


##### NA-Plots ####
dim(AR_dmx)

AR_dmx %>% 
  filter(year >= 1950) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_ccp"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - CCP")

AR_dmx %>% 
  filter(year >= 1950) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_lutz"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - CCP")

####

AR_dmx %>% 
  select_at(vars(ends_with("_ccp"),ends_with("_lutz"))) %>% 
  summarise_all(list(min=min, mean=mean, max=max), na.rm=T) %>% 
  mutate_all(~round(.,2)) %>% 
  melt() 


AR_dmx %>% 
  select_at(vars(ends_with("_ccp"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free") 


AR_dmx %>% 
  select_at(vars(ends_with("_lutz"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

AR_dmx %>% 
  select_at(vars(starts_with("arate"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


# Transformation Variables ####

AR_dmx_norm = AR_dmx  %>% 
  mutate_at(vars(ends_with("_ccp")), funs(scaler_perc)) %>% 
  mutate_at(vars(ends_with("_ccp")), funs(folded_ladder_fun(., plotting =T))) %>% 
  mutate_at(vars(ends_with("_lutz")), funs(ladder_fun(.))) %>% 
  mutate_at(vars(ends_with("_ccp"), ends_with("_lutz")), funs(scale)) 
  

AR_dmx_norm %>% 
  select_at(vars(ends_with("_ccp"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")


AR_dmx_norm %>% 
  select_at(vars(ends_with("_lutz"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

AR_dmx_norm %>% 
  select_at(vars(starts_with("arate"))) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

# Index

AR_final = AR_dmx_norm %>%
  select_at(vars(country, country_text_id, regions, year, classification_context, cluster_label_1st,
                 starts_with("arate"), starts_with("warate"))) %>% 
  mutate_at(vars(starts_with("arate"), starts_with("warate")), ~EPI_fun(.)) %>% 
  rename(GA_ccp_index = arate_ccp,
         GA_lutz_index = arate_lutz,
  )

# Descriptives

samples = c("GBR","NZL", "SWE", "USA", "DEU", "FRA")
samples = sample(unique(performance_all$country_text_id), 10)

AR_final %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("index"))) %>% 
  filter(year > 1990) %>% 
  group_by(country_text_id) %>% 
  summarise_all(mean, na.rm=T) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=country_text_id, y=value, col=country_text_id)) +
  geom_point(size=2) +
  ylim(0,100) +
  facet_wrap(variable ~ ., scales="free_y") +
  theme_bw() 
