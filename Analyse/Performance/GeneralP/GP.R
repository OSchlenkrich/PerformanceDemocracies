# General Performance

# Load Sustainable Governance Indicators Dataset
SGI = readxl::read_xlsx("C:/RTest/SGI2019_Scores.xlsx", sheet=1, na = "n/a") %>% 
  rename(country = 1) %>% 
  mutate(year = 2019) %>% 
  bind_rows(readxl::read_xlsx("C:/RTest/SGI2019_Scores.xlsx", sheet=2, na = "n/a") %>% 
              rename(country = 1) %>% 
              mutate(year = 2018)) %>% 
  bind_rows(readxl::read_xlsx("C:/RTest/SGI2019_Scores.xlsx", sheet=3, na = "n/a") %>% 
              rename(country = 1) %>% 
              mutate(year = 2017)) %>% 
  bind_rows(readxl::read_xlsx("C:/RTest/SGI2019_Scores.xlsx", sheet=4, na = "n/a") %>% 
              rename(country = 1) %>% 
              mutate(year = 2016)) %>% 
  bind_rows(readxl::read_xlsx("C:/RTest/SGI2019_Scores.xlsx", sheet=5, na = "n/a") %>% 
              rename(country = 1) %>% 
              mutate(year = 2015)) %>% 
  bind_rows(readxl::read_xlsx("C:/RTest/SGI2019_Scores.xlsx", sheet=6, na = "n/a") %>% 
              rename(country = 1) %>% 
              mutate(year = 2014)) %>% 
  left_join(fread("C:/RTest/SGI_regions.csv"), by = "country") %>% 
  mutate(country = fct_recode(country, "United States of America" = "United States",
                              "Czech Republic" = "Czechia")) %>% 
  select(country, year, everything())


# 'Strategic Capacity'
# Implementation
# `Organizational Reform`
# 'Adaptability'
# 'Societal Consultation'
# 'Interministerial Coordination'

SGI_dmx = SGI %>% 
  left_join(modes_cluster, by="country") %>% 
  left_join(VoC_Welfare_types, by="country") %>% 
  select(country, year, region_SGI,
         #VoC_HS,
         welfare_E,
         #VoC_Kitschelt,
         QoD = 'Quality of Democracy', 
         Economy, 
         #Soc_Pol = 'Social Policies',
         cluster_label_1st_mode, 
         var = `Adaptability`) %>% 
  mutate(cluster_label_1st_mode = relevel(cluster_label_1st_mode, ref = "fEC"))


SGI_dmx %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n()/6)

##



m1 = lm(var ~ welfare_E +  cluster_label_1st_mode, SGI_dmx)
summary(m1)
plot(effect(mod=m1, term="cluster_label_1st_mode"))


library(car)
outlierTest(m1)
leveragePlots(m1) 
influencePlot(m1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

cutoff <- 4/((nrow(SGI_dmx)-length(m1$coefficients)-2))
plot(m1, which=4, cook.levels=cutoff)

