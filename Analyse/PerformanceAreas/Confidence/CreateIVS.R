# CREATE IVS DATASET

# Identifier
IVS_ctry_id = read.csv("C:/RTest/IVS/ctry_id.csv", header=T, encoding = "UTF-8") %>% 
  rename(S003 = V2) %>% 
  mutate(country = fct_recode(country,
                              "United Kingdom" = "Great Britain",
                              "Czech Republic" = "Czech Rep.",
                              "Bosnia and Herzegovina" = "Bosnian Federation",
                              "Republic of Vietnam" = "Viet Nam",
                              "Bosnia and Herzegovina" = "Bosnia",
                              "United States of America" = "United States")) %>% 
  filter(S003 > 0)



cty_identifier = V_dem %>% 
  dplyr::select(country, country_text_id) %>% 
  group_by(country) %>% 
  dplyr::slice(1) 


# Check if identifier matches with own sample (Cyprus?)

dmx_trade_cluster %>% 
  dplyr::select(country) %>% 
  distinct() %>% 
  left_join(cty_identifier, by="country") %>% 
  mutate(DMX = "DMX") %>% 
  left_join(IVS_ctry_id%>% 
              mutate(IVS = "IVS"), by="country")



# Create Integrated Value Survey + country_text_id for matching
EVS = read_spss("C:/RTest/IVS/ZA4804_v3-0-0.sav", user_na = T)
WVS = read_spss("C:/RTest/IVS/WVS_Longitudinal_1981_2016_Spss_v20180912.sav", user_na = T)

# remove labels; needed for bind_rows
WVS = zap_labels(WVS)
EVS = zap_labels(EVS)

IVS = WVS %>% 
  bind_rows(EVS) %>% 
  left_join(IVS_ctry_id, by="S003") %>% 
  left_join(cty_identifier, by="country")

rm(EVS)
rm(WVS)
gc()
