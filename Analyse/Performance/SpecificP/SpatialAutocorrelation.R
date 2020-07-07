# SPATIAL AUTOCORRELATION ####

geoCountries = read_dta("Datasets/geo_cepii.dta")
distanceCountries = read_dta("Datasets/dist_cepii.dta")

# country_text_id vs iso3 ####
# shows countries and iso3 codes
test_before = data.frame(country_text_id = unique(distanceCountries$iso_o)) %>% 
  mutate(ind = "GEO") %>% 
  full_join(performance_all  %>% 
              select(country_text_id, country) %>% 
              distinct() %>% 
              mutate(ind2 = "VDEM"), by="country_text_id")



# ISO3 --> VDEM
# YUG Serbia and Montenegro	--> MNE SRB XKX
# ROM  Romania --> ROU
# TMP Timor Leste --> TLS


# Distance
distanceCountries = read_dta("Datasets/dist_cepii.dta") %>% 
  mutate(iso_o = ifelse(iso_o == "ROM", "ROU", iso_o),
         iso_d = ifelse(iso_d == "ROM", "ROU", iso_d)) %>% 
  mutate(iso_o = ifelse(iso_o == "TMP", "TLS", iso_o),
         iso_d = ifelse(iso_d == "TMP", "TLS", iso_d))

distance_MNE = distanceCountries %>% 
  filter(iso_o == "YUG") %>% 
  mutate(iso_o = ifelse(iso_o == "YUG", "MNE", iso_o),
         iso_d = ifelse(iso_d == "YUG", "MNE", iso_d))
distance_SRB = distanceCountries %>% 
  filter(iso_o == "YUG") %>% 
  mutate(iso_o = ifelse(iso_o == "YUG", "SRB", iso_o),
         iso_d = ifelse(iso_d == "YUG", "SRB", iso_d))
distance_XKX = distanceCountries %>% 
  filter(iso_o == "YUG") %>% 
  mutate(iso_o = ifelse(iso_o == "YUG", "XKX", iso_o),
         iso_d = ifelse(iso_d == "YUG", "XKX", iso_d))
distance_DDR = distanceCountries %>% 
  filter(iso_o == "DEU") %>% 
  mutate(iso_o = ifelse(iso_o == "DEU", "DDR", iso_o),
         iso_d = ifelse(iso_d == "DEU", "DDR", iso_d))
distance_PSE = distanceCountries %>% 
  filter(iso_o == "ISR") %>% 
  mutate(iso_o = ifelse(iso_o == "ISR", "PSE", iso_o),
         iso_d = ifelse(iso_d == "ISR", "PSE", iso_d))

distanceCountries_final = distanceCountries %>% 
  bind_rows(distance_MNE) %>% 
  bind_rows(distance_SRB) %>% 
  bind_rows(distance_XKX) %>% 
  bind_rows(distance_DDR) %>% 
  bind_rows(distance_PSE)

# Final Successful Test
test_after = data.frame(country_text_id = unique(distanceCountries_final$iso_o)) %>% 
  mutate(ind = "GEO") %>% 
  full_join(performance_all  %>% 
              select(country_text_id, country) %>% 
              distinct() %>% 
              mutate(ind2 = "VDEM"), by="country_text_id")

# Create Spatially weighted Dependent Variable
# Average DV (only between effect)



spatial_dv = function(tscs_data, dvar) {
  average_dependent = tscs_data %>% 
    select(country_text_id, dvar = dvar) %>% 
    group_by(country_text_id) %>% 
    summarize(mean_dv = mean(dvar, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(mean_dv = scale_this(mean_dv)) 
  
  # Create Weighted DV
  spatial_cor = distanceCountries_final %>% 
    left_join(average_dependent, by = c("iso_o" = "country_text_id")) %>% 
    left_join(average_dependent, by = c("iso_d" = "country_text_id")) %>% 
    group_by(iso_o) %>%
    # min-max
    mutate(dist = (dist - min(dist))/(max(dist) - min(dist)),
           # inversing so that countries far apart weight less
           dist = 1-dist,
           # weighting
           weighted_dv = dist * mean_dv.y) %>% 
    summarise(weighted_dv = mean(weighted_dv, na.rm=T)) %>% 
    rename(country_text_id = iso_o)
  
  colnames(spatial_cor)[2] = paste(dvar, "_spatial_ctl", sep="")
  
  return(spatial_cor)
}



spatial_dv_year = function(tscs_data, dvar) {
  dependent_year = tscs_data %>% 
    select(country_text_id, year, dvar = dvar) %>% 
    mutate(dvar = scale_this(dvar))
  
  # Create Weighted DV
  spatial_cor = distanceCountries_final %>% 
    right_join(dependent_year, by = c("iso_o" = "country_text_id")) %>% 
    right_join(dependent_year, by = c("iso_d" = "country_text_id", "year")) %>% 
    # min-max
    mutate(dist = (dist - min(dist))/(max(dist) - min(dist)),
           # inversing so that countries far apart weight less
           dist = 1-dist,
           # weighting
           weighted_dv = dist * dvar.y) %>% 
    group_by(iso_o, year) %>%
    summarise(weighted_dv = mean(weighted_dv, na.rm=T)) %>% 
    ungroup() %>% 
    rename(country_text_id = iso_o)  %>% 
    mutate(weighted_dv = scale_this(weighted_dv))
  
  colnames(spatial_cor)[3] = paste(dvar, "_spatial_ctl", sep="")
  
  return(spatial_cor)
}

# Plots

# tscs_data %>%
#   select(country_text_id, year, regions) %>%
#   #distinct() %>%
#   left_join(spatial_cor, by = c("country_text_id", "year")) %>%
#   ggplot(aes(x=regions, y=air_env_spatial_ctl)) +
#   geom_point() +
#   coord_flip() +
#   theme_bw()
  
 
