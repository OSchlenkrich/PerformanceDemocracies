# OTHER DEMOCRACY PROFILES ####

# Lijphart ####

consensusdemocracy = fread("Datasets/Lijphart.csv") %>% 
  as_tibble() %>% 
  rename(country_text_id = country, federal_unitary_1981_2010 = federal_unitry_1981_2010) %>% 
  mutate(country_text_id = ifelse(country_text_id == "US", "USA", country_text_id),
         country_text_id = ifelse(country_text_id == "AUL", "AUS", country_text_id),
         country_text_id = ifelse(country_text_id == "BAR", "BRB", country_text_id),
         country_text_id = ifelse(country_text_id == "BOT", "BWA", country_text_id),
         country_text_id = ifelse(country_text_id == "DEN", "DNK", country_text_id),
         country_text_id = ifelse(country_text_id == "GER", "DEU", country_text_id),
         country_text_id = ifelse(country_text_id == "GRE", "GRC", country_text_id),
         country_text_id = ifelse(country_text_id == "UK", "GBR", country_text_id),
         country_text_id = ifelse(country_text_id == "URU", "URY", country_text_id),
         country_text_id = ifelse(country_text_id == "SWI", "CHE", country_text_id),
         country_text_id = ifelse(country_text_id == "SPA", "ESP", country_text_id),
         country_text_id = ifelse(country_text_id == "NZ", "NZL", country_text_id),
         country_text_id = ifelse(country_text_id == "POR", "PRT", country_text_id),
         country_text_id = ifelse(country_text_id == "TRI", "TTO", country_text_id),
         country_text_id = ifelse(country_text_id == "MAU", "MUS", country_text_id),
         country_text_id = ifelse(country_text_id == "IRE", "IRL", country_text_id),
         country_text_id = ifelse(country_text_id == "NET", "NLD", country_text_id),
         country_text_id = ifelse(country_text_id == "ICE", "ISL", country_text_id),
         country_text_id = ifelse(country_text_id == "MAL", "MLT", country_text_id),
         country_text_id = ifelse(country_text_id == "CR", "CRI", country_text_id))


# GERRING/THACKER ####

centripetalism = read_dta("Datasets/centripetalism.dta") %>% 
  rename(country = Country, year = Year) %>% 
  filter(country != "", year >= 1900) %>%
  mutate(country = fct_recode(country,
                              "Timor-Leste" =	"Timor, East",
                              "Egypt" = 	"Egypt, Arab Rep.",
                              "Trinidad and Tobago" = "Trinidad & Tobago",
                              "São Tomé and Príncipe"	 = "Sao Tome and Principe",
                              "Kyrgyzstan" = "Kyrgyztan",
                              "The Gambia" = "Gambia, The",
                              "Slovakia" = "Slovak Republic",
                              "Cyprus" = "Cyprus, Greek (Cyprus)",
                              "Burma/Myanmar"  = "Myanmar (Burma)",
                              "Bosnia and Herzegovina" = "Bosnia-Herzegovina",
                              "Belarus" = "Belarus (Byelorussian SSR)"	)) %>% 
  left_join(V_dem_all_v9 %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(country_text_id, year, everything(), -country)

