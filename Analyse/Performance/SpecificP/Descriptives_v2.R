source("Analyse/CreateDatasets.R")

# Setup ####
OECD_member = fread("Datasets/OECD_member.csv") %>% 
  left_join(V_dem %>%  select(country_name = country, country_text_id) %>%  distinct(), by="country_name") %>% 
  select(country_text_id, OECD, OECD_founder)


scores_eco_performance = performance_wdi_eco %>% 
  select(country_text_id, year, wealth_eco, productivity_eco) %>% 
  right_join(dmx_trade_cluster %>%  select(-country, -regions), by=c("country_text_id", "year")) %>% 
  group_by(country_text_id) %>% 
  filter(year >= 1950) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA)) %>% 
  ungroup() %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  left_join(performance_env_scores %>%  select(country_text_id, year, air_env, abstraction_env), by=c("country_text_id", "year")) %>% 
  left_join(performance_ds %>%  select(country_text_id, year, pubsafe_ds), by=c("country_text_id", "year")) %>% 
  mutate_at(vars(ends_with("_eco"), ends_with("_env"), ends_with("_ds")), ~EPI_fun(., lower = 0.01, upper=0.99))


# NA values ####
scores_eco_performance %>% 
  filter(is.na(classification_core) == F) %>% 
  group_by(year) %>% 
  select_at(vars(wealth_eco, productivity_eco)) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample - Economic Performance (OECD)")

# ECONOMY ####
# Wealth ####
# Ranking 
frame_coef = scores_eco_performance %>%
  select(country_text_id, year,  wealth_eco, productivity_eco) %>% 
  na.omit() %>% 
  distinct(country_text_id) %>% 
  mutate(trend_wealth = NA,
         trend_prod = NA)
for (i in 1:dim(frame_coef)[1]) {
  scores_eco_performance_lm = scores_eco_performance %>% 
    filter(country_text_id == frame_coef$country_text_id[i]) 
  scores_eco_performance_lm$year = scale(scores_eco_performance_lm$year)[,1]
  m1_lm = lm(wealth_eco ~ year, scores_eco_performance_lm)
  m2_lm = lm(productivity_eco ~ year, scores_eco_performance_lm)
  
  frame_coef$trend_wealth[i] = round(coef(m1_lm)[2],2)
  frame_coef$trend_prod[i] = round(coef(m2_lm)[2],2)
  
}



table_complete = scores_eco_performance %>%
  select(country_text_id, year,  wealth_eco, productivity_eco) %>% 
  na.omit() %>% 
  filter(year >= 1980) %>% 
  mutate(year5 = floor(year/5)*5,
         #year5 = ifelse(year5 == 2015, 2010, year5)
         ) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_eco_performance = mean(wealth_eco, na.rm=T)) %>% 
  mutate(mean_eco_performance = round(mean_eco_performance, 1)) %>%
  arrange(country_text_id, year5) %>% 
  pivot_wider(names_from = year5, values_from = mean_eco_performance) %>% 
  select(country_text_id, `1980`, `1985`, everything()) %>% 
  left_join(frame_coef %>%  select(country_text_id, trend_wealth), by="country_text_id") %>% 
  bind_rows(total_scores = scores_eco_performance %>%
              select(country_text_id, year,  wealth_eco, productivity_eco) %>% 
              na.omit() %>% 
              filter(year >= 1980) %>% 
              mutate(country_text_id = "All countries", year5 = floor(year/5)*5,
                     #year5 = ifelse(year5 == 2015, 2010, year5)
              ) %>% 
              group_by(country_text_id, year5) %>% 
              summarise(wealth_prod = mean(wealth_eco, na.rm=T)) %>% 
              mutate(wealth_prod = round(wealth_prod, 1))  %>% 
              pivot_wider(names_from = year5, values_from = c(wealth_prod)) ) %>% 
  ungroup()

split1 = table_complete %>% 
  slice(1:ceiling(((dim(table_complete)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
split2 = table_complete %>% 
  anti_join(split1)  %>% 
  slice(1:ceiling(((dim(table_complete)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
# split3 = table_complete %>% 
#   anti_join(split1)  %>% 
#   anti_join(split2)  %>% 
#   mutate(id = 1:dim(.)[1])

wide_table_df =full_join(split1, split2, by="id") %>% 
  #full_join(split3, by="id") %>% 
  select(-id)

dust(wide_table_df)  %>% 
  sprinkle_colnames("country",
                    "1980",
                    "1985",
                    "1990",
                    "1995",
                    "2000",
                    "2005",
                    "2010",
                    "2015",
                    "trend") %>%
  sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
  sprinkle(rows = 2:dim(wide_table_df)[1], border = "top", border_color = "black", border_thickness=1) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, part="head") %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, pad=5) %>%
  sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3) %>%
  sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-2, border = "right", border_color = "black", border_thickness=3) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-2, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-1, border = "right", border_color = "black", border_thickness=2) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(font_size = 9, font_size_units = "pt", part="head") %>%
  sprinkle(font_size = 8, font_size_units = "pt", part="body") %>%
  sprinkle_na_string(na_string = "") %>% 
  sprinkle_print_method("html")

# Top Performer
table_complete %>% 
  arrange(-trend_wealth)
table_complete %>% 
  arrange(-`2010`)

# Development
scores_eco_performance %>%
  filter(year >= 1980) %>% 
  select_at(vars(year, matches("wealth_eco"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw() +
  ggtitle("Economic Performace - Wealth")

scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("wealth_eco"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw()  +
  ggtitle("Economic Performace (Wealth) of OECD founding states")


scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("wealth_eco"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(sd), na.rm=T) %>% 
  ggplot(aes(x=year, y=wealth_eco)) +
  geom_line(size=1) +
  geom_point() +
  #ylim(0,100) +
  theme_bw()  +
  ggtitle("Economic Performace (Wealth) of OECD founding states (SD)")


# Productivity ####
# Ranking
table_complete_prod = scores_eco_performance %>%
  select(country_text_id, year,  wealth_eco, productivity_eco) %>% 
  na.omit() %>% 
  filter(year >= 1980) %>% 
  mutate(year5 = floor(year/5)*5,
         #year5 = ifelse(year5 == 2015, 2010, year5)
  ) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_eco_performance = mean(productivity_eco, na.rm=T)) %>% 
  mutate(mean_eco_performance = round(mean_eco_performance, 1)) %>%
  arrange(country_text_id, year5) %>% 
  pivot_wider(names_from = year5, values_from = mean_eco_performance) %>% 
  select(country_text_id, `1980`, `1985`, everything()) %>% 
  left_join(frame_coef %>%  select(country_text_id, trend_prod), by="country_text_id") %>% 
  bind_rows(total_scores = scores_eco_performance %>%
              select(country_text_id, year,  wealth_eco, productivity_eco) %>% 
              na.omit() %>% 
              filter(year >= 1980) %>% 
              mutate(country_text_id = "All countries", year5 = floor(year/5)*5,
                     #year5 = ifelse(year5 == 2015, 2010, year5)
              ) %>% 
              group_by(country_text_id, year5) %>% 
              summarise(productivity_eco = mean(productivity_eco, na.rm=T)) %>% 
              mutate(productivity_eco = round(productivity_eco, 1))  %>% 
              pivot_wider(names_from = year5, values_from = c(productivity_eco)) ) %>% 
  ungroup()

split1_prod = table_complete_prod %>% 
  slice(1:ceiling(((dim(table_complete_prod)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
split2_prod = table_complete_prod %>% 
  anti_join(split1)  %>% 
  slice(1:ceiling(((dim(table_complete_prod)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
# split3 = table_complete_prod %>% 
#   anti_join(split1)  %>% 
#   anti_join(split2)  %>% 
#   mutate(id = 1:dim(.)[1])


wide_table_df_prod =full_join(split1, split2, by="id") %>% 
  #full_join(split3, by="id") %>% 
  select(-id)

dust(wide_table_df_prod)  %>% 
  sprinkle_colnames("country",
                    "1980",
                    "1985",
                    "1990",
                    "1995",
                    "2000",
                    "2005",
                    "2010",
                    "2015",
                    "trend") %>%
  sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
  sprinkle(rows = 2:dim(wide_table_df)[1], border = "top", border_color = "black", border_thickness=1) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, part="head") %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, pad=5) %>%
  sprinkle(col = dim(split1_prod)[2]-1, border = "right", border_color = "black", border_thickness=3) %>%
  sprinkle(col = dim(split1_prod)[2]-1, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  sprinkle(col = dim(split1_prod)[2], border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = dim(split1_prod)[2], border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  # sprinkle(col = dim(split1_prod)[2] + dim(split2_prod)[2]-2, border = "right", border_color = "black", border_thickness=3) %>%
  # sprinkle(col = dim(split1_prod)[2] + dim(split2_prod)[2]-2, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  # sprinkle(col = dim(split1_prod)[2] + dim(split2_prod)[2]-1, border = "right", border_color = "black", border_thickness=2) %>%
  # sprinkle(col = dim(split1_prod)[2] + dim(split2_prod)[2]-1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(font_size = 9, font_size_units = "pt", part="head") %>%
  sprinkle(font_size = 8, font_size_units = "pt", part="body") %>%
  sprinkle_na_string(na_string = "") %>% 
  sprinkle_print_method("html")

# Top Performer
table_complete_prod %>% 
  arrange(-trend_prod)
table_complete_prod %>% 
  arrange(-`2010`)

scores_eco_performance %>%
  filter(year >= 1980) %>% 
  select_at(vars(year, matches("productivity_eco"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw() +
  ggtitle("Economic Performace - Productivity")


scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("productivity_eco"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw()  +
  ggtitle("Economic Performace (Productivity) of OECD founding states")

scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("productivity_eco"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(sd), na.rm=T) %>% 
  ggplot(aes(x=year, y=productivity_eco)) +
  geom_line(size=1) +
  geom_point() +
  ylim(0,100) +
  theme_bw()  +
  ggtitle("Economic Performace (Productivity) of OECD founding states (SD)")

# ENVIRONMENT ####
# Ranking 
frame_coef = scores_eco_performance %>%
  select(country_text_id, year,  air_env, abstraction_env) %>% 
  na.omit() %>% 
  distinct(country_text_id) %>% 
  mutate(trend_air = NA,
         trend_abstraction = NA)


for (i in 1:dim(frame_coef)[1]) {
  scores_eco_performance_lm = scores_eco_performance %>% 
    filter(country_text_id == frame_coef$country_text_id[i]) 
  scores_eco_performance_lm$year = scale(scores_eco_performance_lm$year)[,1]
  m1_lm = lm(air_env ~ year, scores_eco_performance_lm)
  m2_lm = lm(abstraction_env ~ year, scores_eco_performance_lm)
  
  frame_coef$trend_air[i] = round(coef(m1_lm)[2],2)
  frame_coef$trend_abstraction[i] = round(coef(m2_lm)[2],2)
  
}



table_env_complete = scores_eco_performance %>%
  select(country_text_id, year, air_env) %>% 
  na.omit() %>% 
  filter(year >= 1990) %>% 
  mutate(year5 = floor(year/5)*5,
         #year5 = ifelse(year5 == 2015, 2010, year5)
  ) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_env_performance = mean(air_env, na.rm=T)) %>% 
  mutate(mean_env_performance = round(mean_env_performance, 1)) %>%
  arrange(country_text_id, year5) %>% 
  pivot_wider(names_from = year5, values_from = mean_env_performance) %>% 
  select(country_text_id, `1990`, `1995`, everything()) %>% 
  left_join(frame_coef %>%  select(country_text_id, trend_air), by="country_text_id") %>% 
  bind_rows(scores_eco_performance %>%
              select(country_text_id, year,  air_env) %>% 
              na.omit() %>% 
              filter(year >= 1990) %>% 
              mutate(country_text_id = "All countries", year5 = floor(year/5)*5,
                     #year5 = ifelse(year5 == 2015, 2010, year5)
              ) %>% 
              group_by(country_text_id, year5) %>% 
              summarise(env_prod = mean(air_env, na.rm=T)) %>% 
              mutate(env_prod = round(env_prod, 1))  %>% 
              pivot_wider(names_from = year5, values_from = c(env_prod)) ) %>% 
  ungroup()

split1 = table_env_complete %>% 
  slice(1:ceiling(((dim(table_env_complete)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
split2 = table_env_complete %>% 
  anti_join(split1)  %>% 
  slice(1:ceiling(((dim(table_env_complete)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
# split3 = table_env_complete %>% 
#   anti_join(split1)  %>% 
#   anti_join(split2)  %>% 
#   mutate(id = 1:dim(.)[1])

wide_table_df =full_join(split1, split2, by="id") %>% 
  #full_join(split3, by="id") %>% 
  select(-id)

dust(wide_table_df)  %>% 
  sprinkle_colnames("country",
                    "1990",
                    "1995",
                    "2000",
                    "2005",
                    "2010",
                    "2015",
                    "trend") %>%
  sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
  sprinkle(rows = 2:dim(wide_table_df)[1], border = "top", border_color = "black", border_thickness=1) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, part="head") %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, pad=5) %>%
  sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3) %>%
  sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-2, border = "right", border_color = "black", border_thickness=3) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-2, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-1, border = "right", border_color = "black", border_thickness=2) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(font_size = 9, font_size_units = "pt", part="head") %>%
  sprinkle(font_size = 8, font_size_units = "pt", part="body") %>%
  sprinkle_na_string(na_string = "") %>% 
  sprinkle_print_method("html")

# Top Performer
table_env_complete %>% 
  arrange(-trend_air)
table_env_complete %>% 
  arrange(-`2010`)

# Development
scores_eco_performance %>%
  filter(year >= 1980) %>% 
  select_at(vars(year, matches("air_env"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw() +
  ggtitle("Environmental Performace - Emissions")

scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("air_env"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw()  +
  ggtitle("Environmental Performace (Emissions) of OECD founding states")

scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("air_env"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(sd), na.rm=T) %>% 
  ggplot(aes(x=year, y=air_env)) +
  geom_line(size=1) +
  geom_point() +
  #ylim(0,100) +
  theme_bw()  +
  ggtitle("Environmental Performace (Emissions) of OECD founding states (SD)")

# PUBLIC ORDER AND SAFETY ####
# Ranking 


frame_coef = scores_eco_performance %>%
  select(country_text_id, year,  pubsafe_ds) %>% 
  na.omit() %>% 
  distinct(country_text_id) %>% 
  mutate(trend_pubsafe = NA)


for (i in 1:dim(frame_coef)[1]) {
  scores_eco_performance_lm = scores_eco_performance %>% 
    filter(country_text_id == frame_coef$country_text_id[i]) 
  scores_eco_performance_lm$year = scale(scores_eco_performance_lm$year)[,1]
  m1_lm = lm(pubsafe_ds ~ year, scores_eco_performance_lm)

  frame_coef$trend_pubsafe[i] = round(coef(m1_lm)[2],2)

}



table_ds_complete = scores_eco_performance %>%
  select(country_text_id, year, pubsafe_ds) %>% 
  na.omit() %>% 
  filter(year >= 1990) %>% 
  mutate(year5 = floor(year/5)*5,
         #year5 = ifelse(year5 == 2015, 2010, year5)
  ) %>% 
  group_by(country_text_id, year5) %>% 
  summarise(mean_ds_performance = mean(pubsafe_ds, na.rm=T)) %>% 
  mutate(mean_ds_performance = round(mean_ds_performance, 1)) %>%
  arrange(country_text_id, year5) %>% 
  pivot_wider(names_from = year5, values_from = mean_ds_performance) %>% 
  select(country_text_id, `1990`, `1995`, everything()) %>% 
  left_join(frame_coef %>%  select(country_text_id, trend_pubsafe), by="country_text_id") %>% 
  bind_rows(scores_eco_performance %>%
              select(country_text_id, year,  pubsafe_ds) %>% 
              na.omit() %>% 
              filter(year >= 1990) %>% 
              mutate(country_text_id = "All countries", year5 = floor(year/5)*5,
                     #year5 = ifelse(year5 == 2015, 2010, year5)
              ) %>% 
              group_by(country_text_id, year5) %>% 
              summarise(ds_pubsafe = mean(pubsafe_ds, na.rm=T)) %>% 
              mutate(ds_pubsafe = round(ds_pubsafe, 1))  %>% 
              pivot_wider(names_from = year5, values_from = c(ds_pubsafe)) ) %>% 
  ungroup()

split1 = table_ds_complete %>% 
  slice(1:ceiling(((dim(table_ds_complete)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
split2 = table_ds_complete %>% 
  anti_join(split1)  %>% 
  slice(1:ceiling(((dim(table_ds_complete)[1])/2))) %>% 
  mutate(id = 1:dim(.)[1])
# split3 = table_env_complete %>% 
#   anti_join(split1)  %>% 
#   anti_join(split2)  %>% 
#   mutate(id = 1:dim(.)[1])

wide_table_df =full_join(split1, split2, by="id") %>% 
  #full_join(split3, by="id") %>% 
  select(-id)

dust(wide_table_df)  %>% 
  sprinkle_colnames("country",
                    "1990",
                    "1995",
                    "2000",
                    "2005",
                    "2010",
                    "2015",
                    "trend") %>%
  sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
  sprinkle(rows = 2:dim(wide_table_df)[1], border = "top", border_color = "black", border_thickness=1) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = 1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, part="head") %>%
  sprinkle(col = 2:dim(wide_table_df)[2], border = "right", border_color = "black", border_thickness=1, pad=5) %>%
  sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3) %>%
  sprinkle(col = dim(split1)[2]-1, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2) %>%
  sprinkle(col = dim(split1)[2], border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-2, border = "right", border_color = "black", border_thickness=3) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-2, border = "right", border_color = "black", border_thickness=3, part="head", pad=5) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-1, border = "right", border_color = "black", border_thickness=2) %>%
  # sprinkle(col = dim(split1)[2] + dim(split2)[2]-1, border = "right", border_color = "black", border_thickness=2, part="head", pad=5) %>%
  sprinkle(font_size = 9, font_size_units = "pt", part="head") %>%
  sprinkle(font_size = 8, font_size_units = "pt", part="body") %>%
  sprinkle_na_string(na_string = "") %>% 
  sprinkle_print_method("html")

# Top Performer
table_env_complete %>% 
  arrange(-trend_air)
table_env_complete %>% 
  arrange(-`2010`)

# Development
scores_eco_performance %>%
  filter(year >= 1980) %>% 
  select_at(vars(year, matches("air_env"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw() +
  ggtitle("Environmental Performace - Emissions")

scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("air_env"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(Q10 = fun_quantile10, Q50 = median, Q90=fun_quantile90), na.rm=T) %>% 
  ggplot(aes(x=year, y=Q50, ymin=Q10, ymax=Q90)) +
  geom_line(size=1) +
  geom_point() +
  geom_errorbar() +
  ylim(0,100) +
  theme_bw()  +
  ggtitle("Environmental Performace (Emissions) of OECD founding states")

scores_eco_performance %>%
  filter(year >= 1980, OECD_founder == 1) %>% 
  select_at(vars(year, matches("air_env"))) %>% 
  group_by(year) %>% 
  summarise_all(funs(sd), na.rm=T) %>% 
  ggplot(aes(x=year, y=air_env)) +
  geom_line(size=1) +
  geom_point() +
  #ylim(0,100) +
  theme_bw()  +
  ggtitle("Environmental Performace (Emissions) of OECD founding states (SD)")

