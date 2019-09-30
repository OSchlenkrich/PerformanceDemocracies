dmx_all = fread("C:/RTest/DemocracyMatrix_v1_1_incl_VDem.csv")


test = dmx_all %>% 
  select_at(vars(country, year, matches("trade_off"), matches("ghallager")))

hist(test$rules_settlement_freedom_trade_off_facto)
hist(test$rules_settlement_control_trade_off_facto)
hist(test$rights_control_trade_off_facto)

test %>% 
  select(var = decision_equality_trade_off_facto) %>% 
  ggplot(aes(x=var)) + 
  geom_histogram()

Pathways = dmx_trade_cluster %>% 
  filter(year > 1945) %>% 
  arrange(year) %>% 
  group_by(country) %>% 
  distinct(cluster_label_1st) %>% 
  summarize(Paths = paste(cluster_label_1st, collapse = "->"))
  
table(Pathways$Paths)


test_nyears = dmx_trade_cluster %>% 
  filter(year > 1945) %>% 
  arrange(year) %>%
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         change = if_else(cluster_label_1st == cluster_label_1st_lag, 1, 0)) %>% 
  na.omit() %>%
  group_by(country) %>% 
  mutate(change_test = marking(change)) %>% 
  group_by(country, change_test) %>% 
  summarise(n_years = n())
  
test = dmx_trade_cluster %>% 
  filter(year > 1945) %>% 
  arrange(country, year) %>%
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         change = if_else(cluster_label_1st == cluster_label_1st_lag, 1, 0)) %>% 
  group_by(country) %>% 
  mutate(change_test = marking(change)) %>% 
  group_by(country, change_test) %>% 
  slice(1) %>% 
  left_join(test_nyears, by=c("country", "change_test")) %>% 
  filter(n_years >= 5) %>% 
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         change = if_else(cluster_label_1st == cluster_label_1st_lag, 1, 0)) %>% 
  group_by(country) %>% 
  mutate(change_test = marking(change))  %>% 
  group_by(country, change_test, cluster_label_1st) %>% 
  summarise(n_years = sum(n_years)) %>% 
  group_by(country) %>% 
  summarize(Paths = paste(cluster_label_1st, collapse = "->"),
            PathsYears = paste(cluster_label_1st, "(", n_years, ")", collapse = "->"))

table(test$Paths)


marking = function(change) {
  marker = 1
  if (length(change) == 1) {
    change = 1
  } else {
    for (i in 2:length(change)) {
      change[i-1] = marker
      if (change[i] == 0) {
        marker = marker + 1
        if (i == length(change)) {
          change[i] = marker
        }
      }
    }    
  }
  return(change)
}



NoYears = dmx_trade_cluster %>% 
  filter(year > 1945) %>% 
  arrange(year) %>%
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         change = if_else(cluster_label_1st == cluster_label_1st_lag, 1, 0)) %>% 
  na.omit() %>%
  group_by(country) %>% 
  mutate(change_test = marking(change)) %>% 
  group_by(country, change_test) %>% 
  summarise(NoYears = n()) 



NoYearsC = dmx_trade_cluster %>% 
  filter(year > 1945) %>% 
  arrange(year) %>%
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         change = if_else(cluster_label_1st == cluster_label_1st_lag, 1, 0)) %>% 
  na.omit() %>%
  group_by(country) %>% 
  mutate(change_test = marking(change)) %>% 
  group_by(country) %>% 
  summarise(NoYearsC = n()) 


Perc_years = NoYears %>% 
  left_join(NoYearsC, by=c("country")) %>% 
  mutate(perc = NoYears/NoYearsC) %>% 
  filter(perc > 0.1)



test = dmx_trade_cluster %>% 
  filter(year > 1945) %>% 
  arrange(year) %>%
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         change = if_else(cluster_label_1st == cluster_label_1st_lag, 1, 0)) %>% 
  na.omit() %>%
  group_by(country) %>% 
  mutate(change_test = marking(change)) %>% 
  group_by(country, change_test) %>% 
  slice(1)  %>% 
  right_join(Perc_years, by=c("country", "change_test")) %>% 
  group_by(country) %>% 
  summarize(Paths = paste(cluster_label_1st, collapse = "->"))

sum(table(test$Paths))

table(test$Paths)

###

dmx_trade_cluster %>% 
  filter(year > 1945) %>% 
  arrange(year) %>%
  select(country, year, cluster_label_1st) %>% 
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         change = if_else(cluster_label_1st == cluster_label_1st_lag, 1, 0)) %>% 
  na.omit() %>%
  group_by(country) %>% 
  mutate(change_test = marking(change)) %>% 
  group_by(country, change_test) %>% 
  slice(1)  %>% 
  right_join(Perc_years, by=c("country", "change_test")) %>% 
  ungroup() %>% 
  select(country, change_test, cluster_label_1st) %>%
  group_by(country) %>% 
  mutate(cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1)) %>% 
  ungroup() %>% 
  select(cluster_label_1st, cluster_label_1st_lag) %>% 
  table() %>% 
  as.matrix() %>% 
  chorddiag()

library(chorddiag)
diag(m1) = 0
chorddiag(m1)