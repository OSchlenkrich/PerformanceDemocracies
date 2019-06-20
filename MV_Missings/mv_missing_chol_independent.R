## Gini Stan with MV missings (PCSE)
NA_maker = function(x) {
  x = if_else(is.na(x)==T, -99, x)
}
NA_identifier = function(x) {
  x = if_else(x==-99, NA_real_, 1)
}

NA_make_fct = function(x) {
  x = fct_recode(x, NULL =  "NA")
}

Stan_identifier = function(x) {
  x = if_else(is.na(x), 0, 1)
}

year_begin = 1962

y_GINI_wide = GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin) %>% 
  select(country, year, Gini) %>%
  spread("country", "Gini") %>% 
  mutate_all(NA_maker) %>% 
  select(-year) 

lag_GINI_wide = GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin) %>% 
  select(country, year, Gini_lag) %>% 
  spread("country", "Gini_lag") %>% 
  mutate_all(NA_maker) %>% 
  select(-year) 


cso_lag_wide = GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin) %>% 
  select(country, year, cso_lag) %>% 
  spread("country", "cso_lag") %>% 
  mutate_all(NA_maker) %>% 
  select(-year) 

age65_lag_wide = GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin) %>% 
  select(country, year, age65_lag) %>% 
  spread("country", "age65_lag") %>% 
  mutate_all(NA_maker) %>% 
  select(-year) 


GINI_ident_nas = y_GINI_wide %>% 
  mutate_all(NA_identifier) 
GINI_lag_ident_nas = lag_GINI_wide %>% 
  mutate_all(NA_identifier)  
cso_lag_wide_ident_nas = cso_lag_wide %>% 
  mutate_all(NA_identifier) 
age65_lag_wide_ident_nas = age65_lag_wide %>% 
  mutate_all(NA_identifier)


ident_nas = (GINI_ident_nas + GINI_lag_ident_nas + cso_lag_wide_ident_nas + age65_lag_wide_ident_nas)%>% 
  mutate_all(Stan_identifier)

gini_length_obs = apply(ident_nas, 1, sum)

# predictor

lag_GINI_pred <- stats::model.matrix(~ 1 +Gini_lag +  cso_lag + age65_lag + mod_cluster_1st, 
                                     data = GINI_df_data_all %>% 
                                      ungroup() %>% 
                                      filter(year >= year_begin))[,-1]


country_index =GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin) %>% 
  group_indices(country)

time_index = GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin) %>% 
  group_indices(year)

time_index_array = array(0, dim=c(max(time_index), max(country_index)))
for (i in 1:max(time_index)) {
  get_length = length(which(time_index == i))
  time_index_array[i,1:get_length] =  which(time_index == i)
}

#N-GINI

y_GINI_long = GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin, country %in% colnames(y_GINI_wide)) %>% 
  pull(Gini)

gini_input_stan = list( 
  T = dim(y_GINI_wide)[1],
  G = dim(y_GINI_wide)[2],
  N = length(country_index),
  
  y = y_GINI_long,
  X = lag_GINI_pred,
  
  y_obs_id = ident_nas,
  y_obs_id_vec = ident_nas,
  length_obs = gini_length_obs,
  
  beta_nr = dim(lag_GINI_pred)[2],
  ranef_nr = 1,

  country_index = country_index,
  time_index = time_index_array
  
)


M = stan(file = 'MV_Missings/mv_missing_chol_independent_eff.stan', 
         chains = 4,
         warmup = 1000,
         iter = 2000,
         cores = 4,
         data = gini_input_stan,
         init=0)

print(M)
print(M, "mu_lvl2")
print(M, "beta")
print(M, "sigma_lvl2")
print(M, "mu[9,1]")
print(M, "mu_obs[1,1]")
print(M, "Sigma")


library(onehot)
test = GINI_df_data_all %>% 
  ungroup() %>% 
  filter(year >= year_begin) %>% 
  select(mod_cluster_1st) %>% 
  onehot()
mod_predictor = predict(test, GINI_df_data_all  %>% 
          ungroup() %>% 
          filter(year >= year_begin) ) %>% 
  as.data.frame() %>% 
  select(-1)


# Design Matrix
dummy_variables <- stats::model.matrix(~ 1 + mod_cluster_1st, data = GINI_df_data_all %>% 
                                  ungroup() %>% 
                                  filter(year >= year_begin))
X <- model.matrix(outcome ~ predictor1 + predictor2 ..., data = your_dataset)

table(GINI_df_data_all$mod_cluster_1st)
