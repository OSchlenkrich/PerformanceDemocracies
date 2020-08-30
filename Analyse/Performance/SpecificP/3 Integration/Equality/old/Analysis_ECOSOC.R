# Analysis Social and Economic Inequality

# Setup ####
library(rstan)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")



# Economic Inequality ####
ECO_list = loadBRMS("ECO_list", "Equality")

ECO_list[[1]][[1]]
ECO_list[[2]][[1]]
ECO_list[[3]][[1]]
ECO_list[[4]][[1]]
ECO_list[[5]][[1]]
ECO_list[[6]][[1]]
ECO_list[[7]][[1]]


# Get Ns
ECO_list[[1]][[1]]$data %>% 
  summarise(countries = n_distinct(country_text_id),
            years = n_distinct(year_0),
            N = n())

tscs_data %>% 
  select(soc_equal_soc, country, year) %>% 
  na.omit() %>% 
  summarise(min(year), max(year))


# Create Dataset ####
Combined_ECO = list()
for (i in 1:length(ECO_list)) {
  Combined_ECO[[i]] = combine_models(ECO_list[[i]][[1]], 
                                     ECO_list[[i]][[2]], 
                                     ECO_list[[i]][[3]], 
                                     ECO_list[[i]][[4]], 
                                     ECO_list[[i]][[5]],
                                     check_data = F)
}


# Lag Distribution ####

interval = 0.95
dep_label = "Economic Equality"

plotlist = list() 
for (i in 1:length(Combined_ECO)) {
  plotlist[[i]] = lag_distribution_both(Combined_ECO[[i]], LDV_label = "eco_equal_soc_wi_lag", IndV_label = "FKM", 
                                         ci=interval, dep_label=dep_label, unit="sd")
}
ggarrange(plotlist = plotlist)

p2 = list(
  lag_distribution_both(Combined_ECO[[1]], LDV_label = "eco_equal_soc_wi_lag", IndV_label = "gdp", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_ECO[[1]], LDV_label = "eco_equal_soc_wi_lag", IndV_label = "corp", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_ECO[[1]], LDV_label = "eco_equal_soc_wi_lag", IndV_label = "corr", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_ECO[[1]], LDV_label = "eco_equal_soc_wi_lag", IndV_label = "trade", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_ECO[[1]], LDV_label = "eco_equal_soc_wi_lag", IndV_label = "state", ci=interval, dep_label=dep_label, unit="sd"))

ggarrange(plotlist = c(p2))


make_sig_plot(Combined_ECO, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_ECO, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

make_sig_plot(Combined_ECO, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_ECO, pars ="bw", prob_interval = 0.9, dep_label=dep_label)


# make table ####
make_brms_reg_table(Combined_ECO[[1]], Combined_ECO[[2]], Combined_ECO[[3]], 
                    Combined_ECO[[4]], Combined_ECO[[5]], Combined_ECO[[6]], Combined_ECO[[7]], prob_interval=0.95 )


# Predictions ####
Combined_ECO[[2]]$data %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.numeric, funs(mean (., na.rm=T))) %>% 
  select(country_text_id, eco_equal_soc) %>% 
  arrange(-eco_equal_soc)

mean_values = Combined_ECO[[2]]$data %>% 
  #filter(country_text_id == "USA") %>% 
  summarise_if(is.numeric, funs(mean (., na.rm=T))) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)


Combined_ECO[[2]]$data %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.01, na.rm=T),
                                Q95= quantile (., probs=0.99, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM")))

framework = Combined_ECO[[2]]$data %>% 
  filter(country_text_id == "SWE") %>% 
  mutate(eco_equal_soc_minus = mean(eco_equal_soc)) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)

framework = Combined_ECO[[2]]$data %>% 
  filter(country_text_id == "SWE") %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)

framework = Combined_ECO[[2]]$data %>% 
  filter(country_text_id == "SWE") %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  mutate(eco_equal_soc = mean(eco_equal_soc)) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)

scenario1 = data.frame(FKM5_FeC_wi = seq(0 , 1.025377, length.out = 15)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = "SWE",
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FeC_wi = seq(0, -1.526495, length.out = 15)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = "SWE",
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0) %>%
  dplyr::slice(-1)


FeC_prediction1 = get_dynsim(scenario1, Combined_ECO[[2]], ecm=F, simulations = 1000, 
           LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
           length_preds = dim(scenario1)[1],
           minus_wi = scenario1$eco_equal_soc[1])

FeC_prediction2 = get_dynsim(scenario2, Combined_ECO[[2]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1],
                             minus_wi= scenario2$eco_equal_soc[1])
FeC_prediction1 %>% 
  mutate(Scenario = "1") %>% 
  bind_rows(FeC_prediction2 %>%
              mutate(Scenario = "2"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() 


scenario2 = data.frame(country_text_id = NA, 
                       FKM5_FeC_wi = seq(1.226443, -3.031521, length.out = 15)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(mean_values) %>% 
  mutate(eco_equal_soc=NA,
         country_text_id=NA,
         year_0 = 18,
         FKM5_FeC_bw = -1)

scenario3 = data.frame(country_text_id = NA, 
                       FKM5_FeC_wi = seq(-3.031521, 1.226443, length.out = 15)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(mean_values) %>% 
  mutate(eco_equal_soc=NA,
         country_text_id=NA,
         year_0 = 18,
         FKM5_FeC_bw = 1)

scenario4 = data.frame(country_text_id = NA, 
                       FKM5_FeC_wi = seq(1.226443, -3.031521, length.out = 15)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(mean_values) %>% 
  mutate(eco_equal_soc=NA,
         country_text_id=NA,
         year_0 = 18,
         FKM5_FeC_bw = 1)

p_sc1 = get_prediction(scenario1, brms_model = Combined_ECO[[2]])
p_sc2 = get_prediction(scenario2, brms_model = Combined_ECO[[2]])
p_sc3 = get_prediction(scenario3, brms_model = Combined_ECO[[2]])
p_sc4 = get_prediction(scenario4, brms_model = Combined_ECO[[2]])

p_sc1 %>% 
  select(year_0, sc1 = eco_equal_soc) %>% 
  bind_cols(p_sc2 %>% 
              select(sc2 = eco_equal_soc) ) %>% 
  bind_cols(p_sc3 %>% 
              select(sc3 = eco_equal_soc) ) %>% 
  bind_cols(p_sc4 %>% 
              select(sc4 = eco_equal_soc) ) %>% 
  select(year_0, sc1, sc2, sc3, sc4) %>%
  na.omit() %>% 
  pivot_longer(cols=-year_0) %>% 
  ggplot(aes(x=year_0, y=value, col=name)) +
  geom_line(size=1.1) +
  theme_bw()



# dim E

summary(Combined_ECO[[6]]$data)


mean_values = Combined_ECO[[6]]$data %>% 
  #filter(country_text_id == "USA") %>% 
  summarise_if(is.numeric, funs(mean (., na.rm=T))) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)


scenario1 = data.frame(country_text_id = NA, 
                       FKM4_E_wi = rep(-2.649857, 15)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(mean_values) %>% 
  mutate(eco_equal_soc=NA,
         country_text_id=NA,
         year_0 = 18,
         FKM4_E_bw = 2)

scenario2 = data.frame(country_text_id = NA, 
                       FKM4_E_wi =  rep(2.876473, 15)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(mean_values) %>% 
  mutate(eco_equal_soc=NA,
         country_text_id=NA,
         year_0 = 18,
         FKM4_E_bw = -2)

p_sc1 = get_prediction(scenario1, brms_model = Combined_ECO[[6]])
p_sc2 = get_prediction(scenario2, brms_model = Combined_ECO[[6]])

p_sc1 %>% 
  select(year_0, sc1 = eco_equal_soc) %>% 
  bind_cols(p_sc2 %>% 
              select(sc2 = eco_equal_soc) ) %>% 
  select(year_0, sc1, sc2) %>%
  na.omit() %>% 
  pivot_longer(cols=-year_0) %>% 
  ggplot(aes(x=year_0, y=value, col=name)) +
  geom_line(size=1.1) +
  theme_bw()



# Social Inequality ####
SOC_list = loadBRMS("SOC_list_v2", "Equality")

SOC_list[[1]][[1]]
SOC_list[[2]][[1]]
SOC_list[[3]][[1]]
SOC_list[[4]][[1]]
SOC_list[[5]][[1]]
SOC_list[[6]][[1]]
SOC_list[[7]][[1]]


Combined_SOC = list()
for (i in 1:length(SOC_list)) {
  Combined_SOC[[i]] = combine_models(SOC_list[[i]][[1]], 
                                     SOC_list[[i]][[2]], 
                                     SOC_list[[i]][[3]], 
                                     SOC_list[[i]][[4]], 
                                     SOC_list[[i]][[5]],
                                     check_data = F)
}

Combined_SOC[[1]]
Combined_SOC[[2]]
Combined_SOC[[3]]
Combined_SOC[[4]]
Combined_SOC[[5]]
Combined_SOC[[6]]
Combined_SOC[[7]]


# Lag Distribution ####

interval = 0.95
dep_label = "Social Equality"

plotlist = list() 
for (i in 1:length(Combined_SOC)) {
  plotlist[[i]] = lag_distribution_both(Combined_SOC[[i]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "FKM", 
                                         ci=interval, dep_label=dep_label, unit="sd")
}
ggarrange(plotlist = plotlist)

p2 = list(
  lag_distribution_bayes_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "gdp", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_bayes_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "corp", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_bayes_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "corr", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_bayes_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "trade", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_bayes_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "state", ci=interval, dep_label=dep_label, unit="sd"))

ggarrange(plotlist = c(p2))


make_sig_plot(Combined_SOC, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_SOC, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

make_sig_plot(Combined_SOC, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_SOC, pars ="bw", prob_interval = 0.9, dep_label=dep_label)


# make table ####
make_brms_reg_table(Combined_SOC[[1]], Combined_SOC[[2]], Combined_SOC[[3]], 
                    Combined_SOC[[4]], Combined_SOC[[5]], Combined_SOC[[6]], Combined_SOC[[7]], prob_interval=0.95 )


# Predicitions
sc1 = Combined_SOC[[6]]$data %>% 
  filter(country_text_id == "USA") %>% 
  mutate(FKM4_E_bw = 2)
sc2 = Combined_SOC[[6]]$data %>% 
  filter(country_text_id == "USA") %>% 
  mutate(FKM4_E_bw = -2)

p_sc1 = data.frame(predict( Combined_SOC[[6]], newdata=sc1)) %>%  
  rename(SC1 = Estimate)
p_sc2 = data.frame(predict( Combined_SOC[[6]], newdata=sc2)) %>%  
  rename(SC2 = Estimate)

p_sc1 %>% 
  bind_cols(p_sc2) %>% 
  bind_cols(Combined_SOC[[6]]$data %>% 
              filter(country_text_id == "USA")) %>% 
  select(year_0, SC1, SC2) %>% 
  pivot_longer(cols=-year_0) %>% 
  ggplot(aes(x=year_0, y=value, col=name)) +
  geom_line(size=1.1) +
  theme_bw()