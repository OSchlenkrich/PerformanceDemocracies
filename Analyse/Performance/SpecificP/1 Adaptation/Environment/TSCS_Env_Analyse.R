# Analysis Environmental Performance

# Setup ####
library(rstan)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_env_v4.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_env.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/TSCS_imputation_Environment.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")


GEP_mod_list = loadBRMS("GEP_mod_list", "Environment")

GEP_mod_list[[1]][[1]]
GEP_mod_list[[2]][[1]]
GEP_mod_list[[3]][[1]]
GEP_mod_list[[4]][[1]]
GEP_mod_list[[5]][[1]]
GEP_mod_list[[6]][[1]]
GEP_mod_list[[7]][[1]]



# Get Ns
GEP_mod_list[[1]][[1]]$data %>% 
  summarise(countries = n_distinct(country_text_id),
            years = n_distinct(year_0),
            N = n())

tscs_data %>% 
  select(GEP_env, country, year) %>% 
  na.omit() %>% 
  summarise(min(year), max(year))

# Create Dataset ####
Combined_GEP = list()
for (i in 1:length(GEP_mod_list)) {
  Combined_GEP[[i]] = combine_models(GEP_mod_list[[i]][[1]], 
                                     GEP_mod_list[[i]][[2]], 
                                     GEP_mod_list[[i]][[3]], 
                                     GEP_mod_list[[i]][[4]], 
                                     GEP_mod_list[[i]][[5]],
                                     check_data = F)
}
rm(GEP_mod_list)


# Lag Distribution ####

interval = 0.95
dep_label = "GEP"

plotlist = list() 
for (i in 1:length(Combined_GEP)) {
  plotlist[[i]] = lag_distribution_both(Combined_GEP[[i]], LDV_label = "GEP_env_wi_lag", IndV_label = "FKM", 
                                         ci=interval, dep_label=dep_label, unit="SD")
}
ggarrange(plotlist = plotlist)

p2 = list(
  lag_distribution_both(Combined_GEP[[1]], LDV_label = "GEP_env_wi_lag", IndV_label = "gdp", ci=interval, dep_label=dep_label, unit="SD"),
  lag_distribution_both(Combined_GEP[[1]], LDV_label = "GEP_env_wi_lag", IndV_label = "corp", ci=interval, dep_label=dep_label, unit="SD"),
  lag_distribution_both(Combined_GEP[[1]], LDV_label = "GEP_env_wi_lag", IndV_label = "corr", ci=interval, dep_label=dep_label, unit="SD"),
  lag_distribution_both(Combined_GEP[[1]], LDV_label = "GEP_env_wi_lag", IndV_label = "trade", ci=interval, dep_label=dep_label, unit="SD"),
  lag_distribution_both(Combined_GEP[[1]], LDV_label = "GEP_env_wi_lag", IndV_label = "state", ci=interval, dep_label=dep_label, unit="SD"))

ggarrange(plotlist = c(p2))


make_sig_plot(Combined_GEP, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_GEP, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

make_sig_plot(Combined_GEP, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_GEP, pars ="bw", prob_interval = 0.9, dep_label=dep_label)


# Residual Plot ####

fitted_res_plot(Combined_GEP[[1]], title = "TSCS: GEP")
ppc_resid_time(Combined_GEP[[1]])



# make table ####
make_brms_reg_table(Combined_GEP[[1]], Combined_GEP[[2]], Combined_GEP[[3]], 
                    Combined_GEP[[4]], Combined_GEP[[5]], Combined_GEP[[6]], Combined_GEP[[7]], prob_interval=0.95 )




# Predictions ####
#  CHE
# AUS
test = Combined_GEP[[6]]$data %>% 
  group_by(country_text_id) %>% 
  summarise(GEP_env = mean(GEP_env, na.rm=T),
            FKM4_E_bw = mean(FKM4_E_bw, na.rm=T)) %>% 
  arrange(GEP_env)

# E
country = "CHE"
framework = Combined_GEP[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)
Combined_GEP[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_GEP[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , 0.903   , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 14,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         GEP_env_wi_lag =-1.302396 ) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0, -0.860 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 14,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         GEP_env_wi_lag = -1.302396) %>%
  dplyr::slice(-1)


E_prediction1_che = get_dynsim(scenario1, Combined_GEP[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "GEP_env_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$GEP_env[1])

E_prediction2_che = get_dynsim(scenario2, Combined_GEP[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "GEP_env_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$GEP_env[1])
p1 = E_prediction1_che %>% 
  mutate(Scenario = "+ Inclusion") %>% 
  bind_rows(E_prediction2_che %>%
              mutate(Scenario = "- Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("GEP") +
  xlab("Year") +
  ggtitle("CHE") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p1

# E
country = "AUS"
framework = Combined_GEP[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)
Combined_GEP[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_GEP[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , 0.903   , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 14,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         GEP_env_wi_lag = -0.7745191 ) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0, -0.860 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 14,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         GEP_env_wi_lag =  -0.7745191) %>%
  dplyr::slice(-1)


E_prediction1_aus = get_dynsim(scenario1, Combined_GEP[[6]], ecm=F, simulations = 1000, 
                               LDV1 = "GEP_env_wi_lag", 
                               length_preds = dim(scenario1)[1],
                               minus_wi = scenario1$GEP_env[1])

E_prediction2_aus = get_dynsim(scenario2, Combined_GEP[[6]], ecm=F, simulations = 1000, 
                               LDV1 = "GEP_env_wi_lag", 
                               length_preds = dim(scenario1)[1],
                               minus_wi= scenario2$GEP_env[1])
p2 = E_prediction1_aus %>% 
  mutate(Scenario = "+ Inclusion") %>% 
  bind_rows(E_prediction2_aus %>%
              mutate(Scenario = "- Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("GEP") +
  xlab("Year") +
  ggtitle("AUS") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p2

ggarrange(p1, p2, ncol=2, common.legend = T, legend="bottom")
