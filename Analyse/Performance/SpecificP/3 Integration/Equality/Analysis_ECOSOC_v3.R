# Analysis Social and Economic Inequality

# Setup ####
library(rstan)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
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


# make_sig_plot(Combined_ECO, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
# make_sig_plot(Combined_ECO, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

make_sig_plot(Combined_ECO, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_ECO, pars ="bw", prob_interval = 0.9, dep_label=dep_label)


# make table ####
make_brms_reg_table(Combined_ECO[[1]], Combined_ECO[[2]], Combined_ECO[[3]], 
                    Combined_ECO[[4]], Combined_ECO[[5]], Combined_ECO[[6]], Combined_ECO[[7]], prob_interval=0.95 )




# Predictions ####

country = "USA"
framework = Combined_ECO[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)
Combined_ECO[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_ECO[[2]]$data %>%
  filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_FeC_wi = seq(0 , 0.836, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=0.197405 ,
         eco_equal_soc_wi_lag2=0.1425147) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FeC_wi = seq(0, -0.967, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=0.197405 ,
         eco_equal_soc_wi_lag2=0.1425147) %>%
  dplyr::slice(-1)


FeC_prediction1_usa = get_dynsim(scenario1, Combined_ECO[[2]], ecm=F, simulations = 1000, 
           LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
           length_preds = dim(scenario1)[1],
           minus_wi = scenario1$eco_equal_soc[1])

FeC_prediction2_usa = get_dynsim(scenario2, Combined_ECO[[2]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1],
                             minus_wi= scenario2$eco_equal_soc[1])
p1 = FeC_prediction1_usa %>% 
  mutate(Scenario = "+ FeC") %>% 
  bind_rows(FeC_prediction2_usa %>%
              mutate(Scenario = "- FeC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Economic Equality") +
  xlab("Year") +
  ggtitle("USA")
p1


#
country = "SWE"
framework = Combined_ECO[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)
Combined_ECO[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_ECO[[2]]$data %>%
  # filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_FeC_wi = seq(0 , 0.428, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=-0.016732525 ,
         eco_equal_soc_wi_lag2=-0.07595089 ) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FeC_wi = seq(0, -1.51 , length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=-0.01673252 ,
         eco_equal_soc_wi_lag2=-0.07595089 ) %>%
  dplyr::slice(-1)


FeC_prediction1_Swe = get_dynsim(scenario1, Combined_ECO[[2]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1],
                             minus_wi = scenario1$eco_equal_soc[1])

FeC_prediction2_Swe = get_dynsim(scenario2, Combined_ECO[[2]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1],
                             minus_wi= scenario2$eco_equal_soc[1])
p2 = FeC_prediction1_Swe %>% 
  mutate(Scenario = "+ FeC") %>% 
  bind_rows(FeC_prediction2_Swe %>%
              mutate(Scenario = "- FeC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Economic Equality") +
  xlab("Year") +
  ggtitle("Sweden")


ggarrange(p1,p2)

# dim E


country = "USA"
framework = Combined_ECO[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  mutate(wi_maker = eco_equal_soc) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)

Combined_ECO[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_ECO[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , -0.799, length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=0.197405 ,
         eco_equal_soc_wi_lag2=0.1425147) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0, 0.819  , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=0.197405 ,
         eco_equal_soc_wi_lag2=0.1425147) %>%
  dplyr::slice(-1)


E_prediction1_usa_ee = get_dynsim(scenario1, Combined_ECO[[6]], ecm=F, simulations = 1000, 
                           LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                           length_preds = dim(scenario1)[1],
                           minus_wi = scenario1$wi_maker[1])

E_prediction2_usa_ee = get_dynsim(scenario2, Combined_ECO[[6]], ecm=F, simulations = 1000, 
                           LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                           length_preds = dim(scenario1)[1],
                           minus_wi= scenario2$wi_maker[1])
p3 = E_prediction1_usa_ee %>% 
  mutate(Scenario = "- Inclusion") %>% 
  bind_rows(E_prediction2_usa_ee %>%
              mutate(Scenario = "+ Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  theme_bw() +
  ylab("Economic Equality") +
  xlab("Year") +
  ggtitle("USA")


#


country = "SWE"
framework = Combined_ECO[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  mutate(wi_maker = eco_equal_soc) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)

Combined_ECO[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_ECO[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , -0.799, length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=-0.016732525 ,
         eco_equal_soc_wi_lag2=-0.07595089) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0, 0.819  , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         eco_equal_soc_wi_lag=-0.01673252 ,
         eco_equal_soc_wi_lag2=-0.07595089) %>%
  dplyr::slice(-1)


E_prediction1 = get_dynsim(scenario1, Combined_ECO[[6]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1],
                             minus_wi = scenario1$wi_maker[1])

E_prediction2 = get_dynsim(scenario2, Combined_ECO[[6]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1],
                             minus_wi= scenario2$wi_maker[1])
p4 = E_prediction1 %>% 
  mutate(Scenario = "- Inclusion") %>% 
  bind_rows(E_prediction2 %>%
              mutate(Scenario = "+ Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  theme_bw() +
  ylab("Economic Equality") +
  xlab("Year") +
  ggtitle("Sweden")

ggarrange(p1,p2, p3,p4, ncol=2, nrow=2)





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
  lag_distribution_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "gdp", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "corp", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "corr", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "trade", ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_SOC[[1]], LDV_label = "soc_equal_soc_wi_lag", IndV_label = "state", ci=interval, dep_label=dep_label, unit="sd"))

ggarrange(plotlist = c(p2))


make_sig_plot(Combined_SOC, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_SOC, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

make_sig_plot(Combined_SOC, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_SOC, pars ="bw", prob_interval = 0.9, dep_label=dep_label)

# Residual Plot ####

p2 = fitted_res_plot(Combined_SOC[[1]], title = "TSCS: Social Equality")
ppc_resid_time(Combined_SOC[[1]])
p1 = fitted_res_plot(Combined_ECO[[1]], title = "TSCS: Economic Equality")

ggarrange(p1, p2, ncol=2)


# make table ####
make_brms_reg_table(Combined_SOC[[1]], Combined_SOC[[2]], Combined_SOC[[3]], 
                    Combined_SOC[[4]], Combined_SOC[[5]], Combined_SOC[[6]], Combined_SOC[[7]], prob_interval=0.95 )


# Predicitions ####

test = Combined_SOC[[2]]$data %>% 
  group_by(country_text_id) %>% 
  summarise(soc_equal_soc = mean(soc_equal_soc, na.rm=T)) %>% 
  arrange(soc_equal_soc)

# FeC
country = "USA"
framework = Combined_SOC[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)
Combined_SOC[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_SOC[[2]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_FeC_wi = seq(0 , 0.646, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.5806665 ,
         soc_equal_soc_wi_lag2 =-0.7011444) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FeC_wi = seq(0, -0.863, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.5806665 ,
         soc_equal_soc_wi_lag2 =-0.7011444) %>%
  dplyr::slice(-1)


FeC_prediction1_usa_se = get_dynsim(scenario1, Combined_SOC[[2]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$soc_equal_soc[1])

FeC_prediction2_usa_se = get_dynsim(scenario2, Combined_SOC[[2]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$soc_equal_soc[1])
p1 = FeC_prediction1_usa_se %>% 
  mutate(Scenario = "+ FeC") %>% 
  bind_rows(FeC_prediction2_usa_se %>%
              mutate(Scenario = "- FeC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Social Equality") +
  xlab("Year") +
  ggtitle("USA")
p1


# 
country = "SWE"
framework = Combined_SOC[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)
Combined_SOC[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_SOC[[2]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_FeC_wi = seq(0 , 0.646, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag = -0.6647699 ,
         soc_equal_soc_wi_lag2 = -0.6115176) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FeC_wi = seq(0, -0.863, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag = -0.6647699 ,
         soc_equal_soc_wi_lag2 = -0.6115176) %>%
  dplyr::slice(-1)


FeC_prediction1_swe_se = get_dynsim(scenario1, Combined_SOC[[2]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$soc_equal_soc[1])

FeC_prediction2_swe_se = get_dynsim(scenario2, Combined_SOC[[2]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$soc_equal_soc[1])
p2 = FeC_prediction1_swe_se %>% 
  mutate(Scenario = "+ FeC") %>% 
  bind_rows(FeC_prediction2_swe_se %>%
              mutate(Scenario = "- FeC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Social Equality") +
  xlab("Year") +
  ggtitle("Sweden")
p2

# Fec
country = "USA"
framework = Combined_SOC[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_Fec_wi, -FKM5_Fec_wi_lag)
Combined_SOC[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_SOC[[1]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_Fec_wi = seq(0 , 0.914, length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.5806665 ,
         soc_equal_soc_wi_lag2 =-0.7011444) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_Fec_wi = seq(0, -1.18 , length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.5806665 ,
         soc_equal_soc_wi_lag2 =-0.7011444) %>%
  dplyr::slice(-1)


Fec_prediction1_usa_se = get_dynsim(scenario1, Combined_SOC[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$soc_equal_soc[1])

Fec_prediction2_usa_se = get_dynsim(scenario2, Combined_SOC[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$soc_equal_soc[1])
p3 = Fec_prediction1_usa_se %>% 
  mutate(Scenario = "+ Fec") %>% 
  bind_rows(Fec_prediction2_usa_se %>%
              mutate(Scenario = "- Fec"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Social Equality") +
  xlab("Year") +
  ggtitle("USA")
p3


# 
country = "SWE"
framework = Combined_SOC[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_Fec_wi, -FKM5_Fec_wi_lag)
Combined_SOC[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_SOC[[1]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_Fec_wi = seq(0 , 0.914, length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.6647699 ,
         soc_equal_soc_wi_lag2 =-0.6115176) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_Fec_wi = seq(0, -1.18, length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.6647699 ,
         soc_equal_soc_wi_lag2 =-0.6115176) %>%
  dplyr::slice(-1)


Fec_prediction1_swe_se = get_dynsim(scenario1, Combined_SOC[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$soc_equal_soc[1])

Fec_prediction2_swe_se = get_dynsim(scenario2, Combined_SOC[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$soc_equal_soc[1])
p4 = Fec_prediction1_swe_se %>% 
  mutate(Scenario = "+ Fec") %>% 
  bind_rows(Fec_prediction2_swe_se %>%
              mutate(Scenario = "- Fec"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Social Equality") +
  xlab("Year") +
  ggtitle("Sweden")
p4

# E
country = "USA"
framework = Combined_SOC[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)
Combined_SOC[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_SOC[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , 0.819 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.5806665 ,
         soc_equal_soc_wi_lag2 =-0.7011444) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0, -0.799 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.5806665 ,
         soc_equal_soc_wi_lag2 =-0.7011444) %>%
  dplyr::slice(-1)


E_prediction1_usa_se = get_dynsim(scenario1, Combined_SOC[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$soc_equal_soc[1])

E_prediction2_usa_se = get_dynsim(scenario2, Combined_SOC[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$soc_equal_soc[1])
p5 = E_prediction1_usa_se %>% 
  mutate(Scenario = "+ Inclusion") %>% 
  bind_rows(E_prediction2_usa_se %>%
              mutate(Scenario = "- Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Social Equality") +
  xlab("Year") +
  ggtitle("USA")
p5


# 
country = "SWE"
framework = Combined_SOC[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)
Combined_SOC[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_SOC[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , 0.819 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.6647699 ,
         soc_equal_soc_wi_lag2 =-0.6115176) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0,-0.799 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         soc_equal_soc_wi_lag =-0.6647699 ,
         soc_equal_soc_wi_lag2 =-0.6115176) %>%
  dplyr::slice(-1)


E_prediction1_swe_se = get_dynsim(scenario1, Combined_SOC[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$soc_equal_soc[1])

E_prediction2_swe_se = get_dynsim(scenario2, Combined_SOC[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "soc_equal_soc_wi_lag", LDV2 = "soc_equal_soc_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$soc_equal_soc[1])
p6 = E_prediction1_swe_se %>% 
  mutate(Scenario = "+ Inclusion") %>% 
  bind_rows(E_prediction2_swe_se %>%
              mutate(Scenario = "- Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Social Equality") +
  xlab("Year") +
  ggtitle("Sweden")


ggarrange(p1, p2, p3, p4, p5, p6, ncol=2, nrow=3)

