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


# make_sig_plot(Combined_ECO, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
# make_sig_plot(Combined_ECO, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

make_sig_plot(Combined_ECO, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
make_sig_plot(Combined_ECO, pars ="bw", prob_interval = 0.9, dep_label=dep_label)


# make table ####
make_brms_reg_table(Combined_ECO[[1]], Combined_ECO[[2]], Combined_ECO[[3]], 
                    Combined_ECO[[4]], Combined_ECO[[5]], Combined_ECO[[6]], Combined_ECO[[7]], prob_interval=0.95 )


sd(Combined_ECO[[1]]$data$FKM5_Fec_bw)


# Predictions ####
# Within ####
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
  filter(country_text_id == country) %>% 
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


E_prediction1_usa = get_dynsim(scenario1, Combined_ECO[[6]], ecm=F, simulations = 1000, 
                           LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                           length_preds = dim(scenario1)[1],
                           minus_wi = scenario1$wi_maker[1])

E_prediction2_usa = get_dynsim(scenario2, Combined_ECO[[6]], ecm=F, simulations = 1000, 
                           LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                           length_preds = dim(scenario1)[1],
                           minus_wi= scenario2$wi_maker[1])
p3 = E_prediction1_usa %>% 
  mutate(Scenario = "- Inclusion") %>% 
  bind_rows(E_prediction2_usa %>%
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


# Between ####

Combined_ECO[[7]]$data %>% 
  select(country_text_id, FKM4_c_bw) %>% 
  distinct() %>% 
  arrange(FKM4_c_bw)

country = "USA"
# country = "IRL"
# country = "DEU"

framework = Combined_ECO[[7]]$data  %>% 
  filter(country_text_id == country) %>% 
  #summarise_if(is.numeric, mean, na.rm=T) %>% 
  dplyr::slice(1) %>% 
  mutate_at(vars(matches("_wi"), -matches("eco_equal_soc_wi")), funs(. - .)) %>% 
  
  mutate(country_text_id = country)  

Combined_ECO[[7]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_wi"))) %>% 
  pivot_longer(cols=everything())

scenario1 = framework %>% 
  mutate(FKM4_c_bw = -1.06)
scenario2 = framework %>% 
  mutate(FKM4_c_bw = 1.44)

c_prediction1 = get_dynsimxx(scenario1, Combined_ECO[[7]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])

c_prediction2 = get_dynsimxx(scenario2, Combined_ECO[[7]], ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])


p1 = c_prediction1 %>% 
  mutate(Scenario = "Control-Focused") %>% 
  bind_rows(c_prediction2 %>% 
              mutate(Scenario = "Majoritarian")) %>% 
  ggplot(aes(x=Scenario, y=preds)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Economic Equality")

# E
brms_model = Combined_ECO[[6]]

brms_model$data %>% 
  select(country_text_id, FKM4_E_bw) %>% 
  distinct() %>% 
  arrange(FKM4_E_bw)

country = "GBR"
country = "FIN"
country = "USA"

framework = brms_model$data  %>% 
  filter(country_text_id == country) %>% 
  #summarise_if(is.numeric, mean, na.rm=T) %>% 
  dplyr::slice(1) %>% 
  mutate_at(vars(matches("_wi"), -matches("eco_equal_soc_wi")), funs(. - .)) %>% 
  
  mutate(country_text_id = country) 

brms_model$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_wi"))) %>% 
  pivot_longer(cols=everything())

scenario1 = framework %>% 
  mutate(FKM4_E_bw = -1.16)
scenario2 = framework %>% 
  mutate(FKM4_E_bw = 1.37)

E_prediction1 = get_dynsimxx(scenario1, brms_model, ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])

E_prediction2 = get_dynsimxx(scenario2, brms_model, ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])


p2 = E_prediction1 %>% 
  mutate(Scenario = "Libertarian") %>% 
  bind_rows(E_prediction2 %>% 
              mutate(Scenario = "Egalitarian")) %>% 
  ggplot(aes(x=Scenario, y=preds)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Economic Equality")

# fEc
brms_model = Combined_ECO[[4]]

brms_model$data %>% 
  select(country_text_id, FKM5_fEc_bw) %>% 
  distinct() %>% 
  arrange(FKM5_fEc_bw)

country = "AUS"
country = "SWE"
country = "USA"

framework = brms_model$data  %>% 
  filter(country_text_id == country) %>% 
  #summarise_if(is.numeric, mean, na.rm=T) %>% 
  dplyr::slice(1) %>% 
  mutate_at(vars(matches("_wi"), -matches("eco_equal_soc_wi")), funs(. - .)) %>% 
  
  mutate(country_text_id = country) 

brms_model$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_wi"))) %>% 
  pivot_longer(cols=everything())

scenario1 = framework %>% 
  mutate(FKM5_fEc_bw = -0.715)
scenario2 = framework %>% 
  mutate(FKM5_fEc_bw = 1.97)

fEc_prediction1 = get_dynsimxx(scenario1, brms_model, ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])

fEc_prediction2 = get_dynsimxx(scenario2, brms_model, ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])


p3 = fEc_prediction1 %>% 
  mutate(Scenario = "- fEc") %>% 
  bind_rows(fEc_prediction2 %>% 
              mutate(Scenario = "+ fEc")) %>% 
  ggplot(aes(x=Scenario, y=preds)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Economic Equality")


# fEC
brms_model = Combined_ECO[[3]]

brms_model$data %>% 
  select(country_text_id, FKM5_fEC_bw) %>% 
  distinct() %>% 
  arrange(FKM5_fEC_bw)

country = "BRA"
country = "USA"

framework = brms_model$data %>% 
  filter(country_text_id == country) %>% 
   #summarise_if(is.numeric, mean, na.rm=T) %>% 
  dplyr::slice(1) %>% 
  mutate_at(vars(matches("_wi"), -matches("eco_equal_soc_wi")), funs(. - .)) %>% 
  
  mutate(country_text_id = country) 

brms_model$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_wi"))) %>% 
  pivot_longer(cols=everything())

scenario1 = framework %>% 
  mutate(FKM5_fEC_bw =-0.983 )
scenario2 = framework %>% 
  mutate(FKM5_fEC_bw = 1.85 )

fEC_prediction1 = get_dynsimxx(scenario1, brms_model, ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])

fEC_prediction2 = get_dynsimxx(scenario2, brms_model, ecm=F, simulations = 1000, 
                             LDV1 = "eco_equal_soc_wi_lag", LDV2 = "eco_equal_soc_wi_lag2", 
                             length_preds = dim(scenario1)[1])


p4 = fEC_prediction1 %>% 
  mutate(Scenario = "- fEC") %>% 
  bind_rows(fEC_prediction2 %>% 
              mutate(Scenario = "+ fEC")) %>% 
  ggplot(aes(x=Scenario, y=preds)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Economic Equality")


ggarrange(p1, p2,p3,p4) %>% 
  annotate_figure(top="USA")

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