# Analysis Environmental Performance

# Setup ####
library(rstan)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")



# Wealth
ECO_mod_list = loadBRMS("ECO_mod_list", "Economy")

ECO_mod_list[[1]][[1]]
ECO_mod_list[[2]][[1]]
ECO_mod_list[[3]][[1]]
ECO_mod_list[[4]][[1]]
ECO_mod_list[[5]][[1]]
ECO_mod_list[[6]][[1]]
ECO_mod_list[[7]][[1]]



# Get Ns
ECO_mod_list[[1]][[1]]$data %>% 
  summarise(countries = n_distinct(country_text_id),
            years = n_distinct(year_0),
            N = n())

tscs_data %>% 
  select(wealth_eco, country, year) %>% 
  na.omit() %>% 
  summarise(min(year), max(year))

# Create Dataset ####
Combined_Wealth = list()
for (i in 1:length(ECO_mod_list)) {
  Combined_Wealth[[i]] = combine_models(ECO_mod_list[[i]][[1]], 
                                        ECO_mod_list[[i]][[2]], 
                                      ECO_mod_list[[i]][[3]], 
                                      ECO_mod_list[[i]][[4]], 
                                      ECO_mod_list[[i]][[5]],
                                      check_data = F)
}
rm(ECO_mod_list)


# Lag Distribution ####


interval = 0.95
dep_label = "Wealth"

plotlist = list() 
for (i in 1:length(Combined_Wealth)) {
  plotlist[[i]] = lag_distribution_both(Combined_Wealth[[i]], LDV_label = "wealth_eco_wi_lag", IndV_label = "FKM", 
                                         ci=interval, dep_label=dep_label, ecm=T, unit="sd")
}
ggarrange(plotlist = plotlist)



p2 = list(
  lag_distribution_both(Combined_Wealth[[1]], LDV_label = "wealth_eco_wi_lag", IndV_label = "corp", 
                        ci=interval, dep_label=dep_label, ecm=T, unit="sd"),
  lag_distribution_both(Combined_Wealth[[1]], LDV_label = "wealth_eco_wi_lag", IndV_label = "cbi", 
                        ci=interval, dep_label=dep_label, ecm=T, unit="sd"),
  lag_distribution_both(Combined_Wealth[[1]], LDV_label = "wealth_eco_wi_lag", IndV_label = "corr", 
                        ci=interval, dep_label=dep_label, ecm=T, unit="sd"),
  lag_distribution_both(Combined_Wealth[[1]], LDV_label = "wealth_eco_wi_lag", IndV_label = "trade", 
                        ci=interval, dep_label=dep_label, ecm=T, unit="sd"),
  lag_distribution_both(Combined_Wealth[[1]], LDV_label = "wealth_eco_wi_lag", IndV_label = "state", 
                        ci=interval, dep_label=dep_label, ecm=T, unit="sd"))

ggarrange(plotlist = c(p2))


wi_p1 = make_sig_plot(Combined_Wealth, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
wi_p19 = make_sig_plot(Combined_Wealth, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

bw_p1 = make_sig_plot(Combined_Wealth, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
bw_p19 = make_sig_plot(Combined_Wealth, pars ="bw", prob_interval = 0.9, dep_label=dep_label)

ggarrange(wi_p1, bw_p1)
ggarrange(wi_p19, bw_p19)


# Residual Plot ####

fitted_res_plot(Combined_Wealth[[1]], title = "TSCS: Wealth")
ppc_resid_time(Combined_Wealth[[1]])



# make table ####
make_brms_reg_table(Combined_Wealth[[1]], Combined_Wealth[[2]], Combined_Wealth[[3]], 
                    Combined_Wealth[[4]], Combined_Wealth[[5]], Combined_Wealth[[6]], Combined_Wealth[[7]], prob_interval=0.95 )




# Predictions ####
test = wealth_list[[1]] %>% 
  group_by(country_text_id) %>% 
  summarise(wealth_eco = mean(wealth_eco, na.rm=T)) %>% 
  arrange(wealth_eco)



country = "USA"
framework = Combined_Wealth[[7]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_c_df_wi, -FKM4_c_wi_lag)
Combined_Wealth[[7]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Wealth[[7]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_c_df_wi = seq(0 , 0.671 , length.out = 45)) %>% 
  mutate(FKM4_c_wi_lag= dplyr::lag(FKM4_c_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         wealth_eco_wi_lag=-0.920285 ,
         wealth_eco_wi_lag2=-1.010303) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_c_df_wi = seq(0, -0.650, length.out = 45)) %>% 
  mutate(FKM4_c_wi_lag= dplyr::lag(FKM4_c_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         wealth_eco_wi_lag=-0.920285 ,
         wealth_eco_wi_lag2=-1.010303) %>%
  dplyr::slice(-1)

# minus_wi 
minus_wi = wealth_list[[1]] %>% 
  filter(country_text_id == country) %>% 
  select_at(vars(year_0, starts_with("wealth"))) %>% 
  mutate(minus_wi = mean(wealth_eco_lag, na.rm=T)) %>% 
  pull(minus_wi)


FeC_prediction1_usa = get_dynsim(scenario1, Combined_Wealth[[7]], ecm=T, simulations = 1000, 
                                 LDV1 = "wealth_eco_wi_lag", LDV2 = "wealth_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = minus_wi[1])

FeC_prediction2_usa = get_dynsim(scenario2, Combined_Wealth[[7]], ecm=T, simulations = 1000, 
                                 LDV1 = "wealth_eco_wi_lag", LDV2 = "wealth_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= minus_wi[1])
p1 = FeC_prediction1_usa %>% 
  mutate(Scenario = "majoritarian") %>% 
  bind_rows(FeC_prediction2_usa %>%
              mutate(Scenario = "control-focused"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Wealth") +
  xlab("Year") +
  ggtitle("USA")
p1


#
country = "IND"
framework = Combined_Wealth[[7]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_c_df_wi, -FKM4_c_wi_lag)
Combined_Wealth[[7]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Wealth[[7]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_c_df_wi = seq(0 , 0.671 , length.out = 45)) %>% 
  mutate(FKM4_c_wi_lag= dplyr::lag(FKM4_c_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         wealth_eco_wi_lag=-0.8480139 ,
         wealth_eco_wi_lag2= -0.9141166) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_c_df_wi = seq(0, -0.650, length.out = 45)) %>% 
  mutate(FKM4_c_wi_lag= dplyr::lag(FKM4_c_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 2,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         wealth_eco_wi_lag=-0.8480139 ,
         wealth_eco_wi_lag2= -0.9141166) %>%
  dplyr::slice(-1)

minus_wi = wealth_list[[1]] %>% 
  filter(country_text_id == country) %>% 
  select_at(vars(year_0, starts_with("wealth"))) %>% 
  mutate(minus_wi = mean(wealth_eco_lag, na.rm=T)) %>% 
  pull(minus_wi)

FeC_prediction1_ind = get_dynsim(scenario1, Combined_Wealth[[7]], ecm=T, simulations = 1000, 
                                 LDV1 = "wealth_eco_wi_lag", LDV2 = "wealth_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = minus_wi[1])

FeC_prediction2_ind = get_dynsim(scenario2, Combined_Wealth[[7]], ecm=T, simulations = 1000, 
                                 LDV1 = "wealth_eco_wi_lag", LDV2 = "wealth_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= minus_wi[1])
p2 = FeC_prediction1_ind %>% 
  mutate(Scenario = "majoritarian") %>% 
  bind_rows(FeC_prediction2_ind %>%
              mutate(Scenario = "control-focused"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Wealth") +
  xlab("Year") +
  ggtitle("India")
p2

ggarrange(p1, p2, common.legend = T, legend="bottom")

# Productivity ####
PROD_mod_list = loadBRMS("PROD_mod_list", "Economy")

PROD_mod_list[[1]][[1]]
PROD_mod_list[[2]][[1]]
PROD_mod_list[[3]][[1]]
PROD_mod_list[[4]][[1]]
PROD_mod_list[[5]][[1]]
PROD_mod_list[[6]][[1]]
PROD_mod_list[[7]][[1]]



# Get Ns
PROD_mod_list[[1]][[1]]$data %>% 
  summarise(countries = n_distinct(country_text_id),
            years = n_distinct(year_0),
            N = n())

tscs_data %>% 
  select(productivity_eco, country, year) %>% 
  na.omit() %>% 
  summarise(min(year), max(year))

# Create Dataset ####
Combined_Prod = list()
for (i in 1:length(PROD_mod_list)) {
  Combined_Prod[[i]] = combine_models(PROD_mod_list[[i]][[1]], 
                                     PROD_mod_list[[i]][[2]], 
                                     PROD_mod_list[[i]][[3]], 
                                     PROD_mod_list[[i]][[4]], 
                                     PROD_mod_list[[i]][[5]],
                                     check_data = F)
}
rm(PROD_mod_list)


# Lag Distribution ####

interval = 0.95
dep_label = "Productivity"

plotlist = list() 
for (i in 1:length(Combined_Prod)) {
  plotlist[[i]] = lag_distribution_both(Combined_Prod[[i]], LDV_label = "productivity_eco_wi_lag", IndV_label = "FKM", 
                                         ci=interval, dep_label=dep_label, unit="sd")
}
ggarrange(plotlist = plotlist)

p2 = list(
  lag_distribution_both(Combined_Prod[[1]], LDV_label = "productivity_eco_wi_lag", IndV_label = "corp", 
                        ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_Prod[[1]], LDV_label = "productivity_eco_wi_lag", IndV_label = "cbi", 
                        ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_Prod[[1]], LDV_label = "productivity_eco_wi_lag", IndV_label = "corr", 
                        ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_Prod[[1]], LDV_label = "productivity_eco_wi_lag", IndV_label = "trade", 
                        ci=interval, dep_label=dep_label, unit="sd"),
  lag_distribution_both(Combined_Prod[[1]], LDV_label = "productivity_eco_wi_lag", IndV_label = "state", 
                        ci=interval, dep_label=dep_label, unit="sd"))

ggarrange(plotlist = c(p2))


wi_p1 = make_sig_plot(Combined_Prod, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
wi_p19 = make_sig_plot(Combined_Prod, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

bw_p1 = make_sig_plot(Combined_Prod, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
bw_p19 = make_sig_plot(Combined_Prod, pars ="bw", prob_interval = 0.9, dep_label=dep_label)

ggarrange(wi_p1, bw_p1)
ggarrange(wi_p19, bw_p19)


# Residual Plot ####

p2 = fitted_res_plot(Combined_Prod[[1]], title = "TSCS: Productivity")
ppc_resid_time(Combined_Prod[[1]])
p1 = fitted_res_plot(Combined_Wealth[[1]], title = "TSCS: Wealth")

ggarrange(p1, p2, ncol=2)


# make table ####
make_brms_reg_table(Combined_Prod[[1]], Combined_Prod[[2]], Combined_Prod[[3]], 
                    Combined_Prod[[4]], Combined_Prod[[5]], Combined_Prod[[6]], Combined_Prod[[7]], prob_interval=0.95 )




# Predictions ####


test = Combined_Prod[[1]]$data %>% 
  group_by(country_text_id) %>% 
  summarise(productivity_eco = mean(productivity_eco, na.rm=T),
            FKM5_Fec_bw = mean(FKM5_Fec_bw, na.rm=T)) %>% 
  arrange(-FKM5_Fec_bw)

# Fec
country = "CHE"
framework = Combined_Prod[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_Fec_wi, -FKM5_Fec_wi_lag)
Combined_Prod[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Prod[[1]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_Fec_wi = seq(0 , 0.876  , length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =1.11580 ,
         productivity_eco_wi_lag2 = 1.10479 ) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_Fec_wi = seq(0, -1.03, length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =1.11580 ,
         productivity_eco_wi_lag2 = 1.10479 ) %>%
  dplyr::slice(-1)


Fec_prediction1_che = get_dynsim(scenario1, Combined_Prod[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$productivity_eco[1])

Fec_prediction2_che = get_dynsim(scenario2, Combined_Prod[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$productivity_eco[1])
p1 = Fec_prediction1_che %>% 
  mutate(Scenario = "+ Fec") %>% 
  bind_rows(Fec_prediction2_che %>%
              mutate(Scenario = "- Fec"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Productivity") +
  xlab("Year") +
  ggtitle("CHE") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p1



country = "IND"
framework = Combined_Prod[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_Fec_wi, -FKM5_Fec_wi_lag)
Combined_Prod[[1]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Prod[[1]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_Fec_wi = seq(0 , 0.876 , length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =-0.3517419  ,
         productivity_eco_wi_lag2 =-0.3182337) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_Fec_wi = seq(0, -1.03, length.out = 45)) %>% 
  mutate(FKM5_Fec_wi_lag= dplyr::lag(FKM5_Fec_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =-0.3517419  ,
         productivity_eco_wi_lag2 =-0.3182337) %>%
  dplyr::slice(-1)


Fec_prediction1_ind = get_dynsim(scenario1, Combined_Prod[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$productivity_eco[1])

Fec_prediction2_ind = get_dynsim(scenario2, Combined_Prod[[1]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$productivity_eco[1])
p2 = Fec_prediction1_ind %>% 
  mutate(Scenario = "+ Fec") %>% 
  bind_rows(Fec_prediction2_ind %>%
              mutate(Scenario = "- Fec"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Productivity") +
  xlab("Year") +
  ggtitle("IND") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p2


# FeC
country = "CHE"
framework = Combined_Prod[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)
Combined_Prod[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Prod[[2]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_FeC_wi = seq(0 , 0.575 , length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =1.115807 ,
         productivity_eco_wi_lag2 =1.10479) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FeC_wi = seq(0, -0.812, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =1.115807 ,
         productivity_eco_wi_lag2 =1.10479) %>%
  dplyr::slice(-1)


FeC_prediction1_che = get_dynsim(scenario1, Combined_Prod[[2]], ecm=F, simulations = 1000, 
                                    LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                    length_preds = dim(scenario1)[1],
                                    minus_wi = scenario1$productivity_eco[1])

FeC_prediction2_che = get_dynsim(scenario2, Combined_Prod[[2]], ecm=F, simulations = 1000, 
                                    LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                    length_preds = dim(scenario1)[1],
                                    minus_wi= scenario2$productivity_eco[1])
p3 = FeC_prediction1_che %>% 
  mutate(Scenario = "+ FeC") %>% 
  bind_rows(FeC_prediction2_che %>%
              mutate(Scenario = "- FeC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Productivity") +
  xlab("Year") +
  ggtitle("CHE") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p3



country = "IND"
framework = Combined_Prod[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FeC_wi, -FKM5_FeC_wi_lag)
Combined_Prod[[2]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Prod[[2]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM5_FeC_wi = seq(0 , 0.575 , length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =-0.3517419  ,
         productivity_eco_wi_lag2 =-0.3182337) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FeC_wi = seq(0, -0.812, length.out = 45)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =-0.3517419  ,
         productivity_eco_wi_lag2 =-0.3182337) %>%
  dplyr::slice(-1)


FeC_prediction1_ind = get_dynsim(scenario1, Combined_Prod[[2]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$productivity_eco[1])

FeC_prediction2_ind = get_dynsim(scenario2, Combined_Prod[[2]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$productivity_eco[1])
p4 = FeC_prediction1_ind %>% 
  mutate(Scenario = "+ FeC") %>% 
  bind_rows(FeC_prediction2_ind %>%
              mutate(Scenario = "- FeC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Productivity") +
  xlab("Year") +
  ggtitle("IND") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p4


# E
country = "CHE"
framework = Combined_Prod[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)
Combined_Prod[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Prod[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , 0.874 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =1.115807 ,
         productivity_eco_wi_lag2 =1.10479) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0, -0.882 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =1.115807 ,
         productivity_eco_wi_lag2 =1.10479) %>%
  dplyr::slice(-1)


E_prediction1_che = get_dynsim(scenario1, Combined_Prod[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$productivity_eco[1])

E_prediction2_che = get_dynsim(scenario2, Combined_Prod[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$productivity_eco[1])
p5 = E_prediction1_che %>% 
  mutate(Scenario = "+ Inclusion") %>% 
  bind_rows(E_prediction2_che %>%
              mutate(Scenario = "- Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Productivity") +
  xlab("Year") +
  ggtitle("CHE") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p5



country = "IND"
framework = Combined_Prod[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM4_E_wi, -FKM4_E_wi_lag)
Combined_Prod[[6]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)

Combined_Prod[[6]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything())


scenario1 = data.frame(FKM4_E_wi = seq(0 , 0.874 , length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =-0.3517419  ,
         productivity_eco_wi_lag2 =-0.3182337) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM4_E_wi = seq(0, -0.882, length.out = 45)) %>% 
  mutate(FKM4_E_wi_lag= dplyr::lag(FKM4_E_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 26,
         trend = 0,
         productivity_eco_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         productivity_eco_wi_lag =-0.3517419  ,
         productivity_eco_wi_lag2 =-0.3182337) %>%
  dplyr::slice(-1)


E_prediction1_ind = get_dynsim(scenario1, Combined_Prod[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = scenario1$productivity_eco[1])

E_prediction2_ind = get_dynsim(scenario2, Combined_Prod[[6]], ecm=F, simulations = 1000, 
                                 LDV1 = "productivity_eco_wi_lag", LDV2 = "productivity_eco_wi_lag2", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= scenario2$productivity_eco[1])
p6 = E_prediction1_ind %>% 
  mutate(Scenario = "+ Inclusion") %>% 
  bind_rows(E_prediction2_ind %>%
              mutate(Scenario = "- Inclusion"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Productivity") +
  xlab("Year") +
  ggtitle("IND") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p6

ggarrange(p1,p2,p3,p4,p5,p6, ncol=2, nrow=3)
