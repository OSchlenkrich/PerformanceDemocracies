# Analysis Environmental Performance

# Setup ####
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_ds_v4.Rdata")
# source("Analyse/Performance/SpecificP/3 Integration/TSCS_imputation_DomesticP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_ds.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")


DOM_mod_list = loadBRMS("DOM_mod_list", "Domestic")

DOM_mod_list[[1]]
DOM_mod_list[[2]]
DOM_mod_list[[3]]
DOM_mod_list[[4]]
DOM_mod_list[[5]]
DOM_mod_list[[6]]
DOM_mod_list[[7]]


# Get Ns
DOM_mod_list[[1]]$data %>% 
  summarise(countries = n_distinct(country_text_id),
            years = n_distinct(year_0),
            N = n())

tscs_data %>% 
  select(domsec_ds, country, year) %>% 
  na.omit() %>% 
  summarise(min(year), max(year))


# Lag Distribution ####

interval = 0.95
dep_label = "Domestic Security"

plotlist_dom = list() 
for (i in 1:length(DOM_mod_list)) {
  plotlist_dom[[i]] = lag_distribution_both(DOM_mod_list[[i]], LDV_label = "domsec_ds_wi_lag", IndV_label = "FKM", 
                                         ci=interval, dep_label=dep_label, ecm=T, unit="SD")
}
ggarrange(plotlist = plotlist_dom)

p2 = list(
  lag_distribution_both(DOM_mod_list[[1]], LDV_label = "domsec_ds_wi_lag", IndV_label = "gdp", ci=interval, dep_label=dep_label, ecm=T, unit="SD"),
  lag_distribution_both(DOM_mod_list[[1]], LDV_label = "domsec_ds_wi_lag", IndV_label = "eco_inequal", ci=interval, dep_label=dep_label, ecm=T, unit="SD"),
  lag_distribution_both(DOM_mod_list[[1]], LDV_label = "domsec_ds_wi_lag", IndV_label = "corr", ci=interval, dep_label=dep_label, ecm=T, unit="SD"),
  lag_distribution_both(DOM_mod_list[[1]], LDV_label = "domsec_ds_wi_lag", IndV_label = "state", ci=interval, dep_label=dep_label, ecm=T, unit="SD"))

ggarrange(plotlist = c(p2))


wi_p1 = make_sig_plot(DOM_mod_list, pars ="wi", prob_interval = 0.95, dep_label=dep_label)
wi_p19 = make_sig_plot(DOM_mod_list, pars ="wi", prob_interval = 0.9, dep_label=dep_label)

bw_p1 = make_sig_plot(DOM_mod_list, pars ="bw", prob_interval = 0.95, dep_label=dep_label)
bw_p19 = make_sig_plot(DOM_mod_list, pars ="bw", prob_interval = 0.9, dep_label=dep_label)

ggarrange(wi_p1, bw_p1)
ggarrange(bw_p1, bw_p19)


# Residual Plot ####

fitted_res_plot(DOM_mod_list[[1]], title = "TSCS: Domestic Security Performance")
ppc_resid_time(DOM_mod_list[[1]])


# make table ####
make_brms_reg_table(DOM_mod_list[[1]], DOM_mod_list[[2]], DOM_mod_list[[3]], 
                    DOM_mod_list[[4]], DOM_mod_list[[5]], DOM_mod_list[[6]], DOM_mod_list[[7]], prob_interval=0.95 )


# Predictions ####

#  DEU
# ARG
test = domsec_ds_list[[1]] %>% 
  group_by(country_text_id) %>% 
  summarise(domsec_ds = mean(domsec_ds, na.rm=T)) %>% 
  arrange(-domsec_ds)

# fEC

country = "DEU"
framework = DOM_mod_list[[3]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_fEC_df_wi, -FKM5_fEC_wi_lag)
DOM_mod_list[[3]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)


DOM_mod_list[[3]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(value = round(value, 3))


scenario1 = data.frame(FKM5_fEC_df_wi = seq(0 , 0.68 , length.out = 45)) %>% 
  mutate(FKM5_fEC_wi_lag= dplyr::lag(FKM5_fEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=-0.5057671) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_fEC_df_wi = seq(0, -0.668, length.out = 45)) %>% 
  mutate(FKM5_fEC_wi_lag= dplyr::lag(FKM5_fEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=-0.5057671) %>%
  dplyr::slice(-1)

# minus_wi 
minus_wi = domsec_ds_list[[1]] %>% 
  filter(country_text_id == country) %>% 
  select_at(vars(year_0, starts_with("domsec"))) %>% 
  mutate(minus_wi = mean(domsec_ds_lag, na.rm=T)) %>% 
  pull(minus_wi)


fEC_prediction1_usa = get_dynsim(scenario1, DOM_mod_list[[3]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = minus_wi[1])

fEC_prediction2_usa = get_dynsim(scenario2, DOM_mod_list[[3]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= minus_wi[1])
p1 = fEC_prediction1_usa %>% 
  mutate(Scenario = "+ fEC") %>% 
  bind_rows(fEC_prediction2_usa %>%
              mutate(Scenario = "- fEC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Domestic Security") +
  xlab("Year") +
  ggtitle("DEU") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p1


country = "ARG"
framework = DOM_mod_list[[3]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_fEC_df_wi, -FKM5_fEC_wi_lag)
DOM_mod_list[[3]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)


DOM_mod_list[[3]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(value = round(value, 3))


scenario1 = data.frame(FKM5_fEC_df_wi = seq(0 , 0.68 , length.out = 45)) %>% 
  mutate(FKM5_fEC_wi_lag= dplyr::lag(FKM5_fEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=0.3008758) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_fEC_df_wi = seq(0, -0.668, length.out = 45)) %>% 
  mutate(FKM5_fEC_wi_lag= dplyr::lag(FKM5_fEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=0.3008758) %>%
  dplyr::slice(-1)

# minus_wi 
minus_wi = domsec_ds_list[[1]] %>% 
  filter(country_text_id == country) %>% 
  select_at(vars(year_0, starts_with("domsec"))) %>% 
  mutate(minus_wi = mean(domsec_ds_lag, na.rm=T)) %>% 
  pull(minus_wi)


fEC_prediction1_arg = get_dynsim(scenario1, DOM_mod_list[[3]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = minus_wi[1])

fEC_prediction2_arg = get_dynsim(scenario2, DOM_mod_list[[3]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= minus_wi[1])
p2 = fEC_prediction1_arg %>% 
  mutate(Scenario = "+ fEC") %>% 
  bind_rows(fEC_prediction2_arg %>%
              mutate(Scenario = "- fEC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Domestic Security") +
  xlab("Year") +
  ggtitle("ARG") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p2

# FEC

country = "DEU"
framework = DOM_mod_list[[5]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FEC_df_wi, -FKM5_FEC_wi_lag)
DOM_mod_list[[5]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)


DOM_mod_list[[5]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(value = round(value, 3))


scenario1 = data.frame(FKM5_FEC_df_wi = seq(0 , 0.804 , length.out = 45)) %>% 
  mutate(FKM5_FEC_wi_lag= dplyr::lag(FKM5_FEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=-0.5057671) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FEC_df_wi = seq(0, -0.821, length.out = 45)) %>% 
  mutate(FKM5_FEC_wi_lag= dplyr::lag(FKM5_FEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=-0.5057671) %>%
  dplyr::slice(-1)

# minus_wi 
minus_wi = domsec_ds_list[[1]] %>% 
  filter(country_text_id == country) %>% 
  select_at(vars(year_0, starts_with("domsec"))) %>% 
  mutate(minus_wi = mean(domsec_ds_lag, na.rm=T)) %>% 
  pull(minus_wi)


FEC_prediction1_usa = get_dynsim(scenario1, DOM_mod_list[[5]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = minus_wi[1])

FEC_prediction2_usa = get_dynsim(scenario2, DOM_mod_list[[5]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= minus_wi[1])
p3 = FEC_prediction1_usa %>% 
  mutate(Scenario = "+ FEC") %>% 
  bind_rows(FEC_prediction2_usa %>%
              mutate(Scenario = "- FEC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Domestic Security") +
  xlab("Year") +
  ggtitle("DEU") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p3


country = "ARG"
framework = DOM_mod_list[[5]]$data %>% 
  filter(country_text_id == country) %>% 
  mutate_at(vars(matches("_wi")), funs(. - .)) %>% 
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-FKM5_FEC_df_wi, -FKM5_FEC_wi_lag)
DOM_mod_list[[5]]$data %>% 
  filter(country_text_id == country) %>% 
  dplyr::slice(1)


DOM_mod_list[[5]]$data %>%
  #filter(country_text_id == country) %>% 
  summarise_if(is.numeric, funs(Q05= quantile (., probs=0.05, na.rm=T),
                                Q50= mean (., na.rm=T),
                                Q95= quantile (., probs=0.95, na.rm=T))) %>% 
  select_at(vars(starts_with("FKM"), -matches("_lag"), -matches("_bw"))) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(value = round(value, 3))


scenario1 = data.frame(FKM5_FEC_df_wi = seq(0 , 0.804 , length.out = 45)) %>% 
  mutate(FKM5_FEC_wi_lag= dplyr::lag(FKM5_FEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=0.3008758) %>%
  dplyr::slice(-1)

scenario2 = data.frame(FKM5_FEC_df_wi = seq(0, -0.821, length.out = 45)) %>% 
  mutate(FKM5_FEC_wi_lag= dplyr::lag(FKM5_FEC_df_wi, 1)) %>% 
  bind_cols(framework) %>% 
  mutate(country_text_id = country,
         year_0 = 15,
         trend = 0,
         eco_equal_soc_spatial_ctl = 0,
         fiscalcrisis_cat_ctl = 0,
         domsec_ds_wi_lag=0.3008758) %>%
  dplyr::slice(-1)

# minus_wi 
minus_wi = domsec_ds_list[[1]] %>% 
  filter(country_text_id == country) %>% 
  select_at(vars(year_0, starts_with("domsec"))) %>% 
  mutate(minus_wi = mean(domsec_ds_lag, na.rm=T)) %>% 
  pull(minus_wi)


FEC_prediction1_arg = get_dynsim(scenario1, DOM_mod_list[[5]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi = minus_wi[1])

FEC_prediction2_arg = get_dynsim(scenario2, DOM_mod_list[[5]], ecm=T, simulations = 1000, 
                                 LDV1 = "domsec_ds_wi_lag", 
                                 length_preds = dim(scenario1)[1],
                                 minus_wi= minus_wi[1])
p4 = FEC_prediction1_arg %>% 
  mutate(Scenario = "+ FEC") %>% 
  bind_rows(FEC_prediction2_arg %>%
              mutate(Scenario = "- FEC"))  %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw() +
  ylab("Domestic Security") +
  xlab("Year") +
  ggtitle("ARG") +
  scale_y_continuous(breaks=seq(-3,3, 0.2))
p4

ggarrange(p1,p2,p3,p4, ncol=2, nrow=2)

