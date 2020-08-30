# Analysis Environmental Performance

# Setup ####
library(rstan)
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
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


# make table ####
make_brms_reg_table(Combined_Prod[[1]], Combined_Prod[[2]], Combined_Prod[[3]], 
                    Combined_Prod[[4]], Combined_Prod[[5]], Combined_Prod[[6]], Combined_Prod[[7]], prob_interval=0.95 )




# Predictions ####
get_prediction = function(scenario, nr_lags = 2, brms_model) {
  
  length_preds = dim(scenario)[1]
  mypredictions = matrix(NA, nrow=length_preds, ncol=4) %>% 
    data.frame() %>% 
    rename("lower" = 1, "est" = 2, "upper" = 3, "year_0" = 4)

  for (i in (nr_lags + 1):(length_preds)) {
    p_sc1 = data.frame(predictions = predict( brms_model, newdata=scenario[i-1,], summary=T)) %>%
      summarize(Estimate = predictions.Estimate,
                lower = predictions.Q2.5,
                upper = predictions.Q97.5)


    scenario$productivity_eco_wi_lag2[i] = scenario$productivity_eco_wi_lag[i-1]
    scenario$productivity_eco_wi_lag[i] = p_sc1$Estimate
    
    mypredictions[i,1] = p_sc1$lower
    mypredictions[i,2] = p_sc1$Estimate
    mypredictions[i,3] = p_sc1$upper
    mypredictions[i,4] = i

    
    
  }
  return(mypredictions)
} 

summary(Combined_Prod[[2]]$data)

Combined_Prod[[5]]$data %>% 
  summarise_if(is.numeric, funs(quantile(., probs=0.1)))
Combined_Prod[[5]]$data %>% 
  summarise_if(is.numeric, funs(quantile(., probs=0.9)))


mean_values = Combined_Prod[[5]]$data %>% 
  #filter(country_text_id == "USA") %>% 
  summarise_if(is.numeric, funs( mean(., na.rm=T))) %>% 
  select(-FKM5_FEC_wi, -FKM5_FEC_wi_lag)

scenario1 = data.frame(country_text_id = NA, 
                       FKM5_FeC_wi = seq(-3.23153, -3.23153, length.out = 20)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(mean_values) %>% 
  mutate(productivity_eco=NA,
         country_text_id=NA,
         year_0 = 18,
         FKM5_FeC_bw = -2)

scenario2 = data.frame(country_text_id = NA, 
                       FKM5_FeC_wi = seq(1.24792, 1.24792, length.out = 20)) %>% 
  mutate(FKM5_FeC_wi_lag= dplyr::lag(FKM5_FeC_wi, 1)) %>% 
  bind_cols(mean_values) %>% 
  mutate(productivity_eco=NA,
         country_text_id=NA,
         year_0 = 18,
         FKM5_FeC_bw = 3)

p_sc1 = get_prediction(scenario1, brms_model = Combined_Prod[[2]])
p_sc2 = get_prediction(scenario2, brms_model = Combined_Prod[[2]])


p_sc1 %>% 
  mutate(Scenario = "1") %>% 
  bind_rows(p_sc2 %>% 
              mutate(Scenario = "2"))  %>% 
  na.omit() %>% 
  ggplot(aes(x=year_0, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw()






get_prediction_wealth = function(scenario, brms_model, LDV1, LDV2 = NULL, simulations = 100, length_preds = 20, ecm=F) {
  
  mysimulations = matrix(NA, nrow=length_preds, ncol=simulations) %>% 
    data.frame() 
  
  scenario_filler = scenario 
  
  for (i in 1:length_preds) {
    
    p_sc1 = data.frame(predictions = fitted( brms_model, newdata=scenario_filler, summary=F, nsamples=simulations)) %>%
      pull(predictions) %>%
      as.numeric()
    
    if (ecm == T) {
      p_sc1 = p_sc1 + as.numeric(scenario_filler[LDV1])
    }
    
    if (is.null(LDV2) == F) {
      scenario_filler[LDV2] =  as.numeric(scenario_filler[LDV1])
    }
    
    scenario_filler[LDV1] = mean(p_sc1)
    mysimulations[i,] = p_sc1
  }
  
  final_frame = data.frame(
    est = rowMeans(mysimulations, na.rm=T),
    lower = apply(mysimulations, 1, FUN = function(x) hdi(x)[1]),
    upper = apply(mysimulations, 1, FUN = function(x) hdi(x)[2]),
    year = 1:length_preds
  )
  return(final_frame)
} 



p_sc1 = get_prediction_wealth(scenario1, Combined_Wealth[[6]], ecm=T, simulations = 1000, 
                              LDV1 = "wealth_eco_wi_lag", LDV2 = "wealth_eco_wi_lag2", 
                              length_preds = 40)
p_sc2 = get_prediction_wealth(scenario2, Combined_Wealth[[6]], ecm=T, simulations = 1000, 
                              LDV1 = "wealth_eco_wi_lag", LDV2 = "wealth_eco_wi_lag2", 
                              length_preds = 40)
p_sc1 %>% 
  mutate(Scenario = "1") %>% 
  bind_rows(p_sc2 %>%
              mutate(Scenario = "2"))  %>%
  # bind_rows(p_sc3 %>%
  #             mutate(Scenario = "3")) %>%
  na.omit() %>% 
  ggplot(aes(x=year, y=est, ymin=lower, ymax=upper, col=Scenario, fill=Scenario)) +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.4) +
  theme_bw()
