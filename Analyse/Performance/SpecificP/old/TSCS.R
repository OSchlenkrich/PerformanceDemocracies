source("Analyse/CreateDatasets.R")

# Setup ####
library(brms)
options(mc.cores = parallel::detectCores())
scale_this = function(x) {
  x_scaled = (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
  return(x_scaled)
}

first_DF = function(x) {
  return(x - dplyr::lag(x,1))
}

prior_tscs <- c(set_prior("normal(0,10)", class = "b"),
                set_prior("cauchy(0,5)", class = "sd")
)

# Create Datasets ####

CBI_control = readstata13::read.dta13("Datasets/CBI dataset_2019 corrections.dta") %>%  
  select(-cname) %>% 
  left_join(QoC_data %>% 
              select(country, country_text_id,
                     year,
                     ccodewb) %>% 
              na.omit(), by=c("ccodewb", "year")) %>% 
  select(country_text_id, year, cbi_u_cbi_num_ctl = lvau_garriga, cbi_w_cbi_num_ctl =  lvaw_garriga )

V_dem_control = V_dem_all %>% 
  select(country_text_id, year, corruption_vdem_num_ctl = v2x_corr) 

control_vars = QoC_data %>% 
  dplyr::select(country_text_id, year,
                
                pop_over65_wdi_num_ctl = wdi_pop65,
                
                green_vt_cpds_num_ctl = cpds_vg,
                
                green_plt_cpds_num_ctl = cpds_lg,
                socdem_plt_cpds_num_ctl = cpds_ls,
                liberal_plt_cpds_num_ctl = cpds_ll,
                cabinet_cpds_num_ctl = cpds_govlr, 
                
                unions_vi_num_ctl = vi_udr,
                #government_cpds_ord_ctl = cpds_tg,
                
                trade_wdi_num_ctl = wdi_trade,
                
                #gini_wdi_num_ctl = wdi_gini
                
            ) %>% 
  mutate(
    #government_cpds_ord_ctl = ifelse(government_cpds_ord_ctl == 1, 1, 0),
    # cabinet_cpds_ord_ctl = fct_recode(as.factor(cabinet_cpds_ord_ctl), 
    #                                         "H_RW" = "1", 
    #                                         "D_RW" = "2", 
    #                                         "B" = "3", 
    #                                         "D_SD" = "4", 
    #                                         "H_SD" = "5")
    ) %>% 
  filter(country_text_id %in% dmx_cluster_names) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  arrange(country_text_id, year)
  # group_by(country_text_id, year) %>% 
  # summarise_all(sum, na.rm=T) 

tscs_data = performance_all %>% 
  # main independent variable
  left_join(dmx_trade_cluster %>%  
              select_at(vars(country_text_id, year, starts_with("FKM"))), 
            by=c("country_text_id", "year")) %>% 

  # Control Vars
  left_join(control_vars, by=c("country_text_id", "year")) %>% 
  left_join(CBI_control, by=c("country_text_id", "year")) %>% 
  left_join(V_dem_control, by=c("country_text_id", "year")) %>% 

  filter(classification_core == "Deficient Democracy" | classification_core == "Working Democracy") %>% 
  
  # create NA indicator variables
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("ctl")), funs("na" = if_else(all(is.na(.) == T), 1, 0))) %>%
  ungroup() %>% 
  arrange(country_text_id, year) 


# ADAPTATION ####

# Multiple Imputation ####
mice_data = as.data.frame(tscs_data) %>%
  select_at(vars(country_text_id, year,
                 matches("_mb_"),
                 ends_with("eco"), 
                 ends_with("ctl"), 
                 eco_inequal_soc_num_ctl = eco_inequal_soc,
                 classification_core)) %>% 
  
  # filter NAs
  filter(is.na(wealth_eco) == F)   %>% 
  
  mutate(year_0 = year - min(year),
         classification_core = ifelse(classification_core == "Deficient Democracy", 1, 0)) %>% 

  select(-year) %>% 
  ungroup() %>% 
  # scaling
  mutate_at(vars(ends_with("num_ctl")), funs(scale_this(.)))  %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("num_ctl")), funs(
    "lag"= dplyr::lag(.,1),
    "lag2"= dplyr::lag(.,2),
    "lag3"= dplyr::lag(.,3),
    "lead"= dplyr::lead(.,1),
    "lead2"= dplyr::lead(.,2),
    "lead3"= dplyr::lead(.,3))
  ) %>%
  ungroup() %>% 
  select(country_text_id, year_0, everything())

mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })

mice_data %>% 
  select_at(vars(-matches("lag"),-matches("lead"))) %>% 
  group_by(year_0) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year_0") %>% 
  ggplot(aes(x=year_0, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in TSCS Economy - Environmental")


# corrplot
corrplot(cor(mice_data %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out <- amelia(mice_data, 
                m = nr_imputations, 
                ts = "year_0", 
                cs = "country_text_id", 
                noms=c("classification_core"), 
                polytime = 1,
                intercs = T,
                p2s = 2,
                parallel = "snow",
                ncpus	= nr_cores,
                empri = .05*nrow(mice_data)
)

a.out



# ECONOMY ####
wealth_list = list()

for (i in 1:5) {
  wealth_list[[i]]  = a.out$imputations[[i]] %>% 
    select_at(vars(-matches("lag"),-matches("lead"))) %>% 
    # filter(year != 2017) %>% 
    group_by(country_text_id) %>% 
    mutate(
      wealth_eco_lag = dplyr::lag(wealth_eco, 1),
      wealth_eco_df = first_DF(wealth_eco),
      
      productivity_eco_lag = dplyr::lag(productivity_eco, 1),
      productivity_eco_df = first_DF(productivity_eco),
      
      classification_core_num_ctl = classification_core,
    )  %>% 
    ungroup()  %>% 
    mutate(
      # cabinet_cpds_num_ctl = cabinet_cpds_ord_ctl,
      # cabinet_cpds_ord_ctl = fct_recode(as.factor(cabinet_cpds_ord_ctl), 
      #                                   "H_RW" = "1", 
      #                                   "D_RW" = "2", 
      #                                   "B" = "3", 
      #                                   "D_SD" = "4", 
      #                                   "H_SD" = "5"),
      ) %>% 

    group_by(country_text_id) %>%
    # within effect
    mutate_at(vars(ends_with("num_ctl")), funs(wi = . - mean(., na.rm=T))) %>%
    mutate_at(vars(ends_with("num_ctl_wi")), funs(lag = dplyr::lag(.,1))) %>% 
    mutate_at(vars(ends_with("num_ctl_wi")), funs(df = first_DF(.)))  %>%

    #between effect
    mutate_at(vars(ends_with("num_ctl")), funs(bw = mean(., na.rm=T))) %>% 
    

        
    select_at(vars(country_text_id, year_0, 
                   starts_with("FKM_5"),
                   matches("eco"), 
                   matches("ctl"), 
                   -matches("index"))
              ) %>% 
    ungroup()   %>% 
    as.data.frame() 
}

wealth_list[[1]] %>% 
  group_by(year_0) %>% 
  select_at(vars(ends_with("ctl"), ends_with("eco"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year_0") %>% 
  ggplot(aes(x=year_0, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample - Environmental")


# corrplot
corrplot(cor(wealth_list[[1]] %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),
                              -matches("lead"),
                              -ends_with("ctl"))), use="pairwise"), method="number")

# Wealth ####
# ADL
wealth_mod1 = brm_multiple(
  wealth_eco ~ 1 + 
    # path dependence
    wealth_eco_lag +

    #economic modernization
    # pop_over65_wdi_num_ctl_wi + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +

    # formal institutions
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +

    # informal institutions
    classification_core_num_ctl_wi + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
  
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    FKM_5_mb_FEC + FKM_5_mb_Fec + FKM_5_mb_FeC + FKM_5_mb_fEC +
    (1|country_text_id) + (1|year_0),
  data = wealth_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 10000
)



summary(wealth_mod1, prob = .95)
prior_summary(wealth_mod1, data = eco_tscs, family = gaussian())
plot(conditional_effects(wealth_mod1))
plot(conditional_effects(wealth_mod1, "wealth_eco_lag:cluster_label_1st"))

wealth_list[[1]]$cabinet_cpds_num_ctl_df
(1.176199e-02 +-9.497195e-03)/(1-9.745475e-01)

posterior_summary(wealth_mod2)


# ECM
wealth_mod2 = brm_multiple(
  wealth_eco_df ~ 1 + 
    # path dependence
    wealth_eco_lag +
    
    #economic modernization
    # pop_over65_wdi_num_ctl_wi + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi_df + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    cbi_w_cbi_num_ctl_wi_df + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    classification_core_num_ctl_wi_df + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi_df + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    as.factor(cluster_mode) +
    (1|country_text_id) + (1|year_0),
  data = wealth_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 10000
)
summary(wealth_mod2, prob = .95)



# Variance at level 2
0.02/(0.02 + 0.04 )
# Variance at level 1

test = residuals(wealth_mod1)
resid_test = data.frame(country = wealth_mod1$data$country, year =  wealth_mod1$data$year, resid = test[,1])  %>% 
  filter(country == "Denmark") %>% 
  mutate(resid_lag = dplyr::lag(resid, 1))

mod2 = lm(resid ~ resid_lag, resid_test) 

summary(mod2)

data.frame(country = wealth_mod1$data$country, year =  wealth_mod1$data$year, resid = test[,1]) %>% 
  filter(country == "France") %>% 
  ggplot(aes(x=year, y=resid, col=country)) +
  geom_point() +
  geom_hline(yintercept = 0)


wealth_mod2 <- brm(
  wealth_eco ~ 1 + wealth_eco_lag +
    cluster_label_1st +
    (1|country) + (1|year),
  data = eco_tscs, family = gaussian(),
  #prior = prior3,
  warmup = 2000, iter = 10000
)
summary(wealth_mod2)

# ECM
wealth_mod3 <- brm(
  wealtH_eco_df ~ 1 + wealth_eco_lag +
    cluster_label_1st +
    (1|country) + (1|year),
  data = eco_tscs, family = gaussian(),
  #prior = prior3,
  warmup = 2000, iter = 10000
)
summary(wealth_mod3)


test2 = residuals(wealth_mod3)
resid_test = data.frame(country = wealth_mod3$data$country, year =  wealth_mod3$data$year, resid = test2[,1])  %>% 
  filter(country == "Denmark") %>% 
  mutate(resid_lag = dplyr::lag(resid, 1))

mod2 = lm(resid ~ resid_lag, resid_test) 
summary(mod2)

# Productivity ####
prod_mod1 = brm_multiple(
  productivity_eco ~ 1 + 
    # path dependence
    productivity_eco_lag +
    
    #economic modernization
    # pop_over65_wdi_num_ctl_wi + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    classification_core_num_ctl_wi + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    as.factor(cluster_mode) +
    (1|country_text_id) + (1|year_0),
  data = wealth_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 10000,
  control = list(adapt_delta = 0.99)
)
summary(prod_mod1, prob = .95)
prior_summary(prod_mod1)



# Environment ####
env_tscs = tscs_data %>% 
  filter(year != 2017) %>% 
  group_by(country_text_id) %>% 
  mutate(air_env_lag = dplyr::lag(air_env, 1),
         air_env_df = first_DF(air_env),
         abstraction_env_lag = dplyr::lag(abstraction_env, 1),
         abstraction_env_df = first_DF(abstraction_env),
         cluster_label_1st_lag = dplyr::lag(cluster_label_1st, 1),
         wealth_eco_ctl_df = first_DF(wealth_eco),
         wealth_eco_ctl_lag = dplyr::lag(wealth_eco,1))   %>% 
  ungroup() %>% 
  # scaling
  mutate_at(vars(ends_with("num_ctl")), funs(scale_this(.))) %>% 
  
  group_by(country_text_id) %>%
  mutate_at(vars(ends_with("ctl")), funs(lag = dplyr::lag(.,1))) %>% 
  mutate_at(vars(ends_with("ctl")), funs(lag2 = dplyr::lag(.,2)))%>% 
  
  mutate_at(vars(ends_with("ctl")), funs(df = first_DF(.)))  %>% 
  select_at(vars(country, country_text_id, year, cluster_label_1st, matches("env"), matches("ctl"), -matches("index"))) %>% 
  ungroup() 


env_tscs %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("ctl"), ends_with("env"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample - Environmental")


# corrplot
corrplot(cor(env_tscs %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("df"),-matches("lag"))), use="pairwise"), method="number")


ggplot(env_tscs, aes(x=air_env, y=green_vt_cpds_num_ctl)) +
  geom_point()
ggplot(env_tscs, aes(x=green_plt_cpds_num_ctl, y=green_vt_cpds_num_ctl)) +
  geom_point()




env_mod1 <- brm(
  air_env_df ~ 1 + 
    # path dependence
    air_env_lag +

    #economic modernization
    pop_over65_wdi_num_ctl_df + pop_over65_wdi_num_ctl_lag + 
    wealth_eco_ctl_df + wealth_eco_ctl_lag +

    green_vt_cpds_num_ctl_df + green_vt_cpds_num_ctl_lag + green_vt_cpds_num_ctl_lag2 +
    
    # Parties
    # green_plt_cpds_num_ctl_df + green_plt_cpds_num_ctl_lag +
    #socdem_plt_cpds_num_ctl_df + socdem_plt_cpds_num_ctl_lag +
    #liberal_plt_cpds_num_ctl_df + liberal_plt_cpds_num_ctl_lag +
    
    #PRT
    unions_vi_num_ctl_df + unions_vi_num_ctl_lag +
    
    # international factors
    trade_wdi_num_ctl_df + trade_wdi_num_ctl_lag +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    cluster_label_1st +
    (1|country) + (1|year),
  data = env_tscs, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 10000
)
summary(env_mod1, prob = .95)
prior_summary(env_mod1, data = eco_tscs, family = gaussian())


env_mod2 <- brm(
  abstraction_env_df ~ 1 + 
    # path dependence
    abstraction_env_lag +
    year +
    
    #economic modernization
    pop_over65_wdi_num_ctl_df + pop_over65_wdi_num_ctl_lag + 
    wealth_eco_ctl_df + wealth_eco_ctl_lag +
    
    #green_vt_cpds_num_ctl_df + green_vt_cpds_num_ctl_lag +
    
    # Parties
    green_plt_cpds_num_ctl_df + green_plt_cpds_num_ctl_lag +
    #socdem_plt_cpds_num_ctl_df + socdem_plt_cpds_num_ctl_lag +
    #liberal_plt_cpds_num_ctl_df + liberal_plt_cpds_num_ctl_lag +
    
    #PRT
    unions_vi_num_ctl_df + unions_vi_num_ctl_lag +
    
    # international factors
    trade_wdi_num_ctl_df + trade_wdi_num_ctl_lag +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    cluster_label_1st +
    (1|country) + (1|year),
  data = env_tscs, family = gaussian(),
  #prior = prior3,
  warmup = 2000, iter = 10000
)
summary(env_mod2, prob = .95)

# GOAL ATTAINMENT ####

tscs_data_GA = tscs_data %>% 
  left_join(AR_dmx %>% select(country_text_id, year, systid, GA_raw_ccp = arate_ccp, GA_raw_lutz = arate_lutz),
            by=c("country_text_id", "year"))

hist(tscs_data_GA$GA_raw_ccp)
hist(tscs_data_GA$GA_raw_lutz)
hist(tscs_data$GA_lutz_ga)
hist(tscs_data$GA_ccp_ga)

prior_GA <- c(set_prior("normal(0,10)", class = "b"),
                set_prior("cauchy(0,5)", class = "sd")
)

test = tscs_data_GA %>%     
  select_at(vars(country_text_id, year,systid,
                                    starts_with("FKM_5"),
                                    starts_with("GA"), 
                                    #matches("ctl"), 
                                    -matches("index"))
            ) %>%  
  group_by(country_text_id, systid) %>% 
  summarise_at(vars(-year), funs(mean(., na.rm=T))) %>% 
  filter(is.na(systid) == F) %>% 
  ungroup()

# corrplot
corrplot(cor(test %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("df"),-matches("lag"))), use="pairwise"), method="number")


# CCP ####
GA_mod = brm(bf(
  GA_ccp_ga ~ 1 + FKM_5_mb_FEC + FKM_5_mb_fEC + FKM_5_mb_Fec + FKM_5_mb_FeC +
    (1 | country_text_id)),
  family=gaussian(),
  test,
  prior = prior_GA_lutz,
  warmup = 2000, iter = 8000,
  control = list(adapt_delta = 0.99), 
  chains = 6
)
summary(GA_mod, prob = .95)
prior_summary(GA_mod)

# Lutz ####
prior_GA_lutz <- c(set_prior("normal(0,10)", class = "b")
)

GA_mod_lutz = brm(bf(
  GA_lutz_ga ~ 1 + FKM_5_mb_fEC + FKM_5_mb_fEc + FKM_5_mb_Fec + FKM_5_mb_FeC
  ),
  family=gaussian(),
  test,
  prior = prior_GA_lutz,
  warmup = 2000, iter = 8000,
  control = list(adapt_delta = 0.90), 
  chains = 6)

summary(GA_mod_lutz, prob = .95)


# INTEGRATION ####
# Multiple Imputation ####
mice_data_soc = as.data.frame(tscs_data) %>%
  select_at(vars(country_text_id, year,
                 matches("_mb_"),
                 ends_with("soc"),
                 ends_with("ctl"), 
                 classification_core)) %>% 
  
  # filter NAs
  filter(is.na(eco_inequal_soc) == F)   %>% 
  
  mutate(year_0 = year - min(year),
         classification_core = ifelse(classification_core == "Deficient Democracy", 1, 0)) %>% 
  
  select(-year) %>% 
  ungroup() %>% 
  # scaling
  mutate_at(vars(ends_with("num_ctl")), funs(scale_this(.)))  %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("num_ctl")), funs(
    "lag"= dplyr::lag(.,1),
    "lag2"= dplyr::lag(.,2),
    "lag3"= dplyr::lag(.,3),
    "lead"= dplyr::lead(.,1),
    "lead2"= dplyr::lead(.,2),
    "lead3"= dplyr::lead(.,3))
  ) %>%
  ungroup() %>% 
  select(country_text_id, year_0, everything())

mice_data_soc[] <- lapply(mice_data_soc, function(x) { attributes(x) <- NULL; x })

mice_data_soc %>% 
  select_at(vars(-matches("lag"),-matches("lead"))) %>% 
  group_by(year_0) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year_0") %>% 
  ggplot(aes(x=year_0, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in TSCS Economy - Environmental")


# corrplot
corrplot(cor(mice_data_soc %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_soc <- amelia(mice_data_soc, 
                m = nr_imputations, 
                ts = "year_0", 
                cs = "country_text_id", 
                noms=c("classification_core"), 
                polytime = 1,
                intercs = T,
                p2s = 2,
                parallel = "snow",
                ncpus	= nr_cores,
                empri = .05*nrow(mice_data_soc)
)

a.out_soc

#
soc_list = list()

for (i in 1:2) {
  soc_list[[i]]  = a.out_soc$imputations[[i]] %>% 
    select_at(vars(-matches("lag"),-matches("lead"))) %>% 
    # filter(year != 2017) %>% 
    group_by(country_text_id) %>% 
    mutate(
      eco_inequal_soc_lag = dplyr::lag(eco_inequal_soc, 1),
      eco_inequal_soc_df = first_DF(eco_inequal_soc),
      
      soc_inequal_soc_lag = dplyr::lag(soc_inequal_soc, 1),
      soc_inequal_soc_df = first_DF(soc_inequal_soc),
      
      classification_core_num_ctl = classification_core,
    )  %>% 
    ungroup()  %>% 
    mutate(
      # cabinet_cpds_num_ctl = cabinet_cpds_ord_ctl,
      # cabinet_cpds_ord_ctl = fct_recode(as.factor(cabinet_cpds_ord_ctl), 
      #                                   "H_RW" = "1", 
      #                                   "D_RW" = "2", 
      #                                   "B" = "3", 
      #                                   "D_SD" = "4", 
      #                                   "H_SD" = "5"),
    ) %>% 
    
    group_by(country_text_id) %>%
    # within effect
    mutate_at(vars(ends_with("num_ctl")), funs(wi = . - mean(., na.rm=T))) %>%
    mutate_at(vars(ends_with("num_ctl_wi")), funs(lag = dplyr::lag(.,1))) %>% 
    mutate_at(vars(ends_with("num_ctl_wi")), funs(df = first_DF(.)))  %>%
    
    mutate_at(vars(starts_with("FKM_")), funs(wi = . - mean(., na.rm=T))) %>%
    mutate_at(vars(starts_with("FKM_")), funs(lag = dplyr::lag(.,1))) %>% 
    
    #between effect
    mutate_at(vars(ends_with("num_ctl")), funs(bw = mean(., na.rm=T))) %>% 
    mutate_at(vars(starts_with("FKM_")), funs(bw = mean(., na.rm=T))) %>% 
    
    
    
    select_at(vars(country_text_id, year_0, 
                   starts_with("FKM_5"),
                   matches("soc"), 
                   matches("ctl"), 
                   -matches("index"))
    ) %>% 
    ungroup()   %>% 
    as.data.frame() 
}


soc_list[[1]] %>% 
  group_by(year_0) %>% 
  select_at(vars(ends_with("ctl"), ends_with("soc"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year_0") %>% 
  ggplot(aes(x=year_0, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample - Environmental")


# corrplot
corrplot(cor(soc_list[[1]] %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),
                              -matches("lead"),
                              -ends_with("ctl"))), use="pairwise"), method="number")



#Economic Inequality ####
# ADL
eco_inequal_mod1 = brm_multiple(
  eco_inequal_soc ~ 1 + 
    # path dependence
    eco_inequal_soc_lag +
    
    #economic modernization
    pop_over65_wdi_num_ctl_wi + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    classification_core_num_ctl_wi + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    FKM_5_mb_FEC_wi + FKM_5_mb_FEC_wi_lag + FKM_5_mb_FEC_bw +
    FKM_5_mb_Fec_wi + FKM_5_mb_Fec_wi_lag + FKM_5_mb_Fec_bw + 
    FKM_5_mb_FeC_wi + FKM_5_mb_FeC_wi_lag + FKM_5_mb_FeC_bw + 
    FKM_5_mb_fEC_wi + FKM_5_mb_fEC_wi_lag + FKM_5_mb_fEC_bw +
    (1|country_text_id) + (1|year_0),
  data = soc_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 10000,
  control = list(adapt_delta = 0.99)
)

summary(eco_inequal_mod1, prob = .95)


#Social Inequality ####
# ADL
soc_inequal_mod1 = brm_multiple(
  soc_inequal_soc ~ 1 + 
    # path dependence
    soc_inequal_soc_lag +
    
    #economic modernization
    pop_over65_wdi_num_ctl_wi + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    classification_core_num_ctl_wi + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    FKM_5_mb_FEC_wi + FKM_5_mb_FEC_wi_lag + FKM_5_mb_FEC_bw +
    FKM_5_mb_Fec_wi + FKM_5_mb_Fec_wi_lag + FKM_5_mb_Fec_bw + 
    FKM_5_mb_FeC_wi + FKM_5_mb_FeC_wi_lag + FKM_5_mb_FeC_bw + 
    FKM_5_mb_fEC_wi + FKM_5_mb_fEC_wi_lag + FKM_5_mb_fEC_bw +
    (1|country_text_id) + (1|year_0),
  data = soc_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 10000,
  control = list(adapt_delta = 0.99)
)

summary(soc_inequal_mod1, prob = .95)



# Domestic Security ####
# Multiple Imputation ####
mice_data_ds = as.data.frame(tscs_data) %>%
  select_at(vars(country_text_id, year,
                 matches("_mb_"),
                 ends_with("ds"),
                 ends_with("ctl"), 
                 classification_core)) %>% 
  
  # filter NAs
  filter(is.na(pubsafe_ds) == F)   %>% 
  
  mutate(year_0 = year - min(year),
         classification_core = ifelse(classification_core == "Deficient Democracy", 1, 0)) %>% 
  
  select(-year) %>% 
  ungroup() %>% 
  # scaling
  mutate_at(vars(ends_with("num_ctl")), funs(scale_this(.)))  %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("num_ctl")), funs(
    "lag"= dplyr::lag(.,1),
    "lag2"= dplyr::lag(.,2),
    "lag3"= dplyr::lag(.,3),
    "lead"= dplyr::lead(.,1),
    "lead2"= dplyr::lead(.,2),
    "lead3"= dplyr::lead(.,3))
  ) %>%
  ungroup() %>% 
  select(country_text_id, year_0, everything())

mice_data_ds[] <- lapply(mice_data_ds, function(x) { attributes(x) <- NULL; x })

mice_data_ds %>% 
  select_at(vars(-matches("lag"),-matches("lead"))) %>% 
  group_by(year_0) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year_0") %>% 
  ggplot(aes(x=year_0, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in TSCS Economy - Environmental")


# corrplot
corrplot(cor(mice_data_ds %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_ds <- amelia(mice_data_ds, 
                    m = nr_imputations, 
                    ts = "year_0", 
                    cs = "country_text_id", 
                    noms=c("classification_core"), 
                    polytime = 1,
                    intercs = T,
                    p2s = 2,
                    parallel = "snow",
                    ncpus	= nr_cores,
                    empri = .05*nrow(mice_data_ds)
)

a.out_ds

#
ds_list = list()

for (i in 1:2) {
  ds_list[[i]]  = a.out_ds$imputations[[i]] %>% 
    select_at(vars(-matches("lag"),-matches("lead"))) %>% 
    # filter(year != 2017) %>% 
    group_by(country_text_id) %>% 
    mutate(
      pubsafe_ds_lag = dplyr::lag(pubsafe_ds, 1),
      pubsafe_ds_df = first_DF(pubsafe_ds),

      classification_core_num_ctl = classification_core,
    )  %>% 
    ungroup()  %>% 
    mutate(
      # cabinet_cpds_num_ctl = cabinet_cpds_ord_ctl,
      # cabinet_cpds_ord_ctl = fct_recode(as.factor(cabinet_cpds_ord_ctl), 
      #                                   "H_RW" = "1", 
      #                                   "D_RW" = "2", 
      #                                   "B" = "3", 
      #                                   "D_SD" = "4", 
      #                                   "H_SD" = "5"),
    ) %>% 
    
    group_by(country_text_id) %>%
    # within effect
    mutate_at(vars(ends_with("num_ctl")), funs(wi = . - mean(., na.rm=T))) %>%
    mutate_at(vars(ends_with("num_ctl_wi")), funs(lag = dplyr::lag(.,1))) %>% 
    mutate_at(vars(ends_with("num_ctl_wi")), funs(df = first_DF(.)))  %>%
    
    mutate_at(vars(starts_with("FKM_")), funs(wi = . - mean(., na.rm=T))) %>%
    mutate_at(vars(starts_with("FKM_")), funs(lag = dplyr::lag(.,1))) %>% 
    mutate_at(vars(starts_with("FKM_")), funs(df = first_DF(.)))  %>%
    
    #between effect
    mutate_at(vars(ends_with("num_ctl")), funs(bw = mean(., na.rm=T))) %>% 
    mutate_at(vars(starts_with("FKM_")), funs(bw = mean(., na.rm=T))) %>% 
    
    
    
    select_at(vars(country_text_id, year_0, 
                   starts_with("FKM_5"),
                   matches("ds"), 
                   matches("ctl"), 
                   -matches("index"))
    ) %>% 
    ungroup()   %>% 
    as.data.frame() 
}



ds_list[[1]] %>% 
  group_by(year_0) %>% 
  select_at(vars(ends_with("ctl"), ends_with("ds"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year_0") %>% 
  ggplot(aes(x=year_0, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample - Domestic Security")


# corrplot
corrplot(cor(ds_list[[1]] %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),
                              -matches("lead"),
                              -ends_with("ctl"))), use="pairwise"), method="number")



#Economic Inequality ####
# ADL
pubsafe_ds_mod1 = brm_multiple(
  pubsafe_ds ~ 1 + 
    # path dependence
    pubsafe_ds_lag +
    
    #economic modernization
    pop_over65_wdi_num_ctl_wi + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    classification_core_num_ctl_wi + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    FKM_5_mb_FEC_wi + FKM_5_mb_FEC_wi_lag + FKM_5_mb_FEC_bw +
    FKM_5_mb_Fec_wi + FKM_5_mb_Fec_wi_lag + FKM_5_mb_Fec_bw + 
    FKM_5_mb_FeC_wi + FKM_5_mb_FeC_wi_lag + FKM_5_mb_FeC_bw + 
    FKM_5_mb_fEC_wi + FKM_5_mb_fEC_wi_lag + FKM_5_mb_fEC_bw +
    (1|country_text_id) + (1|year_0),
  data = ds_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 8000,
  control = list(adapt_delta = 0.95), 
  chains = 6
)

summary(pubsafe_ds_mod1, prob = .95)
prior_summary(pubsafe_ds_mod1)

# ECM
pubsafe_ds_mod_ecm = brm_multiple(
  pubsafe_ds_df ~ 1 + 
    # path dependence
    pubsafe_ds_lag +
    
    #economic modernization
    pop_over65_wdi_num_ctl_wi_df + pop_over65_wdi_num_ctl_wi_lag + pop_over65_wdi_num_ctl_bw + 
    
    # Parties
    cabinet_cpds_num_ctl_wi_df + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw + 
    
    #PRT
    unions_vi_num_ctl_wi_df + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    
    # formal institutions
    # cbi_w_cbi_num_ctl_wi_df + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    
    # informal institutions
    classification_core_num_ctl_wi_df + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi_df + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    
    # international factors
    trade_wdi_num_ctl_wi_df + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
    
    # ideosyncratic factors
    # gini_wdi_num_ctl_df + gini_wdi_num_ctl_lag +
    
    # main independent variable
    FKM_5_mb_fEc_wi_df + FKM_5_mb_fEc_wi_lag + FKM_5_mb_fEc_bw +
    FKM_5_mb_FEC_wi_df + FKM_5_mb_FEC_wi_lag + FKM_5_mb_FEC_bw +
    FKM_5_mb_Fec_wi_df + FKM_5_mb_Fec_wi_lag + FKM_5_mb_Fec_bw + 
    #FKM_5_mb_FeC_wi_df + FKM_5_mb_FeC_wi_lag + FKM_5_mb_FeC_bw + 
    FKM_5_mb_fEC_wi_df + FKM_5_mb_fEC_wi_lag + FKM_5_mb_fEC_bw +
    (1|country_text_id) + (1|year_0),
  data = ds_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 8000,
  control = list(adapt_delta = 0.99), 
  chains = 6
)

summary(pubsafe_ds_mod_ecm, prob = .95)
plot(pubsafe_ds_mod_ecm)
marginal_effects(pubsafe_ds_mod_ecm)
conditional_effects(pubsafe_ds_mod_ecm, "FKM_")


# LATENT PATTERN MAINTENANCE ####
# Confidence ####

glmmTMB()
        