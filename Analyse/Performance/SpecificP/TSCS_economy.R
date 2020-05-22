#  Economy TSCS

library(itsadug)
library(lme4)
library(lmerTest)
library(glmmTMB)


load("Analyse/Performance/SpecificP/Datasets/economy_out.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")



# Create List-Data ####
LogRATIOS_eco_basis = LR(economy_out$imputations[[1]] %>%  select_at(vars(starts_with("FKM_5"))) %>% 
                     mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))

LogRATIOS_eco_dim = LR(economy_out$imputations[[1]] %>%  
                         select_at(vars(starts_with("FKM_5"))) %>%
                         mutate(FKM_5_mb_dim_c = FKM_5_mb_fEc + FKM_5_mb_Fec,
                                FKM_5_mb_dim_C = FKM_5_mb_fEC + FKM_5_mb_FeC,
                                FKM_5_mb_dim_E = FKM_5_mb_fEc + FKM_5_mb_fEC,
                                FKM_5_mb_dim_e = FKM_5_mb_Fec + FKM_5_mb_FeC) %>% 
                         select_at(vars(matches("_dim_"))) %>% 
                         mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))

LogRATIOS_eco_total = LR(economy_out$imputations[[1]] %>%  
                           select_at(vars(starts_with("FKM_5"))) %>% 
                           mutate(
                             FKM_5_mb_tot_fEcT = FKM_5_mb_Fec + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_FEC,
                             FKM_5_mb_tot_FecT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_FEC,
                             FKM_5_mb_tot_FECT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_FeC + FKM_5_mb_Fec,
                             FKM_5_mb_tot_FeCT = FKM_5_mb_fEc + FKM_5_mb_fEC + FKM_5_mb_Fec + FKM_5_mb_FEC,
                             FKM_5_mb_tot_fECT = FKM_5_mb_fEc + FKM_5_mb_Fec + FKM_5_mb_FeC + FKM_5_mb_FEC)%>% 
                           mutate_at(vars(starts_with("FKM")), funs(zeroadjuster(.))))
LogRATIOS_eco_total = data.frame(LogRATIOS_eco_total$LR) %>% 
  select_at(vars(matches("_tot_"))) 



wealth_list = list()

for (i in 1:1) {
  wealth_list[[i]]  = economy_out$imputations[[i]] %>% 
    select_at(vars(-matches("lag"),-matches("lead"),-matches("_mb_"))) %>% 
    
    #add logratios
    bind_cols(data.frame(LogRATIOS_eco_basis$LR)) %>% 
    bind_cols(data.frame(LogRATIOS_eco_dim$LR)) %>% 
    bind_cols(data.frame(LogRATIOS_eco_total)) %>% 
    
    
    group_by(country_text_id) %>% 
    # fill in gaps for correct lags
    tidyr::complete(country_text_id, year_0 = min(year_0):max(year_0), fill = list(NA)) %>% 
    
    mutate(
      wealth_eco_lag = dplyr::lag(wealth_eco, 1),
      wealth_eco_lag2 = dplyr::lag(wealth_eco, 2),
      wealth_eco_lag3 = dplyr::lag(wealth_eco, 3),
      
      wealth_eco_df = first_DF(wealth_eco),
      
      productivity_eco_lag = dplyr::lag(productivity_eco, 1),
      productivity_eco_lag2 = dplyr::lag(productivity_eco, 2),
      productivity_eco_lag3 = dplyr::lag(productivity_eco, 3),
      productivity_eco_lag4 = dplyr::lag(productivity_eco, 4),
      productivity_eco_lag5 = dplyr::lag(productivity_eco, 5),
      
      productivity_eco_df = first_DF(productivity_eco),
      
      classification_core_num_ctl = classification_core,
      trend = year_0 - median(year_0, na.rm=T)
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
    # LDV
    mutate_at(vars(matches("lag")), funs(wi = . - mean(., na.rm=T))) %>%
    
    # INDEPENDENT VARIABLES
    mutate_at(vars(ends_with("num_ctl")), funs(df = first_DF(.)))  %>%
    mutate_at(vars(ends_with("num_ctl")), funs(wi = . - mean(., na.rm=T))) %>%
    mutate_at(vars(ends_with("num_ctl_wi")), funs(df = first_DF(.)))  %>%
    mutate_at(vars(ends_with("num_ctl_wi")), funs(lag = dplyr::lag(.,1))) %>% 
    mutate_at(vars(ends_with("num_ctl_wi")), funs(lag2 = dplyr::lag(.,2))) %>% 
    # mutate_at(vars(ends_with("num_ctl_wi")), funs(lag3 = dplyr::lag(.,3))) %>% 
    # mutate_at(vars(ends_with("num_ctl_wi")), funs(lag4 = dplyr::lag(.,4))) %>% 
    
    # DEMOCRACY PROFILES
    mutate_at(vars(starts_with("FKM_")), funs(df = first_DF(.))) %>%
    mutate_at(vars(starts_with("FKM_")), funs(wi = . - mean(., na.rm=T))) %>%
    mutate_at(vars(matches("FKM_"), -matches("df")), funs(lag = dplyr::lag(.,1))) %>%
    mutate_at(vars(matches("FKM_"), -matches("df"), -matches("lag")), funs(lag2 = dplyr::lag(.,2))) %>%
    # mutate_at(vars(matches("FKM_"), -matches("df"), -matches("lag")), funs(lag3 = dplyr::lag(.,3))) %>%
    # mutate_at(vars(matches("FKM_"), -matches("df"), -matches("lag")), funs(lag4 = dplyr::lag(.,4))) %>%
    
    
    #between effect
    mutate_at(vars(ends_with("num_ctl")), funs(bw = mean(., na.rm=T))) %>% 
    mutate_at(vars(starts_with("FKM_"), -matches("wi"), -matches("df"), -matches("lag")), funs(bw = mean(., na.rm=T))) %>% 
    
    
    select_at(vars(country_text_id, year_0, trend, 
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
                              -matches("wi"),
                              -matches("df"),
                              -matches("mb"),
                               ends_with("ctl"),
                              -ends_with("ctl"))), use="pairwise"))



# WEALTH ####

# Check Stationarity ####
vars = wealth_list[[1]] %>% 
  select_if(is.numeric) %>% 
  select_at(vars(ends_with("eco"),
                 ends_with("ctl"), 
                 -fiscalcrisis_num_ctl,
                 -cbi_u_cbi_num_ctl,          
                 -cbi_w_cbi_num_ctl,
                 -corporatism_vdem_num_ctl,
                 -classification_core_num_ctl)) %>% 
  colnames()

chech_stationarity_Beck(vars, wealth_list, model = "glmmTMB")
chech_stationarity_Fisher(vars, wealth_list)

# Check Unit Heterogeneity #####
check_wealth_data = wealth_list[[1]] %>% 
  select(country_text_id, year_0, 
         wealth_eco_df, 
         wealth_eco_lag, 
         wealth_eco_lag2, 
         wealth_eco_lag3, 
         trend
         ) %>% 
  na.omit()


wealth_bayes_homogen = brm(
  wealth_eco_df ~ 1,
  data = check_wealth_data, family = gaussian(),
  warmup = 2000, iter = 7000,
  chains = 6
)
wealth_bayes_homogen <- add_criterion(wealth_bayes_homogen, "loo", reloo=T)

wealth_bayes_unit = brm(
  wealth_eco_df ~ 
    (1|country_text_id),
  data = check_wealth_data, family = gaussian(),
  prior = prior_unit_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6
)
wealth_bayes_unit <- add_criterion(wealth_bayes_unit, "loo", reloo=T)

loo_compare(wealth_bayes_homogen, wealth_bayes_unit, criterion = "loo")

#PPC
ppc_unithet(wealth_bayes_homogen, wealth_bayes_unit, dataset = check_wealth_data, "wealth_eco_df", "country")


# Check Contemporaneous Correlation ####

wealth_bayes_corr = brm(
  wealth_eco_df ~ 
    (1|country_text_id) + (1|year_0),
  data = check_wealth_data, family = gaussian(),
  prior = prior_unit_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6
)
wealth_bayes_corr <- add_criterion(wealth_bayes_corr, "waic", reloo=T)
loo_compare(wealth_bayes_unit, wealth_bayes_corr, criterion = "loo")

#PPC
ppc_unithet(wealth_bayes_unit, wealth_bayes_corr, dataset = check_wealth_data, "wealth_eco_df", "year_0")


# Check Panel heteroscedasticity ####

wealth_bayes_panelhet = brm(
  bf(
    wealth_eco_df ~ 
      wealth_eco_lag +
      (1 + wealth_eco_lag|country_text_id) + (1|year_0),
    sigma = ~ 1 + (1|country_text_id)),
  data = check_wealth_data, family = gaussian(),
  prior = prior_full_phet_tscs,
  warmup = 2000, iter = 7000,
  chains = 6, 
  control = list(adapt_delta = 0.95)
)

wealth_bayes_panelhet <- add_criterion(wealth_bayes_panelhet, "loo", reloo=T)
loo_compare(wealth_bayes_panelhet, wealth_bayes_lag_rc, criterion = "waic")

ppc_panelhet(wealth_bayes_corr, wealth_bayes_panelhet, dataset = check_wealth_data, "wealth_eco_df")

# Check Autocorrelation ####

wealth_bayes_lag = brm(
  bf(wealth_eco_df ~ 
       wealth_eco_lag_wi +
    (1|country_text_id) + (1|year_0),
    sigma = ~ 1 + (1|country_text_id)),
  data = check_wealth_data, family = gaussian(),
  prior = prior_full_phet_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6, 
  control = list(adapt_delta = 0.95)
)
wealth_bayes_lag <- add_criterion(wealth_bayes_lag, "loo", reloo=T)

ppc_auto(wealth_bayes_lag, dataset = check_wealth_data, "wealth_eco_df")

wealth_bayes_lag2 = brm(
  bf(wealth_eco_df ~ 
    wealth_eco_lag +
    wealth_eco_lag2 +
    (1|country_text_id) + (1|year_0),
    sigma = ~ 1 + (1|country_text_id)),
  data = check_wealth_data, family = gaussian(),
  prior = prior_full_phet_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6, 
  control = list(adapt_delta = 0.95)
)
wealth_bayes_lag2 <- add_criterion(wealth_bayes_lag2, "loo", reloo=T)
ppc_auto(wealth_bayes_lag2, dataset = check_wealth_data, "wealth_eco_df")

wealth_bayes_lag3 = brm(
  bf(wealth_eco_df ~ 
    wealth_eco_lag +
    wealth_eco_lag2 +
    wealth_eco_lag3 +
    (1|country_text_id) + (1|year_0),
    sigma = ~ 1 + (1|country_text_id)),
  data = check_wealth_data, family = gaussian(),
  prior = prior_full_phet_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6, 
  control = list(adapt_delta = 0.95)
)
wealth_bayes_lag3 <- add_criterion(wealth_bayes_lag3, "loo", reloo=T)
ppc_resid_time(wealth_bayes_lag3, dataset = check_wealth_data,  country_sample =c("USA", "TTO", "GRC"),  title="lag")

loo_compare(wealth_bayes_lag, wealth_bayes_lag2, wealth_bayes_lag3, criterion = "loo")

loo_compare(wealth_bayes_corr, wealth_bayes_lag)

# Check Autocorrelation Random Coefficient ####
wealth_bayes_lag_rc = brm(
  bf(wealth_eco_df ~ 
    wealth_eco_lag +
    (1 + wealth_eco_lag|country_text_id) + (1|year_0),
    sigma = ~ 1 + (1|country_text_id)),
  data = check_wealth_data, family = gaussian(),
  prior = prior_full_phet_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6
)

wealth_bayes_lag_rc <- add_criterion(wealth_bayes_lag_rc, "loo", reloo=T)

ppc_auto(wealth_bayes_lag, dataset = check_wealth_data, "wealth_eco_df", country_sample = c("CHE","USA", "ZAF", "VUT"))
ppc_auto(wealth_bayes_lag_rc, dataset = check_wealth_data, "wealth_eco_df", country_sample = c("CHE","USA", "ZAF", "VUT"))

ppc_resid_time(wealth_bayes_lag_rc, dataset = check_wealth_data,  country_sample =c("USA", "TTO", "GRC"), title="lag + rc")
ppc_resid_time(wealth_bayes_lag, dataset = check_wealth_data,  country_sample =c("USA", "TTO", "GRC"),  title="lag")
ppc_resid_time(wealth_bayes_panelhet, dataset = check_wealth_data, title= "no lag")

loo_compare(wealth_bayes_lag,
            wealth_bayes_lag_rc, criterion = "loo")


### Check Trend ####

wealth_bayes_trend = brm(
  bf(wealth_eco_df ~ 
       wealth_eco_lag_wi +
       poly(trend,5) +
       (1 + poly(trend,5)|country_text_id) + (1|year_0),
     sigma = ~ 1 + (1|country_text_id)),
  data = check_wealth_data, family = gaussian(),
  prior = prior_full_phet_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6
)

wealth_bayes_trend <- add_criterion(wealth_bayes_trend, "loo", reloo=T)
ppc_resid_time(wealth_bayes_trend, dataset = check_wealth_data,  country_sample =c("TWN","FIN", "ITA", "CHE", "JAM"), title="lag + rc")
ppc_resid_time(wealth_bayes_lag, dataset = check_wealth_data,  country_sample =c("TWN","FIN", "ITA", "CHE", "JAM"),  title="lag")

loo_compare(wealth_bayes_trend, wealth_bayes_lag, criterion = "loo")

###

wealth_bayes_m1 = brm(
  bf(wealth_eco_df ~ 
       wealth_eco_lag_wi +
       poly(trend,4) +
       socdem_plt_cpds_num_ctl_wi_df + socdem_plt_cpds_num_ctl_wi_lag + socdem_plt_cpds_num_ctl_bw + 
       (1 + poly(trend,4)|country_text_id) + (1|year_0),
     sigma = ~ 1 + (1|country_text_id)),
  data = wealth_list[[1]], family = gaussian(),
  prior = prior_full_phet_tscs ,
  warmup = 2000, iter = 7000,
  chains = 6
)
wealth_bayes_m1
lag_distribution_bayes(wealth_bayes_m1, "wealth_eco_lag_wi", "socdem", unit = 1, time_periods=4, ci=0.95)



# ECM Regression ####
general_formula = as.formula(
  wealth_eco_df ~ 1 + 
    
    (1|country_text_id) +  (1|year_0)  +
    wealth_eco_lag
    
    
    
    # socdem_plt_cpds_num_ctl_wi_df + socdem_plt_cpds_num_ctl_wi_lag + socdem_plt_cpds_num_ctl_bw +
    # liberal_plt_cpds_num_ctl_wi_df + liberal_plt_cpds_num_ctl_wi_lag + liberal_plt_cpds_num_ctl_bw

    # liberal_plt_cpds_num_ctl_bw +
    # socdem_plt_cpds_num_ctl_bw +
    # unions_vi_num_ctl_bw +
    #corruption_vdem_num_ctl_bw +
    # trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag +trade_wdi_num_ctl_bw +
    #classification_core_num_ctl_bw
    # FKM_5_mb_dim_E.FKM_5_mb_dim_e_bw
    # # # poly(year_0,2) +
    # # 
    # # wealth_eco_lag3 +
    # 
    # #fiscalcrisis_num_ctl +
    # 
    # #economic modernization
    # 
    # # # Parties
    #cabinet_cpds_num_ctl_wi + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw +
    # #socdem_plt_cpds_num_ctl_wi + socdem_plt_cpds_num_ctl_wi_lag + socdem_plt_cpds_num_ctl_bw +
    # # #liberal_plt_cpds_num_ctl_wi + liberal_plt_cpds_num_ctl_wi_lag + liberal_plt_cpds_num_ctl_bw +
    # # # #green_plt_cpds_num_ctl_wif + green_plt_cpds_num_ctl_wi_lag + green_plt_cpds_num_ctl_bw +
    # # #
    # # # #PRT
    # unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw  +
    # # # # #corporatism_vdem_num_ctl_wi_df + corporatism_vdem_num_ctl_wi_lag + corporatism_vdem_num_ctl_bw +
    # # # #
    # # # # # formal institutions
    # #cbi_w_cbi_num_ctl_bw +
    # # # #
    # # # # # informal institutions
    # classification_core_num_ctl_wi + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    # corruption_vdem_num_ctl_wi + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    # # # #
    #  # # international factors
    # trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw 
    # # # #
    # FKM_5_mb_dim_c.FKM_5_mb_dim_C_df + FKM_5_mb_dim_c.FKM_5_mb_dim_C_wi_lag + FKM_5_mb_dim_c.FKM_5_mb_dim_C_bw +
    # FKM_5_mb_dim_E.FKM_5_mb_dim_e_df + FKM_5_mb_dim_E.FKM_5_mb_dim_e_wi_lag + FKM_5_mb_dim_E.FKM_5_mb_dim_e_bw
    # 
    # #FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_df_wi + FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_wi_lag + FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_bw
    # #FKM_5_mb_fEC.FKM_5_mb_tot_fECT_df_wi + FKM_5_mb_fEC.FKM_5_mb_tot_fECT_wi_lag + FKM_5_mb_fEC.FKM_5_mb_tot_fECT_bw
    # #FKM_5_mb_FEC.FKM_5_mb_tot_FECT_df_wi + FKM_5_mb_FEC.FKM_5_mb_tot_FECT_wi_lag + FKM_5_mb_FEC.FKM_5_mb_tot_FECT_bw
    # # #FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_df_wi + FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_wi_lag + FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_bw
    # # #FKM_5_mb_Fec.FKM_5_mb_tot_FecT_df_wi + FKM_5_mb_Fec.FKM_5_mb_tot_FecT_wi_lag + FKM_5_mb_Fec.FKM_5_mb_tot_FecT_bw

)
# country_text_id (Intercept) 0.0003347 0.01830 
# Residual                    0.0013827 0.03718 


general_model = glmmTMB(
  general_formula,
  #dispformula  = ~ 1 + country_text_id, 
  wealth_list[[1]] 
)


confint(general_model, level = .95)
summary(general_model)
ranef(general_model)
acf_n_plots(residuals(general_model), split_by = list(general_model$frame$country_text_id), random=T)
plot(effect(term="wealth_eco_lag_wi:FKM_5_mb_dim_c.FKM_5_mb_dim_C_bw", general_model, se=F))

performance::icc(general_model)
0.0003347/(0.0003347 +  0.0013967)
9.795e-05/(9.795e-05 + 1.126e-03)


confint(general_model, "wealth_eco_lag")
confint(general_model, "wealth_eco_lag2")

# Check for Autocorrelation  ####
check_LM_plm(general_model, 5)

# Fit in Bayes  ####
wealth_bayes_mod = brm_multiple(
  general_formula,
  data = wealth_list[1], family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 8000,
  control = list(adapt_delta = 0.95), 
  chains = 6
)
summary(wealth_bayes_mod)
wealth_bayes_mod$rhats

ppc = posterior_predict(wealth_bayes_mod)
dim(ppc)

test = data.frame(wealth_bayes_mod$data, posterior = ppc[1,])



test2 %>% 
  filter(country_text_id %in% sample(unique(test$country_text_id), 2)) %>% 
  group_by(country_text_id) %>% 
  mutate(time = 1:length(wealth_eco)) %>% 
  pivot_longer(cols=c("wealth_eco", "posterior")) %>% 
  ggplot(aes(x=time, y=value, col=name)) +
  geom_line(size=1.1) +
  facet_wrap(country_text_id ~ .)


# LRM ####
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "unions", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "cbi_w_cbi", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "corruption", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "trade_wdi", unit = 1, time_periods=4, ci=0.95)


# PRODUCTIVITY ####
# ECM Regression ####
general_formula_prod = as.formula(
  productivity_eco ~ 1 + 
    
    (1 |country_text_id) + (1 |year_0) +
    
    # path dependence
    productivity_eco_lag_wi +
    productivity_eco_lag2_wi  +
    productivity_eco_lag3_wi +
    
    year_0 +
  
    # productivity_eco_lag3 +
    # productivity_eco_lag4 +

    
    #economic modernization
    
    # Parties
    # cabinet_cpds_num_ctl_wi_df + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw 
    # socdem_plt_cpds_num_ctl_wi + socdem_plt_cpds_num_ctl_wi_lag + socdem_plt_cpds_num_ctl_bw +
    # liberal_plt_cpds_num_ctl_wi + liberal_plt_cpds_num_ctl_wi_lag + liberal_plt_cpds_num_ctl_bw +
    cabinet_cpds_num_ctl_wi + cabinet_cpds_num_ctl_wi_lag + cabinet_cpds_num_ctl_bw +
    
    # #PRT
    unions_vi_num_ctl_wi + unions_vi_num_ctl_wi_lag + unions_vi_num_ctl_bw +
    # #corporatism_vdem_num_ctl_wi_df + corporatism_vdem_num_ctl_wi_lag + corporatism_vdem_num_ctl_bw +
    # 
    # # formal institutions
    cbi_w_cbi_num_ctl_wi + cbi_w_cbi_num_ctl_wi_lag + cbi_w_cbi_num_ctl_bw +
    # 
    # # informal institutions
    classification_core_num_ctl_wi + classification_core_num_ctl_wi_lag + classification_core_num_ctl_bw +
    corruption_vdem_num_ctl_wi + corruption_vdem_num_ctl_wi_lag + corruption_vdem_num_ctl_bw +
    # 
    # # international factors
    trade_wdi_num_ctl_wi + trade_wdi_num_ctl_wi_lag + trade_wdi_num_ctl_bw +
     
    FKM_5_mb_dim_c.FKM_5_mb_dim_C_wi + FKM_5_mb_dim_c.FKM_5_mb_dim_C_wi_lag + FKM_5_mb_dim_c.FKM_5_mb_dim_C_bw +
    FKM_5_mb_dim_E.FKM_5_mb_dim_e_wi + FKM_5_mb_dim_E.FKM_5_mb_dim_e_wi_lag + FKM_5_mb_dim_E.FKM_5_mb_dim_e_bw
     
    # FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_df_wi + FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_wi_lag + FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_bw
    # FKM_5_mb_fEC.FKM_5_mb_tot_fECT_df_wi + FKM_5_mb_fEC.FKM_5_mb_tot_fECT_wi_lag + FKM_5_mb_fEC.FKM_5_mb_tot_fECT_bw
    # FKM_5_mb_FEC.FKM_5_mb_tot_FECT_df_wi + FKM_5_mb_FEC.FKM_5_mb_tot_FECT_wi_lag + FKM_5_mb_FEC.FKM_5_mb_tot_FECT_bw
    # FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_df_wi + FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_wi_lag + FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_bw
    # FKM_5_mb_Fec.FKM_5_mb_tot_FecT_df_wi + FKM_5_mb_Fec.FKM_5_mb_tot_FecT_wi_lag + FKM_5_mb_Fec.FKM_5_mb_tot_FecT_bw
  
)

library(lme4)
library(lmerTest)
general_model_prod = glmmTMB(
  general_formula_prod,
  #dispformula  = ~ 1 + country_text_id, 
  wealth_list[[1]],
  control=glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))
)


summary(general_model_prod)




wealth_list[[1]]$productivity_eco_lag

test_cases = sample(unique(general_model_prod$frame$country_text_id), 5)

ggplot(wealth_list[[1]] %>% 
         filter(country_text_id %in% test_cases), 
       aes(x=year_0, y=productivity_eco_lag, col=country_text_id)) +
  geom_line()

test = tscs_data %>% 
  select(country_text_id, year, cabinet_cpds_num_ctl, socdem_plt_cpds_num_ctl)


0.8207 /(0.8207 + 5.261e-02  )

ranef(general_model_prod)
acf_n_plots(residuals(general_model_prod), split_by = list(general_model_prod$frame$country_text_id), random=T)

confint(general_model_prod, "productivity_eco_lag")
confint(general_model_prod, "productivity_eco_lag2")
confint(general_model_prod, "productivity_eco_lag3")

# Check for Autocorrelation  ####
check_LM_plm(general_model_prod, 5)

test_cases = sample(unique(general_model_prod$frame$country_text_id), 5)
test_cases = general_model_prod$frame %>% 
  group_by(country_text_id) %>% 
  summarise(T = n()) %>% 
  filter(T>10) %>% 
  pull(country_text_id)
test_cases2 = sample(test_cases, 5)


test = data.frame(resid = residuals(general_model_prod, type="pearson")) 

gghistogram(test, x="resid", 
            y = "..density..",
            add = "median",
            rug = TRUE ,
            add_density = F) +
  stat_overlay_normal_density(color="red", size=2)


data.frame(general_model_prod$frame, resid = residuals(general_model_prod)) %>% 
  filter(country_text_id %in% test_cases2) %>% 
  ggplot(aes(x=year_0, y=resid, col=country_text_id)) +
  geom_point(size=2) +
  geom_smooth(se=F, span = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(country_text_id ~ .) +
  theme_bw() +
  theme(legend.position = "none")

hist(general_model_prod$frame$FKM_5_mb_dim_c.FKM_5_mb_dim_C_wi)

data.frame(general_model_prod$frame, resid = residuals(general_model_prod)) %>% 
  filter(country_text_id %in% test_cases2) %>% 
  ggplot(aes(x=socdem_plt_cpds_num_ctl_wi , y=resid)) +
  geom_point(size=2) + 
  geom_smooth(se=F, span = 1) +
  geom_hline(yintercept = 0) +
  #facet_wrap(country_text_id ~ .) +
  theme_bw() +
  theme(legend.position = "none")

wealth_list[[1]] %>% 
  filter(country_text_id %in% test_cases2) %>% 
  group_by(year_0) %>% 
  summarise(mean_var = mean(productivity_eco, na.rm=T)) %>% 
  ggplot(aes(x=year_0, y=mean_var)) +
  geom_line()

wealth_list[[1]] %>% 
  bind_cols(resid =residuals(lm(productivity_eco ~ year_0, wealth_list[[1]] ))) %>% 
  filter(country_text_id %in% test_cases2) %>% 
  group_by(year_0) %>% 
  summarise(mean_var = mean(resid, na.rm=T)) %>% 
  ggplot(aes(x=year_0, y=mean_var)) +
  geom_line()


# Fit in Bayes  ####
wealth_bayes_mod = brm_multiple(
  general_formula,
  data = wealth_list, family = gaussian(),
  prior = prior_tscs,
  warmup = 2000, iter = 8000,
  control = list(adapt_delta = 0.95), 
  chains = 6
)
summary(wealth_bayes_mod)

# LRM ####
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "cabinet", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "unions", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "cbi_w_cbi", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "corruption", unit = 1, time_periods=4, ci=0.95)
lag_distribution_bayes(wealth_bayes_mod, "wealth_eco_lag", "trade_wdi", unit = 1, time_periods=4, ci=0.95)
