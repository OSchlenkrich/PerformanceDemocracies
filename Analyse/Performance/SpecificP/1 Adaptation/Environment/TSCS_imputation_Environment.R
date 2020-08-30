# ENVIRONMENT IMPUTATION ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")

# Create Dataset ####
mice_data_env = as.data.frame(tscs_data) %>%
  select_at(vars(country_text_id, year,
                 starts_with("FKM"),
                 -matches("_cluster"),
                 ends_with("env"), 
                 ends_with("ctl"), 
                 ends_with("odempr"),
                 -pop_over65_wdi_pr_ctl)) %>% 
  # deselect central bank independence: not imporant for environment
  select_at(vars(-matches("_cbi_"))) %>% 
  # filter NAs
  filter(is.na(GEP_env) == F)   %>% 
  
  # Spatial Correlation %>% 
  left_join(spatial_dv_year(tscs_data, "GEP_env"), by=c("country_text_id", "year")) %>% 

  mutate(year_0 = year - min(year)) %>% 
  
  select(-year) %>% 
  ungroup() 


# Transformations ####
mice_data_env_trans = mice_data_env %>% 
  # trimming, scaling & transforming to normal data
  mutate_at(vars(trade_wdi_num_ctl,
                 corporatism_vdem_pr_ctl), funs(trim(., 0.01, minimum=T))) %>%
  mutate_at(vars(stateterr_vdem_pr_ctl), funs(trim(., 0.02, minimum=T))) %>%
  mutate_at(vars(matches("notrans")), funs(trim(., 0.02, minimum=F))) %>%
  
  mutate_at(vars(ends_with("num_ctl"), ends_with("spatial_ctl")), funs(ladder_fun(.))) %>% 
  mutate_at(vars(matches("_pr_"), -matches("plt"), -matches("cpds")), funs(folded_ladder_fun(., plotting =F))) %>% 
  mutate_at(vars(ends_with("num_ctl"), matches("_pr_"), ends_with("spatial_ctl")), funs(scale_this(.)))  


#missings frame ####

mice_data_naframe_env = mice_data_env_trans %>% 
  mutate_at(vars(-matches("lag"),-matches("lead")), funs(is_na = ifelse(is.na(.)==T, 1,0))) %>% 
  select(matches("is_na"))

  
 
# Missings Plot ####
mice_data_env_trans %>% 
  select_at(vars(-matches("lag"),-matches("lead"), -country_text_id,-matches("odempr"))) %>% 
  group_by(year_0) %>% 
  summarise_all(pMiss_01) %>% 
  pivot_longer(cols=-year_0) %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_notrans_ctl", "", name),
         name = gsub("_ctl", "", name)) %>% 
  ggplot(aes(x=year_0, y=value, fill=name)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(name~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in TSCS - Environmental Performance")


# Distributions Plot ####
raw_dist = mice_data_env %>% 
  select_if(is.numeric) %>% 
  select_at(vars(-matches("lag"),-matches("lead"), -year_0,-matches("odempr"))) %>% 
  pivot_longer(cols=everything())  %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_notrans_ctl", "", name),
         name = gsub("_ctl", "", name)) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(name~., scales = "free") +
  theme_bw() +
  ggtitle("Raw Values") +
  xlab("") +
  ylab("")


trans_dist = mice_data_env_trans %>% 
  select_if(is.numeric) %>% 
  select_at(vars(-matches("lag"),-matches("lead"), -year_0,-matches("odempr"))) %>% 
  pivot_longer(cols=everything())  %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_notrans_ctl", "", name),
         name = gsub("_ctl", "", name)) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(name~., scales = "free") +
  theme_bw()  +
  ggtitle("Transformed Values") +
  xlab("") +
  ylab("")

ggarrange(raw_dist, trans_dist, ncol=1)


# corrplot  Plot ####
corrplot(cor(mice_data_env_trans %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))) %>% 
               rename_all(funs(gsub("_pr_ctl", "", .)))  %>% 
               rename_all(funs(gsub("_num_ctl", "", .))) %>% 
               rename_all(funs(gsub("_notrans_ctl", "", .))) %>% 
               rename_all(funs(gsub("_ctl", "", .))) %>% 
               rename_all(funs(gsub("_odempr", "", .))), use="pairwise"))


# XYPlots  Plot ####
names(mice_data_env_trans)
p1 = xyplot(mice_data_env_trans, "GEP_env", c("GEP_env_spatial_ctl")) 
p1

# Imputation ####
mice_data_env_trans_mice = mice_data_env_trans %>% 
  
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

mice_data_env_trans_mice[] <- lapply(mice_data_env_trans_mice, function(x) { attributes(x) <- NULL; x })


# Imputation
# includes: FE, Polynomial

nr_imputations = 5
nr_cores = 5

a.out_env <- amelia(mice_data_env_trans_mice, 
                    m = nr_imputations, 
                    ts = "year_0", 
                    cs = "country_text_id", 
                    # noms=c("fiscalcrisis_cat_ctl"), 
                    polytime = 1,
                    intercs = T,
                    p2s = 2,
                    parallel = "snow",
                    ncpus	= nr_cores,
                    empri = .05*nrow(mice_data_env_trans_mice)
)

a.out_env

# Saving ####

save(a.out_env, file = "Analyse/Performance/SpecificP/Datasets/a.out_env_v4.Rdata")
save(mice_data_naframe_env, file = "Analyse/Performance/SpecificP/Datasets/mice_data_naframe_env.Rdata")

# load("Analyse/Performance/SpecificP/Datasets/a.out_env_v4.Rdata")


# Diagnostics ####

disp_env_tscs = disperse(a.out_env, dims = 1, m = 5)
# saveRDS(disp_env_tscs, "Analyse/Performance/SpecificP/1 Adaptation/Environment/RObjects/disp_env_tscs.RDS")



disp_env_tscs = readRDS("Analyse/Performance/SpecificP/1 Adaptation/Environment/RObjects/disp_env_tscs.RDS")
convergence_amelia(disp_env_tscs)  +
  ggtitle("TSCS Environment: Overdispersed Starting Values")


gdppc_env = Amelia::overimpute(a.out_env, var = "gdppc_wdi_num_ctl")
# saveRDS(gdppc_env, "Analyse/Performance/SpecificP/1 Adaptation/Environment/RObjects/gdppc_env.RDS")
trade_wdi_env = Amelia::overimpute(a.out_env, var = "trade_wdi_num_ctl")
# saveRDS(trade_wdi_env, "Analyse/Performance/SpecificP/1 Adaptation/Environment/RObjects/trade_wdi_env.RDS")

ggarrange(
  overimpute_gglot(gdppc_env, "gdppc_wdi"),
  overimpute_gglot(trade_wdi_env, "trade_wdi")
)
