# Impute and Setup TSCS Data ####

# Setup
source("Analyse/CreateDatasets.R")
source("Analyse/Performance/SpecificP/SpatialAutocorrelation.R")


# Create TSCS Datasets ####

CBI_control = readstata13::read.dta13("Datasets/CBI dataset_2019 corrections.dta") %>%  
  select(-cname) %>% 
  left_join(QoC_data %>% 
              select(country, country_text_id,
                     year,
                     ccodewb) %>% 
              na.omit(), by=c("ccodewb", "year")) %>% 
  select(country_text_id, year, cbi_u_cbi_num_ctl = lvau_garriga, cbi_w_cbi_num_ctl =  lvaw_garriga )

V_dem_control = V_dem_all %>% 
  select(country_text_id, year, 
         corruption_vdem_num_ctl = v2x_corr, 
         corporatism_vdem_num_ctl = v2csstruc_1,
         statehoodr_vdem_num_ctl = v2svstterr) 

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
  
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  # mutate(
  #   fiscalcrisis_num_ctl = if_else(year == 2008 | year == 2009, 1, 0)
    #government_cpds_ord_ctl = ifelse(government_cpds_ord_ctl == 1, 1, 0),
    # cabinet_cpds_ord_ctl = fct_recode(as.factor(cabinet_cpds_ord_ctl), 
    #                                         "H_RW" = "1", 
    #                                         "D_RW" = "2", 
    #                                         "B" = "3", 
    #                                         "D_SD" = "4", 
    #                                         "H_SD" = "5")
    # ) %>% 
  arrange(country_text_id, year)


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
  
  # Create Indpedenent Variable: Democracy Profile
  
  group_by(country_text_id) %>% 
  mutate(FKM_5_mb_fEc = rollapply(FKM_5_mb_fEc, 10, FUN = function(x) mean(x, na.rm=T),
                                            fill=NA, align="right", partial=T),
         FKM_5_mb_Fec = rollapply(FKM_5_mb_Fec, 10, FUN = function(x) mean(x, na.rm=T),
                                   fill=NA, align="right", partial=T),
         FKM_5_mb_FeC = rollapply(FKM_5_mb_FeC, 10, FUN = function(x) mean(x, na.rm=T),
                                   fill=NA, align="right", partial=T),
         FKM_5_mb_fEC = rollapply(FKM_5_mb_fEC, 10, FUN = function(x) mean(x, na.rm=T),
                                   fill=NA, align="right", partial=T),
         FKM_5_mb_FEC = rollapply(FKM_5_mb_FEC, 10, FUN = function(x) mean(x, na.rm=T),
                                   fill=NA, align="right", partial=T),
         
         sum_bw = FKM_5_mb_fEc + FKM_5_mb_Fec + FKM_5_mb_FeC + FKM_5_mb_fEC + FKM_5_mb_FEC,
         
         FKM_5_mb_fEc = FKM_5_mb_fEc/sum_bw,
         FKM_5_mb_Fec = FKM_5_mb_Fec/sum_bw,
         FKM_5_mb_FeC = FKM_5_mb_FeC/sum_bw,
         FKM_5_mb_fEC = FKM_5_mb_fEC/sum_bw,
         FKM_5_mb_FEC = FKM_5_mb_FEC/sum_bw
  ) %>% 
  # fiscal crisis
  mutate(
    fiscalcrisis_num_ctl = if_else(year == 2008 | year == 2009, 1, 0)
  ) %>% 
  # create NA indicator variables
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("ctl")), funs("na" = if_else(all(is.na(.) == T), 1, 0))) %>%
  ungroup() %>% 
  arrange(country_text_id, year)  %>% 
  select(-sum_bw)



# ECONOMY ####

# Multiple Imputation ####
mice_data = as.data.frame(tscs_data) %>%
  select_at(vars(country_text_id, year,
                 starts_with("FKM"),
                 -matches("_cluster"),
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
  # Spatial Correlation %>% 
  left_join(spatial_dv(tscs_data, "wealth_eco"), by="country_text_id") %>% 
  left_join(spatial_dv(tscs_data, "productivity_eco"), by="country_text_id") %>% 
  select(country_text_id, year_0, everything()) 

mice_data_naframe = mice_data %>% 
  mutate_at(vars(-matches("lag"),-matches("lead")), funs(is_na = ifelse(is.na(.)==T, 1,0))) %>% 
  select(matches("is_na"))

mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })

# Missings
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
  ggtitle("Missings in TSCS Economy - Economy")

# Distributions
mice_data %>% 
  select_if(is.numeric) %>% 
  select_at(vars(-matches("lag"),-matches("lead"), -year_0)) %>% 
  melt() %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(variable~., scales = "free")

# corrplot
corrplot(cor(mice_data %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))



# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

economy_out <- amelia(mice_data, 
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


#save(economy_out, file = "Analyse/Performance/SpecificP/Datasets/economy_out_v2.Rdata")
economy_out = load("Analyse/Performance/SpecificP/Datasets/economy_out_v2.Rdata")



# ENVIRONMENT ####
# Multiple Imputation ####
mice_data_env = as.data.frame(tscs_data) %>%
  select_at(vars(country_text_id, year,
                 starts_with("FKM"),
                 -matches("_cluster"),
                 ends_with("env"), 
                 ends_with("ctl"), 
                 wealth_eco_num_ctl = wealth_eco,
                 classification_core)) %>% 
  
  # filter NAs
  filter(is.na(air_env) == F)   %>% 
  
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

mice_data_env[] <- lapply(mice_data_env, function(x) { attributes(x) <- NULL; x })

mice_data_env %>% 
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
corrplot(cor(mice_data_env %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_env <- amelia(mice_data_env, 
                m = nr_imputations, 
                ts = "year_0", 
                cs = "country_text_id", 
                noms=c("classification_core"), 
                polytime = 1,
                intercs = T,
                p2s = 2,
                parallel = "snow",
                ncpus	= nr_cores,
                empri = .05*nrow(mice_data_env)
)

a.out_env
save(a.out_env, file = "Analyse/Performance/SpecificP/Datasets/a.out_env.Rdata")
a.out_env = load("Analyse/Performance/SpecificP/Datasets/a.out_env.Rdata")


# GOAL ATTAINMENT ####
# Lutz ####
# Control Variables: Length and Age of Constitution
ccp_ctl = fread("C:/RTest/ccp_csv.csv") %>% 
  as_tibble() %>% 
  select(systid_ccp = systid, length_const_num_ctl = length, age_const_num_ctl = systyear) %>% 
  group_by(systid_ccp) %>% 
  summarise_all(mean, na.rm=T) 


LogRATIOS_GA = LR(tscs_data %>%  select_at(vars(starts_with("FKM_5_mb"))) %>% 
                 mutate_at(vars(starts_with("FKM_5_mb")), funs(zeroadjuster(.))))



tscs_data_GA_Lutz = tscs_data %>% 
  left_join(ccp_ctl, by="systid_ccp") %>% 
  select_at(vars(country_text_id, year,
                 starts_with("FKM"),
                 GA_lutz_ga,
                 -FKM_5_cluster,
                 cabinet_cpds_num_ctl,
                 wealth_eco_num_ctl = wealth_eco,
                 length_const_num_ctl,
                 age_const_num_ctl,
                 classification_core_num_ctl = classification_core)
  ) %>% 
  mutate(classification_core_num_ctl = ifelse(classification_core_num_ctl == "Deficient Democracy", 1,0 )) %>% 
  
  # Filter NAs Lutz
  filter(is.na(GA_lutz_ga) == F) %>% 
  
  # Calculate Between Values
  group_by(country_text_id) %>% 
  mutate_at(vars(starts_with("FKM_5")), funs(bw = mean(., na.rm=T))) %>% 
  ungroup() %>% 
  
  # Mean Summarize all Values
  group_by(country_text_id) %>% 
  summarise_at(vars(-year), funs(mean(., na.rm=T))) %>%
  ungroup() %>% 
  
  # Transform Values to range between 0 and 1
  mutate(mb_sum = FKM_5_mb_FEC + FKM_5_mb_fEc + FKM_5_mb_Fec + FKM_5_mb_FeC + FKM_5_mb_fEC) %>% 
  mutate_at(vars(matches("_mb_"), -matches("bw")), funs(./mb_sum)) %>%
  select(-mb_sum) %>% 
  

  # scaling
  mutate_at(vars(ends_with("num_ctl")), funs(scale_this(.))) 

# Multiple Imputation ####
tscs_data_GA_Lutz[] <- lapply(tscs_data_GA_Lutz, function(x) { attributes(x) <- NULL; x })

md.pattern(tscs_data_GA_Lutz)

# corrplot
corrplot(cor(tscs_data_GA_Lutz %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

impus = 2

a.out_Lutz <- mice(tscs_data_GA_Lutz, m = impus)


# not exactly normally distributed...
hist(complete(a.out_Lutz, 1)$GA_lutz_ga)



# INTEGRATION I #### 
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
save(a.out_soc, file = "Analyse/Performance/SpecificP/Datasets/a.out_soc.Rdata")
a.out_soc = load("Analyse/Performance/SpecificP/Datasets/a.out_soc.Rdata")



# INTEGRATION II ####
# Domestic Security ####
# Multiple Imputation ####

mice_data_ds = as.data.frame(tscs_data) %>%
  select_at(vars(country_text_id, year,
                 matches("_mb_"),
                 ends_with("ds"),
                 ends_with("ctl"), 
                 classification_core,
                 
                 # Economic Inequality
                 eco_inequal_soc_num_ctl = eco_inequal_soc)) %>% 
  
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
save(a.out_ds, file = "Analyse/Performance/SpecificP/Datasets/a.out_ds.Rdata")
a.out_ds = load("Analyse/Performance/SpecificP/Datasets/a.out_ds.Rdata")



# LATENT PATTERN MAINTENANCE ####
# Confidence ####

        