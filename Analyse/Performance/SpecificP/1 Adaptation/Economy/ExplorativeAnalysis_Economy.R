# Econonmic Explorative Analysis ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/economy_out_v4.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_eco.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/Economy/TSCS_imputation_Economy.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")


# Create TSCS Data ####
wealth_list = make_reg_data(economy_out, "wealth_eco", 
                            naframe = mice_data_naframe_eco, 
                            vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                             "execpar_1981_odempr", "feduni1981_odempr",
                                             "centrip_odempr"),
                            lag2 = T,
                            nr_imputations = 5) 
prod_list = make_reg_data(economy_out, "productivity_eco", 
                            naframe = mice_data_naframe_eco, 
                            vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                             "execpar_1981_odempr", "feduni1981_odempr",
                                             "centrip_odempr"),
                            lag2 = T,
                          nr_imputations = 5) 

# Combine TSCS Data ####
tscs_eco_list = list()
for (i in 1:length(prod_list)) {
  tscs_eco_list[[i]] = wealth_list[[i]] %>% 
    select(-productivity_eco, -productivity_eco_spatial_ctl) %>% 
    bind_cols(prod_list[[i]] %>% 
                select(productivity_eco, productivity_eco_df, productivity_eco_lag, productivity_eco_lag2, 
                       productivity_eco_wi_lag, productivity_eco_wi_lag2, productivity_eco_spatial_ctl))
}


# Missing Data ####
tscs_eco_list[[1]] %>% 
  filter(is.na(wealth_eco) == F) %>% 
  group_by(year_0) %>% 
  select_at(vars(ends_with("ctl"), ends_with("eco"))) %>% 
  summarise_all(pMiss_01) %>% 
  melt(id.vars="year_0") %>%
  mutate(variable = gsub("_num_ctl", "", variable)) %>% 
  ggplot(aes(x=year_0, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) + 
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample - Economy")


# Collinearity ####
tscs_eco_list[[1]] %>% 
  select_at(vars(ends_with("ctl"), ends_with("eco"), ends_with("T", ignore.case	=F))) %>% 
  rename_all(funs(gsub("_num_ctl","",.)))  %>%
  rename_all(funs(gsub("\\..*T", "", .)))  %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot()
tscs_eco_list[[1]] %>% 
  select_at(vars(ends_with("ctl"), ends_with("eco"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.)))  %>%
  rename_all(funs(gsub("\\..*T", "", .)))  %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")


# XY Plots ####
p1 = xyplot(tscs_eco_list[[1]] %>% 
              select_at(vars(FKM4_E, FKM4_c, FKM5_Fec, FKM5_fEc, FKM5_fEC, FKM5_FEC, ends_with("ctl"), ends_with("eco"))), 
            "wealth_eco", 
            c("productivity_eco", "productivity_eco_spatial_ctl", "gdppc_wdi_num_ctl")) 
p2 = xyplot(tscs_eco_list[[1]] %>% 
              select_at(vars(FKM4_E, FKM4_c, FKM5_Fec, FKM5_fEc, FKM5_fEC, FKM5_FEC, ends_with("ctl"), ends_with("eco"))), 
            "productivity_eco", c("wealth_eco","wealth_eco_spatial_ctl", "gdppc_wdi_num_ctl")) 
ggarrange(p1, p2, ncol=1)


tscs_eco_list[[1]]$FKM5_E
# Check Stationarity ####
vars = tscs_eco_list[[1]] %>% 
  select_if(is.numeric) %>% 
  select_at(vars(wealth_eco,
                 productivity_eco,
                 FKM4_E,
                 FKM4_c,
                 FKM4_c,
                 FKM5_fEC,
                 FKM5_FeC,
                 FKM5_Fec,
                 FKM5_fEc,
                 FKM5_FEC,
                 ends_with("ctl"),
                 -matches("_cat"),
                 -gdppc_wdi_num_ctl_wi,
                 -matches("_spatial"))) %>% 
  colnames()

chech_stationarity_Beck(vars, tscs_eco_list, model = "glmmTMB")

# Only Between Effect of Population Size

