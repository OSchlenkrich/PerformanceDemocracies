# Econonmic Explorative Analysis ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_env_v4.Rdata")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_env.Rdata")
# source("Analyse/Performance/SpecificP/1 Adaptation/Economy/TSCS_imputation_Economy.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")
source("Setup/brms_tables.R")


# Create TSCS Data ####
air_list = make_reg_data(a.out_env, "GEP_env", 
                         naframe = mice_data_naframe_env, 
                         vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl", 
                                          "green_plt_cpds_notrans_ctl", "strength_green_notrans_ctl",
                                          "execpar_1981_odempr", "feduni1981_odempr",
                                          "centrip_odempr"),
                         lag2 = T,
                         nr_imputations = 5)


# Missing Data ####
air_list[[1]] %>% 
  filter(is.na(GEP_env) == F) %>% 
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
p1 = xyplot(air_list[[1]] %>% 
              select_at(vars(FKM5_E, FKM5_c, FKM5_Fec, FKM5_fEc, FKM5_fEC, FKM5_FEC, ends_with("ctl"), ends_with("env"))), 
            "GEP_env") 

p1


# Check Stationarity ####
vars = air_list[[1]] %>% 
  select_if(is.numeric) %>% 
  select_at(vars(GEP_env,
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
                 -matches("_spatial"))) %>% 
  colnames()

chech_stationarity_Beck(vars, air_list, model = "glmmTMB")


