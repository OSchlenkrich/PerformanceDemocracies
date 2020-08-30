# Domestic Security Regression ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_ds_v4.Rdata")
# source("Analyse/Performance/SpecificP/3 Integration/TSCS_imputation_DomesticP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_ds.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")


# Create TSCS Data ####
domsec_ds_list = make_reg_data(a.out_ds, "domsec_ds", 
                                 naframe = mice_data_naframe_ds, 
                                 vars_noimput = c("cabinet_cpds_cat_ctl",
                                                  "execpar_1981_odempr", "feduni1981_odempr",
                                                  "centrip_odempr"),
                                 lag2 = T,
                                 lag3 = F,
                               nr_imputations = 10) 




# Missing Data ####
domsec_ds_list[[1]] %>% 
  filter(is.na(domsec_ds) == F) %>% 
  group_by(year_0) %>% 
  select_at(vars(ends_with("ctl"), ends_with("ds"))) %>% 
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
  ggtitle("Missings in Democracy Profile Sample - Social Performance")


# Collinearity ####
domsec_ds_list[[1]] %>% 
  select_at(vars(ends_with("ctl"), ends_with("ds"), ends_with("T", ignore.case	=F))) %>% 
  rename_all(funs(gsub("_num_ctl","",.)))  %>%
  rename_all(funs(gsub("\\..*T", "", .)))  %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot()
domsec_ds_list[[1]] %>% 
  select_at(vars(ends_with("ctl"), ends_with("ds"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.)))  %>%
  rename_all(funs(gsub("\\..*T", "", .)))  %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")

# XY Plots ####
p1 = xyplot(domsec_ds_list[[1]] %>% 
              select_at(vars(FKM5_E, FKM5_c, FKM5_Fec, FKM5_fEc, FKM5_fEC, FKM5_FEC, ends_with("ctl"), ends_with("ds"))), 
            "domsec_ds") 

p1

# Check Stationarity ####
vars = domsec_ds_list[[1]] %>% 
  select_if(is.numeric) %>% 
  select_at(vars(domsec_ds,
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

chech_stationarity_Beck(vars, domsec_ds_list, model = "glmmTMB")
