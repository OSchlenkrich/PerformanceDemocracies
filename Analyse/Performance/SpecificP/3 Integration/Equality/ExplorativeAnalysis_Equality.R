# Econonmic Explorative Analysis ####

# Setup ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/a.out_soc_v4.Rdata")
#source("Analyse/Performance/SpecificP/3 Integration/Equality/TSCS_imputation_SocialP.R")
load("Analyse/Performance/SpecificP/Datasets/mice_data_naframe_soc.Rdata")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")


# Create TSCS Data ####
eco_equal_list = make_reg_data(a.out_soc, "eco_equal_soc", 
                               naframe = mice_data_naframe_soc, 
                               vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                                "execpar_1981_odempr", "feduni1981_odempr",
                                                "centrip_odempr"),
                               lag2 = T,
                               nr_imputations = 5) 

soc_equal_list = make_reg_data(a.out_soc, "soc_equal_soc", 
                               naframe = mice_data_naframe_soc, 
                               vars_noimput = c("cabinet_cpds_cat_ctl", "unions_vi_num_ctl",
                                                "execpar_1981_odempr", "feduni1981_odempr",
                                                "centrip_odempr"),
                               lag2 = T,
                               nr_imputations = 5) 


# Combine TSCS Data ####
tscs_soc_list = list()
for (i in 1:length(prod_list)) {
  tscs_soc_list[[i]] = eco_equal_list[[i]] %>% 
    select(-soc_equal_soc, -soc_equal_soc_spatial_ctl) %>% 
    bind_cols(soc_equal_list[[i]] %>% 
                select(soc_equal_soc, soc_equal_soc_df, soc_equal_soc_lag, soc_equal_soc_lag2, 
                       soc_equal_soc_wi_lag, soc_equal_soc_wi_lag2, soc_equal_soc_spatial_ctl))
}


# Missing Data ####
tscs_soc_list[[1]] %>% 
  filter(is.na(eco_equal_soc) == F) %>% 
  group_by(year_0) %>% 
  select_at(vars(ends_with("ctl"), ends_with("soc"))) %>% 
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
tscs_soc_list[[1]] %>% 
  select_at(vars(ends_with("ctl"), ends_with("soc"), ends_with("T", ignore.case	=F))) %>% 
  rename_all(funs(gsub("_num_ctl","",.)))  %>%
  rename_all(funs(gsub("\\..*T", "", .)))  %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot()
tscs_soc_list[[1]] %>% 
  select_at(vars(ends_with("ctl"), ends_with("soc"))) %>% 
  rename_all(funs(gsub("_num_ctl","",.)))  %>%
  rename_all(funs(gsub("\\..*T", "", .)))  %>% 
  cor(use="pairwise", method = "pearson") %>% 
  corrplot(method="number")


# XY Plots ####
p1 = xyplot(tscs_soc_list[[1]] %>% 
              select_at(vars(FKM5_E, FKM5_c, FKM5_Fec, FKM5_fEc, FKM5_fEC, FKM5_FEC, ends_with("ctl"), ends_with("soc"))), 
            "eco_equal_soc", c("soc_equal_soc","soc_equal_soc_spatial_ctl")) 
p2 = xyplot(tscs_soc_list[[1]] %>% 
              select_at(vars(FKM5_E, FKM5_c, FKM5_Fec, FKM5_fEc, FKM5_fEC, FKM5_FEC, ends_with("ctl"), ends_with("soc"))), 
            "soc_equal_soc", c("eco_equal_soc","eco_equal_soc_spatial_ctl")) 
ggarrange(p1, p2, ncol=1)


# Check Stationarity ####
vars = tscs_soc_list[[1]] %>% 
  select_if(is.numeric) %>% 
  select_at(vars(eco_equal_soc,
                 soc_equal_soc,
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

chech_stationarity_Beck(vars, tscs_soc_list, model = "glmmTMB")


