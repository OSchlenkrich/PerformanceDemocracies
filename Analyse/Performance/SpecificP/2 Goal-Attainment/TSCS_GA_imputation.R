# GOAL ATTAINMENT IMPUTATION ####

source("Analyse/Performance/SpecificP/LoadTSCSData.R")


# Extra Control Variables: Length and Age of Constitution ####

ccp_ctl = fread("C:/RTest/ccp_csv.csv") %>% 
  as_tibble() %>% 
  select(systid_ccp = systid, length_const_num_ctl = length, age_const_num_ctl = systyear) %>% 
  group_by(systid_ccp) %>% 
  summarise_all(mean, na.rm=T) %>% 
  # Calculate Age: Reference year 2016
  mutate(age_const_num_ctl = 2016 - age_const_num_ctl) %>% 
  # Fill Missing Length of Constituion for Switzerland
  mutate(length_const_num_ctl = ifelse(systid_ccp == 355, 13992, length_const_num_ctl))



# Create Dataset
tscs_data_GA_Lutz = tscs_data %>% 
  select_at(vars(country_text_id, year, systid_ccp,
                 starts_with("FKM"),
                 -matches("_cluster"),
                 ends_with("GA"),
                 cabinet_cpds_cat_ctl, 
                 ends_with("odempr"))) %>% 

  left_join(ccp_ctl, by="systid_ccp") %>% 
  
  # Filter NAs Lutz
  filter(is.na(arate_lutz_ga) == F | is.na(arate_ccp_ga) == F) %>%  
  # not a time-series dataset: Mean Summarize all Values
  group_by(country_text_id, systid_ccp) %>% 
  summarise_at(vars(-year), funs(mean(., na.rm=T))) %>%
  ungroup() %>% 
  # transform nans
  mutate_all(funs(ifelse(is.nan(.), NA, .)))



# Transformations ####
tscs_data_GA_Lutz_trans = tscs_data_GA_Lutz %>% 
  # trimming, scaling & transforming to normal data
  mutate_at(vars(age_const_num_ctl, length_const_num_ctl), funs(trim(., 0.02, minimum=T))) %>%
  
  mutate_at(vars(ends_with("num_ctl"), ends_with("spatial_ctl")), funs(ladder_fun(.))) %>% 
  mutate_at(vars(matches("_pr_"), -matches("plt"), -matches("cpds")), funs(folded_ladder_fun(., plotting =F))) %>% 
  ungroup() %>% 
  mutate_at(vars(ends_with("num_ctl"), matches("_pr_"), ends_with("spatial_ctl")), funs(scale_this(.)))  



# Missings Plot ####
miss_Lutz = tscs_data_GA_Lutz %>% 
  select_at(vars(-systid_ccp, -arate_ccp_ga, -matches("odempr"))) %>% 
  filter(is.na(arate_lutz_ga) == F) %>% 
  summarise_all(pMiss_01) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_ctl", "", name),
         name = gsub("_cat", "", name)) %>% 
  ggplot(aes(x=name, y=value)) +
  geom_bar(stat="identity", width=1) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  xlab("") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in TSCS - Goal-Attainment Lutz")

miss_CCP = tscs_data_GA_Lutz %>% 
  select_at(vars(-systid_ccp, -arate_lutz_ga, -matches("odempr"))) %>% 
  filter(is.na(arate_ccp_ga) == F) %>% 
  summarise_all(pMiss_01) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_ctl", "", name),
         name = gsub("_cat", "", name)) %>% 
  ggplot(aes(x=name, y=value)) +
  geom_bar(stat="identity", width=1) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  xlab("") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in TSCS - Goal-Attainment CCP")


ggarrange(miss_Lutz, miss_CCP, ncol=2)


# Distributions  Plot ####
raw_dist = tscs_data_GA_Lutz %>% 
  select_at(vars(-starts_with("FKM"), -ends_with("ga"), -systid_ccp, -matches("odempr"))) %>% 
  select_if(is.numeric) %>% 
  pivot_longer(cols=everything())  %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_ctl", "", name)) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(name~., scales = "free") +
  theme_bw() +
  ggtitle("Raw Values") +
  xlab("") +
  ylab("")

trans_dist = tscs_data_GA_Lutz_trans %>% 
  select_at(vars(-starts_with("FKM"), -ends_with("ga"), -systid_ccp, -matches("odempr"))) %>% 
  select_if(is.numeric) %>% 
  pivot_longer(cols=everything())  %>% 
  mutate(name = gsub("_num_ctl", "", name),
         name = gsub("_pr_ctl", "", name),
         name = gsub("_ctl", "", name)) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram()  +
  facet_wrap(name~., scales = "free") +
  theme_bw() +
  ggtitle("Transformed Values") +
  xlab("") +
  ylab("")

ggarrange(raw_dist, trans_dist, ncol=1)


# corrplot Plot ####
corrplot(cor(tscs_data_GA_Lutz_trans %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))) %>% 
               rename_all(funs(gsub("_pr_ctl", "", .)))  %>% 
               rename_all(funs(gsub("_num_ctl", "", .))) %>% 
               rename_all(funs(gsub("_ctl", "", .))) %>% 
               rename_all(funs(gsub("_odempr", "", .))), use="pairwise"))

  


# XYPlots Plot ####
names(tscs_data_GA_Lutz_trans)
p1 = xyplot(tscs_data_GA_Lutz_trans %>% 
              select_at(vars(-matches("odempr"))), "arate_ccp_ga", c("systid_ccp"))
p2 = xyplot(tscs_data_GA_Lutz_trans%>% 
              select_at(vars(-matches("odempr"))), "arate_lutz_ga", c("systid_ccp")) 
ggarrange(p1, p2, ncol=1)


# No Imputation Possible ####

# Saving ####
save(tscs_data_GA_Lutz_trans, file = "Analyse/Performance/SpecificP/Datasets/tscs_data_GA_Lutz_trans_v2.Rdata")
