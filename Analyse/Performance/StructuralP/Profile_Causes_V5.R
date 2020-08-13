# Causes of Democracy Profiles

source("Analyse/CreateDatasets.R")
#source("Analyse/Performance/StructuralP/SimulateFunctions.R")
source("Setup/Sig_Tables.R")
source("Setup/Simulation_Dirichlet.R")

# Functions: #####
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}



plot_diri_cont = function(data, variable, y_var) {
  
  plot1 = data %>% 
    select_at(vars(country, vars = variable, starts_with(y_var))) %>% 
    pivot_longer(cols=starts_with("mp_")) %>% 
    mutate(name = gsub("mp_Cluster", "", name)) %>% 
    #na.omit() %>% 
    ggplot(aes(x=vars, y=value)) +
    geom_point() +
    geom_smooth(method="loess", se=T, color="black", span = 1.5) +
    facet_wrap(name~.) +
    theme(legend.position = "none") +
    xlab(variable) +
    theme_bw() +
    xlab("") +
    ylab("")

  return(plot1)
}

plot_residual_diri = function(model) {
  
  plot_data = data.frame(
    residuals_comp = residuals(model, "composite"),
    residuals_std_1 = residuals(model, "standardized")[,1],
    residuals_std_2 = residuals(model, "standardized")[,2],
    residuals_std_3 = residuals(model, "standardized")[,3],
    residuals_std_4 = residuals(model, "standardized")[,4],
    id = seq(1, dim(model$d)[1], 1),
    fitted = fitted(model),
    country = model$data$country)
  
  names(plot_data)[2:5] = paste("res_std_", colnames(residuals(model, "standardized")), sep="")
  
  p1 = plot_data %>% 
    ggplot(aes(x=id, y=residuals_comp, label=country)) +
    geom_point() +
    geom_text() +
    ylab("Composite Residuals")
  
  p2 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("Fec", ignore.case = F), country)) %>%
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2))  +
    ggtitle("Fec") +
    ylab("Standardized Residuals") +
    xlab("Fitted Values")
  
  
  p3 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("fEc", ignore.case = F), country)) %>% 
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2))  +
    ggtitle("fEc") +
    ylab("Standardized Residuals")  +
    xlab("Fitted Values")
  
  
  p4 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("fEC", ignore.case = F), country)) %>%
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2))  +
    ggtitle("fEC") +
    ylab("Standardized Residuals") +
    xlab("Fitted Values")
  
  
  p5 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("FeC", ignore.case = F), country)) %>% 
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2)) +
    ggtitle("FeC") +
    ylab("Standardized Residuals") +
    xlab("Fitted Values")
  
  lay = rbind(c(1,1),
              c(2,3),
              c(4,5))
  grid.arrange(p1,p2,p3,p4,p5, layout_matrix = lay)
  
}


# transform to DR data (Dirichletreg)
make_DR = function(dataset, mp_var) {
  dataset$Y_Fec = DirichletReg::DR_data(dataset %>%  
                                          select_at(vars(starts_with(mp_var)))) 
                                        
                                        #  select(X_Fec, everything()))
  
  # dataset$Y_fEc = DirichletReg::DR_data(dataset %>%  
  #                                         select_at(vars(starts_with(mp_var))) %>% 
  #                                         select(X_fEc, everything()))
  # 
  # dataset$Y_fEC = DirichletReg::DR_data(dataset %>%  
  #                                         select_at(vars(starts_with(mp_var))) %>% 
  #                                         select(X_fEC, everything()))
  # 
  # dataset$Y_FeC = DirichletReg::DR_data(dataset %>%  
  #                                         select_at(vars(starts_with(mp_var))) %>% 
  #                                         select(X_FeC, everything()))
  return(dataset)
}

mean_NA = function(x) {
  if(all(is.na(x)) == F) {
    return(mean(x, na.rm=T))
  } else {
    return(NA)
  }
    
}

# Independent Variables ####

fract_caus = fread("Datasets/ethnic_frac.csv", encoding="Latin-1") %>% 
  rename(ethn_caus = Ethnic, ling_caus = Linguistic, rel_caus = Religious) %>%
  mutate(country = substring(country, 2),
         country = fct_recode(country,
                              "Timor-Leste" =	"East Timor",
                              "The Gambia" = "Gambia",
                              )
                              
         ) %>%
  mutate(fract_sum_caus = (ethn_caus + rel_caus + ling_caus)/3,
         fract_prod_caus = (ethn_caus * ling_caus * rel_caus)^(1/3)) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  select(country_text_id, fract_sum_caus, fract_prod_caus)


QoG_caus = QoC_data %>% 
  dplyr::select(country_text_id, year,
                pop_size_caus = wdi_pop,
                gdp_caus = wdi_gdpcapcur,
                englegal_caus = lp_legor,
                union_caus = vi_udr,
                sd_caus = cpds_ls,
                left_caus = cpds_lls)  %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  group_by(country_text_id, year) %>% 
  summarise_all(mean_NA) %>% 
  ungroup() %>% 
  mutate(
    pop_size_caus = ifelse(pop_size_caus == 0, NA, pop_size_caus),
    pop_size_caus = log(pop_size_caus),
    gdp_caus = log(gdp_caus),
    # 1: English Legal Origin; 0: Other
    englegal_caus = ifelse(englegal_caus == 1, 1, 0),
    leftist_caus = sd_caus + left_caus
  ) %>% 
  select(-sd_caus, -left_caus)

QoG_caus_num = QoG_caus %>% 
  filter(year >= 1950) %>% 
  select(-year) %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.numeric, list(~mean_NA(.)))

QoG_caus_cat = QoG_caus %>% 
  filter(year >= 1950) %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.character, list(~getmode(.))) 


colonial_caus = fread("Datasets/coldata110.csv", sep=",") %>% 
  select(COWcode = State, Name, ColRuler)  %>% 
  mutate(COWcode = ifelse(COWcode == 305, 300, COWcode),
         COWcode = ifelse(COWcode == 732, 730, COWcode)) %>% 
  mutate(UKcol_caus = ifelse(ColRuler == 200, 1, 0),
         UKcol_caus = ifelse(COWcode == 200, 1, UKcol_caus),
         # 1 is British Colony; 0: Other
         UKcol_caus = ifelse(UKcol_caus == 1, 1, 0)) %>% 
  select(COWcode, UKcol_caus)


wrp_caus = fread("Datasets/WRP_national.csv") %>% 
  select(name, COWcode = state, rel_cath_wrp_caus = chrstcat, pop) %>% 
  mutate(COWcode = ifelse(COWcode == 305, 300, COWcode),
         COWcode = ifelse(COWcode == 732, 730, COWcode),
         rel_cath_wrp_caus = rel_cath_wrp_caus/pop
         ) %>%
  group_by(COWcode) %>% 
  summarise(cath_caus = mean(rel_cath_wrp_caus, na.rm=T)) %>% 
  ungroup()
  

schwartz_caus = fread("Datasets/Schwartz_culture.csv") %>% 
  select(country, 
         mastery_cult_caus = mastery, 
         harm_cult_caus = harmony, 
         egalit_cult_caus = egalitarianism, 
         embeddedness_cult_caus = embeddedness,
         hierarchy_cult_caus = hierarchy, 
         aff_auto_cult_caus = affective_autonomy, 
         int_auto_cult_caus = intellectual_autonomy) %>% 
  select(country, mastery_cult_caus, hierarchy_cult_caus)


# Dependent Variables ####
profiles_agg_cult = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  select_at(vars(country, starts_with("mp_cluster4"), starts_with("mp_cluster5"))) %>% 
  group_by(country) %>% 
  summarise_all(mean, na.omit=T) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country")

### Create Dataset ######

caus_culture_profiles_data = profiles_agg_cult  %>% 
  left_join(V_dem %>% select(country, COWcode) %>%  distinct() %>%  group_by(country) %>% slice(1), by="country") %>% 
  left_join(fract_caus, by="country_text_id") %>% 
  left_join(QoG_caus_num, by="country_text_id") %>% 
  left_join(QoG_caus_cat, by="country_text_id") %>% 
  left_join(colonial_caus, by="COWcode") %>% 
  left_join(wrp_caus, by="COWcode")  %>%
  left_join(schwartz, by="country") %>% 
  select_at(vars(country, country_text_id, COWcode, starts_with("mp_"), everything())) 


# Exploratory Analysis ####

missd_pattern(caus_culture_profiles_data %>% 
                select_at(vars(ends_with("_caus"))) %>% 
                rename_all(funs(gsub("_caus","",.))) )


corrplot(cor(caus_culture_profiles_data %>% 
               select_at(vars(starts_with("mp_"), ends_with("_caus"))) %>% 
               rename_all(funs(gsub("_caus","",.))) %>% 
               rename_all(funs(gsub("mp_","",.))), use="pairwise"))


# XY Plots
names_caus = colnames(caus_culture_profiles_data %>% 
           select_at(vars(ends_with("_caus"))))
plotlist = list()
for (i in 1:length(names_caus)) {
  plotlist[[i]] = plot_diri_cont(caus_culture_profiles_data, names_caus[i], "mp_Cluster4") 
}
ggarrange(plotlist=plotlist)

names_caus = colnames(caus_culture_profiles_data %>% 
                        select_at(vars(ends_with("_caus"))))
plotlist5 = list()
for (i in 1:length(names_caus)) {
  plotlist5[[i]] = plot_diri_cont(caus_culture_profiles_data, names_caus[i], "mp_Cluster5") 
}
ggarrange(plotlist=plotlist5)




# Create Dataset for Analysis: Structural Variables ####
caus_culture_profiles_data_struct = caus_culture_profiles_data %>% 
  make_DR(., "mp_Cluster4")


caus_culture_profiles_data_struct = caus_culture_profiles_data %>% 
  select_at(vars(country, 
                 starts_with("X"), 
                 ends_with("_caus"), -matches("cult"), -matches("hof"), 
                 -protestant_centr_caus)) %>% 
  na.omit()  %>% 
  filter(country != "Sierra Leone", country != "Switzerland", country != "Israel", country != "Uruguay") %>% 
  make_DR()




# Regression Analysis
m1  = DirichReg(Y_Fec ~  1,
                caus_culture_profiles_data_struct, "alternative")
summary(m1)
plot_residual_diri(m1)


# GDP not significant related to Democracy Profiles
m2  = DirichReg(Y_Fec ~  gdp_caus,
                caus_culture_profiles_data_struct, "alternative")
summary(m2)
anova(m1,m2)
plot_residual_diri(m2)

# pop_size_caus is significant related
m3  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus,
                caus_culture_profiles_data_struct, "alternative")
summary(m3)
anova(m2,m3)
plot_residual_diri(m3)

# englegal_centr_caus is significant related
m4  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + UKcol_caus,
                caus_culture_profiles_data_struct, "alternative")
summary(m4)
anova(m3,m4)
plot_residual_diri(m4)


# diverse_caus is significant related
m5  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + fract_prod_caus  | 1,
                caus_culture_profiles_data_struct, "alternative")
summary(m5)
anova(m4,m5)
plot_residual_diri(m5)

# rel_cath_wrp_caus is not significant related
m6  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + UKcol_caus + fract_prod_caus + union_caus + leftist_caus| 1,
                caus_culture_profiles_data_struct, "alternative")
summary(m6)
plot_residual_diri(m6)
anova(m5,m6)



# time_democratic_caus is significant related
m7  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + UKcol_caus + fract_prod_caus +  
                  mastery_cult_caus  | 1,
                caus_culture_profiles_data_struct, "alternative")
summary(m7)
anova(m5,m7)
plot_residual_diri(m7)

# Make Summary Table
make_table_diri(m1,m2,m3,m4,m5,m6,m7, oddsRatios=F)


m7_Fec  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                caus_culture_profiles_data_struct, "alternative")
m7_FeC  = DirichReg(Y_FeC ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                    caus_culture_profiles_data_struct, "alternative")
m7_fEc  = DirichReg(Y_fEc ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                    caus_culture_profiles_data_struct, "alternative")
m7_fEC  = DirichReg(Y_fEC ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                    caus_culture_profiles_data_struct, "alternative")

odds_ratio_plot(m7_Fec, m7_FeC, m7_fEc, m7_fEC, sign_niveau = 0.05)


  
make_table_diri(m7_Fec)
make_table_diri(m7_FeC)
make_table_diri(m7_fEc)
make_table_diri(m7_fEC)

##### Simulation #######
m7$d %>% 
  summarise_all(funs(mean, min, max) )

# pop_size_caus
simu_pop_size = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = seq(4.88044, 8.911705, length.out = 10),
  English_caus = 0.2621359,
  diverse_caus =  0.3817845,
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608 
)

# englegal_centr_caus
simu_english = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 6.81844 ,
  English_caus = c(0,1),
  diverse_caus =  0.3817845,
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608 
)

# diverse_caus
simu_diverse = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 6.81844 ,
  English_caus = 0.2621359,
  diverse_caus = seq(0.00515, 0.9061, length.out = 10),
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608
)

# time_democratic_caus
simu_time_democratic = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 6.81844 ,
  English_caus = 0.2621359,
  diverse_caus = 0.3817845,
  time_democratic_caus = seq(min(m7$d$time_democratic_caus), max(m7$d$time_democratic_caus), length.out = 20),
  rel_cath_wrp_caus =  0.3657608
)

p1 = data.frame(predict(m7, simu_pop_size)) %>%
  rename_all(funs(colnames(m7$Y))) %>% 
  cbind(pop_size_caus = simu_pop_size$pop_size_caus) %>% 
  pivot_longer(cols=-pop_size_caus) %>% 
  ggplot(aes(x=pop_size_caus, y=value, col=name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ggtitle("pop_size_caus")

p2 = data.frame(predict(m7, simu_diverse)) %>%
  rename_all(funs(colnames(m7$Y))) %>% 
  cbind(diverse_caus = simu_diverse$diverse_caus) %>% 
  pivot_longer(cols=-diverse_caus) %>% 
  ggplot(aes(x=diverse_caus, y=value, col=name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ggtitle("diverse_caus")

p3 = data.frame(predict(m7, simu_time_democratic)) %>%
  rename_all(funs(colnames(m7$Y))) %>% 
  cbind(time_democratic_caus = simu_time_democratic$time_democratic_caus) %>% 
  pivot_longer(cols=-time_democratic_caus) %>% 
  ggplot(aes(x=time_democratic_caus, y=value, col=name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ggtitle("time_democratic_caus")

p4 = simu_diri_function_expected(m7, simu_english, draws = 100, selected_variable = "English_caus", categorical = T ) 
grid.arrange(p1,p2,p3,p4)

# simu_diri_function_expected(m7, simu_pop_size, draws = 100, selected_variable = "pop_size_caus", categorical = F )
# simu_diri_function_expected(m7, simu_diverse, draws = 100, selected_variable = "diverse_caus", categorical = F )
# simu_diri_function_expected(m7, simu_time_democratic, draws = 100, selected_variable = "time_democratic_caus", categorical = F )

#simu_diri_function_predicted(m7, simu)

# Configurations
# Small State + Low Diversity + english
simu_conf1 = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = mean(m7$d$pop_size_caus) - 2*sd(m7$d$pop_size_caus),
  English_caus = 1,
  diverse_caus =  mean(m7$d$diverse_caus) - 2*sd(m7$d$diverse_caus),
  time_democratic_caus = 3.803093,
  rel_cath_wrp_caus =  0.3657608 
)

p1 = simu_diri_function_expected(m7, simu_conf1, draws = 100, selected_variable = "English_caus", categorical = T )

# Big State + High Diversity + nonenglish
simu_conf2 = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = mean(m7$d$pop_size_caus) + 2*sd(m7$d$pop_size_caus) ,
  English_caus = 0,
  diverse_caus =  mean(m7$d$diverse_caus) + 2*sd(m7$d$diverse_caus),
  time_democratic_caus = 3.803093,
  rel_cath_wrp_caus =  0.3657608 
)
p2 = simu_diri_function_expected(m7, simu_conf2, draws = 100, selected_variable = "English_caus", categorical = T )

p1$data %>% 
  bind_rows(p2$data) %>%
  ggplot(aes(x=name, y=mean, ymin = lower, ymax = upper, col=name)) + 
  geom_errorbar() +
  facet_wrap(select_x~.) +
  theme_bw()


############ Consolidated Democracies####
longdemocracies_Lijphart = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(nr = n()) %>% 
  filter(nr > 20) %>% 
  pull(country)

caus_culture_profiles_data_struct_consol = caus_culture_profiles_data %>% 
  filter(country %in% longdemocracies_Lijphart) %>% 
  select_at(vars(country, 
                 starts_with("X"), 
                 ends_with("_caus"), -matches("cult"), -matches("hof"), 
                 -protestant_centr_caus)) %>% 
  na.omit()  
  #filter(country != "Israel", country != "Uruguay")


caus_culture_profiles_data_struct_consol$Y_Fec = DirichletReg::DR_data(caus_culture_profiles_data_struct_consol %>%  
                                                                  select_at(vars(starts_with("X_"))) %>% 
                                                                  select(X_Fec, everything()))

caus_culture_profiles_data_struct_consol$Y_fEc = DirichletReg::DR_data(caus_culture_profiles_data_struct_consol %>%  
                                                                  select_at(vars(starts_with("X_"))) %>% 
                                                                  select(X_fEc, everything()))

caus_culture_profiles_data_struct_consol$Y_fEC = DirichletReg::DR_data(caus_culture_profiles_data_struct_consol %>%  
                                                                  select_at(vars(starts_with("X_"))) %>% 
                                                                  select(X_fEC, everything()))

caus_culture_profiles_data_struct_consol$Y_FeC = DirichletReg::DR_data(caus_culture_profiles_data_struct_consol %>%  
                                                                  select_at(vars(starts_with("X_"))) %>% 
                                                                  select(X_FeC, everything()))


# Regression Analysis
m1_c  = DirichReg(Y_Fec ~  1,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m1_c)


# GDP not significant related to Democracy Profiles
m2_c  = DirichReg(Y_Fec ~  gdp_caus,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m2_c)
anova(m1_c,m2_c)

plot_residual_diri(m2_c)


# pop_size_caus is significant related
m3_c  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m3_c)
anova(m2_c,m3_c)

plot_residual_diri(m3_c)


# englegal_centr_caus is significant related
m4_c  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + English_caus,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m4_c)
anova(m3_c,m4_c)

plot_residual_diri(m4_c)


# diverse_caus is significant related
m5_c  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + English_caus + diverse_caus  | 1,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m5_c)
anova(m4_c,m5_c)
plot_residual_diri(m5_c)


# rel_cath_wrp_caus is not significant related
m6_c  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + rel_cath_wrp_caus| 1,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m6_c)
plot_residual_diri(m6_c)
anova(m5_c,m6_c)

# time_democratic_caus is significant related
m7_c  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m7_c)
anova(m5_c,m7_c)
plot_residual_diri(m7_c)

make_table_diri(m1_c,m2_c,m3_c,m4_c,m5_c,m6_c,m7_c)


m7_Fec_c  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")
m7_FeC_c  = DirichReg(Y_FeC ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")
m7_fEc_c  = DirichReg(Y_fEc ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")
m7_fEC_c  = DirichReg(Y_fEC ~  gdp_caus +  pop_size_caus + English_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")

odds_ratio_plot(m7_Fec_c, m7_FeC_c, m7_fEc_c, m7_fEC_c, sign_niveau = 0.05)


make_table_diri(m7_Fec_c)
make_table_diri(m7_FeC_c)
make_table_diri(m7_fEc_c)
make_table_diri(m7_fEC_c)



###### Schwarz: Culture ####

# Plot Cultural Data 

caus_culture_profiles_data_struct_cult = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), 
                 comp_cult_caus,
                 hierarchy_cult_caus,
                 gdp_caus, 
                 pop_size_caus, 
                 diverse_caus,
                 English_caus,
                 ltowvs_cult_caus = ltowvs_hof_caus)) 



md.pattern(caus_culture_profiles_data_struct_cult  %>% 
             select_at(vars(matches("caus"), matches("cult"))), rotate.names = T)

# Box plots: Outlier detection

caus_culture_profiles_data_struct_cult %>% 
  select_at(vars(country, matches("caus"), matches("cult"))) %>%
  pivot_longer(cols= matches("caus")) %>% 
  na.omit() %>% 
  group_by(name) %>% 
  mutate(outlier = ifelse(is_outlier(value), country, NA)) %>%
  ggplot(aes(x=1, y=value, fill=name)) +
  geom_boxplot() +
  facet_wrap(name~., scales = "free_y") +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

# Collinearity
caus_culture_profiles_data_struct_cult %>% 
  select_at(vars(matches("caus"), matches("cult"))) %>% 
  cor(use="pairwise", method = "spearman") %>% 
  corrplot(method="number")

# XY Plots
p1 = plot_diri_cont(caus_culture_profiles_data_struct_cult, "comp_cult_caus") 
p2 = plot_diri_cont(caus_culture_profiles_data_struct_cult, "hierarchy_cult_caus") 
p3 = plot_diri_cont(caus_culture_profiles_data_struct_cult, "ltowvs_cult_caus") 
p1
p2
p3

### Cultural Values Regression

caus_culture_profiles_data_struct_schwartz = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), 
                 comp_cult_caus,
                 hierarchy_cult_caus,
                 gdp_caus, 
                 pop_size_caus, 
                 diverse_caus,
                 English_caus
                 )) %>% 
  na.omit() %>% 
  make_DR(.)

#
mE_null = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + diverse_caus, 
                  caus_culture_profiles_data_struct_schwartz, "alternative")
summary(mE_null)


#### comp_cult_caus
mE_1  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus  + comp_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz, "alternative")

summary(mE_1)
anova(mE_null, mE_1)
plot_residual_diri(mE_1)


# without outlier

caus_culture_profiles_data_struct_schwartz_out = caus_culture_profiles_data_struct_schwartz %>% 
  filter(country != "Chile", country != "Israel") %>% 
  make_DR(.)


mE_1_Fec  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus + comp_cult_caus | 1, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")
mE_1_fEc  = DirichReg(Y_fEc ~ gdp_caus + pop_size_caus + diverse_caus + comp_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")
mE_1_fEC  = DirichReg(Y_fEC ~ gdp_caus + pop_size_caus + diverse_caus +  comp_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")
mE_1_FeC  = DirichReg(Y_FeC ~ gdp_caus + pop_size_caus + diverse_caus +  comp_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")

mE_null_out = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + diverse_caus, 
                        caus_culture_profiles_data_struct_schwartz_out, "alternative")

mE_Eng_out = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + diverse_caus  + comp_cult_caus + English_caus, 
                        caus_culture_profiles_data_struct_schwartz_out, "alternative")

summary(mE_Eng_out)
plot_residual_diri(mE_Eng_out)

anova(mE_null_out, mE_1_Fec)


make_table_diri(mE_null, mE_1, mE_null_out, mE_1_Fec, mE_Eng_out)
odds_ratio_plot(mE_1_Fec, mE_1_fEc, mE_1_fEC, mE_1_FeC, sign_niveau = 0.05)

comp_plot = odds_ratio_plot(mE_1_Fec, mE_1_fEc, mE_1_fEC, mE_1_FeC, sign_niveau = 0.05, vars_sel = "comp_cult_caus")


simu_conf_comp = data.frame(
  gdp_caus = mean(mE_1_Fec$d$gdp_caus) ,
  pop_size_caus = mean(mE_1_Fec$d$pop_size_caus),
  diverse_caus =  mean(mE_1_Fec$d$diverse_caus),
  comp_cult_caus =  seq(min(mE_1_Fec$d$comp_cult_caus), max(mE_1_Fec$d$comp_cult_caus), length.out = 15) 
)

data.frame(predict(mE_1_Fec, simu_conf_comp)) %>%
  rename_all(funs(colnames(mE_1_Fec$Y))) %>% 
  cbind(comp_cult_caus = simu_conf_comp$comp_cult_caus) %>% 
  pivot_longer(cols=-comp_cult_caus) %>% 
  ggplot(aes(x=comp_cult_caus, y=value, col=name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  ggtitle("comp_cult_caus")



#### hierarchy_cult_caus

mE_3  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus + hierarchy_cult_caus , 
                caus_culture_profiles_data_struct_schwartz, "alternative")
summary(mE_3)
anova(mE_null, mE_3)
plot_residual_diri(mE_3)

caus_culture_profiles_data_struct_schwartz_out = caus_culture_profiles_data_struct_schwartz %>% 
  filter(country != "Chile", country != "Israel") %>% 
  make_DR(.)


mE_3_Fec  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus + hierarchy_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")
mE_3_fEc  = DirichReg(Y_fEc ~ gdp_caus + pop_size_caus + diverse_caus + hierarchy_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")
mE_3_fEC  = DirichReg(Y_fEC ~ gdp_caus + pop_size_caus + diverse_caus + hierarchy_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")
mE_3_FeC  = DirichReg(Y_FeC ~ gdp_caus + pop_size_caus + diverse_caus + hierarchy_cult_caus, 
                      caus_culture_profiles_data_struct_schwartz_out, "alternative")
mE_3_null_out = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + diverse_caus, 
                         caus_culture_profiles_data_struct_schwartz_out, "alternative")

mE_3_Eng_out = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + diverse_caus  +  hierarchy_cult_caus + English_caus, 
                       caus_culture_profiles_data_struct_schwartz_out, "alternative")

anova(mE_3_null_out, mE_3_Fec)
hierarchy_plot = odds_ratio_plot(mE_3_Fec, mE_3_fEc, mE_3_fEC, mE_3_FeC, sign_niveau = 0.05, vars_sel = "hierarchy_cult_caus")



make_table_diri(mE_null_out, mE_1_Fec, mE_3_Fec, mE_Eng_out, mE_3_Eng_out)


# GLOBE: Future Orientation

caus_culture_profiles_data_struct_GLOBE = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), 
                 ltowvs_hof_caus,
                 gdp_caus, 
                 pop_size_caus, 
                 diverse_caus)) %>% 
  na.omit() %>% 
  group_by(country) %>%
  slice(1) %>% 
  ungroup() %>% 
  make_DR(.)


mE_globe_null  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus , 
                      caus_culture_profiles_data_struct_GLOBE, "alternative")
mE_globe  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus + ltowvs_hof_caus , 
                      caus_culture_profiles_data_struct_GLOBE, "alternative")
summary(mE_globe)
anova(mE_globe_null, mE_globe)
plot_residual_diri(mE_globe)

# outlier
caus_culture_profiles_data_struct_GLOBE_out = caus_culture_profiles_data_struct_GLOBE %>% 
  filter(country != "Burkina Faso") %>% 
  make_DR(.)


mE_globe_Fec  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus + ltowvs_hof_caus, 
                          caus_culture_profiles_data_struct_GLOBE_out, "alternative")
mE_globe_fEc  = DirichReg(Y_fEc ~ gdp_caus + pop_size_caus + diverse_caus + ltowvs_hof_caus, 
                          caus_culture_profiles_data_struct_GLOBE_out, "alternative")
mE_globe_fEC  = DirichReg(Y_fEC ~ gdp_caus + pop_size_caus + diverse_caus + ltowvs_hof_caus, 
                          caus_culture_profiles_data_struct_GLOBE_out, "alternative")
mE_globe_FeC  = DirichReg(Y_FeC ~ gdp_caus + pop_size_caus + diverse_caus + ltowvs_hof_caus, 
                          caus_culture_profiles_data_struct_GLOBE_out, "alternative")

mE_globe_null_out  = DirichReg(Y_Fec ~ gdp_caus + pop_size_caus + diverse_caus , 
                           caus_culture_profiles_data_struct_GLOBE_out, "alternative")

odds_ratio_plot(mE_globe_Fec, mE_globe_fEc, mE_globe_fEC, mE_globe_FeC, sign_niveau = 0.05)
ltowvs_plot = odds_ratio_plot(mE_globe_Fec, mE_globe_fEc, mE_globe_fEC, mE_globe_FeC, sign_niveau = 0.05, vars_sel = "ltowvs_hof_caus")

anova(mE_globe_null_out, mE_globe_Fec)
plot_residual_diri(mE_globe_Fec)

make_table_diri(mE_globe_null, mE_globe)

grid.arrange(comp_plot, hierarchy_plot, ltowvs_plot)


