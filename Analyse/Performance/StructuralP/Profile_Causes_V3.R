# Causes of Democracy Profiles

source("Analyse/CreateDatasets.R")
#source("Analyse/Performance/StructuralP/SimulateFunctions.R")
source("Setup/Sig_Tables.R")
source("Setup/Simulation_Dirichlet.R")

library(margins)
library(DirichletReg)
library(betareg)
library(ggrepel)


# Function:
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

###


ethnic_frac = fread("Datasets/ethnic_frac.csv", encoding="Latin-1") %>% 
  rename(ethn_caus = Ethnic, ling_caus = Linguistic, rel_caus = Religious) %>%
  mutate(diverse_caus = (ethn_caus + ling_caus)/2) %>% 
  mutate(country = substring(country, 2)) %>%
  mutate(country = fct_recode(country,
                              "Timor-Leste" =	"East Timor",
                              "The Gambia" = "Gambia",
                              )
                              
         ) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") 


mean_NA = function(x) {
  if(all(is.na(x)) == F) {
    return(mean(x, na.rm=T))
  } else {
    return(NA)
  }
    
}

# create causal variables
WRP = fread("Datasets/WRP_national.csv") %>% 
  select(name, COWcode = state, rel_cath_wrp_caus = chrstcat, pop) %>% 
  mutate(COWcode = ifelse(COWcode == 305, 300, COWcode),
         COWcode = ifelse(COWcode == 732, 730, COWcode),
         rel_cath_wrp_caus = rel_cath_wrp_caus/pop
         ) %>%
  group_by(COWcode) %>% 
  summarise(rel_cath_wrp_caus = round(mean(rel_cath_wrp_caus, na.rm=T),4))
  

schwartz = fread("Datasets/Schwartz_culture.csv") %>% 
  select(country, 
         comp_cult_caus = mastery, 
         harm_cult_caus = harmony, 
         egalit_cult_caus = egalitarianism, 
         embeddedness_cult_caus = embeddedness,
         hierarchy_cult_caus = hierarchy, 
         aff_auto_cult_caus = affective_autonomy, 
         int_auto_cult_caus = intellectual_autonomy)



hofstede = fread("Datasets/hofstede_data.csv") %>% 
  select(-ctr) %>% 
  rename(pdi_hof_caus = pdi,
         idv_hof_caus = idv,
         mas_hof_caus = mas,
         uai_hof_caus = uai,
         ltowvs_hof_caus = ltowvs,
         ivr_hof_caus = ivr)


centripetalism_caus = read_dta("Datasets/centripetalism.dta") %>% 
  select(country = Country, year = Year, protestant_centr_caus = Protestant, englegal_centr_caus = English_legal_origin) %>% 
  filter(country != "", year >= 1900) %>%
  mutate(country = fct_recode(country,
                              "Timor-Leste" =	"Timor, East",
                              "Egypt" = 	"Egypt, Arab Rep.",
                              "Trinidad and Tobago" = "Trinidad & Tobago",
                              "São Tomé and Príncipe"	 = "Sao Tome and Principe",
                              "Kyrgyzstan" = "Kyrgyztan",
                              "The Gambia" = "Gambia, The",
                              "Slovakia" = "Slovak Republic",
                              "Cyprus" = "Cyprus, Greek (Cyprus)",
                              "Burma/Myanmar"  = "Myanmar (Burma)",
                              "Bosnia and Herzegovina" = "Bosnia-Herzegovina",
                              "Belarus" = "Belarus (Byelorussian SSR)",
                              "Serbia" = "Yugoslavia, FR (Serbia/Montenegro)")) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-country) %>% 
  mutate(englegal_centr_caus = if_else(country_text_id == "TLS", 0, englegal_centr_caus))




causal_vars = QoC_data %>% 
  dplyr::select(country_text_id, year,
                pop_size_caus = wdi_pop,
                
                gdp_caus = wdi_gdpcapcur,

                rel_prot_caus = lp_protmg80,
                rel_cath_caus = lp_catho80,
                rel_musl_caus = lp_muslim80,
                rel_oth_caus = lp_no_cpm80,
                

                col_caus = ht_colonial)  %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  group_by(country_text_id, year) %>% 
  summarise_all(mean_NA) %>% 
  ungroup() %>% 
  mutate(
    pop_size_caus = ifelse(pop_size_caus == 0, NA, pop_size_caus),
    pop_size_caus = log10(pop_size_caus),
    gdp_caus = log10(gdp_caus),
    
    col_caus = as.factor(col_caus),
  ) %>% 
  group_by(country_text_id) %>% 
  tidyr::complete(country_text_id, year = 1950:2017, fill = list(NA))
  


### Create Dataset ######

causal_vars_num = causal_vars %>% 
  filter(year >= 1950) %>% 
  left_join(centripetalism_caus, by=c("country_text_id", "year")) %>% 
  select(-year) %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.numeric, list(~mean_NA(.)))

causal_vars_fact = causal_vars %>% 
  filter(year >= 1950) %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.factor, list(~getmode(.)))


time_democratic = dmx_trade_cluster %>% 
  #filter(year >= 1945) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(time_democratic_caus = n()) %>%
  ungroup() %>% 
  mutate(
    time_democratic_caus_n = time_democratic_caus,
    time_democratic_caus = time_democratic_caus/10)


time_democratic_perc = dmx_data %>% 
  filter(year >= 1950) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country, classification_core) %>% 
  na.omit() %>% 
  summarise(count =  n()) %>%
  ungroup() %>% 
  group_by(country) %>% 
  mutate(sum = sum(count)) %>% 
  filter(classification_core == "Deficient Democracy" | classification_core == "Working Democracy") %>% 
  mutate(time_democratic_perc_caus = sum(count)/sum) %>% 
  select(country, time_democratic_perc_caus) %>% 
  distinct()
  


defdemocracies = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  dplyr::select(country, classification_core) %>% 
  filter(classification_core == "Deficient Democracy") %>% 
  group_by(country) %>% 
  summarise(def_democracy_caus  = n())  %>% 
  ungroup() 

time_democratic = time_democratic %>% 
  left_join(defdemocracies, by="country") %>% 
  mutate(def_democracy_caus = ifelse(is.na(def_democracy_caus) == T, 0, def_democracy_caus)) %>% 
  mutate(def_democracy_perc_caus = def_democracy_caus/time_democratic_caus_n) %>% 
  left_join(time_democratic_perc, by="country")



# Select Democracies
longdemocracies = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(nr = n()) %>% 
  filter(nr > 1) %>% 
  pull(country)

# Uncertainty
profiles_agg_cult = dmx_trade_cluster %>% 
  filter(year >= 1950, country %in% longdemocracies) %>% 
  select_at(vars(country, starts_with("X"))) %>% 
  group_by(country) %>% 
  summarise_all(mean, na.omit=T) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country")


# Create Dataset
caus_culture_profiles_data = profiles_agg_cult  %>% 
  left_join(V_dem %>% select(country, COWcode) %>%  distinct() %>%  group_by(country) %>% slice(1), by="country") %>% 
  left_join(ethnic_frac %>% select(-country), by="country_text_id") %>% 
  left_join(causal_vars_num, by="country_text_id") %>%
  left_join(causal_vars_fact, by="country_text_id") %>%
  left_join(schwartz, by="country")  %>% 
  left_join(time_democratic, by="country") %>% 
  select_at(vars(country, country_text_id, COWcode, starts_with("X"), 
                 gdp_caus,
                 pop_size_caus, 
                 englegal_centr_caus,
                 diverse_caus,
                 protestant_centr_caus,
                 comp_cult_caus,
                 harm_cult_caus,
                 egalit_cult_caus,
                 embeddedness_cult_caus,
                 hierarchy_cult_caus,
                 aff_auto_cult_caus,
                 int_auto_cult_caus,
                 def_democracy_perc_caus,
                 time_democratic_caus,
                 #time_democratic_perc_caus
                 )
  ) %>% 
  left_join(hofstede, by="country") %>% 
  left_join(WRP, by="COWcode")


# Make Plots
# NA plot
caus_culture_profiles_data %>% 
  select_at(vars(matches("caus"), -matches("cult"), -matches("hof"))) %>%
  summarise_all(pMiss_Abs) %>% 
  melt() %>% 
  ggplot(aes(x=variable, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  #scale_y_continuous(breaks=seq(0,100, 10), limit=c(0,100))  +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings")

md.pattern(caus_culture_profiles_data %>% 
             select_at(vars(matches("caus"), -matches("cult"), -matches("hof"), -protestant_centr_caus)), rotate.names = T)

# Box plots: Outlier detection
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

caus_culture_profiles_data %>% 
  select_at(vars(country, matches("caus"), -matches("cult"), -matches("hof"))) %>%
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
caus_culture_profiles_data %>% 
  select_at(vars(matches("caus"), -matches("cult"), -matches("hof"), -protestant_centr_caus)) %>% 
  cor(use="pairwise", method = "spearman") %>% 
  corrplot(method="number")

plot_diri_cont = function(data, variable, y_var = NULL, y_value, xvar = NULL) {
  
  if (is.null(y_var) == F) {
    plot1 = data %>% 
      select_at(vars(country, vars = variable, X_sel = y_var, starts_with("X"))) %>% 
      mutate(is.outlier = ifelse(vars <= 2.5 & X_sel >= y_value, country, NA)) %>% 
      rename(!!y_var := X_sel) %>% 
      pivot_longer(cols=starts_with("X")) %>% 
      #na.omit() %>% 
      ggplot(aes(x=vars, y=value, col=name)) +
      geom_point() +
      geom_smooth(method="loess", se=T, color="black", span = 1.5) +
      facet_wrap(name~.) +
      theme(legend.position = "none") +
      xlab(variable) +
      geom_text(aes(label = is.outlier))
  } else {
    plot1 = data %>% 
      select_at(vars(country, vars = variable, starts_with("X"))) %>% 
      pivot_longer(cols=starts_with("X")) %>% 
      #na.omit() %>% 
      ggplot(aes(x=vars, y=value, col=name)) +
      geom_point() +
      geom_smooth(method="loess", se=T, color="black", span = 1.5) +
      facet_wrap(name~.) +
      theme(legend.position = "none") +
      xlab(variable) 
  }
  plot1
  return(plot1)
}

p1 = plot_diri_cont(caus_culture_profiles_data, "gdp_caus", "X_Fec", 0.8, 2.5) 
p2 = plot_diri_cont(caus_culture_profiles_data, "pop_size_caus")
p3 = plot_diri_cont(caus_culture_profiles_data, "diverse_caus",  "X_Fec", 0.85, 1)
p4 = plot_diri_cont(caus_culture_profiles_data, "rel_cath_wrp_caus")
p5 = plot_diri_cont(caus_culture_profiles_data, "time_democratic_caus")

p1 + 
  geom_text(data = p1$data[p1$data$country == "Switzerland",], aes(x=vars, y=value, col=name, label=country))
p3  + 
  geom_text(data = p3$data[p3$data$country == "Switzerland",], aes(x=vars, y=value, col=name, label=country))


caus_culture_profiles_data %>% 
  select_at(vars(country, englegal_centr_caus, starts_with("X"))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  group_by(name, englegal_centr_caus) %>% 
  mutate(outlier = ifelse(is_outlier(value), country, NA)) %>% 
  ungroup() %>% 
  mutate(englegal_centr_caus = as.factor(englegal_centr_caus)) %>% 
  ggplot(aes(x = englegal_centr_caus, y=value, fill= name)) + 
  geom_boxplot()  +
  facet_wrap(name~., scales = "free_y") +
  geom_text_repel(aes(x = englegal_centr_caus, label = outlier), na.rm = TRUE)



############ All Democracies####
############## Create Dataset for Analysis: Structural Variables


caus_culture_profiles_data_struct = caus_culture_profiles_data %>% 
  select_at(vars(country, 
                 starts_with("X"), 
                 ends_with("_caus"), -matches("cult"), -matches("hof"), 
                 -protestant_centr_caus)) %>% 
  na.omit() 
  #filter(country != "Sierra Leone", country != "Switzerland", country != "Israel", country != "Uruguay")


caus_culture_profiles_data_struct$Y_Fec = DirichletReg::DR_data(caus_culture_profiles_data_struct %>%  
                                                               select_at(vars(starts_with("X_"))) %>% 
                                                               select(X_Fec, everything()))

caus_culture_profiles_data_struct$Y_fEc = DirichletReg::DR_data(caus_culture_profiles_data_struct %>%  
                                                               select_at(vars(starts_with("X_"))) %>% 
                                                               select(X_fEc, everything()))

caus_culture_profiles_data_struct$Y_fEC = DirichletReg::DR_data(caus_culture_profiles_data_struct %>%  
                                                               select_at(vars(starts_with("X_"))) %>% 
                                                               select(X_fEC, everything()))

caus_culture_profiles_data_struct$Y_FeC = DirichletReg::DR_data(caus_culture_profiles_data_struct %>%  
                                                               select_at(vars(starts_with("X_"))) %>% 
                                                               select(X_FeC, everything()))





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
m4  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + englegal_centr_caus,
                caus_culture_profiles_data_struct, "alternative")
summary(m4)
anova(m3,m4)
plot_residual_diri(m4)


# diverse_caus is significant related
m5  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + englegal_centr_caus + diverse_caus  | 1,
                caus_culture_profiles_data_struct, "alternative")
summary(m5)
anova(m4,m5)
plot_residual_diri(m5)

# rel_cath_wrp_caus is not significant related
m6  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + time_democratic_caus| 1,
                caus_culture_profiles_data_struct, "alternative")
summary(m6)
plot_residual_diri(m6)
anova(m5,m6)

# time_democratic_caus is significant related
m7  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus  | 1,
                caus_culture_profiles_data_struct, "alternative")
summary(m7)
anova(m5,m7)
plot_residual_diri(m7)

# Make Summary Table
make_table_diri(m1,m2,m3,m4,m5,m6,m7, oddsRatios=F)




m7_Fec  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                caus_culture_profiles_data_struct, "alternative")
m7_FeC  = DirichReg(Y_FeC ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                    caus_culture_profiles_data_struct, "alternative")
m7_fEc  = DirichReg(Y_fEc ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                    caus_culture_profiles_data_struct, "alternative")
m7_fEC  = DirichReg(Y_fEC ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + time_democratic_caus + rel_cath_wrp_caus | 1,
                    caus_culture_profiles_data_struct, "alternative")

odds_ratio_plot(m7_Fec)
odds_ratio_plot(m7_FeC)
odds_ratio_plot(m7_fEc)
odds_ratio_plot(m7_fEC)

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
  pop_size_caus = seq(4.88044, 8.911705, length.out = 15),
  englegal_centr_caus = 0.2989691,
  diverse_caus =  0.3817845,
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608 
)

# englegal_centr_caus
simu_english = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 6.81844 ,
  englegal_centr_caus = c(0,1),
  diverse_caus =  0.3817845,
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608 
)

# diverse_caus
simu_diverse = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 6.81844 ,
  englegal_centr_caus = 0.2989691,
  diverse_caus = seq(0.00515, 0.9061, length.out = 15),
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608
)

# time_democratic_caus
simu_time_democratic = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 6.81844 ,
  englegal_centr_caus = 0.2989691,
  diverse_caus = 0.3817845,
  time_democratic_caus = seq(-22, 93, length.out = 25),
  rel_cath_wrp_caus =  0.3657608
)

simu_diri_function_expected(m7, simu_pop_size, draws = 100, selected_variable = "pop_size_caus", categorical = F )
simu_diri_function_expected(m7, simu_english, draws = 100, selected_variable = "englegal_centr_caus", categorical = T )
simu_diri_function_expected(m7, simu_diverse, draws = 100, selected_variable = "diverse_caus", categorical = F )
simu_diri_function_expected(m7, simu_time_democratic, draws = 100, selected_variable = "time_democratic_caus", categorical = F )

#simu_diri_function_predicted(m7, simu)

# Configurations
# Small State + Low Diversity
simu_conf1 = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 4.88044 ,
  englegal_centr_caus = c(0,1),
  diverse_caus =  0.00515,
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608 
)
simu_diri_function_expected(m7, simu_conf1, draws = 100, selected_variable = "englegal_centr_caus", categorical = T )

# Big State + High Diversity 
simu_conf2 = data.frame(
  gdp_caus = 3.38532,
  pop_size_caus = 9.88044 ,
  englegal_centr_caus = c(0,1),
  diverse_caus =  0.90515,
  time_democratic_caus = 13.03093,
  rel_cath_wrp_caus =  0.3657608 
)
simu_diri_function_expected(m7, simu_conf2, draws = 100, selected_variable = "englegal_centr_caus", categorical = T )


############ Consolidated Democracies####
longdemocracies_Lijphart = dmx_trade_cluster %>% 
  filter(year >= 1990) %>% 
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
m4_c  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + englegal_centr_caus,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m4_c)
anova(m3_c,m4_c)

plot_residual_diri(m4_c)


# diverse_caus is significant related
m5_c  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + englegal_centr_caus + diverse_caus  | 1,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m5_c)
anova(m4_c,m5_c)
plot_residual_diri(m5_c)


# rel_cath_wrp_caus is not significant related
m6_c  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + rel_cath_wrp_caus| 1,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m6_c)
plot_residual_diri(m6_c)
anova(m5_c,m6_c)

# time_democratic_caus is significant related
m7_c  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                caus_culture_profiles_data_struct_consol, "alternative")
summary(m7_c)
anova(m5_c,m7_c)
plot_residual_diri(m7_c)

make_table_diri(m1_c,m2_c,m3_c,m4_c,m5_c,m6_c,m7_c)


m7_Fec_c  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")
m7_FeC_c  = DirichReg(Y_FeC ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")
m7_fEc_c  = DirichReg(Y_fEc ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")
m7_fEC_c  = DirichReg(Y_fEC ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + rel_cath_wrp_caus + time_democratic_caus  | 1,
                      caus_culture_profiles_data_struct_consol, "alternative")

make_table_diri(m7_Fec_c)
make_table_diri(m7_FeC_c)
make_table_diri(m7_fEc_c)
make_table_diri(m7_fEC_c)



###### Schwarz: Culture ####
caus_culture_profiles_data_struct_schwartz = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), gdp_caus, pop_size_caus, diverse_caus, ends_with("cult_caus"))) %>% 
  mutate(X1_F = (X_Fec + X_FeC)/2,
         X1_E = (X_fEC + X_fEc)/2,
         X2_C = (X_fEC + X_FeC)/2,
         X2_M = (X_fEc + X_Fec)/2)  %>% 
  filter(is.na(comp_cult_caus) == F, is.na(gdp_caus) == F) %>% 
  na.omit() %>% 
  filter(country != "India", country != "Brazil", country != "Israel")




caus_culture_profiles_data_struct_schwartz$X_E = DirichletReg::DR_data(caus_culture_profiles_data_struct_schwartz %>%
                                                                          select_at(vars(starts_with("X1"))) %>% 
                                                                          select(X1_F, X1_E))[,2]

caus_culture_profiles_data_struct_schwartz$X_M = DirichletReg::DR_data(caus_culture_profiles_data_struct_schwartz %>%
                                                                         select_at(vars(starts_with("X2"))) %>% 
                                                                         select(X2_C, X2_M))[,2]


mE_null = betareg(X_E ~  1, 
                  caus_culture_profiles_data_struct_schwartz,
                  type="BR")
summary(mE_null)

mM_null = betareg(X_M ~  1, 
                  caus_culture_profiles_data_struct_schwartz,
                  type="BR")
summary(mM_null)


#### comp_cult_caus

mE_1  = betareg(X_E ~ gdp_caus + pop_size_caus + diverse_caus + comp_cult_caus  | 1, 
                     caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mE_1)
lrtest(mE_null, mE_1)


summary(margins(mE_1))

mE_1$model %>% 
  summarise_all(funs(min, mean, max)) %>% 
  pivot_longer(cols=everything()) %>% 
  arrange(name)

predict_data = data.frame(
  comp_cult_caus = c(5, 3),
  diverse_caus =  0.310,
  gdp_caus =       3.72 ,
  pop_size_caus =  7.18
)

predict(mE_1, newdata=predict_data)



par(mfrow=c(1,1))
plot(residuals(mE_1, "pearson") ~ fitted(mE_1)); abline(0,0)
plot(residuals(mE_1, "pearson") ~ mE_1$model$comp_cult_caus); abline(0,0)

data.frame(
  country = caus_culture_profiles_data_struct_schwartz$country,
  mE = caus_culture_profiles_data_struct_schwartz$X_E,
  var = mE_1$model$comp_cult_caus,
  res = residuals(mE_1, "pearson")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)



test  = data.frame(
  country = caus_culture_profiles_data_struct_schwartz$country,
  res = residuals(mE_1, "pearson")
)



mM_1  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + comp_cult_caus, 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mM_1)
lrtest(mM_null, mM_1)

plot(residuals(mM_1, "pearson") ~ fitted(mM_1)); abline(0,0)



#### harm_cult_caus

mE_2  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + harm_cult_caus, 
                caus_culture_profiles_data_struct_schwartz ,
                type="BR")
summary(mE_2)
lrtest(mE_null, mE_2)


data.frame(
  country = caus_culture_profiles_data_struct_schwartz$country,
  mE = caus_culture_profiles_data_struct_schwartz$X_E,
  var = caus_culture_profiles_data_struct_schwartz$harm_cult_caus,
  res = residuals(mE_2, "pearson")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)



plot(residuals(mE_2, "pearson") ~ fitted(mE_2)); abline(0,0)
plot(effect(term="harm_cult_caus", mod=mE_2))



mM_2  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + harm_cult_caus , 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mM_2)
lrtest(mM_null, mM_2)

#### hierarchy_cult_caus

mE_3  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + hierarchy_cult_caus , 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mE_3)
lrtest(mE_null, mE_3)

par(mfrow=c(1,1))
plot(residuals(mE_3) ~ fitted(mE_3)); abline(0,0)


mM_3  = betareg(X_M ~ gdp_caus + pop_size_caus + diverse_caus + hierarchy_cult_caus, 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mM_3)
lrtest(mM_null, mM_3)


#### embeddedness_cult_caus
mE_3  = betareg(X_E ~ gdp_caus  + diverse_caus + embeddedness_cult_caus , 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mE_3)
lrtest(mE_null, mE_3)

mM_3  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + embeddedness_cult_caus, 
                caus_culture_profiles_data_struct_schwartz ,
                type="BR")
summary(mM_3)
lrtest(mM_null, mM_3)



data.frame(
  country = caus_culture_profiles_data_struct_schwartz$country,
  mE = caus_culture_profiles_data_struct_schwartz$X_M,
  var = mM_3$model$embeddedness_cult_caus,
  res = residuals(mM_3, "sweighted2")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)


mM_3$model %>% 
  summarise_all(funs(min, mean, max)) %>% 
  pivot_longer(cols=everything()) %>% 
  arrange(name)

predict_data = data.frame(
  embeddedness_cult_caus = c(4.50, 3.10),
  diverse_caus =  0.294,
  gdp_caus =       3.73 ,
  pop_size_caus =  7.14
)

predict(mM_3, newdata=predict_data)

#### egalit_cult_caus
mE_4  = betareg(X_E ~ gdp_caus  + diverse_caus + egalit_cult_caus , 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mE_4)
lrtest(mE_null, mE_4)


data.frame(
  country = caus_culture_profiles_data_struct_schwartz$country,
  mE = caus_culture_profiles_data_struct_schwartz$X_E,
  var = mE_4$model$egalit_cult_caus,
  res = residuals(mE_4, "sweighted")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)



mM_4  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + egalit_cult_caus, 
                caus_culture_profiles_data_struct_schwartz %>% 
                  filter(country != "Switzerland"),
                type="BR")
summary(mM_4)
lrtest(mM_null, mM_4)

#### int_auto_cult_caus
mE_5  = betareg(X_E ~ gdp_caus  + diverse_caus + int_auto_cult_caus , 
                caus_culture_profiles_data_struct_schwartz ,
                type="BR")
summary(mE_5)
lrtest(mE_null, mE_5)


data.frame(
  country = caus_culture_profiles_data_struct_schwartz$country,
  mE = caus_culture_profiles_data_struct_schwartz$X_E,
  var = mE_5$model$int_auto_cult_caus,
  res = residuals(mE_5, "sweighted")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)



mM_5  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + aff_auto_cult_caus, 
                caus_culture_profiles_data_struct_schwartz %>% 
                  filter(country != "Switzerland"),
                type="BR")
summary(mM_5)
lrtest(mM_null, mM_5)

data.frame(
  country = caus_culture_profiles_data_struct_schwartz$country,
  mE = caus_culture_profiles_data_struct_schwartz$X_M,
  var = mM_5$model$int_auto_cult_caus,
  res = residuals(mM_5, "sweighted")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)


##### Hofstede #######

caus_culture_profiles_data_struct_hofstede = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), gdp_caus, pop_size_caus, diverse_caus, ends_with("hof_caus"))) %>% 
  select(-ltowvs_hof_caus, -ivr_hof_caus) %>% 
  mutate(X1_F = (X_Fec + X_FeC)/2,
         X1_E = (X_fEC + X_fEc)/2,
         X2_C = (X_fEC + X_FeC)/2,
         X2_M = (X_fEc + X_Fec)/2)  %>% 
  na.omit()  
 # filter(country != "India", country != "Brazil", country != "Israel")



caus_culture_profiles_data_struct_hofstede$X_E = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede %>%
                                                                         select_at(vars(starts_with("X1"))) %>% 
                                                                         select(X1_F, X1_E))[,2]

caus_culture_profiles_data_struct_hofstede$X_M = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede %>%
                                                                         select_at(vars(starts_with("X2"))) %>% 
                                                                         select(X2_C, X2_M))[,2]


### Nullmodel ####

mE_Hof_null = betareg(X_E ~  1, 
                  caus_culture_profiles_data_struct_hofstede,
                  type="BR")
summary(mE_Hof_null)

mM_Hof_null = betareg(X_M ~  1, 
                  caus_culture_profiles_data_struct_hofstede,
                  type="BR")
summary(mM_Hof_null)

####


mE1_hof  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + pdi_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede ,
                  type="BR")
summary(mE1_hof)
lrtest(mE_Hof_null,mE1_hof )

data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_E,
  var = caus_culture_profiles_data_struct_hofstede$pdi_hof_caus,
  res = residuals(mE1_hof, "pearson")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)


mM1_hof  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + pdi_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede,
                   type="BR")
summary(mM1_hof)
lrtest(mM_Hof_null,mM1_hof )


data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_E,
  var = caus_culture_profiles_data_struct_hofstede$pdi_hof_caus,
  res = residuals(mM1_hof, "pearson")
) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)



###

# not signficant: idv_hof_caus
mE2_hof  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + idv_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede,
                   type="BR")
summary(mE2_hof)
lrtest(mE_Hof_null,mE2_hof )


data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_E,
  var = caus_culture_profiles_data_struct_hofstede$idv_hof_caus,
  res = residuals(mE2_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)


mM2_hof  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + idv_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede,
                   type="BR")
summary(mM2_hof)
lrtest(mM_Hof_null,mM2_hof )


data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_M,
  var = caus_culture_profiles_data_struct_hofstede$idv_hof_caus,
  res = residuals(mM2_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)





# not signficant: mas_hof_caus

mE3_hof  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + mas_hof_caus,
                   caus_culture_profiles_data_struct_hofstede,
                   type="BR")
summary(mE3_hof)
lrtest(mE_Hof_null, mE3_hof )
summary(margins(mE3_hof))


data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_E,
  var = caus_culture_profiles_data_struct_hofstede$mas_hof_caus,
  res = residuals(mE3_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, col=mE)) +
  geom_smooth(se=F, linetype=6, col="black", size=1.1) +
  geom_point() +
  geom_text(aes(label=country)) +
  geom_hline(yintercept = c(-2,0,2)) 


mM3_hof  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + mas_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede,
                   type="BR")
summary(mM3_hof)
lrtest(mM_Hof_null,mM3_hof )


data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_M,
  var = caus_culture_profiles_data_struct_hofstede$mas_hof_caus,
  res = residuals(mM3_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)  +
  geom_smooth(se=F, linetype=6, col="black", size=1.1) 


#  signficant: uai_hof_caus

mE3_hof  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + uai_hof_caus,
                   caus_culture_profiles_data_struct_hofstede,
                   type="BR")
summary(mE3_hof)
lrtest(mE_Hof_null, mE3_hof )
summary(margins(mE3_hof))


data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_E,
  var = caus_culture_profiles_data_struct_hofstede$uai_hof_caus,
  res = residuals(mE3_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, col=mE)) +
  geom_smooth(se=F, linetype=6, col="black", size=1.1) +
  geom_point() +
  geom_text(aes(label=country)) +
  geom_hline(yintercept = c(-2,0,2)) 


mM3_hof  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + uai_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede,
                   type="BR")
summary(mM3_hof)
lrtest(mM_Hof_null,mM3_hof )


data.frame(
  country = caus_culture_profiles_data_struct_hofstede$country,
  mE = caus_culture_profiles_data_struct_hofstede$X_M,
  var = caus_culture_profiles_data_struct_hofstede$uai_hof_caus,
  res = residuals(mM3_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)  +
  geom_smooth(se=F, linetype=6, col="black", size=1.1) 







# Hofstede 2
caus_culture_profiles_data_struct_hofstede2 = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), ends_with("caus"))) %>% 
  select_at(vars(country, starts_with("X"), gdp_caus, pop_size_caus, diverse_caus, ends_with("hof_caus"))) %>% 
  mutate(X1_F = (X_Fec + X_FeC)/2,
         X1_E = (X_fEC + X_fEc)/2,
         X2_C = (X_fEC + X_FeC)/2,
         X2_M = (X_fEc + X_Fec)/2)  %>% 
  na.omit()  
# filter(country != "India", country != "Brazil", country != "Israel")



caus_culture_profiles_data_struct_hofstede2$X_E = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede2 %>%
                                                                         select_at(vars(starts_with("X1"))) %>% 
                                                                         select(X1_F, X1_E))[,2]

caus_culture_profiles_data_struct_hofstede2$X_M = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede2 %>%
                                                                         select_at(vars(starts_with("X2"))) %>% 
                                                                         select(X2_C, X2_M))[,2]




# not signficant: pdi_hof_caus

mE4_hof  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + pdi_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede2,
                   type="BR")
summary(mE4_hof)


mM4_hof  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + pdi_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede2,
                   type="BR")
summary(mM4_hof)




# not signficant: ltowvs_hof_caus

mE5_hof  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + ltowvs_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede2,
                   type="BR")
summary(mE5_hof)


mM5_hof  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + ltowvs_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede2,
                   type="BR")
summary(mM5_hof)



data.frame(
  country = caus_culture_profiles_data_struct_hofstede2$country,
  mE = caus_culture_profiles_data_struct_hofstede2$X_M,
  var = caus_culture_profiles_data_struct_hofstede2$uai_hof_caus,
  res = residuals(mM5_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)  +
  geom_smooth(se=F, linetype=6, col="black", size=1.1) 



# not signficant: ivr_hof_caus

mE6_hof  = betareg(X_E ~ gdp_caus + pop_size_caus  + diverse_caus + ivr_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede2,
                   type="BR")
summary(mE6_hof)

data.frame(
  country = caus_culture_profiles_data_struct_hofstede2$country,
  mE = caus_culture_profiles_data_struct_hofstede2$X_E,
  var = caus_culture_profiles_data_struct_hofstede2$ivr_hof_caus,
  res = residuals(mE6_hof, "sweighted2")) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = c(-2,0,2))  +
  geom_smooth(se=F, linetype=6, col="black", size=1.1) 



mM6_hof  = betareg(X_M ~ gdp_caus + pop_size_caus  + diverse_caus + ivr_hof_caus, 
                   caus_culture_profiles_data_struct_hofstede2,
                   type="BR")
summary(mM6_hof)



data.frame(
  country = caus_culture_profiles_data_struct_hofstede2$country,
  mE = caus_culture_profiles_data_struct_hofstede2$X_M,
  var = caus_culture_profiles_data_struct_hofstede2$ivr_hof_caus,
  res = residuals(mM5_hof, "pearson")) %>% 
  ggplot(aes(x=var, y=res, label=country, col=mE)) +
  geom_point() +
  geom_text() +
  geom_hline(yintercept = 0)  +
  geom_smooth(se=F, linetype=6, col="black", size=1.1) 







