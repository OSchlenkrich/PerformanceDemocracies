# Causes of Democracy Profiles

source("Analyse/CreateDatasets.R")
# library(brms)
library(gplots)
library(DirichletReg)



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


barbados_sup = fread("Datasets/barbados_supp.csv") 

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
                              "Belarus" = "Belarus (Byelorussian SSR)"	)) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-country)




causal_vars = QoC_data %>% 
  dplyr::select(country_text_id, year,
                pop_size_caus = wdi_pop,
                gdp_caus = wdi_gdpcapcur,
                pop_density_caus = wdi_popden, 
                
                pluralism_1961_caus = r_elf61,
                pluralism_1964_caus = r_atlas,
                pluralism_muller_caus = r_muller,
                rel_prot_caus = lp_protmg80,
                rel_cath_caus = lp_catho80,
                rel_musl_caus = lp_muslim80,
                rel_oth_caus = lp_no_cpm80,
                
                legal_caus = lp_legor,
                col_caus = ht_colonial)  %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  group_by(country_text_id, year) %>% 
  summarise_all(mean_NA) %>% 
  ungroup() %>% 
  mutate(
    pop_size_caus = ifelse(pop_size_caus == 0, NA, pop_size_caus),
    pop_density_caus = ifelse(pop_density_caus == 0, NA, pop_density_caus),
    pop_size_caus = log10(pop_size_caus),
    gdp_caus = log10(gdp_caus),
    
    pop_density_caus = log10(pop_density_caus),
    col_caus = as.factor(col_caus),
    legal_caus = as.factor(legal_caus)
  ) %>% 
  mutate(
    legal_caus = fct_recode(legal_caus,
                            "English" = "1",
                            "French/German/Scandinavian" = "2",
                            "French/German/Scandinavian" = "3",
                            "French/German/Scandinavian" = "4",
                            "French/German/Scandinavian" = "5"),
    legal_caus = relevel(legal_caus, ref="French/German/Scandinavian")
  )

# remove some NAs
# http://pages.ucsd.edu/%7Eproeder/elf.htm
causal_vars$pluralism_1961_caus[causal_vars$country_text_id == "BRB" & causal_vars$year >= 1961] = barbados_sup$ELF85 
causal_vars$pluralism_1964_caus[causal_vars$country_text_id == "BRB"  & causal_vars$year >= 1964] = barbados_sup$ELF85 
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "BRB"  & causal_vars$year >= 1964] = barbados_sup$ELF85 

causal_vars$pluralism_1961_caus[causal_vars$country_text_id == "BWA" & causal_vars$year >= 1961] = 0.511
causal_vars$pluralism_1964_caus[causal_vars$country_text_id == "BWA"  & causal_vars$year >= 1964] = 0.511
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "BWA"  & causal_vars$year >= 1964] = 0.511

causal_vars$pluralism_1961_caus[causal_vars$country_text_id == "CPV" & causal_vars$year >= 1961] = 0.527
causal_vars$pluralism_1964_caus[causal_vars$country_text_id == "CPV"  & causal_vars$year >= 1964] = 0.527
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "CPV"  & causal_vars$year >= 1964] = 0.527

causal_vars$pluralism_1961_caus[causal_vars$country_text_id == "MUS" & causal_vars$year >= 1961] = 0.477
causal_vars$pluralism_1964_caus[causal_vars$country_text_id == "MUS"  & causal_vars$year >= 1964] = 0.477
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "MUS"  & causal_vars$year >= 1964] = 0.477

causal_vars$pluralism_1961_caus[causal_vars$country_text_id == "SUR" & causal_vars$year >= 1961] = 0.702
causal_vars$pluralism_1964_caus[causal_vars$country_text_id == "SUR"  & causal_vars$year >= 1964] = 0.702
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "SUR"  & causal_vars$year >= 1964] = 0.702

causal_vars$pluralism_1961_caus[causal_vars$country_text_id == "KOR" & causal_vars$year >= 1961] = 0.003
causal_vars$pluralism_1964_caus[causal_vars$country_text_id == "KOR"  & causal_vars$year >= 1964] = 0.003
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "KOR"  & causal_vars$year >= 1964] = 0.003

causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "DEU"  & causal_vars$year >= 1964] = 0.026
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "SEN"  & causal_vars$year >= 1964] = 0.748
causal_vars$pluralism_muller_caus[causal_vars$country_text_id == "URY"  & causal_vars$year >= 1964] = 0.200



causal_vars_num = causal_vars %>% 
  left_join(centripetalism_caus, by=c("country_text_id", "year")) %>% 
  select(-year) %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.numeric, list(~mean_NA(.)))

causal_vars_fact = causal_vars %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.factor, list(~getmode(.)))


time_democratic = dmx_trade_cluster %>% 
  filter(year >= 1945) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(time_democratic_caus = n()) %>%
  ungroup() %>% 
  mutate(time_democratic_caus = time_democratic_caus - median(time_democratic_caus))

defdemocracies = dmx_trade_cluster %>% 
  filter(year >= 1945) %>% 
  dplyr::select(country, classification_core) %>% 
  filter(classification_core == "Deficient Democracy") %>% 
  group_by(country) %>% 
  summarise(def_democracy_caus  = n()) 

time_democratic = time_democratic %>% 
  left_join(defdemocracies, by="country") %>% 
  mutate(def_democracy_caus = ifelse(is.na(def_democracy_caus) == T, 0, def_democracy_caus))




# Consolidated Democracy
longdemocracies = dmx_trade_cluster %>% 
  filter(year >= 1945) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(nr = n()) %>% 
  filter(nr > 1) %>% 
  pull(country)

# Uncertainty
profiles_agg_cult = dmx_trade_cluster %>% 
  filter(year >= 1945, country %in% longdemocracies) %>% 
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
                 ethn_caus,
                 diverse_caus,
                 protestant_centr_caus,
                 comp_cult_caus,
                 harm_cult_caus,
                 egalit_cult_caus,
                 embeddedness_cult_caus,
                 hierarchy_cult_caus,
                 aff_auto_cult_caus,
                 int_auto_cult_caus,
                 def_democracy_caus,
                 time_democratic_caus)
  ) %>% 
  left_join(hofstede, by="country") %>% 
  left_join(WRP, by="COWcode")



# Make Plots
caus_culture_profiles_data %>% 
  select_at(vars(englegal_centr_caus, starts_with("X"))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  group_by(englegal_centr_caus, name) %>% 
  summarise(mean = mean(value, na.rm=T)) %>% 
  ggplot(aes(x = englegal_centr_caus, y=mean, fill=name)) + 
  geom_bar(stat="identity", position="dodge")

caus_culture_profiles_data %>% 
  select_at(vars(gdp_caus, starts_with("X"))) %>% 
  mutate(gdp_caus = cut(gdp_caus, quantile(gdp_caus, probs = seq(0, 1, 0.2), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = gdp_caus, y=value, fill=name)) + 
  geom_boxplot()


caus_culture_profiles_data %>% 
  select_at(vars(pop_size_caus, starts_with("X"))) %>% 
  mutate(pop_size_caus = cut(pop_size_caus, quantile(pop_size_caus, probs = seq(0, 1, 0.2), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = pop_size_caus, y=value, fill=name)) + 
  geom_boxplot()

caus_culture_profiles_data %>% 
  select_at(vars(diverse_caus, starts_with("X"))) %>% 
  mutate(diverse_caus = cut(diverse_caus, quantile(diverse_caus, probs = seq(0, 1, 0.2), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = diverse_caus, y=value, fill=name)) + 
  geom_boxplot()

caus_culture_profiles_data %>% 
  select_at(vars(rel_prot_caus, starts_with("X"))) %>% 
  mutate(rel_prot_caus = cut(rel_prot_caus, quantile(rel_prot_caus, probs = seq(0, 1, .25), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = rel_prot_caus, y=value, fill=name)) + 
  geom_boxplot()

caus_culture_profiles_data %>% 
  select_at(vars(rel_cath_wrp_caus, starts_with("X"))) %>% 
  mutate(rel_cath_wrp_caus = cut(rel_cath_wrp_caus, quantile(rel_cath_wrp_caus, probs = seq(0, 1, 0.2), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = rel_cath_wrp_caus, y=value, fill=name)) + 
  geom_boxplot()

caus_culture_profiles_data %>% 
  select_at(vars(time_democratic_caus, starts_with("X"))) %>% 
  mutate(time_democratic_caus = cut(time_democratic_caus, quantile(time_democratic_caus, probs = seq(0, 1, .25), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = time_democratic_caus, y=value, fill=name)) + 
  geom_boxplot()

caus_culture_profiles_data %>% 
  select_at(vars(def_democracy_caus, starts_with("X"))) %>% 
  mutate(def_democracy_caus = cut(def_democracy_caus, quantile(def_democracy_caus, probs = seq(0, 1, .25), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = def_democracy_caus, y=value, fill=name)) + 
  geom_boxplot()

caus_culture_profiles_data %>% 
  select_at(vars(comp_cult_caus , starts_with("X"))) %>% 
  mutate(comp_cult_caus  = cut(comp_cult_caus , quantile(comp_cult_caus , probs = seq(0, 1, .25), na.rm=T))) %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  na.omit() %>% 
  ggplot(aes(x = comp_cult_caus , y=value, fill=name)) + 
  geom_boxplot()


par(mfrow=c(4,2))
hist(caus_culture_profiles_data$gdp_caus)
hist(caus_culture_profiles_data$englegal_centr_caus)
hist(caus_culture_profiles_data$pop_size_caus)
hist(caus_culture_profiles_data$diverse_caus)
hist(caus_culture_profiles_data$rel_prot_caus)
hist(caus_culture_profiles_data$time_democratic)
hist(caus_culture_profiles_data$def_democracy)
par(mfrow=c(1,1))


############## Create Dataset for Analysis: Structural Variables


caus_culture_profiles_data_struct = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), ends_with("_caus"), -matches("cult"), -matches("hof"))) %>% 
  na.omit()   %>% 
  filter(country != "Sierra Leone", country != "Mozambique", country != "Israel",
         country != "Lesotho")
  # mutate(rel_cath_wrp_caus = cut(rel_cath_wrp_caus, quantile(rel_cath_wrp_caus, probs = seq(0, 1, 0.25), na.rm=T), include.lowest=T, labels = NULL)) %>% 
  # mutate(diverse_caus = cut(diverse_caus, quantile(diverse_caus, probs = seq(0, 1, 0.25), na.rm=T), include.lowest=T, labels = NULL)) %>% 
  # mutate(pop_size_caus = cut(pop_size_caus, quantile(pop_size_caus, probs = seq(0, 1, 0.25), na.rm=T), include.lowest=T, labels = NULL))

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



m1  = DirichReg(Y_Fec ~  1,
                caus_culture_profiles_data_struct, "alternative")
summary(m1)


# GDP not significant related to Democracy Profiles
m2  = DirichReg(Y_Fec ~  gdp_caus,
                caus_culture_profiles_data_struct, "alternative")
summary(m2)
anova(m1,m2)

eps = 1e-7
setstep <- function(x) {
  x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
}


h = setstep(data.frame(pop_size_caus = m2$d$pop_size_caus))

d0 = data.frame(pop_size_caus = m2$d$pop_size_caus  + h)
d1 = data.frame(pop_size_caus = m2$d$pop_size_caus  - h)

P0 = predict(m2, newdata=d0)
P1 = predict(m2, newdata=d1)

mean((P1 - P0)[,4] / (d1 - d0)[[1]])
# 
# 
# predict(m2, newdata=data.frame(pop_size_caus = c(7,8)))



par(mfrow=c(4,1))
plot(residuals(m2)[,1] ~ fitted(m2)[,1]); abline(0,0)
plot(residuals(m2)[,2] ~ fitted(m2)[,2]); abline(0,0)
plot(residuals(m2)[,3] ~ fitted(m2)[,3]); abline(0,0)
plot(residuals(m2)[,4] ~ fitted(m2)[,4]); abline(0,0)


# pop_size_caus is significant related
m3  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus,
                caus_culture_profiles_data_struct, "alternative")
summary(m3)
anova(m1,m3)

par(mfrow=c(4,1))
plot(residuals(m3)[,1] ~ fitted(m3)[,1]); abline(0,0)
plot(residuals(m3)[,2] ~ fitted(m3)[,2]); abline(0,0)
plot(residuals(m3)[,3] ~ fitted(m3)[,3]); abline(0,0)
plot(residuals(m3)[,4] ~ fitted(m3)[,4]); abline(0,0)

# englegal_centr_caus is significant related
m4  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + englegal_centr_caus,
                caus_culture_profiles_data_struct, "alternative")
summary(m4)
anova(m3,m4)


par(mfrow=c(4,1))
plot(residuals(m4)[,1] ~ fitted(m4)[,1]); abline(0,0)
plot(residuals(m4)[,2] ~ fitted(m4)[,2]); abline(0,0)
plot(residuals(m4)[,3] ~ fitted(m4)[,3]); abline(0,0)
plot(residuals(m4)[,4] ~ fitted(m4)[,4]); abline(0,0)


# diverse_caus is significant related
m5  = DirichReg(Y_Fec ~  gdp_caus + pop_size_caus + englegal_centr_caus + diverse_caus  | 1,
                caus_culture_profiles_data_struct %>% 
                  mutate(diverse_caus_1 = diverse_caus,
                         diverse_caus_2 = diverse_caus^2), "alternative")
summary(m5)
anova(m4,m5)


m5$d %>% 
  select_at(vars(ends_with("caus"))) %>% 
  summarise_all(funs(mean, max, min)) %>% 
  pivot_longer(cols=everything()) %>% 
  arrange(name)

test = data.frame(
  gdp_caus = 3.400977,
  englegal_centr_caus = 1,
  diverse_caus = 0,
  pop_size_caus = 9.88
)

predict(m5, newdata = test)


par(mfrow=c(4,1))
plot(residuals(m5)[,1] ~ fitted(m5)[,1]); abline(0,0)
plot(residuals(m5)[,2] ~ fitted(m5)[,2]); abline(0,0)
plot(residuals(m5)[,3] ~ fitted(m5)[,3]); abline(0,0)
plot(residuals(m5)[,4] ~ fitted(m5)[,4]); abline(0,0)

plot(residuals(m5)[,1] ~ c(1:dim(m5$d)[1])); abline(0,0)
plot(residuals(m5)[,2] ~ c(1:dim(m5$d)[1])); abline(0,0)
plot(residuals(m5)[,3] ~ c(1:dim(m5$d)[1])); abline(0,0)
plot(residuals(m5)[,4] ~ c(1:dim(m5$d)[1])); abline(0,0)

caus_culture_profiles_data_struct2 = caus_culture_profiles_data_struct %>% 
  select(country) %>% 
  bind_cols(data.frame(residuals(m5)[,1:4]))
caus_culture_profiles_data_struct$protestant_centr_caus
# protestant_centr_caus is significant related
m6  = DirichReg(Y_Fec ~  gdp_caus +  pop_size_caus + englegal_centr_caus + diverse_caus + time_democratic_caus | 1,
                caus_culture_profiles_data_struct %>% 
                  mutate(diverse_caus_1 = diverse_caus,
                         diverse_caus_2 = diverse_caus^2) %>% 
                  mutate(rel_cath_wrp_caus_1 = rel_cath_wrp_caus,
                         rel_cath_wrp_caus_2 = rel_cath_wrp_caus^2), "alternative")
summary(m6)
anova(m5,m6)

m6$d %>% 
  select_at(vars(ends_with("caus"))) %>% 
  summarise_all(funs(mean, max, min)) %>% 
  pivot_longer(cols=everything()) %>% 
  arrange(name)

test = data.frame(
  gdp_caus = 3.400977,
  englegal_centr_caus = 0,
  diverse_caus = 0,
  pop_size_caus = 4.88,
  time_democratic_caus = 48
)

predict(m6, newdata = test)



par(mfrow=c(4,1))
plot(residuals(m6)[,1] ~ fitted(m6)[,1]); abline(0,0)
plot(residuals(m6)[,2] ~ fitted(m6)[,2]); abline(0,0)
plot(residuals(m6)[,3] ~ fitted(m6)[,3]); abline(0,0)
plot(residuals(m6)[,4] ~ fitted(m6)[,4]); abline(0,0)

plot(residuals(m6)[,1] ~ c(1:dim(m6$d)[1])); abline(0,0)
plot(residuals(m6)[,2] ~ c(1:dim(m6$d)[1])); abline(0,0)
plot(residuals(m6)[,3] ~ c(1:dim(m6$d)[1])); abline(0,0)
plot(residuals(m6)[,4] ~ c(1:dim(m6$d)[1])); abline(0,0)

###### Schwarz: Culture
library(betareg)

caus_culture_profiles_data_struct_schwartz = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), ends_with("_caus"), -matches("hof"))) %>% 
  filter(is.na(comp_cult_caus) ==F) %>% 
  mutate(X1_F = (X_Fec + X_FeC)/2,
         X1_E = (X_fEC + X_fEc)/2,
         X2_C = (X_fEC + X_FeC)/2,
         X2_M = (X_fEc + X_Fec)/2)  %>% 
  filter(is.na(comp_cult_caus) == F, is.na(gdp_caus) == F) %>% 
  na.omit() 
  # mutate(comp_cult_caus = cut(comp_cult_caus, quantile(comp_cult_caus, probs = seq(0, 1, 0.25), na.rm=T), include.lowest=T, labels = NULL)) %>% 
  # mutate(harm_cult_caus = cut(harm_cult_caus, quantile(harm_cult_caus, probs = seq(0, 1, 0.25), na.rm=T), include.lowest=T, labels = NULL)) %>% 
  # mutate(hierarchy_cult_caus = cut(hierarchy_cult_caus, quantile(hierarchy_cult_caus, probs = seq(0, 1, 0.25), na.rm=T), include.lowest=T, labels = NULL)) %>% 
  # mutate(embeddedness_cult_caus = cut(embeddedness_cult_caus, quantile(embeddedness_cult_caus, probs = seq(0, 1, 0.25), na.rm=T), include.lowest=T, labels = NULL))





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

library(margins)

mE_1  = betareg(X_E ~  comp_cult_caus, 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
m2_marg = margins(mE_1, vce="bootstrap", iterations=100L)
summary(m2_marg)
N = 200
x = rnorm(N, 0, 1)
g = rnorm(N, 0, 1)
xg = x*g
y = 2 + 0.8*x + 0.5*g + 0.2*xg  + rnorm(N,0,1)
lm1 <- lm(y ~ x * g)
summary(lm1)
test = margins(lm1, vce="delta", at=list(g=seq(-1,1,length.out = 4)))
summary(test)

summary(margins(lm1, vce="delta", iterations=100L))
summary(margins(lm1, vce="bootstrap", iterations=1000L, at=list(g=seq(-1,1,length.out = 2))))



mean(lm1$model$g*lm1$model$x)


eps = 1e-7
setstep <- function(x) {
  x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
}


m_effects = function(data, samp) {
  dataset = data[samp, ]
  
  lm1_temp <- lm(y ~ x * g, dataset)
  
  h = setstep(data.frame(x = dataset$x))[[1]]
  
  d0 = data.frame(x = dataset$x + h, g =  0.3333)
  d1 = data.frame(x = dataset$x - h, g = 0.3333)
  P0 = predict(lm1_temp, newdata=d0)
  P1 = predict(lm1_temp, newdata=d1)
  
  mean(((P1 - P0) / (d1["x"] - d0["x"]))[[1]])
}

data = lm1$model
boots_sample = array(NA, 10000)

for (i in 1:10000) {
  samp <- sample(seq_len(nrow(data)), nrow(data), TRUE)
  boots_sample[i] = m_effects(data, samp)

}

0.7235
0.9009  + sqrt(var(boots_sample)) %*% qnorm(a)

sqrt(var(boots_sample))

var(t(replicate(100, m_effects(data, samp))))


sd(boots_sample)*3
level = 0.95
sd(boots_sample)
mean(boots_sample)
a <- (1 - level)/2
a <- c(a, 1-a)
0.9378  + sqrt(var(boots_sample)) %*% qnorm(a)



quantile(boots_sample, c(0.025, 0.975))


mE_1  = betareg(X_E ~ gdp_caus + poly(comp_cult_caus,1), 
                     caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mE_1)
lrtest(mE_null, mE_1)

margins(mE_1)


par(mfrow=c(1,1))
plot(residuals(mE_1, "pearson") ~ fitted(mE_1)); abline(0,0)


mM_1  = betareg(X_M ~ gdp_caus + poly(comp_cult_caus,1), 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mM_1)
lrtest(mM_null, mM_1)

plot(residuals(mM_1, "pearson") ~ fitted(mM_1)); abline(0,0)

####

mE_2  = betareg(X_E ~ gdp_caus + harm_cult_caus + I(harm_cult_caus^2), 
                caus_culture_profiles_data_struct_schwartz %>% 
                  mutate(harm_cult_caus_1 = harm_cult_caus,
                         harm_cult_caus_2 = harm_cult_caus^2) ,
                type="BR")
summary(mE_2)
lrtest(mE_null, mE_2)
plot(residuals(mE_2, "pearson") ~ fitted(mE_2)); abline(0,0)
plot(effect(term="poly(harm_cult_caus,2)", mod=mE_2))

predict(mE_2)

library(margins)
test = summary(margins(mE_2, at=list(harm_cult_caus = seq(3.352,4.905,0.1)), type = "link"))

test %>% 
  filter(factor=="harm_cult_caus") %>% 
  ggplot(aes(x=harm_cult_caus, y=AME)) +
  geom_line() 

mM_2  = betareg(X_M ~ gdp_caus + poly(harm_cult_caus,2), 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mM_2)
lrtest(mM_null, mM_2)

####

mE_3  = betareg(X_E ~ gdp_caus + poly(hierarchy_cult_caus,2), 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mE_3)
lrtest(mE_null, mE_3)

par(mfrow=c(1,1))
plot(residuals(mE_3) ~ fitted(mE_3)); abline(0,0)


mM_3  = betareg(X_M ~ gdp_caus + pop_size_caus + poly(hierarchy_cult_caus,2), 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mM_3)
lrtest(mM_null, mM_3)

####
mE_3  = betareg(X_E ~ gdp_caus + pop_size_caus + poly(embeddedness_cult_caus,2), 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mE_3)
lrtest(mE_null, mE_3)

mM_3  = betareg(X_M ~ gdp_caus + pop_size_caus + poly(embeddedness_cult_caus,3), 
                caus_culture_profiles_data_struct_schwartz,
                type="BR")
summary(mM_3)
lrtest(mM_null, mM_3)

par(mfrow=c(1,1))
plot(residuals(mM_3) ~ fitted(mM_3)); abline(0,0)





##############

caus_culture_profiles_data_struct_hofstede = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), ends_with("caus"))) %>% 
  filter(is.na(pdi_hof_caus) ==F)

caus_culture_profiles_data_struct_hofstede$Y_Fec = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_Fec, everything()))

caus_culture_profiles_data_struct_hofstede$Y_fEc = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_fEc, everything()))

caus_culture_profiles_data_struct_hofstede$Y_fEC = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_fEC, everything()))

caus_culture_profiles_data_struct_hofstede$Y_FeC = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_FeC, everything()))



m1_hof  = DirichReg(Y_Fec ~  1 + englegal_centr_caus + gdp_caus, 
                    caus_culture_profiles_data_struct_hofstede, "alternative")
summary(m1_hof)

# not signficant: pdi_hof_caus
m2_hof  = DirichReg(Y_Fec ~  englegal_centr_caus + gdp_caus + pdi_hof_caus, 
                    caus_culture_profiles_data_struct_hofstede, "alternative")
summary(m2_hof)
anova(m1_hof, m2_hof)


# not signficant: idv_hof_caus
m3_hof  = DirichReg(Y_Fec ~  englegal_centr_caus + gdp_caus + idv_hof_caus, 
                    caus_culture_profiles_data_struct_hofstede, "alternative")
summary(m3_hof)
anova(m1_hof, m3_hof)


# not signficant: mas_hof_caus
m4_hof  = DirichReg(Y_Fec ~  englegal_centr_caus + gdp_caus + mas_hof_caus , 
                    caus_culture_profiles_data_struct_hofstede, "alternative")
summary(m4_hof)
anova(m1_hof, m4_hof)

# not signficant: uai_hof_caus
m5_hof  = DirichReg(Y_Fec ~  englegal_centr_caus + gdp_caus + uai_hof_caus  , 
                    caus_culture_profiles_data_struct_hofstede, "alternative")
summary(m5_hof)
anova(m1_hof, m5_hof)


# Hofstede 2
caus_culture_profiles_data_struct_hofstede2 = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("X"), ends_with("caus"))) %>% 
  filter(is.na(ltowvs_hof_caus) ==F)

caus_culture_profiles_data_struct_hofstede2$Y_Fec = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede2 %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_Fec, everything()))

caus_culture_profiles_data_struct_hofstede2$Y_fEc = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede2 %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_fEc, everything()))

caus_culture_profiles_data_struct_hofstede2$Y_fEC = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede2 %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_fEC, everything()))

caus_culture_profiles_data_struct_hofstede2$Y_FeC = DirichletReg::DR_data(caus_culture_profiles_data_struct_hofstede2 %>%  
                                                                           select_at(vars(starts_with("X_"))) %>% 
                                                                           select(X_FeC, everything()))


# not signficant: pdi_hof_caus
m1_hof2  = DirichReg(Y_Fec ~  englegal_centr_caus + gdp_caus   , 
                    caus_culture_profiles_data_struct_hofstede2, "alternative")
summary(m1_hof2)


# not signficant: pdi_hof_caus
m2_hof2  = DirichReg(Y_fEc ~  englegal_centr_caus + gdp_caus + ltowvs_hof_caus   , 
                    caus_culture_profiles_data_struct_hofstede2, "alternative")
summary(m2_hof2)
anova(m1_hof2, m2_hof2)








