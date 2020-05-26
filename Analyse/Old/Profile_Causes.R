# Causes of Democracy Profiles

source("Analyse/CreateDatasets.R")
library(brms)
library(gplots)


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


schwartz = fread("Datasets/Schwartz_culture.csv") %>% 
  select(country, 
         comp_cult_caus = mastery, 
         harm_cult_caus = harmony, 
         egalit_cult_caus = egalitarianism, 
         embeddedness_cult_caus = embeddedness,
         hierarchy_cult_caus = hierarchy, 
         aff_auto_cult_caus = affective_autonomy, 
         int_auto_cult_caus = intellectual_autonomy)

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
  summarise(time_democratic = n()) %>%
  ungroup() %>% 
  mutate(time_democratic = time_democratic - median(time_democratic))

defdemocracies = dmx_trade_cluster %>% 
  filter(year >= 1945) %>% 
  dplyr::select(country, classification_core) %>% 
  filter(classification_core == "Deficient Democracy") %>% 
  group_by(country) %>% 
  summarise(def_democracy  = n()) 

time_democratic = time_democratic %>% 
  left_join(defdemocracies, by="country") %>% 
  mutate(def_democracy = ifelse(is.na(def_democracy) == T, 0, def_democracy))


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
profiles_agg = dmx_trade_cluster %>% 
  filter(year >= 1945, country %in% longdemocracies) %>% 
  # group_by(country) %>%
  # top_n(10, -year) %>%
  select(country, cluster_label_1st) %>% 
  group_by(country, cluster_label_1st) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  pivot_wider(names_from = cluster_label_1st, 
              values_from = count, 
              values_fill = list(count = 0), 
              names_prefix ="count_") %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  select(country, country_text_id, count_fEc, everything())
  


# Create Plots
caus_profiles_data = profiles_agg %>% 
  left_join(causal_vars_num, by="country_text_id") %>% 
  left_join(causal_vars_fact, by="country_text_id") %>% 
  left_join(ethnic_frac %>% select(-country), by="country_text_id")  %>% 
  left_join(time_democratic, by="country")

# md.pattern(caus_profiles_data, rotate.names=T)
# md.pattern(caus_profiles_data)

caus_profiles_data %>% 
  select_at(vars(legal_caus, starts_with("count_"))) %>% 
  pivot_longer(cols=starts_with("count")) %>% 
  na.omit() %>% 
  group_by(legal_caus, name) %>% 
  summarise(sum = sum(value, na.rm=T)) %>% 
  ggplot(aes(x = legal_caus, y=sum, fill=name)) + 
  geom_bar(stat="identity", position="dodge")

caus_profiles_data %>% 
  select_at(vars(col_caus, starts_with("count_"))) %>% 
  pivot_longer(cols=starts_with("count")) %>% 
  na.omit() %>% 
  group_by(col_caus, name) %>% 
  summarise(sum = sum(value, na.rm=T)) %>% 
  ggplot(aes(x = col_caus, y=sum, fill=name)) + 
  geom_bar(stat="identity", position="dodge")


get_plot_sample = function(variable, caus_data=caus_profiles_data, reps = 100) {
  data_to_plot = caus_data %>% 
    select_at(vars(starts_with("count_"))) %>% 
    mutate(sum=count_fEc + count_fEC + count_FeC + count_Fec) %>% 
    mutate_all(funs(./sum)) %>% 
    select(-sum)
  
  mean_sample = data.frame(array(NA, dim=c(dim(data_to_plot)[2], reps))) 
  
  
  for (n in 1:reps) {
    get_sample = data.frame(profiles = array(NA, dim=c(dim(data_to_plot)[1], 1))) %>% 
      bind_cols(caus_data %>% select(var_sel = variable))
    for (i in 1:dim(data_to_plot)[1]) {
      get_sample[i,1] = sample(names(data_to_plot), size = 1, prob=data_to_plot[i,])
    }
    
    mean_profiles = get_sample %>% 
      group_by(profiles) %>% 
      summarise(mean = mean(var_sel, na.rm=T)) %>% 
      right_join(data.frame(profiles = names(data_to_plot)), by="profiles")
    mean_sample[,n] = mean_profiles$mean
  }
  
  mean_sample %>% 
    bind_cols(data.frame(profiles = names(data_to_plot))) %>% 
    pivot_longer(cols=starts_with("X")) %>%
    na.omit() %>%
    ggplot(aes(x = profiles, y=value, fill= profiles)) +
    geom_boxplot()
  
}


get_plot_sample("pop_size_caus")
get_plot_sample("pop_density_caus")
get_plot_sample("pluralism_muller_caus")
get_plot_sample("pluralism_1961_caus")
get_plot_sample("pluralism_1964_caus")
get_plot_sample("rel_prot_caus")
get_plot_sample("rel_cath_caus")
get_plot_sample("protestant_centr_caus")
get_plot_sample("ethn_caus")
get_plot_sample("ling_caus")
get_plot_sample("rel_caus")
get_plot_sample("diverse_caus")
get_plot_sample("time_democratic")
get_plot_sample("def_democracy")


############## Create Final Dataset for Analysis
caus_profiles_data_final = caus_profiles_data %>% 
  select_at(vars(country, country_text_id, starts_with("count_"), 
            pop_size_caus, 
            englegal_centr_caus,
            protestant_centr_caus,
            ethn_caus,
            ling_caus,
            rel_caus,
            diverse_caus,
            time_democratic,
            def_democracy)
            ) %>% 
  na.omit() %>% 
  mutate(englegal_centr_caus = as.factor(englegal_centr_caus)) %>% 
  select(country, country_text_id, count_Fec, everything()) 

caus_profiles_data_final$Y_Fec = print(DirichletReg::DR_data(caus_profiles_data_final %>%  
                                                           select_at(vars(starts_with("count_"))) %>% 
                                                           select(count_Fec, everything())
                                                         ), "processed")

caus_profiles_data_final$Y_fEc = print(DirichletReg::DR_data(caus_profiles_data_final %>%  
                                                               select_at(vars(starts_with("count_"))) %>% 
                                                               select(count_fEc, everything())
), "processed")

caus_profiles_data_final$Y_fEC = print(DirichletReg::DR_data(caus_profiles_data_final %>%  
                                                               select_at(vars(starts_with("count_"))) %>% 
                                                               select(count_fEC, everything())
), "processed")

caus_profiles_data_final$Y_FeC = print(DirichletReg::DR_data(caus_profiles_data_final %>%  
                                                               select_at(vars(starts_with("count_"))) %>% 
                                                               select(count_FeC, everything())
), "processed")

caus_profiles_data_final$Y_FEC = print(DirichletReg::DR_data(caus_profiles_data_final %>%  
                                                               select_at(vars(starts_with("count_"))) %>% 
                                                               select(count_FEC, everything())
), "processed")

#######

getRRR = function(model, variable) {
  samples_m1 = brms::posterior_samples(model)
  
  selected_sample = samples_m1[, which(grepl(variable, names(samples_m1)))]
  results = apply(selected_sample, 2, FUN = function(x) {quantile(exp(x), c(0.025, 0.5, 0.975)) }  )
  
  show_results = data.frame(results) %>% 
    bind_cols(data.frame(quantile = c("lower", "mid", "upper"))) %>% 
    pivot_longer(col=ends_with(variable), names_to = "coef") %>% 
    pivot_wider(names_from = quantile) %>% 
    mutate(coef = gsub( "b_mucount", "", coef),
           sig = if_else(lower >=1 | upper <= 1, "*", NA_character_))
  
  names(show_results)[1] = paste("coef_ref", as.character(formula(model))[4], sep="")
  return(show_results)  
}


m1 = brm(
  Y_Fec ~  1 + pop_size_caus + diverse_caus + protestant_centr_caus + englegal_centr_caus + def_democracy,
  caus_profiles_data_final, 
  family=dirichlet(), 
  #prior=prior,
  chains=4, cores=4)
summary(m1, prob=0.95)



m2 = brm(
  Y_fEc ~  1 + pop_size_caus + diverse_caus + protestant_centr_caus + englegal_centr_caus + def_democracy,
  caus_profiles_data_final, 
  family=dirichlet(), 
  #prior=prior,
  chains=4, cores=4)
summary(m2, prob=0.95)



m3 = brm(
  Y_fEC ~  1 + pop_size_caus + diverse_caus + protestant_centr_caus + englegal_centr_caus + def_democracy,
  caus_profiles_data_final, 
  family=dirichlet(), 
  #prior=prior,
  chains=4, cores=4)
summary(m3, prob=0.95)


m4 = brm(
  Y_FeC ~  1 + pop_size_caus + diverse_caus + protestant_centr_caus + englegal_centr_caus + def_democracy,
  caus_profiles_data_final, 
  family=dirichlet(), 
  #prior=prior,
  chains=4, cores=4)
summary(m4, prob=0.95)


m5 = brm(
  Y_FEC ~  1 + pop_size_caus + diverse_caus + protestant_centr_caus + englegal_centr_caus + def_democracy,
  caus_profiles_data_final, 
  family=dirichlet(), 
  #prior=prior,
  chains=4, cores=4)
summary(m5, prob=0.95)


null_m = fit_causes <- brm(
  Y_FEC ~  1,
  caus_profiles_data_final, 
  family=dirichlet(), 
  chains=4, cores=4)
summary(null_m)

test = posterior_predict(null_m)

library(bayesplot)
bayesplot::ppc_dens_overlay(caus_profiles_data_final$Y_FEC[,1], test[1:25,,1])
bayesplot::ppc_dens_overlay(caus_profiles_data_final$Y_FEC[,2], test[1:25,,2])
bayesplot::ppc_dens_overlay(caus_profiles_data_final$Y_FEC[,3], test[1:25,,3])
bayesplot::ppc_dens_overlay(caus_profiles_data_final$Y_FEC[,4], test[1:25,,4])
bayesplot::ppc_dens_overlay(caus_profiles_data_final$Y_FEC[,5], test[1:25,,5])



loo::compare(waic(m1), loo(null_m))
loo::compare(loo(m4), loo(null_m))
loo::compare(loo(m5), loo(null_m))

my_data = conditional_effects(m2, categorical = T, plot=F, probs = c(0.05, 0.95))

my_data$`pop_size_caus:cats__` %>% 
  filter(cats__ %in% c("count_FEC", "count_fEC")) %>% 
  ggplot(aes(x=pop_size_caus, y=estimate__, col=cats__)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower__, ymax=upper__))


getRRR(m1, "pop_size_caus")
getRRR(m2, "pop_size_caus")
getRRR(m3, "pop_size_caus")
getRRR(m4, "pop_size_caus")
getRRR(m5, "pop_size_caus")

getRRR(m1, "protestant_centr_caus")
getRRR(m2, "protestant_centr_caus")
getRRR(m3, "protestant_centr_caus")
getRRR(m4, "protestant_centr_caus")
getRRR(m5, "protestant_centr_caus")


getRRR(m1, "diverse_caus")
getRRR(m2, "diverse_caus")
getRRR(m3, "diverse_caus")
getRRR(m4, "diverse_caus")
getRRR(m5, "diverse_caus")

getRRR(m1, "englegal_centr_caus1")
getRRR(m2, "englegal_centr_caus1")
getRRR(m3, "englegal_centr_caus1")
getRRR(m4, "englegal_centr_caus1")
getRRR(m5, "englegal_centr_caus1")

getRRR(m1, "def_democracy")
getRRR(m2, "def_democracy")
getRRR(m3, "def_democracy")
getRRR(m4, "def_democracy")
getRRR(m5, "def_democracy")









# pop_size_caus
# pop_density_caus
# pluralism_1961_caus
# pluralism_1964_caus 
# 
# rel_prot_caus 
# rel_cath_caus 
# rel_musl_caus
# rel_oth_caus 
# 
# legal_caus 
# col_caus 
prior = c(
  set_prior("normal(0,10)", class="b"),
  set_prior("normal(0,10)", class="Intercept")
          )

library(loo)

null_m = fit_causes <- brm(
  Y ~  1,
  caus_profiles_data_final, 
  family=dirichlet(), 
  chains=4, cores=4)
summary(null_m)


m1_test = fit_causes <- brm(
  Y_FEC ~  1 + pop_size_caus,
  caus_profiles_data_final, 
  family=dirichlet(), 
  chains=4, cores=4)
summary(m1, prob=0.95)

compare(loo(null_m), loo(m1))


m2 = fit_causes <- brm(
  Y ~  1 + pop_size_caus + diverse_caus,
  caus_profiles_data_final, 
  family=dirichlet(), 
  chains=4, cores=4)
summary(m2, prob=0.95)
loo_compare(loo(null_m), loo(m2))
loo_compare(loo(m1), loo(m2))

m3 = fit_causes <- brm(
  Y ~  1 + pop_size_caus + diverse_caus + protestant_centr_caus,
  caus_profiles_data_final, 
  family=dirichlet(), 
  chains=4, cores=4)
summary(m3, prob=0.95)
loo_compare(loo(null_m), loo(m3))
loo_compare(loo(m2), loo(m3))

stanfit = m4$fit
m4 = brm(
  Y ~  1 + pop_size_caus + diverse_caus + protestant_centr_caus + englegal_centr_caus + def_democracy,
  caus_profiles_data_final, 
  family=dirichlet(), 
  #prior=prior,
  chains=4, cores=4)


summary(m4, prob=0.95)

loo_compare(loo(null_m), loo(m4))
loo_compare(loo(m3), loo(m4))


my_data = conditional_effects(m4, categorical = T, plot=F, probs = c(0.05, 0.95))

my_data$`diverse_caus:cats__` %>% 
  filter(cats__ %in% c("count_fEC", "count_fEc")) %>% 
  ggplot(aes(x=diverse_caus, y=estimate__, col=cats__)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower__, ymax=upper__))



x1 = exp(-0.31)/(1+exp(-0.31) + exp(-0.54) + exp(-0.95) + exp(-0.82))
x2 = 1/(1+exp(-0.31) + exp(-0.54) + exp(-0.95) + exp(-0.82))

x1/x2

### Culture


hofstede = fread("Datasets/hofstede_data.csv") %>% 
  select(-ctr) 

# Uncertainty
longdemocracies = dmx_trade_cluster %>% 
  filter(year >= 1945) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(nr = n()) %>% 
  filter(nr > 1) %>% 
  pull(country)


profiles_agg_cult = dmx_trade_cluster %>% 
  filter(year >= 1945, country %in% longdemocracies) %>% 
  # group_by(country) %>%
  # top_n(10, -year) %>%
  select(country, cluster_label_1st) %>% 
  group_by(country, cluster_label_1st) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  pivot_wider(names_from = cluster_label_1st, 
              values_from = count, 
              values_fill = list(count = 0), 
              names_prefix ="count_") %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  select(country, country_text_id, count_fEc, everything())


caus_culture_profiles_data = profiles_agg_cult  %>% 
  left_join(ethnic_frac %>% select(-country), by="country_text_id") %>% 
  left_join(causal_vars_num, by="country_text_id") %>%
  left_join(causal_vars_fact, by="country_text_id") %>%
  left_join(schwartz, by="country")  %>% 
  left_join(time_democratic, by="country") %>% 
  select_at(vars(country, country_text_id, starts_with("count_"), 
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
                 def_democracy,
                 time_democratic)
  ) %>% 
  left_join(hofstede, by="country") %>% 
  select(country, country_text_id, count_fEc, everything()) 
  # mutate(count_agg1_F = count_Fec + count_FeC,
  #        count_agg1_E = count_fEc + count_fEC,
  #        count_agg1_B = count_FEC) %>% 
  # mutate(count_agg2_C = count_fEC + count_FeC,
  #        count_agg2_NC = count_fEc + count_Fec,
  #        count_agg2_B = count_FEC)

caus_culture_profiles_data$Y = print(DirichletReg::DR_data(caus_culture_profiles_data %>%  
                                                             select_at(vars(starts_with("count_")))), "processed")


get_plot_sample("comp_cult_caus", caus_culture_profiles_data)
get_plot_sample("harm_cult_caus", caus_culture_profiles_data)
get_plot_sample("egalit_cult_caus", caus_culture_profiles_data)
get_plot_sample("embeddedness_cult_caus", caus_culture_profiles_data)
get_plot_sample("aff_auto_cult_caus", caus_culture_profiles_data)
get_plot_sample("int_auto_cult_caus", caus_culture_profiles_data)
get_plot_sample("hierarchy_cult_caus", caus_culture_profiles_data)

get_plot_sample("pdi", caus_culture_profiles_data)
get_plot_sample("idv", caus_culture_profiles_data)
get_plot_sample("mas", caus_culture_profiles_data)
get_plot_sample("uai", caus_culture_profiles_data)
get_plot_sample("ltowvs", caus_culture_profiles_data,20)
get_plot_sample("ivr", caus_culture_profiles_data)

####

prior_cult = c(
  set_prior("normal(0,0.2)", class="b"),
  set_prior("normal(0,0.2)", class="Intercept")
)

stanmodel = m_cult$fit
m_cult <- brm(
  Y ~  1 + pop_size_caus + englegal_centr_caus + diverse_caus + protestant_centr_caus + ivr,
  caus_culture_profiles_data, 
  family=dirichlet(), 
  #prior=prior_cult,
  chains=4, cores=4)
summary(m_cult, prob=0.95)


#conditional_effects(m_cult, categorical = T)

my_data = conditional_effects(m_cult, categorical = T, plot=F)

my_data$`comp_cult_caus:cats__` %>% 
  ggplot(aes(x=comp_cult_caus, y=estimate__, col=cats__)) +
  geom_point()
