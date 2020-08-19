source("Analyse/CreateDatasets.R")


# Democracy Profiles ####
profiles_agg_cult = dmx_trade_cluster %>% 
  filter(year >= 1990, year <= 2000) %>% 
  select_at(vars(country, starts_with("mp_cluster4"), starts_with("mp_cluster5"))) %>% 
  group_by(country) %>% 
  summarise_all(mean, na.omit=T) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country")


# VoC & Welfare Data ####
regime_types = fread("Datasets/own_VoC_welfare.csv", sep=";", encoding="UTF-8")%>% 
  select(-country_code, -Coord_Ind_1990)

# Cultural Values ####

schwartz_caus_regime = fread("Datasets/Schwartz_culture.csv") %>% 
  select(country, 
         mastery_cult_caus = mastery, 
         harm_cult_caus = harmony, 
         egalitarian_cult_caus = egalitarianism, 
         embeddedness_cult_caus = embeddedness,
         hierarchy_cult_caus = hierarchy, 
         aff_auto_cult_caus = affective_autonomy, 
         int_auto_cult_caus = intellectual_autonomy) %>% 
  select(country, mastery_cult_caus, egalitarian_cult_caus)



# Create Main Dataset ####
PolicyRegime_data = regime_types  %>% 
  left_join(profiles_agg_cult, by="country") %>% 
  left_join(schwartz_caus_regime, by="country") 


# Plot: VoC, Welfare and Profile ####
p1 = PolicyRegime_data %>%
  select_at(vars(starts_with("mp_Cluster5"), Welfare1990_EA )) %>% 
  mutate(Welfare1990_EA = fct_reorder(Welfare1990_EA, mp_Cluster5_fEc)) %>% 
  pivot_longer(cols = -c(Welfare1990_EA)) %>% 
  mutate(name = gsub("mp_Cluster4_","",name),
         name = gsub("mp_Cluster5_","",name)) %>% 
  na.omit() %>% 
  ggplot(aes(x=Welfare1990_EA, y=value, fill=name)) +
  scale_fill_discrete(name ="") +
  xlab("Welfare States") +
  ylab("Membership Probability") +
  geom_boxplot() +
  theme_bw()

p2 = PolicyRegime_data %>%
  select_at(vars(starts_with("mp_Cluster5"), Welfare1999_EA)) %>% 
  mutate(Welfare1999_EA = fct_reorder(Welfare1999_EA, mp_Cluster5_fEc)) %>% 
  pivot_longer(cols = -c(Welfare1999_EA)) %>% 
  mutate(name = gsub("mp_Cluster4_","",name),
         name = gsub("mp_Cluster5_","",name)) %>% 
  na.omit() %>% 
  filter(Welfare1999_EA != "Universalist/Residual") %>% 
  ggplot(aes(x=Welfare1999_EA, y=value, fill=name))+
  scale_fill_discrete(name ="") +
  geom_boxplot() +
  xlab("Welfare States") +
  ylab("Membership Probability") +
  theme_bw()

p3 = PolicyRegime_data %>%
  select_at(vars(starts_with("mp_Cluster5"), VoC_HS)) %>% 
  mutate(VoC_HS = fct_reorder(VoC_HS, mp_Cluster5_fEc)) %>% 
  pivot_longer(cols = -c(VoC_HS)) %>% 
  mutate(name = gsub("mp_Cluster4_","",name),
         name = gsub("mp_Cluster5_","",name)) %>% 
  na.omit() %>% 
  ggplot(aes(x=VoC_HS, y=value, fill=name))+
  scale_fill_discrete(name ="") +
  geom_boxplot() +
  xlab("Varieties of Capitalism") +
  ylab("Membership Probability") +
  theme_bw()


ggarrange(p1, p3, common.legend = T, legend = "bottom")


# Plot: VoC - Welfare State ####

cnt_welfare_VoC = PolicyRegime_data %>%
  select_at(vars(Welfare1990_EA, VoC_HS)) %>% 
  group_by(VoC_HS, Welfare1990_EA) %>% 
  summarise(count = n()) %>% 
  rename(Welfare = Welfare1990_EA) %>%
  mutate(typology = "Welfare 1990") %>% 
  na.omit() %>% 
  mutate(Welfare = fct_reorder(Welfare, count))
p4 = cnt_welfare_VoC %>% 
  ggplot(aes(x=Welfare, y=count, fill=VoC_HS)) +
  geom_bar(stat="identity", position="dodge") +
  #facet_wrap(typology ~ ., scales="free") + 
  scale_fill_discrete(name="") +
  theme_bw() +
  theme(legend.position = "bottom")

pp1 = ggarrange(p1, p3, common.legend = T, legend = "bottom")
ggarrange(pp1, p4, nrow=2)



###

PolicyRegime_data %>% 
  select_at(vars(starts_with("mp_Cluster5"), Welfare1990_EA, VoC_HS)) %>%
  # mutate(Welfare1990_EA_comb = fct_recode(Welfare1990_EA, 
  #                                         "Social Democratic/\nConservative" = "Social Democratic",
  #                                          "Social Democratic/\nConservative" = "Conservative")) %>% 
  mutate(VoC_HS = ifelse(VoC_HS == "Mixed", NA, VoC_HS)) %>%
  # select(-Welfare1990_EA) %>% 
  pivot_longer(cols = -c(VoC_HS, Welfare1990_EA)) %>% 
  mutate(name = gsub("mp_Cluster4_","",name),
         name = gsub("mp_Cluster5_","",name)) %>% 
  na.omit() %>% 
  ggplot(aes(x=Welfare1990_EA, y=value, fill=name)) +
  geom_boxplot() +
  facet_wrap(VoC_HS ~ ., scales="free")  +
  xlab("") +
  ylab("Membership Probability") +
  theme_bw() +
  scale_fill_discrete(name ="") +
  theme(legend.position = "bottom")
  


### Cultural Values ####

p1 = PolicyRegime_data %>% 
  select_at(vars( mastery_cult_caus, Welfare1990_EA)) %>%
  pivot_longer(cols=-c(Welfare1990_EA)) %>% 
  mutate(Welfare1990_EA = fct_relevel(Welfare1990_EA, "Liberal", "Conservative")) %>% 
  na.omit() %>% 
  mutate(name = gsub("_cult_caus", " orientation", name)) %>% 
  ggplot(aes(x=Welfare1990_EA, y=value)) +
  geom_boxplot(fill="lightgrey") +
  xlab("") +
  ylab("Mastery Orientation") +
  theme_bw() +
  scale_fill_discrete(name ="") +
  theme(legend.position = "bottom")


p2 = PolicyRegime_data %>% 
  select_at(vars( mastery_cult_caus, VoC_HS)) %>%
  pivot_longer(cols=-c(VoC_HS)) %>%
  mutate(VoC_HS = fct_relevel(VoC_HS, "LME", "CME")) %>% 
  na.omit() %>% 
  mutate(name = gsub("_cult_caus", " orientation", name)) %>% 
  ggplot(aes(x=VoC_HS, y=value)) +
  geom_boxplot(fill="lightgrey") +
  xlab("") +
  ylab("Mastery Orientation") +
  theme_bw() +
  scale_fill_discrete(name ="") +
  theme(legend.position = "bottom")

ggarrange(p1, p2, common.legend = T, legend="bottom")
