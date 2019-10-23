# Confidence Factor Analysis

source("Analyse/Confidence/conf_variables_v2.R")



### Inverse Scores

fa_data_conf_inv = fa_data_conf_frame %>%
  mutate_at(vars(starts_with("ord")), funs(. - 1)) %>% 
  mutate_at(vars(starts_with("conf")), funs(max(., na.rm=T) - .)) %>% 
  select_at(vars(survey, country, country_text_id, weights, year_study, starts_with("conf")))

### KOM-Test: Remove Pension_t_GI_num_soc
dim(fa_data_conf_inv)

KMO(fa_data_conf_inv %>% 
      select_at(vars(starts_with("conf")))) 

corrplot(cor(fa_data_conf_inv %>% 
           select_at(vars(starts_with("conf"))) , use="pairwise", method="spearman"))

### Create Dataset

fa_dataset = fa_data_conf_inv %>% 
  select_at(vars(starts_with("conf"))) %>% 
  as.data.frame()
fa_dataset[] <- lapply(fa_dataset, function(x) { attributes(x) <- NULL; x })

### FA: Extract Factor Number ####
par(mfrow=c(1,1))
paran(na.omit(fa_dataset), iterations=0, graph=T, cfa=T, centile=95)

vss(fa_dataset, fm="mle", rotate="none")$map %>% 
  round(.,3)

# VSS: 1 Factor
# Parallel: 2 Factors

# Factor Analysis
# 2 factor solution: one factor is underidentified
fa_2_solution = umxEFA(fa_dataset, 2, rotation="promax")
loadings(fa_2_solution)
plot(fa_2_solution)

# 1 factor solution
fa_solution = umxEFA(fa_dataset, 1, rotation="promax")
loadings(fa_solution)
plot(fa_solution)

# Cronbachs Alpha
alpha(fa_dataset, check.keys=TRUE, n.iter=10)



## Calculate Factor Scores
conf_FIML_scores = umxFactorScores(fa_solution, type = "WeightedML", minManifests = 2)

conf_scores = fa_data_conf_inv %>%
  dplyr::select(survey, country_text_id, weights, year_study) %>% 
  bind_cols(conf_FIML_scores) %>% 
  rename(conf_index = F1,
         year = year_study)

###

samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")

conf_scores %>% 
  filter(country_text_id %in% samples) %>%
  dplyr::select(-survey, -weights) %>% 
  melt(id.vars = c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  #summarise(mean_score = weighted.mean(x = value, w=weights, na.rm=T)) %>% 
  summarise(mean_score = mean(x = value, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_score, col=country_text_id)) +
  geom_line(size=1) +
  theme_bw()


