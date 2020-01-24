# Economic Factor Analysis

source("Analyse/Economy/eco_imputation.R")


produce_fa_scores = function(mice_data, nr_imputations, nr_factors, variable_ending) {
  
  nr_rows = mice_data %>% 
    filter(.imp==1) %>% 
    dim()
  
  scores_data = data.frame(matrix(NA, nr_rows[1], nr_imputations)) %>% 
    rename_all(funs(sub("X", "imp_", .)))
  
  
  for (i in 1:nr_imputations) {
    stack_data = mice_data %>% 
      filter(.imp==i) %>% 
      select_at(vars(ends_with(variable_ending)))
    
    # Extract Bartlett Factor Scores
    fa_stack = fa(stack_data, nr_factors, rotate="promax", missing=F, fm="mle", scores="Bartlett")
    scores_data[,i] = as.numeric(fa_stack$scores)
  }
  return(scores_data)
}


# produce_fa_scores_mi = function(mice_data, nr_immputations, nr_factors) {
#   scores_data = data.frame(matrix(NA, dim(my_imputet_data[[1]])[1], nr_immputations)) %>% 
#     rename_all(funs(sub("X", "imp_", .)))
#   
#   for (i in 1:nr_immputations) {
#     stack_data = my_imputet_data[[i]] %>% 
#       select_at(vars(ends_with("env"), -starts_with("missing"))) %>% 
#       mutate_all(inverser)
#     
#     
#     fa_stack = fa(stack_data, nr_factors, rotate="promax", missing=F, fm="mle")
#     scores_data[,i] = as.numeric(fa_stack$scores)
#   }
#   return(scores_data)
# }


### Inverse Scores

fa_data_oecd_frame_mice_inv = fa_data_oecd_frame_mice %>% 
  mutate_at(vars("Inflation_oecd_num_eco", "Interest_oecd_num_eco"), inverser)


fa_data_oecd_frame_mice_inv = imputed_eco %>% 
  select_at(vars(country_text_id, year_0, ends_with("eco"))) %>% 
  mutate_at(vars("Inflation_oecd_num_eco", "Interest_oecd_num_eco"), inverser) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise_all(mean) %>% 
  ungroup()

### KOM-Test
dim(fa_data_oecd_frame_mice_inv)

KMO(fa_data_oecd_frame_mice_inv %>% 
      select_at(vars(ends_with("eco")))) 
KMO(fa_data_oecd_frame_mice_inv %>% 
      select_at(vars(ends_with("eco"), -"Unemployment_pr_oecd_num_eco", -"Invest_oecd_num_eco"))) 
corrplot(cor(fa_data_oecd_frame_mice_inv %>% 
      select_at(vars(ends_with("eco"))) , use="pairwise"), method="number")


### Factor Analysis


fa_eco_data = fa_data_oecd_frame_mice_inv %>% 
  select_at(vars(ends_with("eco"), -"Unemployment_pr_oecd_num_eco", -"GNI_capita_oecd_num_eco", -"Invest_oecd_num_eco"))

# fa.parallel(fa_eco_data, fm="mle", n.iter=1000, quant=0.95, fa="fa",
#             use="pairwise.complete.obs",
#             main="Parallel Analysis Scree Plots for Environmental Performance")

par(mfrow=c(1,1))
paran(na.omit(fa_eco_data), iterations=0, graph=T, cfa=T, centile=95)

vss(fa_eco_data, fm="mle", rotate="none")$map %>% 
  round(.,3)


# 1 Factor
fa_oecd_eco = fa(fa_eco_data, 1, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_eco, cut=0)
fa_oecd_eco$loadings


# Cronbachs Alpha
alpha(as.matrix(fa_eco_data))


## Calculate Factor Scores
imputed_eco_inv = imputed_eco  %>% 
  mutate_at(vars("Inflation_oecd_num_eco", "Interest_oecd_num_eco"), inverser) 

# Testplot

sample = c("AUS", "IND", "DEU", "SWE")
imputed_eco_inv %>% 
  select_at(vars(country_text_id, year_0, ends_with("_eco"))) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise_if(is.numeric, funs(mean = mean(.))) %>% 
  melt(id.vars=c("country_text_id", "year_0")) %>% 
  ggplot(aes(x=year_0, y=value, col=country_text_id)) + 
  geom_line() +
  #geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  facet_wrap(variable ~ .)

##

eco_scores = fa_data_oecd_frame_mice_inv %>% 
  select(country_text_id, year) %>% 
  bind_cols(produce_fa_scores(imputed_eco_inv %>% 
                                select(-"Unemployment_pr_oecd_num_eco", -"Invest_oecd_num_eco", -"Balance_oecd_num_eco", -"GNI_capita_oecd_num_eco"), 10, 1, "eco"))


fa_stack = fa(fa_eco_data, 1, rotate="promax", missing=F, fm="mle", scores="Bartlett")
fa_data_oecd_frame_mice_inv$scores = as.numeric(fa_stack$scores)


fa_oecd_eco_test = fa(imputed_eco_inv %>% 
                        filter(.imp==1) %>% 
                        select_at(vars(ends_with("eco"))) %>% 
                        select(-"Unemployment_pr_oecd_num_eco", -"Invest_oecd_num_eco", -"Balance_oecd_num_eco", -"GNI_capita_oecd_num_eco"), 1,
                      rotate="promax", missing=F, fm="mle")


###

samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")
samples = c("IND", "USA", "DNK")

fa_data_oecd_frame_mice_inv %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year_0")) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise(mean_score = mean(value, na.rm=T)) %>% 
  ggplot(aes(x=year_0, y=mean_score, col=country_text_id)) +
  geom_line(size=1) +
  theme_bw()


write.csv(eco_scores, file="Datasets/performance_data/ImputetDatasets/eco_scores.csv", row.names = F, fileEncoding ="UTF-8")
write.csv(imputed_eco_inv, file="Datasets/performance_data/ImputetDatasets/imputed_eco.csv", row.names = F, fileEncoding ="UTF-8")

