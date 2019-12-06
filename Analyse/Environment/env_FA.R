# Environment Factor Analysis

source("Analyse/Environment/env_imputation.R")


produce_fa_scores = function(mice_data, nr_imputations, nr_factors) {
  
  nr_rows = mice_data %>% 
    filter(.imp==1) %>% 
    dim()
  
  scores_data = data.frame(matrix(NA, nr_rows[1], nr_imputations)) %>% 
    rename_all(funs(sub("X", "imp_", .)))
  
  
  for (i in 1:nr_imputations) {
    stack_data = mice_data %>% 
      filter(.imp==i) %>% 
      select_at(vars(ends_with("env")))
    
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
  mutate_at(vars(ends_with("_env")), inverser)

### KOM-Test
dim(fa_data_oecd_frame_mice_inv)

KMO(fa_data_oecd_frame_mice_inv %>% 
      select_at(vars(ends_with("env")))) 
cor(fa_data_oecd_frame_mice_inv %>% 
      select_at(vars(ends_with("env"))) , use="pairwise")

### Factor Analysis

fa.parallel(fa_data_oecd_frame_mice_inv%>% 
              select_at(vars(ends_with("env"))), fm="mle", n.iter=100, quant=0.95, fa="fa",
            use="pairwise.complete.obs",
            main="Parallel Analysis Scree Plots for Environmental Performance")

vss(fa_data_oecd_frame_mice_inv%>% 
      select_at(vars(ends_with("env"))), fm="mle", rotate="none")$map %>% 
  round(.,3)


# no simple structure
fa_oecd_env = fa(fa_data_oecd_frame_mice_inv %>% 
                   select_at(vars(ends_with("env"))), 2, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_env, cut=0)
fa_oecd_env$loadings


fa_oecd_env = fa(fa_data_oecd_frame_mice_inv %>% 
                   select_at(vars(ends_with("env"))), 1, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_env, cut=0, main= "Factor Solution for Environmental Performance")
fa_oecd_env


# Cronbachs Alpha
alpha(fa_data_oecd_frame_mice_inv %>% 
        select_at(vars(ends_with("env"))))

# Two Factor Solution
# alpha(fa_data_oecd_frame_mice_inv %>% 
#         select_at(vars(greenhouse_oecd_num_env, co2_oecd_num_env, waste_oecd_num_env)))
# alpha(fa_data_oecd_frame_mice_inv %>% 
#         select_at(vars(sulphur_oecd_num_env, water_oecd_num_env, nitrogen_oecd_num_env)))


## Calculate Factor Scores
imputed_env_inv = imputed_env  %>% 
  mutate_at(vars(ends_with("_env")), inverser)
  
env_scores = fa_data_oecd_frame_mice_inv %>% 
  select(country_text_id, year) %>% 
  bind_cols(produce_fa_scores(imputed_env_inv, 10, 1))


###

samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")


env_scores %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_score = mean(value, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_score, col=country_text_id)) +
  geom_line(size=1) +
  theme_bw()


write.csv(env_scores, file="Datasets/performance_data/ImputetDatasets/env_scores.csv", row.names = F, fileEncoding ="UTF-8")
write.csv(imputed_env_inv, file="Datasets/performance_data/ImputetDatasets/imputed_env.csv", row.names = F, fileEncoding ="UTF-8")

