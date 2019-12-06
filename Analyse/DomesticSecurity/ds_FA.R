# Domestic Security Factor Analysis

source("Analyse/DomesticSecurity/ds_imputation.R")


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

fa_data_ds_frame_mice_inv = fa_data_ds_frame_mice %>% 
  mutate_at(vars(ends_with("_ds"), -matches("order_safety_gdp")), inverser)

### KOM-Test
dim(fa_data_ds_frame_mice_inv)

KMO(fa_data_ds_frame_mice_inv %>% 
      select_at(vars(ends_with("ds")))) 
cor(fa_data_ds_frame_mice_inv %>% 
      select_at(vars(ends_with("ds"))) , use="pairwise")

### Factor Analysis
par(mfrow=c(1,1))
fa.parallel(fa_data_ds_frame_mice_inv%>% 
              select_at(vars(ends_with("ds"))), fm="mle", n.iter=100, quant=0.95, fa="fa",
            use="pairwise.complete.obs",
            main="Parallel Analysis Scree Plots for Domestic Securirty Performance")

vss(fa_data_ds_frame_mice_inv%>% 
      select_at(vars(ends_with("ds"))), fm="mle", rotate="none")$map %>% 
  round(.,3)


# no simple structure
fa_oecd_2_ds = fa(fa_data_ds_frame_mice_inv%>% 
                   select_at(vars(ends_with("ds"))), 2, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_2_ds, cut=0)
fa_oecd_2_ds
fa_oecd_2_ds$loadings


fa_oecd_ds = fa(fa_data_ds_frame_mice_inv %>% 
                   select_at(vars(ends_with("ds"))), 1, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_ds, cut=0, main= "Factor Solution for Domestic Securirty Performance")
fa_oecd_ds


# Cronbachs Alpha
alpha(fa_data_ds_frame_mice_inv %>% 
        select_at(vars(ends_with("ds"))), check.keys=TRUE)

# Two Factor Solution
# alpha(fa_data_ds_frame_mice_inv %>% 
#         select_at(vars(greenhouse_oecd_num_env, co2_oecd_num_env, waste_oecd_num_env)))
# alpha(fa_data_ds_frame_mice_inv %>% 
#         select_at(vars(sulphur_oecd_num_env, water_oecd_num_env, nitrogen_oecd_num_env)))


## Calculate Factor Scores
imputed_ds_inv = imputed_ds %>% 
  mutate_at(vars(ends_with("_ds"), -matches("order_safety_gdp")), inverser)

ds_scores = fa_data_ds_frame_mice_inv %>% 
  select(country_text_id, year) %>% 
  bind_cols(produce_fa_scores(imputed_ds_inv, 10, 1, "ds"))


###

samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")


ds_scores %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_score = mean(value, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_score, col=country_text_id)) +
  geom_line(size=1) +
  theme_bw()


write.csv(ds_scores, file="Datasets/performance_data/ImputetDatasets/ds_scores.csv", row.names = F, fileEncoding ="UTF-8")
write.csv(imputed_ds_inv, file="Datasets/performance_data/ImputetDatasets/imputed_ds.csv", row.names = F, fileEncoding ="UTF-8")
