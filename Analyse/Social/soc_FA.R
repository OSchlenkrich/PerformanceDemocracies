# Social Factor Analysis

source("Analyse/Social/soc_imputation.R")


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


### Inverse Scores

fa_data_soc_frame_mice_inv = fa_data_soc_frame_mice %>% 
  mutate_at(vars(matches("_lis_"), matches("_wdi_")), inverser)

### KOM-Test: Remove Pension_t_GI_num_soc
dim(fa_data_soc_frame_mice_inv)

KMO(fa_data_soc_frame_mice_inv %>% 
      select_at(vars(ends_with("soc"), -gini_wdi_num_soc, -Pension_t_GI_num_soc))) 

corrplot(cor(fa_data_soc_frame_mice_inv %>% 
      select_at(vars(ends_with("soc"))) , use="pairwise"))



fa_dataset = fa_data_soc_frame_mice_inv %>% 
  select_at(vars(ends_with("soc"), -gini_wdi_num_soc, -Pension_t_GI_num_soc))

### Factor Analysis
par(mfrow=c(1,1))
paran(na.omit(fa_dataset), iterations=0, graph=T, cfa=T, centile=95)


vss(fa_dataset, fm="mle", rotate="none")$map %>% 
  round(.,3)


# no simple structure
fa_oecd_3_soc = fa(fa_dataset, 3, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_3_soc, cut=0)
fa_oecd_3_soc
fa_oecd_3_soc$loadings

fa_oecd_2_soc = fa(fa_dataset, 2, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_2_soc, cut=0)
fa_oecd_2_soc
fa_oecd_2_soc$loadings


fa_oecd_soc = fa(fa_dataset, 1, rotate="promax", missing=F, fm="mle")
fa.diagram(fa_oecd_soc, cut=0, main= "Factor Solution for Social Performance")
fa_oecd_soc


# Cronbachs Alpha
alpha(fa_dataset, check.keys=TRUE, n.iter=10)


# Two Factor Solution
# alpha(fa_data_ds_frame_mice_inv %>% 
#         select_at(vars(greenhouse_oecd_num_env, co2_oecd_num_env, waste_oecd_num_env)))
# alpha(fa_data_ds_frame_mice_inv %>% 
#         select_at(vars(sulphur_oecd_num_env, water_oecd_num_env, nitrogen_oecd_num_env)))


## Calculate Factor Scores
imputed_soc_inv = imputed_soc %>% 
  mutate_at(vars(matches("_lis_"), matches("_wdi_")), inverser)

soc_scores = fa_data_soc_frame_mice_inv %>% 
  select(country_text_id, year) %>% 
  bind_cols(produce_fa_scores(imputed_soc_inv%>% 
                                select_at(vars(-gini_wdi_num_soc, -Pension_t_GI_num_soc)), 10, 1, "soc"))


###

samples = c("CAN","DEU", "USA", "SWE", "IND", "FIN", "DNK")


soc_scores %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_score = mean(value, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_score, col=country_text_id)) +
  geom_line(size=1) +
  theme_bw()


write.csv(soc_scores, file="Datasets/performance_data/ImputetDatasets/soc_scores.csv", row.names = F, fileEncoding ="UTF-8")
write.csv(imputed_soc_inv, file="Datasets/performance_data/ImputetDatasets/imputed_soc.csv", row.names = F, fileEncoding ="UTF-8")

