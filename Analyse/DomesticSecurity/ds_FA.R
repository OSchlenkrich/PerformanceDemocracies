# Domestic Security Factor Analysis

source("Analyse/DomesticSecurity/ds_imputation.R")
source("Setup/Sig_Tables.R")


### Inverse Scores

fa_data_ds_frame_mice_inv = imputed_ds_vars %>% 
  select_at(vars(country_text_id, year, ends_with("ds"))) %>% 
  mutate_at(vars(hom_rate_unodc_num_ds), inverser) %>% 
  group_by(country_text_id, year) %>% 
  summarise_all(mean) %>% 
  ungroup() 


### KOM-Test
dim(fa_data_ds_frame_mice_inv)

KMO(fa_data_ds_frame_mice_inv %>% 
      select_at(vars(ends_with("ds")))) 

KMO(fa_data_ds_frame_mice_inv %>% 
      select_at(vars(ends_with("ds"))) %>% 
      select(-order_safety_gdp_perc_oecd_num_ds, -crime_rate_unodc_num_ds)) 

corrplot(cor(fa_data_ds_frame_mice_inv %>% 
               select_at(vars(ends_with("ds"))) , use="pairwise"), method="number")


### Factor Analysis
fa_ds_data = fa_data_ds_frame_mice_inv %>% 
  select_at(vars(ends_with("ds"), -order_safety_gdp_perc_oecd_num_ds,
                 -crime_rate_unodc_num_ds, -orgacrime_gcs_num_ds, -burg_rate_unodc_num_ds))

par(mfrow=c(1,1))
fa.parallel(fa_ds_data, fm="mle", n.iter=100, quant=0.95, fa="fa",
            use="pairwise.complete.obs",
            main="Parallel Analysis Scree Plots for Domestic Securirty Performance")

vss(fa_ds_data, fm="mle", rotate="none")$map %>% 
  round(.,3)


# 1 Factor

fa_oecd_ds = fa(fa_ds_data, 1, rotate="oblimin", missing=F, fm="mle", scores="Bartlett")
fa.diagram(fa_oecd_ds, cut=0, main= "Factor Solution for Domestic Security Performance")
fa_oecd_ds

# pattern matrix
fa_oecd_ds$loadings
# structure matrix
fa_oecd_ds$Structure

fa_table(fa_oecd_ds)


# Reliability
omega(as.matrix(fa_ds_data), nfactors=1, fm="mle", option="second")
alpha(as.matrix(fa_ds_data))

## Calculate Factor Scores
performance_ds = fa_data_ds_frame_mice_inv %>% 
  mutate(pubsafe_ds = scale(fa_oecd_ds$scores[,1])[,1]) 


###

samples = c("CHE", "USA", "LUX", "IND", "FIN", "DNK")
samples = c("DEU", "USA", "BEL", "SWE")
samples = c("BRA", "RUS", "CHE", "IND", "DEU", "COL")
samples = c("FIN", "DNK", "ISL", "DEU", "POL")
samples = c("DEU", "USA", "POL", "IND")

performance_ds %>% 
  select_at(vars(country_text_id, year, pubsafe_ds)) %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) + 
  geom_line(size=1) +
  #geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() +
  scale_y_continuous(name="Economic Performance")  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) +
  facet_wrap(variable~.)


write.csv(ds_scores, file="Datasets/performance_data/ImputetDatasets/ds_scores.csv", row.names = F, fileEncoding ="UTF-8")
write.csv(imputed_ds_inv, file="Datasets/performance_data/ImputetDatasets/imputed_ds.csv", row.names = F, fileEncoding ="UTF-8")
