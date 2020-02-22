# Economic Factor Analysis

source("Analyse/Economy/eco_wdi_imputation.R")
source("Setup/Sig_Tables.R")

### Inverse Scores
fa_data_wdi_frame_mice_inv = imputed_wdi_eco_vars %>% 
  select_at(vars(imp, country_text_id, year, ends_with("_wdi_num_eco"), ends_with("_imf_num_eco"), ends_with("_pwt_num_eco"))) %>% 
  mutate_at(vars("inflation_imf_num_eco", "unemployment_pr_imf_num_eco", "Balance_wdi_num_eco"), inverser) %>% 
  #filter(imp  == 4) %>% 
  select(-imp) %>% 
  group_by(country_text_id, year) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  na.omit() 

names(imputed_wdi_eco_vars)
### KOM-Test
dim(fa_data_wdi_frame_mice_inv)

KMO(fa_data_wdi_frame_mice_inv %>% 
      select_at(vars(ends_with("eco"))))
KMO(fa_data_wdi_frame_mice_inv %>% 
      select_at(vars(ends_with("eco"), -investment_wdi_num_eco, -Balance_wdi_num_eco))) 
corrplot(cor(fa_data_wdi_frame_mice_inv %>% 
               select_at(vars(ends_with("eco"))) , use="pairwise"), method="number")


### Factor Analysis

fa_eco_wdi_data = fa_data_wdi_frame_mice_inv %>% 
  select_at(vars(ends_with("eco"), -investment_wdi_num_eco, -Balance_wdi_num_eco))

par(mfrow=c(1,1))
paran(na.omit(fa_eco_wdi_data), iterations=100, graph=T, cfa=T, centile=95)
fa.parallel(fa_eco_wdi_data, fm="ml")
vss(fa_eco_wdi_data, fm="mle", rotate="none")$map %>% 
  round(.,3)
vss(fa_eco_wdi_data, fm="mle", rotate="none")$vss.stats$SRMR %>% 
  round(.,3)


# Exploratory Factor Analysis
fa_wdi_eco = fa(fa_eco_wdi_data, 2, rotate="oblimin", missing=F, fm="mle", scores="Bartlett")
fa.diagram(fa_wdi_eco, cut=0)
# pattern matrix
fa_wdi_eco$loadings
# structure matrix
fa_wdi_eco$Structure

fa_table(fa_wdi_eco)


# new_Data = fa_eco_wdi_data %>%
#   summarise_all(funs(quantile(., prob=0.9))) %>%
#   select(-unemployment_pr_imf_num_eco) %>%
#   cbind(unemployment_pr_imf_num_eco = seq(-2, 2, length.out = 10))
# new_Data = new_Data[names(fa_eco_wdi_data)]
# # 
# # 
# new_Data = fa_data_wdi_frame_mice_inv %>%
#   filter(year >= 2008, country_text_id=="GRC") %>%
#   select_at(vars(ends_with("eco"), -investment_wdi_num_eco, -Balance_wdi_num_eco)) %>% 
#   mutate(unemployment_pr_imf_num_eco = seq(2, -2, length.out = 10))
# # 
# predict(fa_wdi_eco, data=new_Data, old.data=fa_eco_wdi_data)


# Reliability
omega(as.matrix(fa_eco_wdi_data), nfactors=2, fm="mle", option="second")

## Calculate Factor Scores
performance_wdi_eco = fa_data_wdi_frame_mice_inv %>% 
  mutate(wealth_eco = scale(fa_wdi_eco$scores[,1])[,1],
         productivity_eco = scale(fa_wdi_eco$scores[,2])[,1])


###

samples = c("DEU", "USA", "SWE", "IND", "FIN", "DNK")
samples = c("DEU", "CHE", "BEL", "SWE")
samples = c("BRA", "RUS", "CHE", "IND", "DEU")
samples = c("GRC")

performance_wdi_eco %>% 
  select_at(vars(country_text_id, year, wealth_eco, productivity_eco)) %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) + 
  geom_line(size=1) +
  #geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() +
  scale_y_continuous(name="Economic Performance")  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) +
  facet_wrap(variable~.)

write.csv(performance_wdi_eco, file="Datasets/performance_data/ImputedDatasets/performance_eco.csv", row.names = F, fileEncoding ="UTF-8")

