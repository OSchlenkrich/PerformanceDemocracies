# Social Factor Analysis

source("Analyse/PerformanceAreas/Social/soc_imputation.R")
source("Setup/Sig_Tables.R")

### Inverse Scores
fa_data_soc_frame_mice_inv = imputed_soc_vars %>% 
  select_at(vars(imp, country_text_id, year, ends_with("_GI_num_soc"), ends_with("_lis_num_soc"), 
                 ends_with("_wdi_num_soc"), ends_with("_vdem_num_soc"), ends_with("_is_na"))) %>% 
  mutate_at(vars(poverty9010_lis_num_soc, poverty8020_lis_num_soc, gini_lis_num_soc, gini_wdi_num_soc), inverser) %>% 
  # filter(imp  == 10) %>% 
  select(-imp) %>% 
  group_by(country_text_id, year) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  na.omit() %>% 
  select(-gini_lis_num_soc )


### KOM-Test: Remove Pension_t_GI_num_soc
dim(fa_data_soc_frame_mice_inv)

KMO(fa_data_soc_frame_mice_inv %>% 
      select_at(vars(ends_with("soc"))))

corrplot(cor(fa_data_soc_frame_mice_inv %>% 
               select_at(vars(ends_with("soc"))) , use="pairwise"), method="number")



### Factor Analysis
fa_dataset = fa_data_soc_frame_mice_inv %>% 
  select_at(vars(ends_with("soc"), -v2dlunivl_vdem_num_soc  ))

par(mfrow=c(1,1))
paran(na.omit(fa_dataset), iterations=0, graph=T, cfa=T, centile=95)
fa.parallel(fa_dataset, fm="ml")

vss(fa_dataset, fm="mle", rotate="none")
vss(fa_dataset, fm="mle", rotate="none")$map %>% 
  round(.,3)


# Factor Solution
fa_oecd_2_soc = fa(fa_dataset, 2, rotate="oblimin", missing=F, fm="mle")
fa.diagram(fa_oecd_2_soc, cut=0)
fa_oecd_2_soc
fa_oecd_2_soc$loadings


fa_oecd_1_soc = fa(fa_dataset, 1, rotate="oblimin", missing=F, fm="mle")
fa.diagram(fa_oecd_1_soc, cut=0, main= "Factor Solution for Social Performance")
fa_oecd_1_soc

fa_table(fa_oecd_2_soc)
fa_table(fa_oecd_1_soc)

# Reliability
omega(as.matrix(fa_dataset), nfactors=2, fm="ml")
# alpha(as.matrix(fa_dataset), check.keys=TRUE, n.iter=10)


## Calculate Factor Scores
performance_soc = fa_data_soc_frame_mice_inv %>% 
  mutate(eco_inequal_soc = scale(fa_oecd_2_soc$scores[,1])[,1],
         soc_inequal_soc = scale(fa_oecd_2_soc$scores[,2])[,1]) 



###

samples = c("DEU", "USA", "SWE", "IND", "FIN", "DNK")
samples = c("DEU", "CHE", "BEL", "SWE", "GRC")
samples = c("BRA", "RUS", "CHE", "IND", "DEU")
samples = c("ALB", "BRA", "DEU", "SWE", "USA")

performance_soc %>% 
  select_at(vars(country_text_id, year, eco_inequal_soc, soc_inequal_soc )) %>% 
  filter(country_text_id %in% samples) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) + 
  geom_line(size=1) +
  #geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  theme_bw() +
  scale_y_continuous(name="Economic Performance")  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) +
  facet_wrap(variable~.)

write.csv(performance_soc, file="Datasets/performance_data/ImputedDatasets/performance_soc.csv", row.names = F, fileEncoding ="UTF-8")

