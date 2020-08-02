# Confidence Factor Analysis

source("Analyse/PerformanceAreas/Confidence/conf_variables_v3.R")
source("Setup/Sig_Tables.R")


### Inverse Scores

fa_data_conf_inv = fa_data_conf_frame %>%
  mutate_at(vars(starts_with("ord")), funs(. - 1)) %>% 
  mutate_at(vars(starts_with("conf")), funs(max(., na.rm=T) - .)) %>% 
  select_at(vars(id, survey, country, country_text_id, weights, year_study, starts_with("conf")))


### KOM-Test
dim(fa_data_conf_inv)

KMO(fa_data_conf_inv %>% 
      select_at(vars(starts_with("conf")))) 

KMO_table(KMO(fa_data_conf_inv %>% 
                select_at(vars(starts_with("conf")))) )


corrplot(cor(fa_data_conf_inv %>% 
           select_at(vars(starts_with("conf")))  %>% 
             rename_all(funs(gsub("_ord","",.))), use="pairwise", method="spearman"), method="number")

### Create Dataset

# include confidence in judiciary
fa_dataset_jud = fa_data_conf_inv %>% 
  select_at(vars(starts_with("conf"))) %>% 
  rename_all(funs(gsub("_ord_ivs", "", .))) %>% 
  as.data.frame()
fa_dataset_jud[] <- lapply(fa_dataset_jud, function(x) { attributes(x) <- NULL; x }) 

paran_ggplot(fa.parallel(fa_dataset_jud, fm="mle", n.iter=100, quant=0.95, fa="fa",
                         use="pairwise.complete.obs"))

vss(fa_dataset_jud, fm="mle", rotate="none")

fa_solution_jud = umxEFA(fa_dataset_jud, factors = 2, summary=T, rotation="promax")

dim(loadings(fa_solution_jud))

fa_table_umx(fa_solution_jud, RMSEA = 0.072, TLI = 0.97, omega(as.matrix(fa_dataset_jud), nfactors=1, fm="mle", option="second"))
summary(fa_solution_jud)

semPaths( semPlotModel(fa_solution_jud), style="mx", 
          intercepts=F, 
          residuals=F, 
          whatLabels="par", 
          sizeMan = 12,
          sizeLat= 15,
          nCharNodes = 10, edge.label.cex = 1.2)


# exclude confidence in judiciary due to better RMSEA ####
fa_dataset = fa_data_conf_inv %>% 
  select_at(vars(starts_with("conf"), -conf_judiciary_ord_ivs)) %>% 
  rename_all(funs(gsub("_ord_ivs", "", .))) %>% 
  as.data.frame()
fa_dataset[] <- lapply(fa_dataset, function(x) { attributes(x) <- NULL; x }) 

### FA: Extract Factor Number ####
par(mfrow=c(1,1))
paran(na.omit(fa_dataset), iterations=0, graph=T, cfa=T, centile=95)

paran_ggplot(fa.parallel(fa_dataset, fm="mle", n.iter=100, quant=0.95, fa="fa",
                         use="pairwise.complete.obs"))

vss(fa_dataset, fm="mle", rotate="none")

# VSS: 1 Factor
# Parallel: 1 Factors

# Factor Analysis
# 1 factor solution
fa_solution = umxEFA(fa_dataset, factors = "confidence", summary=T)

loadings(fa_solution)

fa_table_umx(fa_solution, RMSEA = 0.047, TLI = 0.97, omega(as.matrix(fa_dataset), nfactors=1, fm="mle", option="second"))


semPaths( semPlotModel(fa_solution), style="mx", 
         intercepts=F, 
         residuals=F, 
         whatLabels="par", 
         sizeMan = 12,
         sizeLat= 15,
         nCharNodes = 10, edge.label.cex = 1.2)

# Cronbachs Alpha
alpha(fa_dataset, check.keys=TRUE, n.iter=10)
omega(as.matrix(fa_dataset), nfactors=1, fm="mle", option="second")


## Calculate Factor Scores
conf_FIML_scores = umxFactorScores(fa_solution, type = "WeightedML", minManifests = 2)


# Create Final Dataset

conf_scores = fa_data_conf_inv %>%
  #dplyr::select(id, survey, country_text_id, weights, year_study) %>% 
  bind_cols(conf_FIML_scores) %>% 
  rename(conf_index = confidence)

performance_pc = conf_scores %>% 
  dplyr::select(country_text_id, weights, year = year_study, conf_index) %>%
  group_by(country_text_id, year) %>% 
  summarise(conf_pc = weighted.mean( conf_index, weights, na.rm=T)) %>% 
  ungroup() %>%  
  left_join(dmx_trade_cluster %>%  dplyr::select(-country), 
            by=c("country_text_id", "year"))


samples = c("GBR","NZL", "SWE", "USA", "DEU", "FRA", "ZAF")

performance_pc %>% 
  filter(country_text_id %in% samples) %>% 
  select_at(vars(country_text_id, year, matches("conf_pc"))) %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  ggplot(aes(x=year, y=value, col=country_text_id)) +
  geom_line(size=1) +
  xlim(1990, 2020) +
  #ylim(0,100) +
  facet_wrap(variable ~ ., scales="free_y") +
  theme_bw() 
###
write.csv(conf_scores, file="Datasets/performance_data/ImputedDatasets/conf_scores.csv", row.names = F, fileEncoding ="UTF-8")
write.csv(performance_pc, file="Datasets/performance_data/ImputedDatasets/performance_pc.csv", row.names = F, fileEncoding ="UTF-8")

