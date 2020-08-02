# Environment Factor Analysis

source("Analyse/PerformanceAreas/Environment/env_imputation.R")
source("Setup/Sig_Tables.R")

### Inverse Scores
fa_data_oecd_frame_mice_inv = imputed_env_vars %>% 
  select_at(vars(imp, country_text_id, year, ends_with("_env"))) %>% 
  mutate_at(vars(ends_with("_env")), inverser) %>% 
  select(-imp) %>% 
  group_by(country_text_id, year) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  na.omit() 


### KOM-Test
dim(fa_data_oecd_frame_mice_inv)

KMO(fa_data_oecd_frame_mice_inv %>% 
      select_at(vars(ends_with("env")))) 

KMO(fa_data_oecd_frame_mice_inv %>% 
      # these variables are already included in GHG
      select_at(vars(ends_with("env"), -CO2_ugdp_oecd_num_env, -CH4_ugdp_oecd_num_env,-N2O_ugdp_oecd_num_env, -NMVOC_ugdp_oecd_num_env)))

corrplot(cor(fa_data_oecd_frame_mice_inv %>% 
               select_at(vars(ends_with("env"), -CO2_ugdp_oecd_num_env, -CH4_ugdp_oecd_num_env,-N2O_ugdp_oecd_num_env, -NMVOC_ugdp_oecd_num_env)) %>% 
               rename_all(funs(gsub("_num_env","",.)))  , use="pairwise"), method="number")

KMO_table(KMO(fa_data_oecd_frame_mice_inv %>% 
                select_at(vars(ends_with("env"), -CO2_ugdp_oecd_num_env, -CH4_ugdp_oecd_num_env,-N2O_ugdp_oecd_num_env, -NMVOC_ugdp_oecd_num_env))))

### Factor Analysis
fa_env_data = fa_data_oecd_frame_mice_inv %>% 
  select_at(vars(ends_with("env"), -CO2_ugdp_oecd_num_env, -CH4_ugdp_oecd_num_env,-N2O_ugdp_oecd_num_env, -NMVOC_ugdp_oecd_num_env)) %>% 
  rename_all(funs(gsub("_num_env","",.)))


names(fa_env_data)
paran(na.omit(fa_env_data), iterations=100, graph=T, cfa=T, centile=95)
fa.parallel(fa_env_data, fm="mle", n.iter=100, quant=0.95, fa="fa",
            use="pairwise.complete.obs",
            main="Parallel Analysis Scree Plots for Environmental Performance")

paran_ggplot(fa.parallel(fa_env_data, fm="mle", n.iter=100, quant=0.95, fa="fa",
                         use="pairwise.complete.obs",
                         main="Parallel Analysis Scree Plots for Environmental Performance"))

vss(fa_env_data, fm="mle", rotate="none")$map %>% 
  round(.,3)
vss(fa_env_data, fm="mle", rotate="none") 

# Factor Analysis
fa_oecd_env = fa(fa_env_data, 2, rotate="oblimin", missing=F, fm="mle", scores = "Bartlett")
fa_oecd_env


round(fa_oecd_env$residual, 2)

fa.diagram(fa_oecd_env, cut=0, main= "Factor Solution for Environmental Performance")
fa_oecd_env$loadings

fa_table(fa_oecd_env)


# model_plot = semPaths( semPlotModel(factanal(fa_env_data, 2, rotation = "promax")), 
#                        intercepts=F, 
#                        residuals=F, 
#                        whatLabels="par",
#                        curvePivot = TRUE,
#                        sizeMan = 12,
#                        sizeLat= 15,
#                        nCharNodes = 10, edge.label.cex = 1.2,
#                        DoNotPlot=T)
# 
# model_plot$graphAttributes$Nodes$labels["Factor1"] = "air_env"
# model_plot$graphAttributes$Nodes$labels["Factor2"] = "abstraction_env"
# 
# plot(model_plot) %>% 