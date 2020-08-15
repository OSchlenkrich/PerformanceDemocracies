# VoC and Welfare and Democracy Profiles

library(car)
library(stargazer)
source("Analyse/CreateDatasets.R")
source("Setup/Sig_Tables.R")
source("Setup/Simulation_Dirichlet.R")

# Some Functions:
# transform to DR data (Dirichletreg)
make_DR = function(dataset) {
  dataset$Y_Fec = DirichletReg::DR_data(dataset %>%  
                                          select_at(vars(starts_with("X_"))) %>% 
                                          select(X_Fec, everything()))
  
  dataset$Y_fEc = DirichletReg::DR_data(dataset %>%  
                                          select_at(vars(starts_with("X_"))) %>% 
                                          select(X_fEc, everything()))
  
  dataset$Y_fEC = DirichletReg::DR_data(dataset %>%  
                                          select_at(vars(starts_with("X_"))) %>% 
                                          select(X_fEC, everything()))
  
  dataset$Y_FeC = DirichletReg::DR_data(dataset %>%  
                                          select_at(vars(starts_with("X_"))) %>% 
                                          select(X_FeC, everything()))
  return(dataset)
}

plot_residual_diri = function(model) {
  
  plot_data = data.frame(
    residuals_comp = residuals(model, "composite"),
    residuals_std_1 = residuals(model, "standardized")[,1],
    residuals_std_2 = residuals(model, "standardized")[,2],
    residuals_std_3 = residuals(model, "standardized")[,3],
    residuals_std_4 = residuals(model, "standardized")[,4],
    id = seq(1, dim(model$d)[1], 1),
    fitted = fitted(model),
    country = model$data$country)
  
  names(plot_data)[2:5] = paste("res_std_", colnames(residuals(model, "standardized")), sep="")
  
  p1 = plot_data %>% 
    ggplot(aes(x=id, y=residuals_comp, label=country)) +
    geom_point() +
    geom_text() +
    ylab("Composite Residuals")
  
  p2 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("Fec", ignore.case = F), country)) %>%
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2))  +
    ggtitle("Fec") +
    ylab("Standardized Residuals") +
    xlab("Fitted Values")
  
  
  p3 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("fEc", ignore.case = F), country)) %>% 
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2))  +
    ggtitle("fEc") +
    ylab("Standardized Residuals")  +
    xlab("Fitted Values")
  
  
  p4 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("fEC", ignore.case = F), country)) %>%
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2))  +
    ggtitle("fEC") +
    ylab("Standardized Residuals") +
    xlab("Fitted Values")
  
  
  p5 = plot_data %>% 
    select_at(vars(starts_with("res_std"), starts_with("fitted"), country)) %>% 
    select_at(vars(matches("FeC", ignore.case = F), country)) %>% 
    mutate(country_out = ifelse(.[[1]] > 2 | .[[1]] < -2, as.character(country), NA_character_)) %>% 
    ggplot(aes(x=.[,2], y=.[,1], label=country_out)) +
    geom_point() +
    geom_text_repel() +
    geom_hline(yintercept = c(-2,0,2)) +
    ggtitle("FeC") +
    ylab("Standardized Residuals") +
    xlab("Fitted Values")
  
  lay = rbind(c(1,1),
              c(2,3),
              c(4,5))
  grid.arrange(p1,p2,p3,p4,p5, layout_matrix = lay)
  
}


# Democracy Profiles ####
longdemocracies = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  dplyr::select(country, classification_core) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(nr = n()) %>% 
  filter(nr > 1) %>% 
  pull(country)

# Uncertainty
profiles_agg_cult = dmx_trade_cluster %>% 
  filter(year >= 1950, year <= 2000, country %in% longdemocracies) %>% 
  select_at(vars(country, starts_with("X"))) %>% 
  group_by(country) %>% 
  summarise_all(mean, na.omit=T) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country")

### Welfare Data
Welfare_types = fread("Datasets/WelfareStates.csv", sep=";", encoding="UTF-8") %>% 
  select_at(vars(country, starts_with("cl_"))) %>% 
  #filter(is.na(cl_Esping_Andersen_1990) == F) %>% 
  mutate_at(vars(starts_with("cl")), funs(fct_recode(., 
                                                     "Con" = "Cor",
                                                     NULL = "CEE",
                                                     NULL = "Rad",
                                                     NULL = "Sou",
                                                     NULL = "Eur"))) 
  # melt(id.vars="country") %>% 
  # na.omit() %>% 
  # group_by(country, value) %>% 
  # summarise(count = n()) %>% 
  # pivot_wider(names_from = "value", values_from = "count", values_fill = list(count=0), names_prefix = "cnt_") %>% 
  # ungroup() %>% 
  # filter(country != "Greece", country != "Iceland")

   
# VoC Data
VoC_types = fread("Datasets/VoC_Hoepner_2009.csv", sep=";", encoding="UTF-8") %>% 
  select(country, VoC_HS, Coord_Ind_1990_caus = Coord_Ind_1990) %>% 
  mutate(VoC_caus = ifelse(VoC_HS == "LME", 1, 0),
         VoC_M_caus = ifelse(VoC_HS == "LME", 1, 
                             ifelse(VoC_HS == "Mixed", NA, 0)))

# Create Dataset
structuralP = profiles_agg_cult  %>% 
  left_join(V_dem %>% select(country, COWcode) %>%  distinct() %>%  group_by(country) %>% slice(1), by="country") %>% 
  left_join(Welfare_types, by="country") %>% 
  left_join(VoC_types, by="country")


## VoC and Democracy Profile ####

structuralP_VoC = structuralP %>%
  select_at(vars(country, starts_with("X"), VoC_caus)) %>%
  filter(country != "Switzerland") %>% 
  na.omit() %>% 
  make_DR()

m1_VoC  = DirichReg(Y_Fec ~   VoC_caus | 1,
                structuralP_VoC, "alternative")
m2_VoC  = DirichReg(Y_FeC ~  VoC_caus,
                structuralP_VoC, "alternative")
m3_VoC  = DirichReg(Y_fEc ~  VoC_caus,
                structuralP_VoC, "alternative")
m4_VoC  = DirichReg(Y_fEC ~  VoC_caus,
                structuralP_VoC, "alternative")

summary(m1_VoC)
plot_residual_diri(m1_VoC)
odds_ratio_plot(m1_VoC, m2_VoC, m3_VoC, m4_VoC)

make_table_diri(m1_VoC)


simu_diri_function_expected(m1_VoC,  data.frame(VoC_caus = c(0,1)), draws = 100, selected_variable = "VoC_caus", categorical = T ) 

## Welfare State and Democracy Profile ####
# EA 1990 ---------

structuralP_Welfare = structuralP %>%
  select_at(vars(country, starts_with("X"), cl_Esping_Andersen_1990_caus = cl_Esping_Andersen_1990)) %>% 
  mutate(EA_1990_caus = ifelse(cl_Esping_Andersen_1990_caus == "Lib", 1, 0))  %>% 
  na.omit() %>% 
  make_DR()


m1_EA_1990  = DirichReg(Y_Fec ~  1 + EA_1990_caus  | 1,
                        structuralP_Welfare, "alternative")
m2_EA_1990  = DirichReg(Y_FeC ~  + EA_1990_caus,
                        structuralP_Welfare, "alternative")
m3_EA_1990  = DirichReg(Y_fEc ~  + EA_1990_caus,
                        structuralP_Welfare, "alternative")
m4_EA_1990  = DirichReg(Y_fEC ~  + EA_1990_caus,
                        structuralP_Welfare, "alternative")
summary(m1_EA_1990)
p1_res = plot_residual_diri(m1_EA_1990)
p1 = odds_ratio_plot(m1_EA_1990, m2_EA_1990, m3_EA_1990, m4_EA_1990)
p1
make_table_diri(m1_EA_1990)

simu_diri_function_expected(m1_EA_1990,  data.frame(EA_1990_caus = c(0,1)), draws = 100, selected_variable = "EA_1990_caus", categorical = T ) 

# EA 1999 ---------

structuralP_Welfare = structuralP %>%
  select_at(vars(country, starts_with("X"), cl_Esping_Andersen_1999_caus = cl_Esping_Andersen_1999)) %>% 
  mutate(EA_1999_caus = ifelse(cl_Esping_Andersen_1999_caus == "Lib", 1, 0)) %>%
  na.omit() %>% 
  make_DR()


structuralP_Welfare %>% 
  pivot_longer(cols=starts_with("X_")) %>% 
  ggplot(aes(x=cl_Esping_Andersen_1999_caus))


m1_EA_1999  = DirichReg(Y_Fec ~  1 + EA_1999_caus  | 1,
                        structuralP_Welfare, "alternative")
m2_EA_1999  = DirichReg(Y_FeC ~  + EA_1999_caus,
                        structuralP_Welfare, "alternative")
m3_EA_1999  = DirichReg(Y_fEc ~  + EA_1999_caus,
                        structuralP_Welfare, "alternative")
m4_EA_1999  = DirichReg(Y_fEC ~  + EA_1999_caus,
                        structuralP_Welfare, "alternative")
summary(m1_EA_1999)
p2_res = plot_residual_diri(m1_EA_1999)

p2 = odds_ratio_plot(m1_EA_1999, m2_EA_1999, m3_EA_1999, m4_EA_1999)

make_table_diri(m1_EA_1990, m1_EA_1999)
grid.arrange(p1,p2)
grid.arrange(p1_res, p2_res)


## Cultural Explanation ####
schwartz = fread("Datasets/Schwartz_culture.csv") %>% 
  select(country, 
         comp_cult_caus = mastery, 
         harm_cult_caus = harmony, 
         egalit_cult_caus = egalitarianism, 
         embeddedness_cult_caus = embeddedness,
         hierarchy_cult_caus = hierarchy, 
         aff_auto_cult_caus = affective_autonomy, 
         int_auto_cult_caus = intellectual_autonomy)

# Belgium is missing from Schwartz
structuralP_Cult_Welfare_1990 = structuralP %>%
  select_at(vars(country, starts_with("X"), cl_Esping_Andersen_1990_caus = cl_Esping_Andersen_1990)) %>% 
  mutate(EA_1990_caus = ifelse(cl_Esping_Andersen_1990_caus == "Lib", 1, 0))  %>%
  left_join(schwartz, by = "country") %>% 
  na.omit() %>% 
  make_DR()

structuralP_Cult_Welfare_1999 = structuralP %>%
  select_at(vars(country, starts_with("X"), cl_Esping_Andersen_1999_caus = cl_Esping_Andersen_1999)) %>% 
  mutate(EA_1999_caus = ifelse(cl_Esping_Andersen_1999_caus == "Lib", 1, 0)) %>%
  left_join(schwartz, by = "country") %>% 
  na.omit() %>% 
  make_DR()

# EA 1990 ####

m1_EA_1990_diri  = DirichReg(Y_Fec ~  1 + EA_1990_caus  | 1,
                        structuralP_Cult_Welfare_1990, "alternative")
summary(m1_EA_1990_diri)


m1_EA_1990_log  = glm(EA_1990_caus ~  comp_cult_caus,
                  structuralP_Cult_Welfare_1990, family="binomial")

summary(m1_EA_1990_log)

m2_EA_1990_log  = glm(EA_1990_caus ~  egalit_cult_caus,
                      structuralP_Cult_Welfare_1990, family="binomial")

summary(m2_EA_1990_log)

influencePlot(m1_EA_1990_log)
influencePlot(m2_EA_1990_log)


# EA 1999 ####
m1_EA_1999_diri  = DirichReg(Y_Fec ~  1 + EA_1999_caus  | 1,
                        structuralP_Cult_Welfare_1999, "alternative")
summary(m1_EA_1999_diri)

m1_EA_1999_log  = glm(EA_1999_caus ~  1 + comp_cult_caus,
                      structuralP_Cult_Welfare_1999, family="binomial")

summary(m1_EA_1999_log)

m2_EA_1999_log  = glm(EA_1999_caus ~  egalit_cult_caus,
                      structuralP_Cult_Welfare_1999, family="binomial")

summary(m2_EA_1999_log)


influencePlot(m1_EA_1999_log)
influencePlot(m2_EA_1999_log)


## VoC ####

structuralP_cult_VoC = structuralP %>%
  select_at(vars(country, starts_with("X"), VoC_caus)) %>%
  left_join(schwartz, by = "country") %>% 
  na.omit() 

m1_voc_log  = glm(VoC_caus ~  1 + comp_cult_caus,
                  structuralP_cult_VoC, family="binomial")

summary(m1_voc_log)
m2_voc_log  = glm(VoC_caus ~  1 + egalit_cult_caus,
                  structuralP_cult_VoC, family="binomial")

summary(m2_voc_log)

influencePlot(m1_voc_log)
influencePlot(m2_voc_log)


#### Table Results #####
stargazer(# Competition
          m1_EA_1990_log,
          m1_EA_1999_log,
          m1_voc_log,  
          # Egalitarian
          m2_EA_1990_log,
          m2_EA_1999_log,
          m2_voc_log,
          type="html",
          out="Analyse/Performance/StructuralP/glm_tables.doc")


