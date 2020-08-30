# Causes of Democracy Profiles

source("Analyse/CreateDatasets.R")
#source("Analyse/Performance/StructuralP/SimulateFunctions.R")
source("Setup/Sig_Tables.R")
source("Setup/Simulation_Dirichlet.R")

# Functions: #####
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}



plot_diri_cont = function(data, variable, y_var) {
  
  variable_label = gsub("_caus","",variable)
  
  plot1 = data %>% 
    select_at(vars(country, vars = variable, starts_with(y_var))) %>% 
    pivot_longer(cols=starts_with("mp_")) %>% 
    mutate(name = gsub("mp_Cluster", "", name)) %>% 
    #na.omit() %>% 
    ggplot(aes(x=vars, y=value)) +
    geom_point() +
    geom_smooth(method="loess", se=T, color="black", span = 1.5) +
    facet_wrap(name~.) +
    theme(legend.position = "none") +
    xlab(variable) +
    theme_bw() +
    xlab("") +
    ylab("") +
    ggtitle(variable_label)

  return(plot1)
}

plot_residual_diri = function(model) {
  
  plot_data = data.frame(
    residuals_comp = residuals(model, "composite"),
    id = seq(1, dim(model$d)[1], 1),
    fitted = fitted(model),
    country = model$data$country)
  
  p1 = plot_data %>% 
    ggplot(aes(x=id, y=residuals_comp, label=country)) +
    geom_point() +
    geom_text() +
    ylab("Composite Residuals") +
    theme_bw() 

  return(p1)
}

stand_residuals = function(model) {
  dep_dims = dim(residuals(model, "standardized"))[2]
  
  
  fitted_vals = data.frame(fitted(model)) %>% 
    rename_all(funs(gsub("mp_", "fit_",.)))
  
  stand_resid = data.frame(as.matrix(residuals(model, "standardized"))[,1:dep_dims]) %>% 
    rename_all(funs(gsub("mp_", "res_",.)))
  
  
  plotlist = list()
  for (i in 1:dep_dims) {
    plotlabel = colnames(fitted_vals)[i]
    plotlabel = gsub("fit_Cluster3_","", plotlabel)
    plotlabel = gsub("fit_Cluster4_","", plotlabel)
    plotlabel = gsub("fit_Cluster5_","", plotlabel)
    
    
    plot_data = fitted_vals %>%
      select(fitted = i) %>% 
      bind_cols(stand_resid %>%  
                  select(residuals = i)) %>% 
      ggplot(aes(x=fitted, y=residuals)) +
      geom_point() +
      geom_smooth(se=F, linetype = 2, color="black") +
      theme_bw() +
      xlab("Fitted Values") +
      ylab("Standardized Residuals") +
      ggtitle(plotlabel) +
      geom_hline(yintercept=0)
    
    
    plotlist[[i]] = plot_data
  }
  
  p1 = ggarrange(plotlist = plotlist)
  return(p1)
}


# transform to DR data (Dirichletreg)
make_DR = function(dataset, mp_var, ref_cat) {
  
  label = gsub("mp_", "ref_", ref_cat)
  dataset$DR_data = DirichletReg::DR_data(dataset %>%  
                                            select_at(vars(starts_with(mp_var))) %>%  
                                            select(ref_cat, everything()))
  
  colnames(dataset)[which(colnames(dataset) == "DR_data")] = label
  
  return(dataset)
}

simulate_values = function(model, dataset, variable, predictonly =F) {
  
  
  if (predictonly == F) {
    xvar = dataset %>% 
      select(variable = variable)
    
    label_variable = gsub("_caus", "", variable)
    
    p1 = data.frame(predict(model, dataset)) %>%
      rename_all(funs(colnames(model$Y))) %>% 
      cbind(variable = xvar$variable) %>% 
      pivot_longer(cols=-variable) %>% 
      mutate(name = gsub("mp_Cluster4_", "", name),
             name = gsub("mp_Cluster5_", "", name)) %>% 
      ggplot(aes(x=variable, y=value, fill=name)) +
      geom_area() +
      scale_fill_discrete(name = "") +
      theme_bw() +
      theme(legend.position = "bottom") +
      xlab(label_variable) +
      ggtitle(label_variable) +
      scale_y_continuous(name = "Membership Probability", label=percent, breaks=seq(0,1,0.2))
    
    
  } else {
    p1 = data.frame(predict(model, dataset))  %>%
      rename_all(funs(colnames(model$Y)))  %>% 
      pivot_longer(cols=everything())
  }
  return(p1)
}

mean_NA = function(x) {
  if(all(is.na(x)) == F) {
    return(mean(x, na.rm=T))
  } else {
    return(NA)
  }
    
}

# Independent Variables ####

fract_caus = fread("Datasets/ethnic_frac.csv", encoding="Latin-1") %>% 
  rename(ethn_caus = Ethnic, ling_caus = Linguistic, rel_caus = Religious) %>%
  mutate(country = substring(country, 2),
         country = fct_recode(country,
                              "Timor-Leste" =	"East Timor",
                              "The Gambia" = "Gambia",
                              )
                              
         ) %>%
  mutate(fract_sum_caus = (ethn_caus + rel_caus + ling_caus)/3,
         fract_prod_caus = (ethn_caus * ling_caus * rel_caus)^(1/3)) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  select(country_text_id, fract_sum_caus, fract_prod_caus)


QoG_caus = QoC_data %>% 
  dplyr::select(country_text_id, year,
                pop_size_caus = wdi_pop,
                gdp_caus = wdi_gdpcapcur,
                englegal_caus = lp_legor,
                union_caus = vi_udr,
                sd_caus = cpds_ls,
                left_caus = cpds_lls)  %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  group_by(country_text_id, year) %>% 
  summarise_all(mean_NA) %>% 
  ungroup() %>% 
  mutate(
    pop_size_caus = ifelse(pop_size_caus == 0, NA, pop_size_caus),
    pop_size_caus = log(pop_size_caus),
    gdp_caus = log(gdp_caus),
    # 1: English Legal Origin; 0: Other
    englegal_caus = ifelse(englegal_caus == 1, 1, 0),
    leftist_caus = sd_caus + left_caus
  ) %>% 
  select(-sd_caus, -left_caus)

QoG_caus_num = QoG_caus %>% 
  filter(year >= 1950) %>% 
  select(-year) %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.numeric, list(~mean_NA(.)))

QoG_caus_cat = QoG_caus %>% 
  filter(year >= 1950) %>% 
  group_by(country_text_id) %>% 
  summarise_if(is.character, list(~getmode(.))) 


colonial_caus = fread("Datasets/coldata110.csv", sep=",") %>% 
  select(COWcode = State, Name, ColRuler)  %>% 
  mutate(COWcode = ifelse(COWcode == 305, 300, COWcode),
         COWcode = ifelse(COWcode == 732, 730, COWcode)) %>% 
  mutate(UKcol_caus = ifelse(ColRuler == 200, 1, 0),
         UKcol_caus = ifelse(COWcode == 200, 1, UKcol_caus),
         # 1 is British Colony; 0: Other
         UKcol_caus = ifelse(UKcol_caus == 1, 1, 0)) %>% 
  select(COWcode, UKcol_caus)


wrp_caus = fread("Datasets/WRP_national.csv") %>% 
  select(name, COWcode = state, rel_cath_wrp_caus = chrstcat, pop) %>% 
  mutate(COWcode = ifelse(COWcode == 305, 300, COWcode),
         COWcode = ifelse(COWcode == 732, 730, COWcode),
         rel_cath_wrp_caus = rel_cath_wrp_caus/pop
         ) %>%
  group_by(COWcode) %>% 
  summarise(cath_caus = mean(rel_cath_wrp_caus, na.rm=T)) %>% 
  ungroup()
  

schwartz_caus = fread("Datasets/Schwartz_culture.csv") %>% 
  select(country, 
         mastery_cult_caus = mastery, 
         harm_cult_caus = harmony, 
         egalit_cult_caus = egalitarianism, 
         embeddedness_cult_caus = embeddedness,
         hierarchy_cult_caus = hierarchy, 
         aff_auto_cult_caus = affective_autonomy, 
         int_auto_cult_caus = intellectual_autonomy) %>% 
  select(country, mastery_cult_caus, hierarchy_cult_caus)


# Dependent Variables ####
profiles_agg_cult = dmx_trade_cluster %>% 
  
  filter(year >= 1950) %>% 
  #filter(year >= 1950, year <= 1980) %>% 
  
  select_at(vars(country, starts_with("mp_cluster4"), starts_with("mp_cluster5"))) %>% 
  group_by(country) %>% 
  summarise_all(mean, na.omit=T) %>% 
  ungroup() %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country")

### Create Dataset ######

caus_culture_profiles_data = profiles_agg_cult  %>% 
  left_join(V_dem %>% select(country, COWcode) %>%  distinct() %>%  group_by(country) %>% dplyr::slice(1), by="country") %>% 
  left_join(fract_caus, by="country_text_id") %>% 
  left_join(QoG_caus_num, by="country_text_id") %>% 
  left_join(QoG_caus_cat, by="country_text_id") %>% 
  left_join(colonial_caus, by="COWcode") %>% 
  left_join(wrp_caus, by="COWcode")  %>%
  left_join(schwartz_caus, by="country") %>% 
  select_at(vars(country, country_text_id, COWcode, starts_with("mp_"), everything())) %>% 
# QoG does not have the complete data for Legal Origin
  mutate(englegal_caus = ifelse(is.na(englegal_caus), 0, englegal_caus),
         englegal_caus = ifelse(country == "Vanuatu", 1, englegal_caus),
         englegal_caus = ifelse(country == "Namibia", 1, englegal_caus),
         englegal_caus = ifelse(country == "German Democratic Republic", NA, englegal_caus),
         englegal_caus = ifelse(country == "Kosovo", NA, englegal_caus),
         englegal_caus = ifelse(country == "Montenegro", NA, englegal_caus),
         englegal_caus = ifelse(country == "Palestine/West Bank", NA, englegal_caus),
         englegal_caus = ifelse(country == "Timor-Leste	", NA, englegal_caus))

# Exploratory Analysis ####

missd_pattern(caus_culture_profiles_data %>% 
                select_at(vars(ends_with("_caus"))) %>% 
                rename_all(funs(gsub("_caus","",.))) )

length(which(is.na(caus_culture_profiles_data$mastery_cult_caus) == F))

corrplot(cor(caus_culture_profiles_data %>% 
               select_at(vars(starts_with("mp_"), ends_with("_caus"))) %>% 
               rename_all(funs(gsub("_caus","",.))) %>% 
               rename_all(funs(gsub("mp_","",.))), use="pairwise"), method="number")
corrplot(cor(caus_culture_profiles_data %>% 
               select_at(vars(starts_with("mp_"), ends_with("_caus"))) %>% 
               rename_all(funs(gsub("_caus","",.))) %>% 
               rename_all(funs(gsub("mp_","",.))), use="pairwise"))


# XY Plots
names_caus = colnames(caus_culture_profiles_data %>% 
           select_at(vars(ends_with("_caus"))))
plotlist4 = list()
for (i in 1:length(names_caus)) {
  plotlist4[[i]] = plot_diri_cont(caus_culture_profiles_data, names_caus[i], "mp_Cluster4") 
}
ggarrange(plotlist=plotlist4, ncol=3, nrow=4)

plotlist5 = list()
for (i in 1:length(names_caus)) {
  plotlist5[[i]] = plot_diri_cont(caus_culture_profiles_data, names_caus[i], "mp_Cluster5") 
}
ggarrange(plotlist=plotlist5, ncol=3, nrow=4)



# Create Refernce Categories####

# Structural Variables


caus_culture_profiles_data_struct = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("mp_"),
                 gdp_caus, 
                 fract_sum_caus, fract_prod_caus, 
                 englegal_caus,
                 UKcol_caus,
                 pop_size_caus, 
                 cath_caus
                 )) %>%
  na.omit() %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_Fec") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_fEc") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_fEC") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_FeC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_Fec") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_fEc") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_fEC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_FeC")  %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_FEC")

# Power Resource Variables

caus_culture_profiles_data_pr = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("mp_"),
                 #gdp_caus, 
                 #fract_sum_caus, fract_prod_caus, 
                 #englegal_caus,
                 UKcol_caus,
                 pop_size_caus, 
                 #cath_caus
                 union_caus,
                 leftist_caus)) %>%
  na.omit() %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_Fec") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_fEc") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_fEC") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_FeC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_Fec") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_fEc") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_fEC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_FeC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_FEC")

# Cultural Variables
caus_culture_profiles_data_cult = caus_culture_profiles_data %>% 
  select_at(vars(country, starts_with("mp_"),
                 gdp_caus, 
                 fract_sum_caus, fract_prod_caus, 
                 englegal_caus,
                 UKcol_caus,
                 pop_size_caus, 
                 cath_caus,
                 mastery_cult_caus,
                 hierarchy_cult_caus)) %>%
  na.omit() %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_Fec") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_fEc") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_fEC") %>% 
  make_DR(., "mp_Cluster4", "mp_Cluster4_FeC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_Fec") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_fEc") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_fEC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_FeC") %>% 
  make_DR(., "mp_Cluster5", "mp_Cluster5_FEC")


# Regression: 4 Cluster ####
# caus_culture_profiles_data_struct = caus_culture_profiles_data_struct %>% 
#   filter(country != "The Gambia",
#          country != "El Salvador",
#          country != "Guatemala")

# Null Model
clust4_m1  = DirichReg(ref_Cluster4_Fec ~  1,
                       caus_culture_profiles_data_struct, "alternative")

# GDP
clust4_m2  = DirichReg(ref_Cluster4_Fec ~  gdp_caus,
                       caus_culture_profiles_data_struct, "alternative")

# Population Size
clust4_m3  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus,
                       caus_culture_profiles_data_struct, "alternative")

# Cultural Diversity 
clust4_m4a  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus  | 1,
                        caus_culture_profiles_data_struct, "alternative")

clust4_m4b  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus,
                        caus_culture_profiles_data_struct, "alternative")


# British Heritage
clust4_m5a  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus + englegal_caus  | 1,
                        caus_culture_profiles_data_struct, "alternative")

clust4_m5b  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + englegal_caus  | 1,
                        caus_culture_profiles_data_struct, "alternative")

clust4_m5ac  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus + UKcol_caus  | 1,
                         caus_culture_profiles_data_struct, "alternative")

clust4_m5bc  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + UKcol_caus  | 1,
                         caus_culture_profiles_data_struct, "alternative")


# Catholics
clust4_m6a  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus + UKcol_caus + cath_caus| 1,
                        caus_culture_profiles_data_struct, "alternative")

clust4_m6b  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + UKcol_caus + cath_caus| 1,
                        caus_culture_profiles_data_struct, "alternative")

clust4_m6bc  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + UKcol_caus + cath_caus| 1,
                        caus_culture_profiles_data_struct, "alternative")



# Power Resource Theory
# caus_culture_profiles_data_pr = caus_culture_profiles_data_pr %>% 
#   filter(country != "Switzerland")

clust4_m0_pr  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + UKcol_caus,
                          caus_culture_profiles_data_pr, "alternative")
clust4_m1_pr  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + UKcol_caus + 
                            union_caus| 1,
                          caus_culture_profiles_data_pr, "alternative")
clust4_m2_pr  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + UKcol_caus + 
                            leftist_caus| 1,
                          caus_culture_profiles_data_pr, "alternative")
clust4_m3_pr  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + UKcol_caus + 
                            union_caus + leftist_caus| 1,
                          caus_culture_profiles_data_pr, "alternative")

# Culture
# caus_culture_profiles_data_cult = caus_culture_profiles_data_cult %>% 
#   filter(country != "Ireland",
#          country != "Chile",
#          country != "Israel",
#   ) 

clust4_m0_cult  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + cath_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust4_m1_cult  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                              hierarchy_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust4_m2_cult  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                              mastery_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust4_m3_cult  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                              mastery_cult_caus + hierarchy_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")

summary(clust4_m3_cult)

clust4_m0_cult_gdp  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + cath_caus| 1,
                                caus_culture_profiles_data_cult, "alternative")
clust4_m1_cult_gdp  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + cath_caus + 
                                  hierarchy_cult_caus| 1,
                                caus_culture_profiles_data_cult, "alternative")
clust4_m2_cult_gdp  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + cath_caus + 
                                  mastery_cult_caus| 1,
                                caus_culture_profiles_data_cult, "alternative")
clust4_m3_cult_gdp  = DirichReg(ref_Cluster4_Fec ~  pop_size_caus + cath_caus + 
                                  mastery_cult_caus + hierarchy_cult_caus| 1,
                                caus_culture_profiles_data_cult, "alternative")


clust4_m0_cult_uk  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus| 1,
                               caus_culture_profiles_data_cult, "alternative")
clust4_m1_cult_uk  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus + 
                                 hierarchy_cult_caus| 1,
                               caus_culture_profiles_data_cult, "alternative")
clust4_m2_cult_uk  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus + 
                                 mastery_cult_caus| 1,
                               caus_culture_profiles_data_cult, "alternative")
clust4_m3_cult_uk  = DirichReg(ref_Cluster4_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus + 
                                 mastery_cult_caus + hierarchy_cult_caus| 1,
                               caus_culture_profiles_data_cult, "alternative")



# Make Summary Table ####

#Structure ####
make_table_diri(clust4_m1, clust4_m2, clust4_m3, clust4_m4b, clust4_m5bc, clust4_m5b, clust4_m6b, oddsRatios=F)
make_table_diri(clust4_m1, clust4_m2, clust4_m3, clust4_m4a, clust4_m5a, clust4_m6a, oddsRatios=F)

anova(clust4_m1, clust4_m2) %>% 
  make_anova_table(diri = T)
anova(clust4_m2, clust4_m3) %>% 
  make_anova_table(diri = T)
anova(clust4_m3, clust4_m4b) %>% 
  make_anova_table(diri = T)
anova(clust4_m4b, clust4_m5bc) %>% 
  make_anova_table(diri = T)
anova(clust4_m4b, clust4_m5b) %>% 
  make_anova_table(diri = T)
anova(clust4_m5bc, clust4_m6b) %>% 
  make_anova_table(diri = T)

plot_residual_diri(clust4_m6b)
stand_residuals(clust4_m6b)


odds_ratio_plot(clust4_m6b, 
                sign_niveau = 0.05)

# Power Resource Theory ####
make_table_diri(clust4_m0_pr, clust4_m1_pr, clust4_m2_pr, clust4_m3_pr, oddsRatios=F)

anova(clust4_m0_pr, clust4_m1_pr) %>% 
  make_anova_table(diri = T)
anova(clust4_m0_pr, clust4_m2_pr) %>% 
  make_anova_table(diri = T)
anova(clust4_m0_pr, clust4_m3_pr) %>% 
  make_anova_table(diri = T)
anova(clust4_m2_pr, clust4_m3_pr) %>% 
  make_anova_table(diri = T)


plot_residual_diri(clust4_m3_pr)
stand_residuals(clust4_m3_pr)

p1 = odds_ratio_plot(clust4_m3_pr, sign_niveau = 0.05, vars_sel ="union_caus")
p2 = odds_ratio_plot(clust4_m3_pr, sign_niveau = 0.05, vars_sel ="leftist_caus")

ggarrange(p1, p2, nrow=2, common.legend=F)



# Culture ####
make_table_diri(clust4_m0_cult, clust4_m1_cult, clust4_m2_cult, clust4_m3_cult, oddsRatios=F)
make_table_diri(clust4_m0_cult_gdp, clust4_m1_cult_gdp, clust4_m2_cult_gdp, clust4_m3_cult_gdp, oddsRatios=F)
make_table_diri(clust4_m0_cult_uk, clust4_m1_cult_uk, clust4_m2_cult_uk, clust4_m3_cult_uk, oddsRatios=F)

anova(clust4_m0_cult, clust4_m1_cult) %>% 
  make_anova_table(diri = T)
anova(clust4_m0_cult, clust4_m2_cult) %>% 
  make_anova_table(diri = T)
anova(clust4_m1_cult, clust4_m3_cult) %>% 
  make_anova_table(diri = T)

plot_residual_diri(clust4_m3_cult)
stand_residuals(clust4_m3_cult)

make_table_diri(clust4_m0_cult_isr, clust4_m1_cult_isr, clust4_m2_cult_isr, clust4_m3_cult_isr, oddsRatios=F)

anova(clust4_m0_cult_isr, clust4_m1_cult_isr) %>% 
  make_anova_table(diri = T)
anova(clust4_m0_cult_isr, clust4_m2_cult_isr) %>% 
  make_anova_table(diri = T)
anova(clust4_m1_cult_isr, clust4_m3_cult_isr) %>% 
  make_anova_table(diri = T)

p1 = odds_ratio_plot(clust4_m3_cult, sign_niveau = 0.05, vars_sel ="mastery_cult_caus")
p2 = odds_ratio_plot(clust4_m3_cult, sign_niveau = 0.05, vars_sel ="hierarchy_cult_caus")

ggarrange(p1, p2, nrow=2, common.legend=F)



# Regression: 5 Cluster ####
# caus_culture_profiles_data_struct = caus_culture_profiles_data_struct %>% 
#   filter(country != "El Salvador",
#          country != "Guatemala",
#          country != "The Gambia")

# Null Model
clust5_m1  = DirichReg(ref_Cluster5_Fec ~  1,
                       caus_culture_profiles_data_struct, "alternative")

# GDP
clust5_m2  = DirichReg(ref_Cluster5_Fec ~  gdp_caus,
                       caus_culture_profiles_data_struct, "alternative")

# Population Size
clust5_m3  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus,
                       caus_culture_profiles_data_struct, "alternative")

# Cultural Diversity 
clust5_m4a  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus  | 1,
                        caus_culture_profiles_data_struct, "alternative")

clust5_m4b  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus,
                        caus_culture_profiles_data_struct, "alternative")


# British Heritage
clust5_m5a  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus + englegal_caus  | 1,
                        caus_culture_profiles_data_struct, "alternative")

clust5_m5b  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + englegal_caus  | 1,
                        caus_culture_profiles_data_struct, "alternative")

clust5_m5ac  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus + UKcol_caus  | 1,
                         caus_culture_profiles_data_struct, "alternative")

clust5_m5bc  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + UKcol_caus  | 1,
                         caus_culture_profiles_data_struct, "alternative")


# Catholics
clust5_m6a  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_sum_caus + UKcol_caus + cath_caus| 1,
                        caus_culture_profiles_data_struct, "alternative")

clust5_m6b  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + UKcol_caus + cath_caus| 1,
                        caus_culture_profiles_data_struct, "alternative")

clust5_m6b  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + fract_prod_caus + UKcol_caus + cath_caus| 1,
                        caus_culture_profiles_data_struct, "alternative")




# Power Resource Theory
# caus_culture_profiles_data_pr = caus_culture_profiles_data_pr %>% 
#   filter(country != "Switzerland")

clust5_m0_pr  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + UKcol_caus,
                          caus_culture_profiles_data_pr, "alternative")
clust5_m1_pr  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + UKcol_caus + 
                            union_caus| 1,
                          caus_culture_profiles_data_pr, "alternative")
clust5_m2_pr  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + UKcol_caus + 
                            leftist_caus| 1,
                          caus_culture_profiles_data_pr, "alternative")
clust5_m3_pr  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + UKcol_caus + 
                            union_caus + leftist_caus| 1,
                          caus_culture_profiles_data_pr, "alternative")

# Culture
clust5_m0_cult  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus| 1,
                          caus_culture_profiles_data_cult, "alternative")
clust5_m1_cult  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                            hierarchy_cult_caus| 1,
                          caus_culture_profiles_data_cult, "alternative")
clust5_m2_cult  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                            mastery_cult_caus| 1,
                          caus_culture_profiles_data_cult, "alternative")
clust5_m3_cult  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                            mastery_cult_caus + hierarchy_cult_caus| 1,
                          caus_culture_profiles_data_cult, "alternative")

clust5_m0_cult_gdp  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + cath_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust5_m1_cult_gdp  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + cath_caus + 
                              hierarchy_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust5_m2_cult_gdp  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + cath_caus + 
                              mastery_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust5_m3_cult_gdp  = DirichReg(ref_Cluster5_Fec ~  pop_size_caus + cath_caus + 
                              mastery_cult_caus + hierarchy_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")


clust5_m0_cult_uk  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust5_m1_cult_uk  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus + 
                              hierarchy_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust5_m2_cult_uk  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus + 
                              mastery_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")
clust5_m3_cult_uk  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + UKcol_caus + cath_caus + 
                              mastery_cult_caus + hierarchy_cult_caus| 1,
                            caus_culture_profiles_data_cult, "alternative")

clust5_m0_cult_isr  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus| 1,
                                caus_culture_profiles_data_cult_is, "alternative")
clust5_m1_cult_isr  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                              hierarchy_cult_caus| 1,
                            caus_culture_profiles_data_cult_is, "alternative")
clust5_m2_cult_isr  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                              mastery_cult_caus| 1,
                              caus_culture_profiles_data_cult_is, "alternative")
clust5_m3_cult_isr  = DirichReg(ref_Cluster5_Fec ~  gdp_caus + pop_size_caus + cath_caus + 
                              mastery_cult_caus + hierarchy_cult_caus| 1,
                              caus_culture_profiles_data_cult_is, "alternative")
# Make Summary Table ####

#Structure ####
make_table_diri(clust5_m1, clust5_m2, clust5_m3, clust5_m4b, clust5_m5bc, clust5_m5b, clust5_m6b, oddsRatios=F)
make_table_diri(clust5_m1, clust5_m2, clust5_m3, clust5_m4a, clust5_m5a, clust5_m6a, oddsRatios=F)

anova(clust5_m1, clust5_m2) %>% 
  make_anova_table(diri = T)
anova(clust5_m2, clust5_m3) %>% 
  make_anova_table(diri = T)
anova(clust5_m3, clust5_m4b) %>% 
  make_anova_table(diri = T)
anova(clust5_m4b, clust5_m5bc) %>% 
  make_anova_table(diri = T)
anova(clust5_m4b, clust5_m5b) %>% 
  make_anova_table(diri = T)
anova(clust5_m5bc, clust5_m6b) %>% 
  make_anova_table(diri = T)

plot_residual_diri(clust5_m6b)


odds_ratio_plot(clust5_m6b, 
                sign_niveau = 0.05)

# Power Resource Theory ####
make_table_diri(clust5_m0_pr, clust5_m1_pr, clust5_m2_pr, clust5_m3_pr, oddsRatios=F)

anova(clust5_m0_pr, clust5_m1_pr) %>% 
  make_anova_table(diri = T)
anova(clust5_m0_pr, clust5_m2_pr) %>% 
  make_anova_table(diri = T)
anova(clust5_m1_pr, clust5_m3_pr) %>% 
  make_anova_table(diri = T)
anova(clust5_m2_pr, clust5_m3_pr) %>% 
  make_anova_table(diri = T)


plot_residual_diri(clust5_m3_pr)

p1 = odds_ratio_plot(clust5_m3_pr, sign_niveau = 0.05, vars_sel ="union_caus")
p2 = odds_ratio_plot(clust5_m3_pr, sign_niveau = 0.05, vars_sel ="leftist_caus")

ggarrange(p1, p2, nrow=2, common.legend=F)



# Culture ####
make_table_diri(clust5_m0_cult, clust5_m1_cult, clust5_m2_cult, clust5_m3_cult, oddsRatios=F)
make_table_diri(clust5_m0_cult_gdp, clust5_m1_cult_gdp, clust5_m2_cult_gdp, clust5_m3_cult_gdp, oddsRatios=F)
make_table_diri(clust5_m0_cult_uk, clust5_m1_cult_uk, clust5_m2_cult_uk, clust5_m3_cult_uk, oddsRatios=F)

anova(clust5_m0_cult, clust5_m1_cult) %>% 
  make_anova_table(diri = T)
anova(clust5_m0_cult, clust5_m2_cult) %>% 
  make_anova_table(diri = T)
anova(clust5_m1_cult, clust5_m3_cult) %>% 
  make_anova_table(diri = T)

plot_residual_diri(clust5_m3_cult_uk)
make_table_diri(clust5_m0_cult_isr, clust5_m1_cult_isr, clust5_m2_cult_isr, clust5_m3_cult_isr, oddsRatios=F)

anova(clust5_m0_cult_isr, clust5_m1_cult_isr) %>% 
  make_anova_table(diri = T)
anova(clust5_m0_cult_isr, clust5_m2_cult_isr) %>% 
  make_anova_table(diri = T)
anova(clust5_m1_cult_isr, clust5_m3_cult_isr) %>% 
  make_anova_table(diri = T)

p1 = odds_ratio_plot(clust5_m3_cult, sign_niveau = 0.05, vars_sel ="mastery_cult_caus")
p2 = odds_ratio_plot(clust5_m3_cult, sign_niveau = 0.05, vars_sel ="hierarchy_cult_caus")

ggarrange(p1, p2, nrow=2, common.legend=F)


make_table_diri(clust5_m1, clust5_m2, clust5_m3, clust5_m4b, clust5_m5bc, clust5_m5b, clust5_m6b,
                clust5_m0_pr, clust5_m1_pr, clust5_m2_pr, clust5_m3_pr,
                clust5_m1_cult, clust5_m2_cult, clust5_m3_cult,
                oddsRatios=F)

##### Simulation 4 Clusters ####

clust4_m6b$d %>% 
  summarise_all(funs(mean, min, max) )

# pop_size_caus
simu_pop_size = data.frame(
  gdp_caus = 7.785674,
  pop_size_caus = seq( 11.23763, 20.51996, length.out = 10),
  UKcol_caus = 0.2616822,
  fract_prod_caus =  0.3418956,
  cath_caus =  0.3657608 
)

# englegal_centr_caus
simu_english = data.frame(
  gdp_caus = 7.785674,
  pop_size_caus = 15.6948 ,
  UKcol_caus = c(0,1),
  fract_prod_caus =  0.3418956,
  cath_caus =  0.3657608 
)

# diverse_caus
simu_diverse = data.frame(
  gdp_caus = 7.785674,
  pop_size_caus = 15.6948 ,
  UKcol_caus = 0.2616822,
  fract_prod_caus = seq(7.732318e-05, 0.8240186, length.out = 10),
  cath_caus =  0.3657608
)



p1 = simulate_values(clust4_m6b, simu_pop_size, "pop_size_caus")
p2 = simulate_values(clust4_m6b, simu_diverse, "fract_prod_caus")
p3 = simulate_values(clust4_m6b, simu_english, "UKcol_caus")

ggarrange(p1,p2,p3, common.legend = T, legend = "bottom")



# Ideal Configurations
# Small State + Low Diversity + English Heritage
simu_conf1 = data.frame(
  gdp_caus = quantile(clust4_m6b$d$gdp_caus, probs = 0.1),
  pop_size_caus = quantile(clust4_m6b$d$pop_size_caus, probs = 0.1),
  UKcol_caus = 1,
  fract_prod_caus =  quantile(clust4_m6b$d$fract_prod_caus, probs = 0.1),
  cath_caus =  mean(clust4_m6b$d$cath_caus) 
)

# Big State + High Diversity + Non-English Heritage
simu_conf2 = data.frame(
  gdp_caus = quantile(clust4_m6b$d$gdp_caus, probs = 0.9),
  pop_size_caus = quantile(clust4_m6b$d$pop_size_caus, probs = 0.9),
  UKcol_caus = 0,
  fract_prod_caus =  quantile(clust4_m6b$d$fract_prod_caus, probs = 0.9),
  cath_caus =  mean(clust4_m6b$d$cath_caus) 
)

simulate_values(clust4_m6b, simu_conf1,predictonly = T) %>% 
  mutate(cond = "Small State + \nLow Diversity + \nEnglish-Heritage") %>% 
  bind_rows(simulate_values(clust4_m6b, simu_conf2,predictonly = T) %>% 
              mutate(cond = "Large State + \nHigh Diversity + \nNon-English-Heritage")) %>% 
  mutate(name = gsub("mp_Cluster4_", "", name),
         name = gsub("mp_Cluster5_", "", name)) %>% 
  ggplot(aes(x=cond, y=value, fill=name)) +
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "") +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size=12)) +
  scale_y_continuous(name = "Membership Probability", label=percent)




# Power Resource Theory
clust4_m3_pr$d %>% 
  summarise_all(funs(mean, min, max) )

simu_unions = data.frame(
  union_caus = seq(13.06469, 85.78162, length.out = 10),
  pop_size_caus = 16.00002 ,
  UKcol_caus = 0.1944444,
  leftist_caus = 30.6991
)

simu_leftist = data.frame(
  union_caus = 39.6410,
  pop_size_caus = 16.00002 ,
  UKcol_caus = 0.1944444,
  leftist_caus = seq(0, 46.68947, length.out = 10)
)

p1 = simulate_values(clust4_m3_pr, simu_unions, "union_caus")
p2 = simulate_values(clust4_m3_pr, simu_leftist, "leftist_caus")

ggarrange(p1,p2, common.legend = T, legend = "bottom")


test = simulate_values(clust4_m3_pr, simu_unions, "union_caus", predictonly = T)
test = simulate_values(clust4_m3_pr, simu_leftist, "leftist_caus", predictonly = T)

# 2 Conditions 
simu_leftist1 = data.frame(
  union_caus = 39.6410,
  pop_size_caus = quantile(clust4_m3_pr$d$pop_size_caus, probs = 0.1) ,
  UKcol_caus = 1,
  leftist_caus = seq(0, 46.68947, length.out = 10)
)

simu_leftist2 = data.frame(
  union_caus = 39.6410,
  pop_size_caus = quantile(clust4_m3_pr$d$pop_size_caus, probs = 0.9) ,
  UKcol_caus = 0,
  leftist_caus = seq(0, 46.68947, length.out = 10)
)

data.frame(predict(clust4_m3_pr, newdata=simu_leftist1)) %>%
  rename_all(funs(colnames(clust4_m3_pr$Y))) %>% 
  bind_cols(data.frame(leftist_caus = simu_leftist1$leftist_caus)) %>% 
  mutate(cond = "Small State + \nEnglish-Heritage") %>% 
  bind_rows(data.frame(predict(clust4_m3_pr, newdata=simu_leftist2)) %>%
              rename_all(funs(colnames(clust4_m3_pr$Y))) %>% 
              bind_cols(data.frame(leftist_caus = simu_leftist2$leftist_caus)) %>% 
              mutate(cond = "Large State + \nNon-English-Heritage") ) %>% 
  pivot_longer(cols = -c(cond, leftist_caus)) %>% 
  mutate(name = gsub("mp_Cluster4_", "", name),
         name = gsub("mp_Cluster5_", "", name)) %>% 
  ggplot(aes(x=leftist_caus, y=value, fill=name)) +
  geom_area() +
  facet_wrap(cond ~ .) + 
  scale_fill_discrete(name = "") +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size=12)) +
  scale_y_continuous(name = "Membership Probability", label=percent, breaks=seq(0,1,0.2))


# Cultural Orientation
clust4_m3_cult$d %>% 
  summarise_all(funs(mean, min, max) )

simu_culture_mastery = data.frame(
  gdp_caus = 8.57296,
  pop_size_caus = 16.52218 ,
  cath_caus = 0.4120364,
  mastery_cult_caus = seq(3.393, 4.162, length.out = 10),
  hierarchy_cult_caus = 2.1638
)

simu_culture_hierarchy = data.frame(
  gdp_caus = 8.57296,
  pop_size_caus = 16.52218 ,
  cath_caus = 0.4120364,
  mastery_cult_caus = 3.767333,
  hierarchy_cult_caus = seq(1.411, 3.371, length.out = 10)
)

p1 = simulate_values(clust4_m3_cult, simu_culture_mastery, "mastery_cult_caus")
p2 = simulate_values(clust4_m3_cult, simu_culture_hierarchy, "hierarchy_cult_caus")
ggarrange(p1, p2, common.legend = T, legend = "bottom")
test = simulate_values(clust4_m3_cult, simu_culture_hierarchy, "hierarchy_cult_caus", predictonly = T)

 
##### Simulation 5 Clusters ####

clust5_m6b$d %>% 
  summarise_all(funs(mean, min, max) )

# pop_size_caus
simu_pop_size = data.frame(
  gdp_caus = 7.785674,
  pop_size_caus = seq( 11.23763, 20.51996, length.out = 10),
  UKcol_caus = 0.2616822,
  fract_prod_caus =  0.3418956,
  cath_caus =  0.3657608 
)

# englegal_centr_caus
simu_english = data.frame(
  gdp_caus = 7.785674,
  pop_size_caus = 15.6948 ,
  UKcol_caus = c(0,1),
  fract_prod_caus =  0.3418956,
  cath_caus =  0.3657608 
)

# diverse_caus
simu_diverse = data.frame(
  gdp_caus = 7.785674,
  pop_size_caus = 15.6948 ,
  UKcol_caus = 0.2616822,
  fract_prod_caus = seq(7.732318e-05, 0.8240186, length.out = 10),
  cath_caus =  0.3657608
)



p1 = simulate_values(clust5_m6b, simu_pop_size, "pop_size_caus")
p2 = simulate_values(clust5_m6b, simu_diverse, "fract_prod_caus")
p3 = simulate_values(clust5_m6b, simu_english, "UKcol_caus")

ggarrange(p1,p2,p3, common.legend = T, legend = "bottom")



# Ideal Configurations
# Small State + Low Diversity + English Heritage
simu_conf1 = data.frame(
  gdp_caus = quantile(clust5_m6b$d$gdp_caus, probs = 0.1),
  pop_size_caus = quantile(clust5_m6b$d$pop_size_caus, probs = 0.1),
  UKcol_caus = 1,
  fract_prod_caus =  quantile(clust5_m6b$d$fract_prod_caus, probs = 0.1),
  cath_caus =  mean(clust5_m6b$d$cath_caus) 
)

# Big State + High Diversity + Non-English Heritage
simu_conf2 = data.frame(
  gdp_caus = quantile(clust5_m6b$d$gdp_caus, probs = 0.9),
  pop_size_caus = quantile(clust5_m6b$d$pop_size_caus, probs = 0.9),
  UKcol_caus = 0,
  fract_prod_caus =  quantile(clust5_m6b$d$fract_prod_caus, probs = 0.9),
  cath_caus =  mean(clust5_m6b$d$cath_caus) 
)

simulate_values(clust5_m6b, simu_conf1,predictonly = T) %>% 
  mutate(cond = "Small State + \nLow Diversity + \nEnglish-Heritage") %>% 
  bind_rows(simulate_values(clust5_m6b, simu_conf2,predictonly = T) %>% 
              mutate(cond = "Large State + \nHigh Diversity + \nNon-English-Heritage")) %>% 
  mutate(name = gsub("mp_Cluster4_", "", name),
         name = gsub("mp_Cluster5_", "", name)) %>% 
  ggplot(aes(x=cond, y=value, fill=name)) +
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "") +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size=12)) +
  scale_y_continuous(name = "Membership Probability", label=percent)




# Left Parties
clust5_m3_pr$d %>% 
  summarise_all(funs(mean, min, max) )

simu_leftist1 = data.frame(
  union_caus = 39.6410,
  pop_size_caus = 16.00002 ,
  UKcol_caus = 0.1944444,
  leftist_caus = seq(0, 46.68947, length.out = 10)
)

simulate_values(clust5_m3_pr, simu_leftist, "leftist_caus")
test = simulate_values(clust5_m3_pr, simu_leftist, "leftist_caus", predictonly = T)

# 2 Conditions 
simu_leftist1 = data.frame(
  union_caus = 39.6410,
  pop_size_caus = quantile(clust5_m3_pr$d$pop_size_caus, probs = 0.1) ,
  UKcol_caus = 1,
  leftist_caus = seq(0, 46.68947, length.out = 10)
)

simu_leftist2 = data.frame(
  union_caus = 39.6410,
  pop_size_caus = quantile(clust5_m3_pr$d$pop_size_caus, probs = 0.9) ,
  UKcol_caus = 0,
  leftist_caus = seq(0, 46.68947, length.out = 10)
)

data.frame(predict(clust5_m3_pr, newdata=simu_leftist1)) %>%
  rename_all(funs(colnames(clust5_m3_pr$Y))) %>% 
  bind_cols(data.frame(leftist_caus = simu_leftist1$leftist_caus)) %>% 
  mutate(cond = "Small State + \nEnglish-Heritage") %>% 
  bind_rows(data.frame(predict(clust5_m3_pr, newdata=simu_leftist2)) %>%
              rename_all(funs(colnames(clust5_m3_pr$Y))) %>% 
              bind_cols(data.frame(leftist_caus = simu_leftist2$leftist_caus)) %>% 
              mutate(cond = "Large State + \nNon-English-Heritage") ) %>% 
  pivot_longer(cols = -c(cond, leftist_caus)) %>% 
  mutate(name = gsub("mp_Cluster4_", "", name),
       name = gsub("mp_Cluster5_", "", name)) %>% 
  ggplot(aes(x=leftist_caus, y=value, fill=name)) +
  geom_area() +
  facet_wrap(cond ~ .) + 
  scale_fill_discrete(name = "") +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size=12)) +
  scale_y_continuous(name = "Membership Probability", label=percent)


# Cultural Orientation
clust5_m3_cult$d %>% 
  summarise_all(funs(mean, min, max) )

simu_culture_mastery = data.frame(
  gdp_caus = 8.57296,
  pop_size_caus = 16.52218 ,
  cath_caus = 0.4120364,
  mastery_cult_caus = seq(3.393, 4.162, length.out = 10),
  hierarchy_cult_caus = 2.1638
)

simu_culture_hierarchy = data.frame(
  gdp_caus = 8.57296,
  pop_size_caus = 16.52218 ,
  cath_caus = 0.4120364,
  mastery_cult_caus = 3.767333,
  hierarchy_cult_caus = seq(1.411, 3.371, length.out = 10)
)

p1 = simulate_values(clust5_m3_cult, simu_culture_mastery, "mastery_cult_caus")
p2 = simulate_values(clust5_m3_cult, simu_culture_hierarchy, "hierarchy_cult_caus")
ggarrange(p1, p2, common.legend = T, legend = "bottom")
test = simulate_values(clust5_m3_cult, simu_culture_hierarchy, "hierarchy_cult_caus", predictonly = T)


