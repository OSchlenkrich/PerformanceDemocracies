# Environment Imputation

source("Analyse/PerformanceAreas/Environment/env_variables_v2.R")
source("Setup/AuxiliaryVariables.R")



fa_data_oecd_frame_mice = fa_data_oecd_frame %>% 
  select_at(vars(-ends_with("is_na"),  "missing_SUM")) %>% 
  rename_all(funs(sub("_oecd", "_oecd_num_env", .))) %>% 
  # include auxiliary and analyse variables
  left_join(aux_vars %>%  select_at(vars(country_text_id, year, 
                                         ends_with("gen_num_aux"), 
                                         matches("_env_"))
  ), by=c("country_text_id", "year")) %>%
  left_join(aux_vars_dmx_env, by=c("country_text_id", "year")) %>%
  left_join(analyse_vars, by=c("country_text_id", "year")) %>%
  # scale variables
  mutate_at(vars(ends_with("_env"), ends_with("num_aux")), scale) %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("num_env")), funs(
    "lag"= dplyr::lag(.,1),
    "lag2"= dplyr::lag(.,2),
    "lead"= dplyr::lead(.,1),
    "lead2"= dplyr::lead(.,2))
    ) %>%
  ungroup()  %>%
  # at least one of target variables not missing
  #filter(non_na_perc > 0, is.na(cluster_label_1st_fact_anal) == F) %>% 
  filter(missing_SUM != max(missing_SUM, na.rm=T)) %>% 
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  select(-missing_SUM, -country, -classification_core) %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1990 - 2016
  filter(year>=1990) %>% 
  mutate(year_0 = year - min(year))

# auxilary variables
fa_data_oecd_frame_mice %>% 
  select_at(vars(matches("_aux"))) %>% 
  names()


mice_data = as.data.frame(fa_data_oecd_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
corrplot(cor(fa_data_oecd_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))) %>% 
               rename_all(funs(gsub("_num_env","",.))) %>% 
               rename_all(funs(gsub("_num_aux","",.))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out <- amelia(mice_data, 
                m = nr_imputations, 
                ts = "year_0", 
                cs = "country_text_id", 
                polytime = 2,
                intercs = T,
                p2s = 2,
                parallel = "snow",
                ncpus	= nr_cores,
                empri = .05*nrow(mice_data)
                )

a.out

# saveRDS(a.out, "Analyse/PerformanceAreas/Environment/a.out_cap.RDS")
a.out = readRDS("Analyse/PerformanceAreas/Environment/a.out_cap.RDS")



if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disp_env = disperse(a.out, dims = 1, m = 5)
  
  # saveRDS(disp_env, "Analyse/PerformanceAreas/Environment/Diag/disp_env_cap.RDS")
  disp_env = readRDS("Analyse/PerformanceAreas/Environment/Diag/disp_env_cap.RDS")
  convergence_amelia(disp_env)  +
    ggtitle("Environmental Performance: Overdispersed Starting Values")
  
  
  # obs vs. imp
  ggarrange(
    compare.density_own(a.out, var = "water_ugdp_cap_oecd_num_env"),
    compare.density_own(a.out, var = "waste_ugdp_cap_oecd_num_env"),
    compare.density_own(a.out, var = c("GHG_ugdp_oecd_num_env")),
    compare.density_own(a.out, var = c("SOX_ugdp_oecd_num_env")),
    compare.density_own(a.out, var = c("NOX_ugdp_oecd_num_env")),
    compare.density_own(a.out, var = c("CO_ugdp_oecd_num_env")),
    common.legend = T,
    legend = "bottom"
  )
  
  
  # predictive capability
  
  water_imp_env = Amelia::overimpute(a.out, var = "water_ugdp_cap_oecd_num_env")
  # saveRDS(water_imp_env, "Analyse/PerformanceAreas/Environment/Diag/water_imp_env_cap.RDS")
  waste_imp_env = Amelia::overimpute(a.out, var = "waste_ugdp_cap_oecd_num_env")
  # saveRDS(waste_imp_env, "Analyse/PerformanceAreas/Environment/Diag/waste_imp_env_cap.RDS")
  GHG_imp_env = Amelia::overimpute(a.out, var = "GHG_ugdp_oecd_num_env")
  # saveRDS(GHG_imp_env, "Analyse/PerformanceAreas/Environment/Diag/GHG_imp_env.RDS")
  SOX_imp_env = Amelia::overimpute(a.out, var = "SOX_ugdp_oecd_num_env")
  # saveRDS(SOX_imp_env, "Analyse/PerformanceAreas/Environment/Diag/SOX_imp_env.RDS")
  NOX_imp_env = Amelia::overimpute(a.out, var = "NOX_ugdp_oecd_num_env")
  # saveRDS(NOX_imp_env, "Analyse/PerformanceAreas/Environment/Diag/NOX_imp_env.RDS")
  CO_imp_env = Amelia::overimpute(a.out, var = "CO_ugdp_oecd_num_env")
  # saveRDS(CO_imp_env, "Analyse/PerformanceAreas/Environment/Diag/CO_imp_env.RDS")
  
  ggarrange(
    overimpute_gglot(water_imp_env, "water_ugdp"),
    overimpute_gglot(waste_imp_env, "waste_ugdp"),
    overimpute_gglot(GHG_imp_env, "GHG_ugdp"),
    overimpute_gglot(SOX_imp_env, "SOX_ugdp"),
    overimpute_gglot(NOX_imp_env, "NOX_ugdp"),
    overimpute_gglot(CO_imp_env, "CO_ugdp")
  )
  
  
  
  par(mfrow=c(1,1))
  
  tscsPlot(a.out, cs = "AUS",
           var = "water_oecd_num_env")
  
  tscsPlot(a.out, cs = "EST",
           var = "water_oecd_num_env")
  
  tscsPlot(a.out, cs = c("ZAF"),
           var = "waste_oecd_num_env")
  tscsPlot(a.out, cs = "DEU",
           var = "water_oecd_num_env")

}

## Combine Imputation into Long Format
imputed_env = mapply(cbind, a.out$imputations, "imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(imp = as.factor(imp))

# Plausibilty: Relationship with GDP

imputed_env %>% 
  left_join(fa_data_oecd_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_oecd_frame, by=c("country_text_id", "year"))  %>% 
  mutate(is_na = ifelse(NOX_ugdp_oecd_is_na == 1, "Imputed", "Observed")) %>% 
  ggplot(aes(x=GDP_capita_wdi_gen_num_aux, y = NOX_ugdp_oecd_num_env, col=is_na)) +
  geom_point() +
  scale_color_grey(name="") +
  xlab("GDP per capita") +
  ylab("NOx") +
  theme_bw() +
  theme(legend.position = "bottom")


imputed_env_vars = imputed_env %>% 
  left_join(fa_data_oecd_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_oecd_frame, by=c("country_text_id", "year"))  %>% 
  select_at(vars(country, country_text_id, year, imp, 
                 ends_with("_oecd_num_env"), ends_with("oecd_is_na"), "missing_SUM"))


#
sample = c("AUS", "IND", "DEU", "BRA")
imputed_env_vars %>%
  #filter(imp==1) %>% 
  select(country_text_id, year, variable = GHG_ugdp_oecd_num_env) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))

