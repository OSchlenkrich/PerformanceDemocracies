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


mice_data = as.data.frame(fa_data_oecd_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
corrplot(cor(fa_data_oecd_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))


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

if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disperse(a.out, dims = 1, m = 5)
  
  # obs vs. imp
  par(mfrow=c(3,2))
  compare.density(a.out, var = c("greenhouse_oecd_num_env"), main= "Observed vs. Imputed Values of Greenhouse gas emissions")
  compare.density(a.out, var = c("sulphur_oecd_num_env"), main= "Observed vs. Imputed Values of Sulphur Oxides Emissions")
  compare.density(a.out, var = c("nitrogen_oecd_num_env"), main= "Observed vs. Imputed Values of Nitrogene Oxides Emissions")
  compare.density(a.out, var = c("co2_oecd_num_env"), main= "Observed vs. Imputed Values of CO2 emissions from fuel combustion")
  compare.density(a.out, var = "waste_oecd_num_env", main= "Observed vs. Imputed Values of Municipal waste")
  compare.density(a.out, var = "water_oecd_num_env", main= "Observed vs. Imputed Values of Water abstractions")
  
  # predictive capability
  par(mfrow=c(3,2))
  Amelia::overimpute(a.out, var = "greenhouse_oecd_num_env", main= "Observed vs. Imputed Values of Greenhouse gas emissions")
  Amelia::overimpute(a.out, var = "sulphur_oecd_num_env", main= "Observed vs. Imputed Values of Sulphur Oxides Emissions")
  Amelia::overimpute(a.out, var = "nitrogen_oecd_num_env", main= "Observed vs. Imputed Values of Nitrogene Oxides Emissions")
  Amelia::overimpute(a.out, var = "co2_oecd_num_env", main= "Observed vs. Imputed Values of CO2 emissions from fuel combustion")
  Amelia::overimpute(a.out, var = "waste_oecd_num_env", main= "Observed vs. Imputed Values of Municipal waste")
  Amelia::overimpute(a.out, var = "water_oecd_num_env", main= "Observed vs. Imputed Values of Water abstractions")
  
  
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

