# Environment Imputation

source("Setup/AuxiliaryVariables.R")
source("Analyse/Environment/env_variables.R")


fa_data_oecd_frame_mice = fa_data_oecd_frame %>% 
  rename_all(funs(sub("_oecd_int_oecd_per_capita", "_oecd_num_env", .))) %>% 
  # include auxiliary and analyse variables
  left_join(aux_vars_qoc_env, by=c("country_text_id", "year")) %>%
  left_join(aux_vars_dmx_env, by=c("country_text_id", "year")) %>%
  left_join(analyse_vars, by=c("country_text_id", "year")) %>%
  # scale variables
  mutate_at(vars(ends_with("_env"), ends_with("num_aux")), scale) %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(starts_with("greenhouse"), starts_with("water"), starts_with("waste")), funs(
    "lag"= dplyr::lag(.,1),
    "lead"= dplyr::lead(.,1))
    ) %>%
  ungroup()  %>%
  # at least one of target variables not missing
  #filter(non_na_perc > 0, is.na(cluster_label_1st_fact_anal) == F) %>% 
  filter(non_na_perc > 0) %>% 
  filter(classification_context == "Deficient Democracy" |  classification_context == "Working Democracy") %>% 
  select(-non_na_perc, -country, -classification_context) %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1990 - 2016
  filter(year>=1990, year <= 2016) %>% 
  mutate(year_0 = year - min(year))


mice_data = as.data.frame(fa_data_oecd_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
library(corrplot)
corrplot(cor(fa_data_oecd_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-ends_with("lag"),-ends_with("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out <- amelia(mice_data, 
                m = nr_imputations, 
                ts = "year_0", 
                cs = "country_text_id", 
                noms=c("cluster_label_1st_fact_anal"), 
                polytime = 2,
                intercs = T,
                p2s = 2,
                parallel = "snow",
                ncpus	= nr_cores,
                empri = .05*nrow(mice_data)
                )

a.out

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



tscsPlot(a.out, cs = "AUS",
         var = "water_oecd_num_env")

tscsPlot(a.out, cs = "EST",
         var = "water_oecd_num_env")

tscsPlot(a.out, cs = c("ZAF"),
         var = "waste_oecd_num_env")
tscsPlot(a.out, cs = "DEU",
         var = "water_oecd_num_env")


## Combine Imputation into Long Format
imputed_env = mapply(cbind, a.out$imputations, ".imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(.imp = as.factor(.imp))



sample = c("AUS", "IND", "DEU")
imputed_env %>% 
  select(country_text_id, year_0, variable = water_oecd_num_env) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year_0, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))

