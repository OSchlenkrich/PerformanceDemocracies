# Domestic Security Imputation

source("Setup/AuxiliaryVariables.R")
source("Analyse/DomesticSecurity/ds_variables.R")


fa_data_ds_frame_mice = fa_data_ds_frame %>% 
  rename_all(funs(sub("_oecd", "_oecd_num_ds", .))) %>% 
  rename_all(funs(sub("_unodc", "_unodc_num_ds", .))) %>% 
  # include auxiliary and analyse variables
  left_join(aux_vars %>%  select_at(vars(country_text_id, year, 
                                             ends_with("gen_num_aux"), 
                                             ends_with("ds_num_aux"))
                                        ), by=c("country_text_id", "year")) %>%
  left_join(aux_vars_dmx_env, by=c("country_text_id", "year")) %>%
  left_join(analyse_vars, by=c("country_text_id", "year")) %>%
  # scale variables
  mutate_at(vars(ends_with("_ds"), ends_with("num_aux")), scale) %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("_ds")), funs(
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
  filter(year>=1990, year <= 2017) %>% 
  mutate(year_0 = year - min(year))


mice_data = as.data.frame(fa_data_ds_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
library(corrplot)
corrplot(cor(fa_data_ds_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-ends_with("lag"),-ends_with("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_ds <- amelia(mice_data, 
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

a.out_ds

if (Plot_Impu == T) {
# convergence
par(mfrow=c(1,1))
disperse(a.out_ds, dims = 1, m = 5)

# obs vs. imp
par(mfrow=c(2,2), mar=c(4,4,4,4))
compare.density(a.out_ds, var = c("hom_rate_unodc_num_ds"), main= "Observed vs. Imputed Values of Homicides per 100.000")
compare.density(a.out_ds, var = c("rob_rate_unodc_num_ds"), main= "Observed vs. Imputed Values of Robberies per 100.000")
compare.density(a.out_ds, var = c("incarc_ratio_unodc_num_ds"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
compare.density(a.out_ds, var = c("order_safety_gdp_oecd_num_ds"), main= "Observed vs. Imputed Values of General government spending – Public order and safety")



# predictive capability
par(mfrow=c(2,2))
Amelia::overimpute(a.out_ds, var = "hom_rate_unodc_num_ds", main= "Observed vs. Imputed Values of Homicides per 100.000")
Amelia::overimpute(a.out_ds, var = "rob_rate_unodc_num_ds", main= "Observed vs. Imputed Values of Robberies per 100.000")
Amelia::overimpute(a.out_ds, var = "incarc_ratio_unodc_num_ds", main= "Observed vs. Imputed Values of Ratio prison population/convictions")
Amelia::overimpute(a.out_ds, var = "order_safety_gdp_oecd_num_ds", main= "Observed vs. Imputed Values of General government spending – Public order and safety")



tscsPlot(a.out_ds, cs = "AUS",
         var = "order_safety_gdp_oecd_num_ds")

tscsPlot(a.out_ds, cs = "EST",
         var = "water_oecd_num_env")

tscsPlot(a.out_ds, cs = c("ZAF"),
         var = "waste_oecd_num_env")
tscsPlot(a.out_ds, cs = "DEU",
         var = "water_oecd_num_env")
}


## Combine Imputation into Long Format
imputed_ds = mapply(cbind, a.out_ds$imputations, ".imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(.imp = as.factor(.imp))



sample = c("AUS", "IND", "DEU", "USA")
imputed_ds %>% 
  select(country_text_id, year_0, variable = incarc_ratio_unodc_num_ds) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year_0, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))

