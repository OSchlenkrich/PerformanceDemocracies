# Economic Imputation

source("Analyse/Economy/eco_variables.R")
source("Setup/AuxiliaryVariables.R")

fa_data_oecd_frame_mice = fa_data_eco_frame %>% 
  select_at(vars(-ends_with("is_na"), "missing_OECD", -"missing_WDI")) %>% 
  rename_all(funs(sub("_oecd", "_oecd_num_eco", .))) %>% 
  rename_all(funs(sub("_wdi", "_wdi_num_eco", .))) %>% 
  rename_all(funs(sub("_imf", "_imf_num_eco", .))) %>% 
  
  # include auxiliary and analyse variables
  left_join(aux_vars %>%  select_at(vars(country_text_id, year, 
                                         matches("_eco"))), 
            by=c("country_text_id", "year")) %>%
  left_join(aux_vars_dmx_env, by=c("country_text_id", "year")) %>%
  left_join(analyse_vars, by=c("country_text_id", "year")) %>%
  
  # scale variables
  mutate_at(vars(ends_with("_eco"), ends_with("num_aux")), scale) %>% 
  
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("_eco")), funs(
    "lag"= dplyr::lag(.,1),
    "lead"= dplyr::lead(.,1))
    ) %>%
  ungroup()  %>%

  filter(missing_OECD != max(missing_OECD, na.rm=T)) %>% 
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  select(-country, -classification_core, -missing_OECD) %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1970 - 2017
  filter(year>=1970) %>% 
  mutate(year_0 = year - min(year))


mice_data = as.data.frame(fa_data_oecd_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })

# corrplot
par(mfrow=c(1,1))
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

if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disperse(a.out, dims = 1, m = 5)
  
  # obs vs. imp
  par(mfrow=c(3,2))
  compare.density(a.out, var = c("GDP_capita_oecd_num_eco"), main= "Observed vs. Imputed Values of GDP per capita (OECD)")
  compare.density(a.out, var = c("Inflation_oecd_num_eco"), main= "Observed vs. Imputed Values of Inflation (OECD)")
  compare.density(a.out, var = c("Interest_oecd_num_eco"), main= "Observed vs. Imputed Values of Interest Rate (OECD)")
  compare.density(a.out, var = c("Balance_oecd_num_eco"), main= "Observed vs. Imputed Values of Trade Balance (OECD)")
  compare.density(a.out, var = c("Unemployment_pr_oecd_num_eco"), main= "Observed vs. Imputed Values of Unemployment (OECD)")

   # predictive capability
  par(mfrow=c(3,2))
  Amelia::overimpute(a.out, var = "GDP_capita_oecd_num_eco", main= "Observed vs. Imputed Values of GDP per capita (OECD)")
  Amelia::overimpute(a.out, var = "Inflation_oecd_num_eco", main= "Observed vs. Imputed Values of Inflation (OECD)")
  Amelia::overimpute(a.out, var = "Interest_oecd_num_eco", main= "Observed vs. Imputed Values of Interest Rate (OECD)")
  Amelia::overimpute(a.out, var = "Balance_oecd_num_eco", main= "Observed vs. Imputed Values of Current Account Balance (OECD)")
  Amelia::overimpute(a.out, var = "Unemployment_pr_oecd_num_eco", main= "Observed vs. Imputed Values of Unemployment (OECD)")


  par(mfrow=c(1,1))
  tscsPlot(a.out, cs = "IND",
           var = "GDP_capita_oecd_num_eco")
  
  tscsPlot(a.out, cs = "EST",
           var = "GDP_capita_oecd_num_eco")
  
  tscsPlot(a.out, cs = c("ZAF"),
           var = "GDP_capita_oecd_num_eco")
  tscsPlot(a.out, cs = "DEU",
           var = "GDP_capita_oecd_num_eco")

}

## Combine Imputation into Long Format

imputed_oecd_eco = mapply(cbind, a.out$imputations, ".imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(.imp = as.factor(.imp)) 

imputed_oecd_eco_vars = imputed_oecd_eco %>% 
  left_join(fa_data_oecd_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_eco_frame, by=c("country_text_id", "year"))  %>% 
  mutate_at(vars(ends_with("_oecd_num_eco")), funs(ifelse(missing_OECD == max(missing_OECD), NA, .))) %>% 
  mutate_at(vars(ends_with("_wdi_num_eco")), funs(ifelse(missing_WDI == max(missing_WDI), NA, .)))  %>% 
  select_at(vars(country, country_text_id, year, .imp, 
                 ends_with("_oecd_num_eco"), ends_with("_wdi_num_eco"), ends_with("_imf_num_eco"), ends_with("oecd_is_na"), ends_with("imf_is_na"), "missing_OECD"))

  
#
sample = c("DEU")
imputed_oecd_eco_vars %>% 
  select_at(vars(country_text_id, year, ends_with("_oecd_num_eco"), ends_with("_imf_num_eco"))) %>% 
  filter(country_text_id %in% sample) %>% 
  pivot_longer(cols=ends_with("num_eco")) %>% 
  group_by(country_text_id, name, year) %>% 
  summarise(mean = mean(value),
            lower = quantile(value, 0.025, na.rm=T),
            upper = quantile(value, 0.975, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean, col=name)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))



