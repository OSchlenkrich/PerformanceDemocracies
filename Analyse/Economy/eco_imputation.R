# Economic Imputation

source("Analyse/Economy/eco_variables.R")
source("Setup/AuxiliaryVariables.R")

fa_data_oecd_frame_mice = fa_data_oecd_frame %>% 
  # WDI is not factorable, so I exlcude it
  # select_at(vars(-ends_with("wdi"))) %>% 
  rename_all(funs(sub("_oecd", "_oecd_num_eco", .))) %>% 
  rename_all(funs(sub("_wdi", "_wdi_gen_num_aux", .))) %>% 
  # include auxiliary and analyse variables
  left_join(aux_vars %>%  select_at(vars(country_text_id, year, 
                                         matches("_eco"))
  ), by=c("country_text_id", "year")) %>%
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
  # at least one of target variables not missing
  #filter(non_na_perc > 0, is.na(cluster_label_1st_fact_anal) == F) %>% 
  filter(non_na_perc > 0) %>% 
  filter(classification_context == "Deficient Democracy" |  classification_context == "Working Democracy") %>% 
  select(-non_na_perc, -country, -classification_context) %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1970 - 2017
  filter(year>=1970) %>% 
  mutate(year_0 = year - min(year)) %>% 
  select(-Debt_wdi_gen_num_aux)


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
  compare.density(a.out, var = c("GDP_capita_oecd_num_eco"), main= "Observed vs. Imputed Values of GDP per capita")
  compare.density(a.out, var = c("Inflation_oecd_num_eco"), main= "Observed vs. Imputed Values of Inflation")
  compare.density(a.out, var = c("Interest_oecd_num_eco"), main= "Observed vs. Imputed Values of Interest Rate")
  compare.density(a.out, var = c("Balance_oecd_num_eco"), main= "Observed vs. Imputed Values of Trade Balance")
  compare.density(a.out, var = c("Unemployment_pr_oecd_num_eco"), main= "Observed vs. Imputed Values of Unemployment")
  compare.density(a.out, var = c("Invest_oecd_num_eco"), main= "Observed vs. Imputed Values of Investment")
  
  # predictive capability
  par(mfrow=c(3,2))
  Amelia::overimpute(a.out, var = "GDP_capita_oecd_num_eco", main= "Observed vs. Imputed Values of GDP per capita")
  Amelia::overimpute(a.out, var = "Inflation_oecd_num_eco", main= "Observed vs. Imputed Values of Inflation")
  Amelia::overimpute(a.out, var = "Interest_oecd_num_eco", main= "Observed vs. Imputed Values of Interest Rate")
  Amelia::overimpute(a.out, var = "Balance_oecd_num_eco", main= "Observed vs. Imputed Values of Trade Balance")
  Amelia::overimpute(a.out, var = "Unemployment_pr_oecd_num_eco", main= "Observed vs. Imputed Values of Unemployment")
  Amelia::overimpute(a.out, var = "Invest_oecd_num_eco", main= "Observed vs. Imputed Values of Investment")
  
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
imputed_eco = mapply(cbind, a.out$imputations, ".imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(.imp = as.factor(.imp))



sample = c("AUS", "IND", "DEU")
imputed_eco %>% 
  select(country_text_id, year_0, variable = GDP_capita_oecd_num_eco) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year_0, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))


###

sample = c("AUS", "IND", "DEU", "SWE")
imputed_eco %>% 
  select_at(vars(country_text_id, year_0, ends_with("_eco"))) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise_if(is.numeric, funs(mean = mean(.))) %>% 
  melt(id.vars=c("country_text_id", "year_0")) %>% 
  ggplot(aes(x=year_0, y=value, col=country_text_id)) + 
  geom_line() +
  #geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  facet_wrap(variable ~ .)
