library(Amelia)
names(fa_data_oecd_frame_mice)

fa_data_oecd_frame_mice = fa_data_oecd_frame %>% 
  rename_all(funs(sub("_oecd_int_oecd_per_capita", "_oecd_num_env", .))) %>% 
  # include auxiliary and analyse variables
  left_join(aux_vars_env, by=c("country_text_id", "year")) %>%
  left_join(aux_vars_dmx_env, by=c("country_text_id", "year")) %>%
  left_join(analyse_vars, by=c("country_text_id", "year")) %>%
  # scale variables
  mutate_at(vars(ends_with("_env"), ends_with("num_aux")), scale) %>% 
  mutate(year = year - min(year))  %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("_env")), funs(
    "lag"= dplyr::lag(.,1),
    "lead"= dplyr::lead(.,1))
    ) %>%
  ungroup()  %>%
  # at least one of target variables not missing
  filter(non_na_perc > 0) %>% 
  select(-non_na_perc, -country) %>% 
  dplyr::arrange(country_text_id, year)

mice_data = as.data.frame(fa_data_oecd_frame_mice) 
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })

# corrplot
library(corrplot)
corrplot(cor(fa_data_oecd_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-ends_with("lag"),-ends_with("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 20
a.out <- amelia(mice_data, 
                m = nr_imputations, 
                ts = "year", 
                cs = "country_text_id", 
                noms=c("cluster_label_1st_fact_anal"), 
                polytime = 2,
                intercs = T,
                p2s = 2,
                parallel = "snow",
                ncpus	= 10,
                empri = .05*nrow(mice_data)
                )

a.out

# convergence
disperse(a.out, dims = 1, m = 5)


# obs vs. imp
compare.density(a.out, var = "greenhouse_oecd_num_env")
compare.density(a.out, var = "water_oecd_num_env")
compare.density(a.out, var = "waste_oecd_num_env")

# predictive capability
overimpute(a.out, var = "greenhouse_oecd_num_env")
overimpute(a.out, var = "water_oecd_num_env")
overimpute(a.out, var = "waste_oecd_num_env")


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
  bind_rows()


sample = c("AUS", "IND", "DEU")
imputed_env %>% 
  select(country_text_id, year, variable = water_oecd_num_env) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))

