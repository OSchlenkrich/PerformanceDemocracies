# Social Imputation

source("Analyse/PerformanceAreas/Social/soc_variables.R")
source("Setup/AuxiliaryVariables.R")


fa_data_soc_frame_mice = fa_data_soc_frame %>% 
  select_at(vars(-ends_with("is_na"),  "missing_SUM")) %>% 
  rename_all(funs(sub("_GI", "_GI_num_soc", .))) %>% 
  rename_all(funs(sub("_lis", "_lis_num_soc", .))) %>% 
  rename_all(funs(sub("_wdi", "_wdi_num_soc", .))) %>%
  rename_all(funs(sub("_vdem", "_vdem_num_soc", .))) %>%
  # include auxiliary and analyse variables
  left_join(aux_vars %>%  select_at(vars(country_text_id, year, 
                                         ends_with("gen_num_aux"), 
                                         ends_with("soc_num_aux"))
  ), by=c("country_text_id", "year")) %>%
  left_join(aux_vars_dmx_env, by=c("country_text_id", "year")) %>%
  left_join(analyse_vars, by=c("country_text_id", "year")) %>%
  # scale variables
  mutate_at(vars(ends_with("_soc"), ends_with("num_aux")), scale) %>% 
  # leads and lags for better predicition
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("_soc")), funs(
    "lag"= dplyr::lag(.,1),
    "lead"= dplyr::lead(.,1))
  ) %>%
  ungroup()  %>%
  # at least one of target variables not missing
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1970
  filter(year>=1970) %>% 
  mutate(year_0 = year - min(year)) %>% 
  filter(missing_SUM != max(missing_SUM, na.rm=T)) %>% 
  select(-country, -classification_core, -missing_SUM) 


mice_data = as.data.frame(fa_data_soc_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
library(corrplot)
corrplot(cor(fa_data_soc_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-year, -year_0, -ends_with("lag"),-ends_with("lead"))), use="pairwise"))


# Missing Data Pattern

missd_pattern(fa_data_soc_frame_mice %>%  
                select_at(vars(ends_with("_soc"))))            


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_soc <- amelia(mice_data, 
                   m = nr_imputations, 
                   ts = "year_0", 
                   cs = "country_text_id", 
                   noms=c("cluster_label_1st_fact_anal"), 
                   polytime = 1,
                   intercs = T,
                   p2s = 2,
                   parallel = "snow",
                   ncpus	= nr_cores,
                   empri = .05*nrow(mice_data)
)

a.out_soc

if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disperse(a.out_soc, dims = 1, m = 5)
  
  # obs vs. imp
  par(mfrow=c(4,2), mar=c(4,4,4,4))
  compare.density(a.out_soc, var = c("Unemployment_t_GI_num_soc"), main= "Observed vs. Imputed Values of Unemployment Generosity Index")
  compare.density(a.out_soc, var = c("Pension_t_GI_num_soc"), main= "Observed vs. Imputed Values of Pension Generosity Index")
  compare.density(a.out_soc, var = c("Sickness_t_GI_num_soc"), main= "Observed vs. Imputed Values of Sickness Generosity Index")
  compare.density(a.out_soc, var = c("gini_lis_num_soc"), main= "Observed vs. Imputed Values of Gini Coefficient (LIS)")
  compare.density(a.out_soc, var = c("poverty9010_lis_num_soc"), main= "Observed vs. Imputed Values of Percentile Ratio (90/10) (LIS)")
  compare.density(a.out_soc, var = c("poverty8020_lis_num_soc"), main= "Observed vs. Imputed Values of Percentile Ratio (80/20) (LIS)")
  compare.density(a.out_soc, var = c("v2dlunivl_vdem_num_soc"), main= "Observed vs. Imputed Values of Means-tested v. universalistic policy")
  compare.density(a.out_soc, var = c("gini_wdi_num_soc"), main= "Observed vs. Imputed Values of Gini Coefficient (WDI)")
  
  
  # predictive capability
  par(mfrow=c(4,2), mar=c(4,4,4,4))
  Amelia::overimpute(a.out_soc, var = "Unemployment_t_GI_num_soc", main= "Observed vs. Imputed Values of Unemployment Generosity Index")
  Amelia::overimpute(a.out_soc, var = "Pension_t_GI_num_soc", main= "Observed vs. Imputed Values of Pension Generosity Index")
  Amelia::overimpute(a.out_soc, var = "Sickness_t_GI_num_soc", main= "Observed vs. Imputed Values of Sickness Generosity Index")
  Amelia::overimpute(a.out_soc, var = "gini_lis_num_soc", main= "Observed vs. Imputed Values of Gini Coefficient (LIS)")
  Amelia::overimpute(a.out_soc, var = "poverty9010_lis_num_soc", main= "Observed vs. Imputed Values of Imputed Values of Percentile Ratio (90/10) (LIS)")
  Amelia::overimpute(a.out_soc, var = "poverty8020_lis_num_soc", main= "Observed vs. Imputed Values of Imputed Values of Percentile Ratio (80/20) (LIS)")
  Amelia::overimpute(a.out_soc, var = "v2dlunivl_vdem_num_soc", main= "Observed vs. Imputed Values of Means-tested v. universalistic policy")
  Amelia::overimpute(a.out_soc, var = "gini_wdi_num_soc", main= "Observed vs. Imputed Values of Gini Coefficient (WDI)")
  
  par(mfrow=c(1,1))
  
  tscsPlot(a.out_soc, cs = "AUS",
           var = "gini_wdi_num_soc")
  
  tscsPlot(a.out_soc, cs = "EST",
           var = "water_oecd_num_env")
  
  tscsPlot(a.out_soc, cs = c("ZAF"),
           var = "Unemployment_t_GI_num_soc")
  tscsPlot(a.out_soc, cs = "DEU",
           var = "water_oecd_num_env")
}

## Combine Imputation into Long Format
imputed_soc = mapply(cbind, a.out_soc$imputations, "imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(imp = as.factor(imp))

imputed_soc_vars = imputed_soc %>% 
  left_join(fa_data_soc_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_soc_frame, by=c("country_text_id", "year"))  %>% 
  # mutate_at(vars(ends_with("_oecd_num_eco")), funs(ifelse(missing_OECD == max(missing_OECD), NA, .))) %>% 
  # mutate_at(vars(ends_with("_wdi_num_eco")), funs(ifelse(missing_WDI == max(missing_WDI), NA, .)))  %>% 
  select_at(vars(country, country_text_id, year, imp, 
                 ends_with("_GI_num_soc"), ends_with("_lis_num_soc"), ends_with("_wdi_num_soc"), ends_with("_vdem_num_soc"), 
                 ends_with("GI_is_na"), ends_with("lis_is_na"), ends_with("wdi_is_na"), ends_with("vdem_is_na"), "missing_SUM"))  
  # select_at(vars(-starts_with("gini_wdi")))


imputed_soc_vars %>%
  filter(imp == 1) %>%
  group_by(country_text_id) %>%
  filter(year >= 1950) %>%
  right_join(fa_data_soc_frame %>%  select(country_text_id, year), by=c("country_text_id", "year")) %>%
  group_by(year) %>%
  select_at(vars(ends_with("_GI_num_soc"), ends_with("_lis_num_soc"), ends_with("_wdi_num_soc"), ends_with("_vdem_num_soc"))) %>%
  summarise_all(pMiss_01) %>%
  melt(id.vars="year") %>%
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(name=NULL, breaks=seq(0,1, 0.25), limit=c(0,1), labels=percent)  +
  scale_x_continuous(name=NULL, breaks=seq(1950,2020, 10)) +
  scale_fill_grey(start = 0.4, end = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") +
  ggtitle("Missings in Democracy Profile Sample")


sample = c("DEU")
imputed_soc_vars %>%
  select_at(vars(country_text_id, year, ends_with("_GI_num_soc"), ends_with("_lis_num_soc"), ends_with("_wdi_num_soc"), ends_with("_vdem_num_soc"))) %>%
  filter(country_text_id %in% sample) %>%
  pivot_longer(cols=ends_with("num_soc")) %>%
  group_by(country_text_id, name, year) %>%
  summarise(mean = mean(value),
            lower = quantile(value, 0.025, na.rm=T),
            upper = quantile(value, 0.975, na.rm=T)) %>%
  ggplot(aes(x=year, y=mean, col=name)) +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))

