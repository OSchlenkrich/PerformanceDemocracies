# Environment Imputation

source("Setup/AuxiliaryVariables.R")
source("Analyse/Social/soc_variables.R")


fa_data_soc_frame_mice = fa_data_soc_frame %>% 
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
  filter(non_na_perc > 0) %>% 
  filter(classification_context == "Deficient Democracy" |  classification_context == "Working Democracy") %>% 
  select(-non_na_perc, -country, -classification_context) %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1970 - 2016
  filter(year>=1970, year <= 2017) %>% 
  mutate(year_0 = year - min(year))


mice_data = as.data.frame(fa_data_soc_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
library(corrplot)
corrplot(cor(fa_data_soc_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-ends_with("lag"),-ends_with("lead"))), use="pairwise"))


# Missing Data Pattern
col_NR = fa_data_soc_frame_mice %>%  
  select_at(vars(ends_with("_soc"))) %>% 
  dim()

cols_n = fa_data_soc_frame_mice %>%  
  select_at(vars(ends_with("_soc"))) %>% 
  mutate_all(is.na) %>% 
  mutate_all(funs(if_else(. == T, 1,0))) %>%
  summarise_all(sum) %>% 
  melt()


rows_n = fa_data_soc_frame_mice %>%  
  select_at(vars(ends_with("_soc"))) %>% 
  mutate_all(is.na) %>% 
  mutate_all(funs(if_else(. == T, 1,0))) %>%
  unite("sum_n", sep="_") %>% 
  group_by(sum_n) %>% 
  summarise(rows_n=n())

rows_n_frame = fa_data_soc_frame_mice %>%  
  select_at(vars(ends_with("_soc"))) %>% 
  mutate_all(is.na) %>% 
  mutate_all(funs(if_else(. == T, 1,0))) %>%
  distinct() %>%
  unite("sum_n", sep="_", remove=T) %>%
  mutate(id = 1:dim(.)[1]) %>% 
  left_join(rows_n, by="sum_n") %>%
  separate(sum_n, into=paste("C_Miss",c(1:col_NR[2]), sep="_"), sep="_", convert=T) %>% 
  mutate(missing_vars = rowSums(select(., starts_with("C_Miss"))),
         id = as.factor(id),
         id = fct_reorder(id, -missing_vars)) %>% 
  select(id, rows_n, missing_vars) %>% 
  arrange(id)

fa_data_soc_frame_mice %>%  
  select_at(vars(ends_with("_soc"))) %>% 
  mutate_all(is.na) %>%
  distinct() %>% 
  mutate_all(funs(if_else(. == T, "missing","observed"))) %>%
  mutate(id = 1:dim(.)[1]) %>% 
  mutate(id = as.factor(id)) %>% 
  left_join(rows_n_frame, by="id") %>% 
  mutate(id = fct_reorder(id, -missing_vars)) %>% 
  select(-missing_vars) %>% 
  melt(id.vars=c("rows_n", "id")) %>% 
  mutate(variable = as.factor(variable),
         variable = fct_reorder(variable, value)) %>% 
  ggplot(aes(x = variable,
             y = id,
             fill = value)) +
  geom_tile(color="black") +
  scale_x_discrete(position = "top", name=NULL, expand = c(0.05,0.5)) +
  scale_y_discrete(expand = c(0.1,0), labels = rows_n_frame$rows_n) +
  ylab("No. of Missings") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "right") +
  annotate("text", x = cols_n$variable, label= cols_n$value, y=0.2) +
  annotate("text", x = 8.7, label= rows_n_frame$missing_vars, y=rows_n_frame$id) 


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_soc <- amelia(mice_data, 
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

a.out_soc

# convergence
par(mfrow=c(1,1))
disperse(a.out_soc, dims = 1, m = 5)

# obs vs. imp
par(mfrow=c(4,2), mar=c(4,4,4,4))
compare.density(a.out_soc, var = c("Unemployment_t_GI_num_soc"), main= "Observed vs. Imputed Values of Homicides per 100.000")
compare.density(a.out_soc, var = c("Pension_t_GI_num_soc"), main= "Observed vs. Imputed Values of Robberies per 100.000")
compare.density(a.out_soc, var = c("Sickness_t_GI_num_soc"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
compare.density(a.out_soc, var = c("gini_lis_num_soc"), main= "Observed vs. Imputed Values of General government spending – Public order and safety")
compare.density(a.out_soc, var = c("poverty9010_lis_num_soc"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
compare.density(a.out_soc, var = c("poverty8020_lis_num_soc"), main= "Observed vs. Imputed Values of General government spending – Public order and safety")
compare.density(a.out_soc, var = c("gini_wdi_num_soc"), main= "Observed vs. Imputed Values of General government spending – Public order and safety")


# predictive capability
par(mfrow=c(4,2), mar=c(4,4,4,4))
Amelia::overimpute(a.out_soc, var = "Unemployment_t_GI_num_soc", main= "Observed vs. Imputed Values of Homicides per 100.000")
Amelia::overimpute(a.out_soc, var = "Pension_t_GI_num_soc", main= "Observed vs. Imputed Values of Robberies per 100.000")
Amelia::overimpute(a.out_soc, var = "Sickness_t_GI_num_soc", main= "Observed vs. Imputed Values of Ratio prison population/convictions")
Amelia::overimpute(a.out_soc, var = "gini_lis_num_soc", main= "Observed vs. Imputed Values of General government spending – Public order and safety")
Amelia::overimpute(a.out_soc, var = "poverty9010_lis_num_soc", main= "Observed vs. Imputed Values of Robberies per 100.000")
Amelia::overimpute(a.out_soc, var = "poverty8020_lis_num_soc", main= "Observed vs. Imputed Values of Ratio prison population/convictions")
Amelia::overimpute(a.out_soc, var = "gini_wdi_num_soc", main= "Observed vs. Imputed Values of General government spending – Public order and safety")


tscsPlot(a.out_soc, cs = "AUS",
         var = "gini_wdi_num_soc")

tscsPlot(a.out_soc, cs = "EST",
         var = "water_oecd_num_env")

tscsPlot(a.out_soc, cs = c("ZAF"),
         var = "Unemployment_t_GI_num_soc")
tscsPlot(a.out_soc, cs = "DEU",
         var = "water_oecd_num_env")


## Combine Imputation into Long Format
imputed_soc = mapply(cbind, a.out_soc$imputations, ".imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(.imp = as.factor(.imp))



sample = c("SWE", "IND", "DEU", "USA", "POL")
imputed_soc %>% 
  select(country_text_id, year_0, variable = poverty8020_lis_num_soc) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year_0, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))

