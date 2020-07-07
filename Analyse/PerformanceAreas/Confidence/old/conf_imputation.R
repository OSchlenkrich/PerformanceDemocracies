# Confidence Imputation

source("Setup/AuxiliaryVariables.R")
source("Analyse/Confidence/conf_variables_v2.R")


fa_data_conf_frame_mice = fa_data_conf_frame %>% 

  # include auxiliary and analyse variables
  left_join(aux_vars %>%  select_at(vars(country_text_id, year_study = year, 
                                         ends_with("gen_num_aux"))
  ), by=c("country_text_id", "year_study")) %>%
  left_join(aux_vars_dmx_env %>%  select(country_text_id, year_study = year, classification_context), 
            by=c("country_text_id", "year_study")) %>%
  # left_join(analyse_vars %>%  dplyr::rename(year_study = year), by=c("country_text_id", "year_study")) %>%
  # scale variables
  mutate_at(vars(matches("num")), scale) %>% 
  # leads and lags for better predicition
  ungroup()  %>%
  # at least one of target variables not missing
  filter(non_na_perc > 0) %>% 
  filter(classification_context == "Deficient Democracy" |  classification_context == "Working Democracy") %>% 
  select(-non_na_perc, -country, -classification_context, -survey) %>% 
  dplyr::arrange(country_text_id, year_study) %>% 
  # analyse time range: 1970 - 2016
  filter(year_study >= 1970, year_study <= 2017)  %>%
  # id is for mice which cannot handle characters
  mutate(id = group_indices(., country_text_id))

# remove missings from 2nd level variables
fa_data_conf_frame_w2nd_mice = fa_data_conf_frame_mice %>% 
  filter(is.na(GDP_capita_wdi_gen_num_aux) == F, is.na(educ_equal_vdem_gen_num_aux) == F)

mice_data = as.data.frame(fa_data_conf_frame_w2nd_mice) %>% 
  select(-country_text_id,  -year_study) 
  # dplyr::sample_frac(0.1)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })



# corrplot
library(corrplot)
corrplot(cor(fa_data_conf_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-id, -year_study)), use="pairwise", method="spearman"))




# Missing Data Pattern
# see conf_variables.R


# Imputation
# includes: FE, Polynomial
library(mi)
mdf_conf = missing_data.frame(mice_data)
mdf_conf = mi::change(mdf_conf, y = "id" , what = "type", to = "group")

show(mdf_conf)

options(mc.cores = 5)
imputations_conf = mi(mdf_conf, n.iter = 1, n.chains = 5, max.minutes = 20)

round(mipply(imputations_conf, mean, to.matrix = TRUE), 3)
Rhats(imputations_conf)

# Diagnostics
plot(imputations_conf)




pred_matrix = make.predictorMatrix(mice_data)
pred_matrix[, "id"] = -2
pred_matrix

meth_matrix = make.method(mice_data, defaultMethod = "2l.pmm")

#meth_matrix[c("GDP_capita_wdi_gen_num_aux", "educ_equal_vdem_gen_num_aux")] = ""
meth_matrix

mice_data_umx = as.data.frame(fa_data_conf_frame_w2nd_mice) %>% 
  select_at(vars(matches("conf"), -conf_press_ord_aux_ivs)) 

library(umx)
umxEFA(mice_data_umx, 1, scores = "Regression", minManifests = 2)


imp <- parlmice(nhanes,
                maxit=1,
                n.core = 1)

imp <- mice(mice_data, 
            method = meth_matrix, 
            predictorMatrix = pred_matrix, 
            n.imp.core = 1,
            maxit=1)


mice::complete(imp, 1)

densityplot(imp)

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

