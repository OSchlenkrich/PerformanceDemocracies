# Domestic Security Imputation

source("Analyse/PerformanceAreas/DomesticSecurity/ds_variables.R")
source("Setup/AuxiliaryVariables.R")

fa_data_ds_frame_mice = fa_data_ds_frame %>% 
  select_at(vars(-ends_with("is_na"),  "missing_SUM")) %>% 
  rename_all(funs(sub("_oecd", "_oecd_num_ds", .))) %>% 
  rename_all(funs(sub("_unodc", "_unodc_num_ds", .))) %>% 
  rename_all(funs(sub("_gcr", "_gcr_num_ds", .))) %>% 
  rename_all(funs(sub("_gwp", "_gwp_num_ds", .))) %>% 
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
    #"lag2"= dplyr::lag(.,2),
    "lead"= dplyr::lead(.,1),
    #"lead2"= dplyr::lead(.,2)
    )
  ) %>%
  ungroup()  %>%
  # at least one of target variables not missing
  filter(missing_SUM != max(missing_SUM, na.rm=T)) %>% 
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  select(-country, -classification_core, -missing_SUM) %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1990 - 2016
  filter(year>=1990) %>% 
  mutate(year_0 = year - min(year))

# auxilary variables
fa_data_ds_frame_mice %>% 
  select_at(vars(matches("_aux"))) %>% 
  names()


mice_data = as.data.frame(fa_data_ds_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
library(corrplot)
corrplot(cor(fa_data_ds_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-matches("lag"),-matches("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_ds <- amelia(mice_data, 
                m = nr_imputations, 
                ts = "year_0", 
                cs = "country_text_id", 
                polytime = 1,
                intercs = T,
                p2s = 2,
                parallel = "snow",
                ncpus	= nr_cores,
                empri = .05*nrow(mice_data)
)

a.out_ds



# saveRDS(a.out_ds, "Analyse/PerformanceAreas/DomesticSecurity/a.out_ds_trust.RDS")
a.out_ds = readRDS("Analyse/PerformanceAreas/DomesticSecurity/a.out_ds_trust.RDS")


if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disp_ds = disperse(a.out_ds, dims = 1, m = 5)
  
  # saveRDS(disp_ds, "Analyse/PerformanceAreas/DomesticSecurity/Diag/disp_ds_v2.RDS")
  disp_ds = readRDS("Analyse/PerformanceAreas/DomesticSecurity/Diag/disp_ds_v2.RDS")
  
  convergence_amelia(disp_ds)  +
    ggtitle("Domestic Security Performance: Overdispersed Starting Values")
  
  
  # obs vs. imp
  ggarrange(
    compare.density_own(a.out_ds, var = "order_safety_gdpcapita_oecd_num_ds"),
    compare.density_own(a.out_ds, var = "hom_rate_unodc_num_ds"),
    compare.density_own(a.out_ds, var = "theft_rate_unodc_num_ds"), 
    compare.density_own(a.out_ds, var = "reliab_police_gcr_num_ds"),
    #compare.density_own(a.out_ds, var = "trust_gwp_num_ds"),
    common.legend = T,
    legend = "bottom"
  )
  
  # predictive capability
  order_imp_ds = Amelia::overimpute(a.out_ds, var = "order_safety_gdpcapita_oecd_num_ds")
  # saveRDS(order_imp_ds, "Analyse/PerformanceAreas/DomesticSecurity/Diag/order_imp_ds.RDS")
  hom_imp_ds = Amelia::overimpute(a.out_ds, var = "hom_rate_unodc_num_ds")
  # saveRDS(hom_imp_ds, "Analyse/PerformanceAreas/DomesticSecurity/Diag/hom_imp_ds.RDS")
  theft_imp_ds = Amelia::overimpute(a.out_ds, var = "theft_rate_unodc_num_ds")
  # saveRDS(theft_imp_ds, "Analyse/PerformanceAreas/DomesticSecurity/Diag/theft_imp_ds.RDS")
  reliab_imp_ds = Amelia::overimpute(a.out_ds, var = "reliab_police_gcr_num_ds")
  # saveRDS(reliab_imp_ds, "Analyse/PerformanceAreas/DomesticSecurity/Diag/reliab_imp_ds.RDS")
  # trust_imp_ds = Amelia::overimpute(a.out_ds, var = "trust_gwp_num_ds")
  # saveRDS(trust_imp_ds, "Analyse/PerformanceAreas/DomesticSecurity/Diag/trust_imp_ds.RDS")
  
  
  ggarrange(
    overimpute_gglot(order_imp_ds, "order_safety_gdpcapita_oecd"),
    overimpute_gglot(hom_imp_ds, "hom_rate_unodc"),
    overimpute_gglot(theft_imp_ds, "theft_rate_unodc"),
    overimpute_gglot(reliab_imp_ds, "reliab_police_gcr")
    #overimpute_gglot(trust_imp_ds, "trust_gwp")
  )
  
  
  tscsPlot(a.out_ds, cs = "AUS",
           var = "order_safety_gdp_oecd_num_ds")
  
  tscsPlot(a.out_ds, cs = "EST",
           var = "order_safety_gdp_oecd_num_ds")
  
  tscsPlot(a.out_ds, cs = c("ZAF"),
           var = "order_safety_gdp_oecd_num_ds")
  tscsPlot(a.out_ds, cs = "DEU",
           var = "order_safety_gdp_oecd_num_ds")
}


## Combine Imputation into Long Format
imputed_ds = mapply(cbind, a.out_ds$imputations, "imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(imp = as.factor(imp))


imputed_ds_vars = imputed_ds %>% 
  left_join(fa_data_ds_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_ds_frame, by=c("country_text_id", "year"))  %>% 
  select_at(vars(country, country_text_id, year, imp, 
                 ends_with("_oecd_num_ds"), ends_with("_unodc_num_ds"), ends_with("_gcr_num_ds"), ends_with("_gwp_num_ds"),
                 ends_with("oecd_is_na"), ends_with("unodc_is_na"), ends_with("_gcr_is_na"), ends_with("_gwp_is_na"),
                 "missing_SUM"))


sample = c("AUS", "IND", "DEU", "USA")
imputed_ds_vars %>% 
  select(country_text_id, year, variable = order_safety_gdp_perc_oecd_num_ds) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))

