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

# auxilary variables
fa_data_soc_frame_mice %>% 
  select_at(vars(matches("_aux"))) %>% 
  names()

mice_data = as.data.frame(fa_data_soc_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
library(corrplot)
corrplot(cor(fa_data_soc_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-year, -year_0, -ends_with("lag"),-ends_with("lead"))) %>% 
               rename_all(funs(gsub("_num_soc","",.))), use="pairwise"))


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
                   polytime = 1,
                   intercs = T,
                   p2s = 2,
                   parallel = "snow",
                   ncpus	= nr_cores,
                   empri = .05*nrow(mice_data)
)

a.out_soc


# saveRDS(a.out_soc, "Analyse/PerformanceAreas/Social/a.out_soc.RDS")
a.out_soc = readRDS("Analyse/PerformanceAreas/Social/a.out_soc.RDS")



if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disp_soc = disperse(a.out_soc, dims = 1, m = 5)
  
  # saveRDS(disp_soc, "Analyse/PerformanceAreas/Social/Diag/disp_soc.RDS")
  disp_soc = readRDS("Analyse/PerformanceAreas/Social/Diag/disp_soc.RDS")
  convergence_amelia(disp_soc)  +
    ggtitle("Social Performance: Overdispersed Starting Values")
  
  # obs vs. imp
  # obs vs. imp
  ggarrange(
    compare.density_own(a.out_soc, var = "Combined_t_GI_num_soc"),
    compare.density_own(a.out_soc, var = "poverty9010_lis_num_soc"),
    compare.density_own(a.out_soc, var = c("poverty8020_lis_num_soc")),
    compare.density_own(a.out_soc, var = c("femlabor_wdi_num_soc")),
    compare.density_own(a.out_soc, var = c("gini_wdi_num_soc")),
    compare.density_own(a.out_soc, var = c("schools_gender_wdi_num_soc")),
    compare.density_own(a.out_soc, var = c("pub_serv_gender_vdem_num_soc")),
    compare.density_own(a.out_soc, var = c("pub_serv_social_vdem_num_soc")),
    compare.density_own(a.out_soc, var = c("v2dlunivl_vdem_num_soc")),
    common.legend = T,
    legend = "bottom"
  )
  
  # predictive capability
  
  GI_imp_soc = Amelia::overimpute(a.out_soc, var = "Combined_t_GI_num_soc")
  # saveRDS(GI_imp_soc, "Analyse/PerformanceAreas/Social/Diag/GI_imp_soc.RDS")
  pov90_imp_soc = Amelia::overimpute(a.out_soc, var = "poverty9010_lis_num_soc")
  # saveRDS(pov90_imp_soc, "Analyse/PerformanceAreas/Social/Diag/pov90_imp_soc.RDS")
  pov80_imp_soc = Amelia::overimpute(a.out_soc, var = "poverty8020_lis_num_soc")
  # saveRDS(pov80_imp_soc, "Analyse/PerformanceAreas/Social/Diag/pov80_imp_soc.RDS")
  fem_imp_soc = Amelia::overimpute(a.out_soc, var = "femlabor_wdi_num_soc")
  # saveRDS(fem_imp_soc, "Analyse/PerformanceAreas/Social/Diag/fem_imp_soc.RDS")
  schools_imp_soc = Amelia::overimpute(a.out_soc, var = "schools_gender_wdi_num_soc")
  # saveRDS(schools_imp_soc, "Analyse/PerformanceAreas/Social/Diag/schools_imp_soc.RDS")
  pubg_imp_soc = Amelia::overimpute(a.out_soc, var = "pub_serv_gender_vdem_num_soc")
  # saveRDS(pubg_imp_soc, "Analyse/PerformanceAreas/Social/Diag/pubg_imp_soc.RDS")
  pubs_imp_soc = Amelia::overimpute(a.out_soc, var = "pub_serv_social_vdem_num_soc")
  # saveRDS(pubs_imp_soc, "Analyse/PerformanceAreas/Social/Diag/pubs_imp_soc.RDS")
  univ_imp_soc = Amelia::overimpute(a.out_soc, var = "v2dlunivl_vdem_num_soc")
  # saveRDS(univ_imp_soc, "Analyse/PerformanceAreas/Social/Diag/univ_imp_soc.RDS")

  
  ggarrange(
    overimpute_gglot(GI_imp_soc, "Combined_GI"),
    overimpute_gglot(pov90_imp_soc, "poverty9010_lis"),
    overimpute_gglot(pov80_imp_soc, "poverty8010_lis"),
    overimpute_gglot(fem_imp_soc, "femlabor_wdi"),
    overimpute_gglot(schools_imp_soc, "schools_gender_wdi"),
    overimpute_gglot(pubg_imp_soc, "pub_serv_gender_vdem"),
    overimpute_gglot(pubs_imp_soc, "pub_serv_social_vdem"),
    overimpute_gglot(univ_imp_soc, "v2dlunivl_vdem")
  )
  
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


# Plausibilty: Relationship with GDP
imputed_soc %>% 
  left_join(fa_data_soc_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_soc_frame, by=c("country_text_id", "year"))  %>% 
  mutate(is_na = ifelse(gini_lis_is_na == 1, "Imputed", "Observed")) %>% 
  ggplot(aes(x=educ_equal_vdem_gen_num_aux, y = gini_wdi_num_soc, col=is_na)) +
  geom_point() +
  facet_wrap(is_na ~ .) +
  scale_color_grey(name="") +
  xlab("Educational Equality (V-Dem)") +
  ylab("Gini Index (WDI)") +
  theme_bw() +
  theme(legend.position = "bottom")

imputed_soc %>% 
  left_join(fa_data_soc_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_soc_frame, by=c("country_text_id", "year"))  %>% 
  mutate(is_na = ifelse(gini_lis_is_na == 1, "Imputed", "Observed")) %>% 
  ggplot(aes(x=health_vdem_soc_num_aux, y = gini_wdi_num_soc, col=is_na)) +
  geom_point() +
  facet_wrap(is_na ~ .) +
  scale_color_grey(name="") +
  xlab("Health Equality (V-Dem)") +
  ylab("Gini Index (WDI)") +
  theme_bw() +
  theme(legend.position = "bottom")



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

