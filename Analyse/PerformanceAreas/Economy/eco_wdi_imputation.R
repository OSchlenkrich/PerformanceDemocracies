# Economic Imputation

source("Analyse/PerformanceAreas/Economy/eco_variables.R")
source("Setup/AuxiliaryVariables.R")

fa_data_wdi_frame_mice = fa_data_eco_frame %>% 
  select_at(vars(-ends_with("is_na"),  "missing_SUM")) %>% 
  rename_all(funs(sub("_oecd", "_oecd_num_eco", .))) %>% 
  rename_all(funs(sub("_wdi", "_wdi_num_eco", .))) %>% 
  rename_all(funs(sub("_imf", "_imf_num_eco", .)))%>% 
  rename_all(funs(sub("_pwt", "_pwt_num_eco", .))) %>% 
  rename_all(funs(sub("gen_num_wdi_num_eco", "eco_num_aux", .))) %>% 
  
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
  
  filter(classification_core == "Deficient Democracy" |  classification_core == "Working Democracy") %>% 
  dplyr::arrange(country_text_id, year) %>% 
  # analyse time range: 1970 - 2017
  filter(year>=1980) %>% 
  mutate(year_0 = year - min(year)) %>% 
  filter(missing_SUM != max(missing_SUM, na.rm=T)) %>% 
  select(-country, -classification_core, -missing_SUM) 

# names of auxilary variables
fa_data_wdi_frame_mice %>% 
  select_at(vars(matches("_aux"))) %>% 
  names()


mice_data = as.data.frame(fa_data_wdi_frame_mice) %>% 
  select(-year)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })

# corrplot
par(mfrow=c(1,1))
corrplot(cor(fa_data_wdi_frame_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-ends_with("lag"),-ends_with("lead"))), use="pairwise"))


# Imputation
# includes: FE, Polynomial

nr_imputations = 10
nr_cores = 10

a.out_wdi <- amelia(mice_data, 
                    m = nr_imputations, 
                    ts = "year_0", 
                    cs = "country_text_id", 
                    polytime = 1,
                    intercs = T,
                    p2s = 2,
                    parallel = "snow",
                    ncpus	= nr_cores,
                    empri = .025*nrow(mice_data)
)

a.out_wdi

# saveRDS(a.out_wdi, "Analyse/PerformanceAreas/Economy/a.out_wdi.RDS")
a.out_wdi = readRDS("Analyse/PerformanceAreas/Economy/a.out_wdi.RDS")


if (Plot_Impu == T) {
  # convergence
  disp_eco = disperse(a.out_wdi, dims = 1, m = 5)
  # saveRDS(disp_eco, "Analyse/PerformanceAreas/Economy/Diag/disp_eco.RDS")
  disp_eco = readRDS("Analyse/PerformanceAreas/Economy/Diag/disp_eco.RDS")
  convergence_amelia(disp_eco)  +
    ggtitle("Economic Performance: Overdispersed Starting Values")
  
  # obs vs. imp
  ggarrange(
    compare.density_own(a.out_wdi, var = "investment_wdi_num_eco", main= "GDP per capita (WDI)"),
    compare.density_own(a.out_wdi, var = "Balance_wdi_num_eco", main= "Inflation (WDI)"),
    compare.density_own(a.out_wdi, var = c("consumption_cap_wdi_num_eco"), main= " Interest Rate (WDI)"),
    compare.density_own(a.out_wdi, var = c("unemployment_pr_imf_num_eco"), main= " Unemployment (WDI)"),
    compare.density_own(a.out_wdi, var = c("gdp_cap_ppp_imf_num_eco"), main= "Unemployment (WDI)"),
    compare.density_own(a.out_wdi, var = c("inflation_imf_num_eco"), main= "nemployment (WDI)"),
    compare.density_own(a.out_wdi, var = c("gdp_imf_num_eco"), main= "Unemployment (WDI)"),
    compare.density_own(a.out_wdi, var = c("grosscapitalformation_pwt_num_eco"), main= "Unemployment (WDI)"),
    common.legend = T,
    legend = "bottom"
  )
  
  # predictive capability
  
  inv_imp = Amelia::overimpute(a.out_wdi, var = "investment_wdi_num_eco")
  # saveRDS(inv_imp, "Analyse/PerformanceAreas/Economy/Diag/inv_imp.RDS")
  bal_imp = Amelia::overimpute(a.out_wdi, var = "Balance_wdi_num_eco")
  # saveRDS(bal_imp, "Analyse/PerformanceAreas/Economy/Diag/bal_imp.RDS")
  con_imp = Amelia::overimpute(a.out_wdi, var = "consumption_cap_wdi_num_eco")
  # saveRDS(con_imp, "Analyse/PerformanceAreas/Economy/Diag/con_imp.RDS")
  unemp_imp = Amelia::overimpute(a.out_wdi, var = "unemployment_pr_imf_num_eco")
  # saveRDS(unemp_imp, "Analyse/PerformanceAreas/Economy/Diag/unemp_imp.RDS")
  gdpcap_imp = Amelia::overimpute(a.out_wdi, var = "gdp_cap_ppp_imf_num_eco")
  # saveRDS(gdpcap_imp, "Analyse/PerformanceAreas/Economy/Diag/gdpcap_imp.RDS")
  inf_imp = Amelia::overimpute(a.out_wdi, var = "inflation_imf_num_eco")
  # saveRDS(inf_imp, "Analyse/PerformanceAreas/Economy/Diag/inf_imp.RDS")
  gdp_imp = Amelia::overimpute(a.out_wdi, var = "gdp_imf_num_eco")
  # saveRDS(gdp_imp, "Analyse/PerformanceAreas/Economy/Diag/gdp_imp.RDS")
  gross_imp = Amelia::overimpute(a.out_wdi, var = "grosscapitalformation_pwt_num_eco")
  # saveRDS(gross_imp, "Analyse/PerformanceAreas/Economy/Diag/gross_imp.RDS")
  
  
  ggarrange(
    overimpute_gglot(inv_imp, "investment_wdi"),
    overimpute_gglot(bal_imp, "Balance_wdi"),
    overimpute_gglot(con_imp, "consumption_cap_wdi"),
    overimpute_gglot(unemp_imp, "unemployment_pr_imf"),
    overimpute_gglot(gdpcap_imp, "gdp_cap_ppp_imf"),
    overimpute_gglot(inf_imp, "inflation_imf"),
    overimpute_gglot(gdp_imp, "gdp_imf"),
    overimpute_gglot(gross_imp, "grosscapitalformation_pwt")
  )
  
  
  
  
  par(mfrow=c(1,1))
  tscsPlot(a.out_wdi, cs = "IND",
           var = "GDP_capita_wdi_num_eco")
  
  tscsPlot(a.out_wdi, cs = "EST",
           var = "GDP_capita_wdi_num_eco")
  
  tscsPlot(a.out_wdi, cs = c("ZAF"),
           var = "GDP_capita_wdi_num_eco")
  tscsPlot(a.out_wdi, cs = "DEU",
           var = "GDP_capita_wdi_num_eco")
  
}

## Combine Imputation into Long Format

imputed_wdi_eco = mapply(cbind, a.out_wdi$imputations, "imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(imp = as.factor(imp)) 

imputed_wdi_eco_vars = imputed_wdi_eco %>% 
  left_join(fa_data_wdi_frame_mice %>%
              select(country_text_id, year, year_0),
            by = c("country_text_id", "year_0")) %>% 
  left_join(fa_data_eco_frame, by=c("country_text_id", "year"))  %>% 
  # mutate_at(vars(ends_with("_oecd_num_eco")), funs(ifelse(missing_OECD == max(missing_OECD), NA, .))) %>% 
  # mutate_at(vars(ends_with("_wdi_num_eco")), funs(ifelse(missing_WDI == max(missing_WDI), NA, .)))  %>% 
  select_at(vars(country, country_text_id, year, imp, 
                 ends_with("_wdi_num_eco"), ends_with("_imf_num_eco"), ends_with("_pwt_num_eco"), ends_with("wdi_is_na"), ends_with("imf_is_na"), "missing_SUM"))


imputed_wdi_eco_vars %>%
  filter(imp == 1) %>% 
  group_by(country_text_id) %>% 
  filter(year >= 1950) %>% 
  tidyr::complete(country_text_id, year = 1980:2017, fill = list(NA)) %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("_wdi_num_eco"), ends_with("_imf_num_eco"))) %>% 
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

#
sample = c("DEU")
imputed_wdi_eco_vars %>% 
  select_at(vars(country_text_id, year, ends_with("_wdi_num_eco"), ends_with("_imf_num_eco"))) %>% 
  filter(country_text_id %in% sample) %>% 
  pivot_longer(cols=ends_with("num_eco")) %>% 
  group_by(country_text_id, name, year) %>% 
  summarise(mean = mean(value),
            lower = quantile(value, 0.025, na.rm=T),
            upper = quantile(value, 0.975, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean, col=name)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))



