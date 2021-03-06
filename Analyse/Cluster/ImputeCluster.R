#### Imputation of Trade-Off Indicators

source("Setup/AuxiliaryVariables.R")

centripetalism = read_dta("Datasets/centripetalism.dta") %>% 
  select(country = Country, year = Year, unitarism_dp_ord_aux = Unit2) %>% 
  filter(country != "", year >= 1900) %>%
  mutate(country = fct_recode(country,
                              "Timor-Leste" =	"Timor, East",
                              "Egypt" = 	"Egypt, Arab Rep.",
                              "Trinidad and Tobago" = "Trinidad & Tobago",
                              "São Tomé and Príncipe"	 = "Sao Tome and Principe",
                              "Kyrgyzstan" = "Kyrgyztan",
                              "The Gambia" = "Gambia, The",
                              "Slovakia" = "Slovak Republic",
                              "Cyprus" = "Cyprus, Greek (Cyprus)",
                              "Burma/Myanmar"  = "Myanmar (Burma)",
                              "Bosnia and Herzegovina" = "Bosnia-Herzegovina",
                              "Belarus" = "Belarus (Byelorussian SSR)"	)) %>% 
  left_join(V_dem %>%  select(country, country_text_id) %>%  distinct(), by="country") %>% 
  mutate(year = as.numeric(year))


dmx_data_trade_mice = dmx_data %>% 
  dplyr::select_at(vars(country, country_text_id, year, regions, 
                        classification_core, 
                        matches("_trade_off"),
                        -matches("index_trade_off"))) %>%
  left_join(aux_vars %>%  select_at(vars(country_text_id, year, 
                                         ends_with("gen_num_aux"), 
                                         ends_with("dp_ord_aux"), 
                                         ends_with("dp_num_aux")) 
  ), by=c("country_text_id", "year")) %>% 
  left_join(centripetalism %>% 
              select(-country), 
            by=c("country_text_id", "year")) %>% 
  dplyr::arrange(country_text_id, year) %>% 
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("_trade_off")), funs(
    "lag"= dplyr::lag(.,1),
    "lead"= dplyr::lead(.,1))
  ) %>%
  ungroup() %>% 
  filter(classification_core == "Deficient Democracy" | classification_core == "Working Democracy" ) %>% 
  # analyse time range: 1900 - 2017
  filter(year>=1900, year <= 2017) %>% 
  mutate(year_0 = year - min(year)) %>% 
  mutate(classification_core_dp_num_aux = ifelse(classification_core == "Deficient Democracy", 1, 0))


# auxilary variables
dmx_data_trade_mice %>% 
  select_at(vars(matches("_aux"))) %>% 
  names()


dmx_data_trade_mice$na_count = rowSums(is.na(dmx_data_trade_mice %>% 
                select_at(vars(ends_with("_trade_off")))))



mice_data = as.data.frame(dmx_data_trade_mice) %>% 
  select(-country, -year, -regions, -classification_core, -na_count)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })



# corrplot
corrplot(cor(dmx_data_trade_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-ends_with("lag"),-ends_with("lead"), -na_count)), use="pairwise"))

missd_pattern(mice_data %>% 
                select_at(vars(ends_with("trade_off")))%>% 
                rename_all(funs(gsub("_trade_off","",.))) )


nr_imputations = 10
nr_cores = 10

a.out_trade <- amelia(mice_data, 
                   m = nr_imputations, 
                   ts = "year_0", 
                   cs = "country_text_id", 
                   polytime = 1,
                   bounds = as.matrix(data.frame(2:16,0,1)),
                   intercs = T,
                   p2s = 2,
                   parallel = "snow",
                   ncpus	= nr_cores,
                   empri = .05*nrow(mice_data)
)

a.out_trade

save(a.out_trade, file = "Analyse/Cluster/RObjects/a.out_trade.Rdata")




if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disp_trade = disperse(a.out_trade, dims = 1, m = 5)
  
  
  # saveRDS(disp_trade, "Analyse/Cluster/RObjects/disp_trade.RDS")
  disp_trade = readRDS("Analyse/Cluster/RObjects/disp_trade.RDS")
  convergence_amelia(disp_trade)  +
    ggtitle("Trade-Off Indicators: Overdispersed Starting Values")
  
  # obs vs. imp
  ggarrange(
    compare.density_own(a.out_trade, var = "decision_freedom_trade_off"),
    compare.density_own(a.out_trade, var = "decision_equality_trade_off"),
    #compare.density_own(a.out_trade, var = c("decision_control_trade_off")),
    # compare.density_own(a.out_trade, var = c("intermediate_freedom_trade_off")),
    # compare.density_own(a.out_trade, var = c("intermediate_equality_trade_off")),
    # compare.density_own(a.out_trade, var = c("intermediate_control_trade_off")),
    # compare.density_own(a.out_trade, var = c("communication_freedom_trade_off")),
    # compare.density_own(a.out_trade, var = c("communication_equality_trade_off")),
    # compare.density_own(a.out_trade, var = c("communication_control_trade_off")),
    # compare.density_own(a.out_trade, var = c("rights_freedom_trade_off")),
    # compare.density_own(a.out_trade, var = c("rights_equality_trade_off")),
    # compare.density_own(a.out_trade, var = c("rights_control_trade_off")),
    compare.density_own(a.out_trade, var = c("rule_settlement_freedom_trade_off")),
    #compare.density_own(a.out_trade, var = c("rule_settlement_equality_trade_off")),
    compare.density_own(a.out_trade, var = c("rule_settlement_control_trade_off")),
    common.legend = T,
    legend = "bottom"
  )
  
  
  dec_f_trade = Amelia::overimpute(a.out_trade, var = "decision_freedom_trade_off")
  # saveRDS(dec_f_trade, "Analyse/Cluster/RObjects/dec_f_trade.RDS")
  dec_e_trade = Amelia::overimpute(a.out_trade, var = "decision_equality_trade_off")
  # saveRDS(dec_e_trade, "Analyse/Cluster/RObjects/dec_e_trade.RDS")
  dec_c_trade = Amelia::overimpute(a.out_trade, var = "decision_control_trade_off")
  # saveRDS(dec_c_trade, "Analyse/Cluster/RObjects/dec_c_trade.RDS")
  int_f_trade = Amelia::overimpute(a.out_trade, var = "intermediate_freedom_trade_off")
  # saveRDS(int_f_trade, "Analyse/Cluster/RObjects/int_f_trade.RDS")
  int_e_trade = Amelia::overimpute(a.out_trade, var = "intermediate_equality_trade_off")
  # saveRDS(int_e_trade, "Analyse/Cluster/RObjects/int_e_trade.RDS")
  int_c_trade = Amelia::overimpute(a.out_trade, var = "intermediate_control_trade_off")
  # saveRDS(int_c_trade, "Analyse/Cluster/RObjects/int_c_trade.RDS")
  com_f_trade = Amelia::overimpute(a.out_trade, var = "communication_freedom_trade_off")
  # saveRDS(com_f_trade, "Analyse/Cluster/RObjects/com_f_trade.RDS")
  com_e_trade = Amelia::overimpute(a.out_trade, var = "communication_equality_trade_off")
  # saveRDS(com_e_trade, "Analyse/Cluster/RObjects/com_e_trade.RDS")
  com_c_trade = Amelia::overimpute(a.out_trade, var = "communication_control_trade_off")
  # saveRDS(com_c_trade, "Analyse/Cluster/RObjects/com_c_trade.RDS")
  gr_f_trade = Amelia::overimpute(a.out_trade, var = "rights_freedom_trade_off")
  # saveRDS(gr_f_trade, "Analyse/Cluster/RObjects/gr_f_trade.RDS")
  gr_e_trade = Amelia::overimpute(a.out_trade, var = "rights_equality_trade_off")
  # saveRDS(gr_e_trade, "Analyse/Cluster/RObjects/gr_e_trade.RDS")
  gr_c_trade = Amelia::overimpute(a.out_trade, var = "rights_control_trade_off")
  # saveRDS(gr_c_trade, "Analyse/Cluster/RObjects/gr_c_trade.RDS")
  rs_f_trade = Amelia::overimpute(a.out_trade, var = "rule_settlement_freedom_trade_off")
  # saveRDS(rs_f_trade, "Analyse/Cluster/RObjects/rs_f_trade.RDS")
  rs_e_trade = Amelia::overimpute(a.out_trade, var = "rule_settlement_equality_trade_off")
  # saveRDS(rs_e_trade, "Analyse/Cluster/RObjects/rs_e_trade.RDS")
  rs_c_trade = Amelia::overimpute(a.out_trade, var = "rule_settlement_control_trade_off")
  # saveRDS(rs_c_trade, "Analyse/Cluster/RObjects/rs_c_trade.RDS")
  
  ggarrange(
    overimpute_gglot(dec_f_trade, "Decision Freedom", xylims=c(0,1)),
    overimpute_gglot(dec_e_trade, "Decision Equality", xylims=c(0,1)),
    overimpute_gglot(rs_f_trade, "Rules Settlement Freedom", xylims=c(0,1)),
    overimpute_gglot(rs_c_trade, "Rules Settlement Equality", xylims=c(0,1))
  )
  

  
  tscsPlot(a.out_trade, cs = "AUS",
           var = "order_safety_gdp_oecd_num_ds")
  
  tscsPlot(a.out_trade, cs = "EST",
           var = "water_oecd_num_env")
  
  tscsPlot(a.out_trade, cs = c("ZAF"),
           var = "waste_oecd_num_env")
  tscsPlot(a.out_trade, cs = "Germany",
           var = "rule_settlement_control_trade_off")
}

## Combine Imputation into Long Format
imputed_trade = mapply(cbind, a.out_trade$imputations, ".imp" = 1:nr_imputations, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(.imp = as.factor(.imp))



sample = c("AUT", "DEU", "BRB")
imputed_trade %>% 
  select(country_text_id, year_0, variable = decision_freedom_trade_off) %>% 
  filter(country_text_id %in% sample) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise(mean = mean(variable),
            lower = quantile(variable, 0.025),
            upper = quantile(variable, 0.975)) %>% 
  ggplot(aes(x=year_0, y=mean, col=country_text_id)) + 
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper))


# Create Dataset to Cluster: Average over Imputations and Create Indices
dmx_data_trade = imputed_trade %>% 
  select_at(vars(country_text_id, year_0, ends_with("trade_off"))) %>% 
  group_by(country_text_id, year_0) %>% 
  summarise_at(vars(ends_with("trade_off")), mean) %>% 
  ungroup() %>% 
  bind_cols(dmx_data_trade_mice %>%  select(country, year, regions, classification_core, na_count)) %>% 
  mutate(
    freedom_dim_index_trade_off =  (decision_freedom_trade_off * intermediate_freedom_trade_off * communication_freedom_trade_off * rights_freedom_trade_off * rule_settlement_freedom_trade_off)^(1/5),
    equality_dim_index_trade_off =  (decision_equality_trade_off * intermediate_equality_trade_off * communication_equality_trade_off * rights_equality_trade_off *rule_settlement_equality_trade_off)^(1/5),
    control_dim_index_trade_off =  (decision_control_trade_off * intermediate_control_trade_off * communication_control_trade_off * rights_control_trade_off *rule_settlement_control_trade_off)^(1/5),
    
    decision_inst_index_trade_off = (decision_freedom_trade_off * decision_equality_trade_off * decision_control_trade_off)^(1/3),
    intermediate_inst_index_trade_off = (intermediate_freedom_trade_off * intermediate_equality_trade_off * intermediate_control_trade_off)^(1/3),
    communication_inst_index_trade_off = (communication_freedom_trade_off * communication_equality_trade_off * communication_control_trade_off)^(1/3),
    rights_inst_index_trade_off = (rights_freedom_trade_off * rights_equality_trade_off * rights_control_trade_off)^(1/3),
    rule_settlement_inst_index_trade_off = (rule_settlement_freedom_trade_off * rule_settlement_equality_trade_off * rule_settlement_control_trade_off)^(1/3),
    total_index_trade_off = (freedom_dim_index_trade_off * equality_dim_index_trade_off * control_dim_index_trade_off)^(1/3)
  ) %>% 
  dplyr::select_at(vars(country, country_text_id, year, regions, classification_core, na_count, matches("trade_off"))) 
  # rename(freedom = freedom_dim_index_trade_off,
  #        equality = equality_dim_index_trade_off,
  #        control = control_dim_index_trade_off) 

write.csv(dmx_data_trade, file="Datasets/performance_data/ImputedDatasets/dmx_data_trade.csv", row.names = F, fileEncoding ="UTF-8")
