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



dmx_data_trade_mice$na_count = rowSums(is.na(dmx_data_trade_mice %>% 
                select_at(vars(ends_with("_trade_off")))))

# x = dmx_data_trade_mice$decision_freedom_trade_off
# test = ladder_fun(x)
# hist(x)
# hist(test)
# 
# 
# minimum = min(x, na.rm=T)
# constant = abs(minimum) 
# 
# (x-constant+1)^1.975
# 
# test^(1/1.975)+constant-1


mice_data = as.data.frame(dmx_data_trade_mice) %>% 
  select(-country, -year, -regions, -classification_core, -na_count)
mice_data[] <- lapply(mice_data, function(x) { attributes(x) <- NULL; x })


# corrplot
corrplot(cor(dmx_data_trade_mice %>% 
               select_if(is.numeric) %>% 
               select_at(vars(-ends_with("lag"),-ends_with("lead"), -na_count)), use="pairwise"))



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


if (Plot_Impu == T) {
  # convergence
  par(mfrow=c(1,1))
  disperse(a.out_trade, dims = 1, m = 5)
  
  # obs vs. imp
  par(mfrow=c(2,2), mar=c(4,4,4,4))
  compare.density(a.out_trade, var = c("decision_freedom_trade_off"), main= "Observed vs. Imputed Values of Homicides per 100.000")
  compare.density(a.out_trade, var = c("decision_equality_trade_off"), main= "Observed vs. Imputed Values of Homicides per 100.000")
  compare.density(a.out_trade, var = c("decision_control_trade_off"), main= "Observed vs. Imputed Values of Homicides per 100.000")
  
  compare.density(a.out_trade, var = c("intermediate_freedom_trade_off"), main= "Observed vs. Imputed Values of Robberies per 100.000")
  compare.density(a.out_trade, var = c("intermediate_equality_trade_off"), main= "Observed vs. Imputed Values of Robberies per 100.000")
  compare.density(a.out_trade, var = c("intermediate_control_trade_off"), main= "Observed vs. Imputed Values of Robberies per 100.000")
  
  compare.density(a.out_trade, var = c("communication_freedom_trade_off"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
  compare.density(a.out_trade, var = c("communication_equality_trade_off"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
  compare.density(a.out_trade, var = c("communication_control_trade_off"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
  
  compare.density(a.out_trade, var = c("rights_freedom_trade_off"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
  compare.density(a.out_trade, var = c("rights_equality_trade_off"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
  compare.density(a.out_trade, var = c("rights_control_trade_off"), main= "Observed vs. Imputed Values of Ratio prison population/convictions")
  
  compare.density(a.out_trade, var = c("rule_settlement_freedom_trade_off"), main= "Observed vs. Imputed Values of General government spending – Public order and safety")
  compare.density(a.out_trade, var = c("rule_settlement_equality_trade_off"), main= "Observed vs. Imputed Values of General government spending – Public order and safety")
  compare.density(a.out_trade, var = c("rule_settlement_control_trade_off"), main= "Observed vs. Imputed Values of General government spending – Public order and safety")
  
  
  # predictive capability
  par(mfrow=c(2,2))
  Amelia::overimpute(a.out_trade, var = "decision_freedom_trade_off", main= "Observed vs. Imputed Values of PD Freedom TO")
  Amelia::overimpute(a.out_trade, var = "decision_equality_trade_off", main= "Observed vs. Imputed Values of PD Equality TO")
  Amelia::overimpute(a.out_trade, var = "rule_settlement_freedom_trade_off", main= "Observed vs. Imputed Values of RS Freedom TO")
  Amelia::overimpute(a.out_trade, var = "rule_settlement_control_trade_off", main= "Observed vs. Imputed Values of RS Control TO")
  
  
  
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
  dplyr::select_at(vars(country, country_text_id, year, regions, classification_core, na_count, matches("dim_index_trade_off"))) %>% 
  rename(freedom = freedom_dim_index_trade_off,
         equality = equality_dim_index_trade_off,
         control = control_dim_index_trade_off) 
