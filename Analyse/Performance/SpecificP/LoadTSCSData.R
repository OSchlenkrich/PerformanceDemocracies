# Load All Datasets ####
# Create TSCS Datasets ####

# Setup
source("Analyse/CreateDatasets.R")
source("Analyse/Performance/SpecificP/SpatialAutocorrelation.R")

# Control Variables ####
# Central Bank Independence
CBI_control = readstata13::read.dta13("Datasets/CBI dataset_2019 corrections.dta") %>%  
  select(-cname) %>% 
  left_join(QoC_data %>% 
              select(country, country_text_id,
                     year,
                     ccodewb) %>% 
              na.omit(), by=c("ccodewb", "year")) %>% 
  select(country_text_id, year, 
         #cbi_u_cbi_num_ctl = lvau_garriga, 
         cbi_w_cbi_num_ctl =  lvaw_garriga )

#VDem
V_dem_control = V_dem_all_v9 %>% 
  select(country_text_id, year, 
         corruption_vdem_pr_ctl = v2x_corr, 
         corporatism_vdem_pr_ctl = v2csstruc_1,
         stateterr_vdem_pr_ctl = v2svstterr)  %>%
  mutate_at(vars(stateterr_vdem_pr_ctl), funs(./100))

#QoC
control_vars = QoC_data %>% 
  dplyr::select(country_text_id, year,
                
                # Economic Modernization
                gdppc_wdi_num_ctl  = wdi_gdpcapcon2010, 
                pop_over65_wdi_pr_ctl = wdi_pop65,
                
                # PRT
                unions_vi_num_ctl = vi_udr,
                strength_green_notrans_ctl = cpds_vg,
                
                # Partisan Theory
                cabinet_cpds_cat_ctl = cpds_govlr, 
                green_plt_cpds_notrans_ctl = cpds_lg,
                
                
                trade_wdi_num_ctl = wdi_trade,
                
                #gini_wdi_num_ctl = wdi_gini
                
  ) %>% 
  
  filter(country_text_id %in% unique(dmx_trade_cluster$country_text_id)) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  mutate_at(vars(matches("pr")), funs(./100)) %>% 
  arrange(country_text_id, year)

#Other Democracy Profiles
consensusdemocracy_vars = consensusdemocracy %>% 
  select(country_text_id, execpar_1981_odempr = exec_parties_1981_2010, feduni1981_odempr = federal_unitary_1981_2010)

centripetalism_vars = centripetalism %>% 
  select(country_text_id, year, centrip_odempr = Cent_Sdep1)



# Main Independent Variable ####
# Log Ratios Democracy Profiles
source("Analyse/Cluster/LogRatios_v2.R")



# Final Dataset ####
tscs_data = performance_all %>% 
  # main independent variable 
  left_join(LogRATIOS_3_eco_total, 
            by=c("country_text_id", "year")) %>% 
  left_join(LogRATIOS_eco_dim, 
            by=c("country_text_id", "year")) %>% 
  left_join(LogRATIOS_eco_total, 
            by=c("country_text_id", "year")) %>% 
  
  # Control Vars
  left_join(control_vars, by=c("country_text_id", "year")) %>% 
  left_join(CBI_control, by=c("country_text_id", "year")) %>% 
  left_join(V_dem_control, by=c("country_text_id", "year")) %>% 
  
  # Other Dem Profiles
  
  left_join(consensusdemocracy_vars, by=c("country_text_id")) %>%
  left_join(centripetalism_vars, by=c("country_text_id", "year")) %>%
  
  filter(classification_core == "Deficient Democracy" | classification_core == "Working Democracy") %>% 
  
  # fiscal crisis
  mutate(
    #fiscalcrisis_cat_ctl = if_else(year == 2008 | year == 2009, 1, 0),
    fiscalcrisis_cat_ctl = if_else(year >= 2008, 1, 0)
    ) %>% 
  # create NA indicator variables
  group_by(country_text_id) %>% 
  mutate_at(vars(ends_with("ctl")), funs("na" = if_else(all(is.na(.) == T), 1, 0))) %>%
  ungroup() %>% 
  arrange(country_text_id, year)  



# Quick Check 

tscs_data %>% 
  select(country, FKM5_Fec) %>% 
  arrange(-FKM5_Fec)
tscs_data %>% 
  select(country, FKM5_fEc) %>% 
  arrange(-FKM5_fEc)
tscs_data %>% 
  select(country, FKM4_E) %>% 
  arrange(FKM4_E)

