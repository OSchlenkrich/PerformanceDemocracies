# VDEM Analyse Setup #####

source("Setup/Packages.R")
source("Setup/Base_Functions.R")
source("Setup/Plotting_Functions.R")
source("Setup/Sig_Tables.R")
source("Setup/LoadDatasets.R")

library(bayestestR)
library(glmmTMB)
library(ggeffects)
library(performance)
library(multidplyr)
cluster = new_cluster(3)


# Functions ####

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# Identify Historic Coders
if_historic = function(x) {
  historic = ifelse(any(x > 1920), 0, 1) 
  return(historic)
}

# Coder Type Classification
get_type = function(x) {
  coded_more_1 = length(which(x > 1))
  coded1 = length(which(x == 1))
  
  
  type = ifelse(coded_more_1 == 1, "CC", 
                ifelse(coded_more_1 > 1, "BLC", 
                       ifelse(coded_more_1 == 0 & coded1 > 0, "LC",NA)))
  if (type == "CC") {
    type = ifelse((coded1 == 0), "CC", "LC")
  }
  
  return(type)
  
}


# Load V-Dem Dataset and V-Dem Disaggregated Dataset #####
vdem_main = fread("unzip -p C:/RTest/V-Dem-CY-Full+Others-v9.zip", encoding = "UTF-8")

# Transform Historical Date into Year Format and add country labes
vdem_ds = fread("unzip -p C:/RTest/V-Dem_coder_level_ds_v9.zip") %>% 
  mutate(historical_date = as.POSIXct(historical_date, format= "%Y-%m-%d"),
         year = as.numeric(strftime(historical_date, format="%Y"))
  )  %>% 
  left_join(vdem_main %>% 
              select(country_text_id, country = country_name) %>% 
              distinct(), by="country_text_id") 

vdemcontent = fread("Datasets/VDem_content.csv", sep=";", header = T) %>% 
  mutate(update = if_else(update == "", "V6", update))

print("All V-Dem Datasets loaded")

# Modified Datasets ####
# Extract Variable Names from V-Dem Dataset
vdem_varnames  = vdem_main %>% 
  select_at(vars(starts_with("v2"), 
                 -matches("_code"), 
                 -matches("_osp"), 
                 -matches("_ord"), 
                 -matches("_nr"), 
                 -matches("_sd"), 
                 -matches("_mean"), 
                 -matches("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
                 -matches("_11"), 
                 -matches("_12"), 
                 -matches("_13"), 
                 -matches("_14"), 
                 -matches("_15"), 
                 -matches("_16"), 
                 -matches("_17"), 
                 -matches("_18"), 
                 -matches("_19"), 
                 -matches("_20")
  )) %>% 
  colnames() 
vdem_varnames = gsub("_0", "", vdem_varnames)

multiplechoice_values = c("v2csstruc",
                          "v2elsnlfc", "v2elsnmrfc", "v2psbantar", "v2exrmhsol",
                          "v2exctlhs", "v2exrmhgnp", "v2exctlhg", "v2regsupgroups", 
                          "v2clrgstch", "v2clrgwkch", "v2csanmvch","v2exl_legitideolcr")

# Extract Variables Types from the Codebook
cod_type = sub("\\).*", "", vdemcontent$names) 
cod_type_2 = sub(".*\\(", "", cod_type)

cod_name = sub(".*\\) \\(", "", vdemcontent$names)
cod_name_2 = sub("\\).*", "", cod_name)

var_data = data.frame(varname = cod_name_2, codetype = cod_type_2) %>% 
  mutate(update = vdemcontent$update) %>% 
  filter(grepl("v2", varname)) 


length_varname = var_data %>%
  # Only variables which are actually in the dataset
  filter(varname %in% vdem_varnames) %>% 
  filter(grepl("_", varname) == F) %>% 
  mutate(sections = ifelse(grepl("v2el", varname), "el", 
                           ifelse(grepl("v2ps", varname), "ps",
                                  ifelse(grepl("v2dd", varname), "dd",
                                         ifelse(grepl("v2ex", varname), "ex",
                                                ifelse(grepl("v2lg", varname), "lg",
                                                       ifelse(grepl("v2dl", varname), "dl",
                                                              ifelse(grepl("v2dl", varname), "dl",
                                                                     ifelse(grepl("v2ju", varname), "ju",
                                                                            ifelse(grepl("v2cl", varname), "cl",
                                                                                   ifelse(grepl("v2sv", varname), "sv",
                                                                                          ifelse(grepl("v2st", varname), "sv",
                                                                                                 ifelse(grepl("v2cs", varname), "cs",
                                                                                                        ifelse(grepl("v2me", varname), "me",
                                                                                                               ifelse(grepl("v2pe", varname), "pe", NA_character_))))))))))))))) %>% 
  na.omit() %>% 
  mutate(varname = as.character(varname),
         codetype = as.character(codetype),
         codetype = as.factor(codetype)) %>% 
  # these are only binary version of C-variables
  filter(varname != "v2elffelrbin",
         varname != "v2lgdsadlobin",
         varname != "v2mecenefibin")


# vartype = vdem_ds %>% 
#   select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
#   select_at(vars(-ends_with("_beta"),
#                  -ends_with("_conf"),
#                  -ends_with("bin"),
#                  -ends_with("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_11"), 
#                  -matches("_12"), 
#                  -matches("_13"), 
#                  -matches("_14"), 
#                  -matches("_15"), 
#                  -matches("_16"), 
#                  -matches("_17"), 
#                  -matches("_18"), 
#                  -matches("_19"), 
#                  -matches("_20"))) %>% 
#   pivot_longer(cols=starts_with("v2")) %>% 
#   mutate(name = gsub("_0", "", name)) %>% 
#   filter(year >= 1900) %>% 
#   na.omit() %>% 
#   group_by(name) %>% 
#   summarise(vartype = n_distinct(value)) %>% 
#   mutate(percentageVar = ifelse(vartype >= 90, T, F)) %>% 
#   filter(percentageVar == F)

# saveRDS(vartype, file="Analyse/V-Dem/robjects/vartype.RDS")
vartype = readRDS(file="Analyse/V-Dem/robjects/vartype.RDS")

# variable_caus_1 = vdem_ds %>% 
#   select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
#   select_at(vars(-ends_with("_beta"),
#                  -ends_with("_conf"),
#                  -ends_with("bin"),
#                  -ends_with("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_11"), 
#                  -matches("_12"), 
#                  -matches("_13"), 
#                  -matches("_14"), 
#                  -matches("_15"), 
#                  -matches("_16"), 
#                  -matches("_17"), 
#                  -matches("_18"), 
#                  -matches("_19"), 
#                  -matches("_20"))) %>% 
#   pivot_longer(cols=starts_with("v2")) %>% 
#   mutate(name = gsub("_0", "", name)) %>% 
#   filter(year >= 1900) %>% 
#   na.omit()  %>% 
#   right_join(regression_vars %>% rename(name = varname), by="name")
# saveRDS(variable_caus_1, file="Analyse/V-Dem/robjects/variable_caus_1.RDS")
variable_caus_1 = readRDS(file="Analyse/V-Dem/robjects/variable_caus_1.RDS")

# Takes A Very Long Time;
# vdem_vars = vdem_ds %>% 
#   select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
#   select_at(vars(-ends_with("_beta"),
#                  -ends_with("_conf"),
#                  -ends_with("bin"),
#                  -ends_with("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_11"), 
#                  -matches("_12"), 
#                  -matches("_13"), 
#                  -matches("_14"), 
#                  -matches("_15"), 
#                  -matches("_16"), 
#                  -matches("_17"), 
#                  -matches("_18"), 
#                  -matches("_19"), 
#                  -matches("_20"))) %>% 
#   pivot_longer(cols=starts_with("v2")) %>% 
#   na.omit() %>% 
#   # select only C Variables
#   mutate(name = gsub("_0", "", name)) %>% 
#   filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) %>%
#   # if multiple ratings per year, select only the latest (31.04.1931 < 31.12.1931)
#   arrange(name, country_text_id, historical_date) %>% 
#   group_by(name, country_text_id, year, coder_id) %>% 
#   top_n(1, historical_date) %>% 
#   ungroup() %>%
#   select(-historical_date)

# saveRDS(vdem_vars, file="Analyse/V-Dem/robjects/vdem_vars.RDS")
vdem_vars = readRDS(file="Analyse/V-Dem/robjects/vdem_vars.RDS")


# year_type_df = vdem_ds %>% 
#   select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
#   select_at(vars(-ends_with("_beta"),
#                  -ends_with("_conf"),
#                  -ends_with("bin"),
#                  -ends_with("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_1"), 
#                  -matches("_2"), 
#                  -matches("_3"), 
#                  -matches("_4"), 
#                  -matches("_5"), 
#                  -matches("_6"), 
#                  -matches("_7"), 
#                  -matches("_8"), 
#                  -matches("_9"), 
#                  -matches("_10"), 
#                  -matches("_11"), 
#                  -matches("_12"), 
#                  -matches("_13"), 
#                  -matches("_14"), 
#                  -matches("_15"), 
#                  -matches("_16"), 
#                  -matches("_17"), 
#                  -matches("_18"), 
#                  -matches("_19"), 
#                  -matches("_20"))) %>% 
#   pivot_longer(cols=starts_with("v2")) %>% 
#   mutate(name = gsub("_0", "", name)) %>% 
#   filter(year >= 1900) %>% 
#   na.omit() %>% 
#   left_join(country_coder_type, by=c("name", "coder_id")) %>% 
#   na.omit() %>% 
#   # select only C Variables
#   filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) 

# saveRDS(year_type_df, file="Analyse/V-Dem/robjects/year_type_df.RDS")
year_type_df = readRDS(file="Analyse/V-Dem/robjects/year_type_df.RDS")

rm(cod_name)
rm(cod_name_2)
rm(cod_type)
rm(cod_type_2)
rm(var_data)
rm(vdemcontent)
gc()
