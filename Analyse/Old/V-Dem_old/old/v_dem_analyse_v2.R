# V-Dem Chapter 2.1 Script

# Setup #####
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

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

library(bayestestR)
library(glmmTMB)

source("Setup/Packages.R")
source("Setup/Base_Functions.R")
source("Setup/Plotting_Functions.R")


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

posterior_v2elfrfair = fread("unzip -p Datasets/v2elfrfair.10000.Z.sample.zip")



### Number of Variables per Section and per Coders per Section ----

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



# All variables
length_varname %>% 
  group_by(sections) %>% 
  summarise(nr_section = n()) %>% 
  mutate(sections = fct_reorder(sections, -nr_section)) %>% 
  ggplot(aes(x=sections, y=nr_section)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme_bw()

# Only C Variables
p1 = length_varname %>% 
  mutate(sections = as.factor(sections)) %>% 
  filter(codetype == "C") %>% 
  group_by(sections) %>% 
  summarise(nr_section = n()) %>% 
  tidyr::complete(sections, fill = list(nr_section = 0)) %>% 
  mutate(sections = fct_reorder(sections, -nr_section)) %>% 
  ggplot(aes(x=sections, y=nr_section)) +
  geom_bar(stat="identity") + 
  ylab("number of country coders") +
  theme_bw()
p1

# All and C variables
p2 = length_varname %>% 
  group_by(sections, codetype)  %>% 
  summarise(coders = n())  %>% 
  tidyr::complete(codetype, fill = list(coders = 0)) %>% 
  group_by(sections) %>% 
  mutate(total = sum(coders)) %>% 
  filter(codetype == "C") %>% 
  mutate(no_coders = total - coders) %>% 
  ungroup() %>% 
  mutate(sections = fct_reorder(sections, -total)) %>% 
  pivot_longer(cols=c("no_coders", "coders")) %>% 
  ggplot(aes(x=sections , y=value, fill=name)) +
  geom_bar(stat="identity") +
  scale_fill_discrete(labels = c(coders = "yes", no_coders = "no")) +
  guides(fill=guide_legend(title="Coded by Country-Expert")) +
  theme_bw() +
  theme(legend.position = "bottom")
p2


grid.arrange(p2, p1, nrow=2)

# HDI Interval ####

v2elfrfair_wide = data.frame(
  ctry = posterior_v2elfrfair[,1],
  posterior = posterior_v2elfrfair[,-1]) %>% 
  pivot_longer(cols= starts_with("posterior")) %>% 
  pivot_wider(names_from = "V1") %>% 
  select(-name)

hdi_sample = hdi(v2elfrfair_wide, ci=0.68) %>% 
  mutate(mean = rowMeans(posterior_v2elfrfair[,-1])) %>% 
  rename(hdi_low68 = CI_low, hdi_high68 = CI_high) %>% 
  select(-CI) %>% 
  left_join(eti(v2elfrfair_wide, ci=0.68) %>% 
              rename(eti_low68 = CI_low, eti_high68 = CI_high)%>% 
              select(-CI), by="Parameter") %>% 
  left_join(hdi(v2elfrfair_wide, ci=0.95) %>% 
              rename(hdi_low95 = CI_low, hdi_high95 = CI_high)%>% 
              select(-CI), by="Parameter")  %>% 
  rename(country_year = "Parameter")


hdi_sample %>% 
  sample_n(500) %>% 
  mutate(country_year = fct_reorder(country_year, mean)) %>% 
  ggplot(aes(x=country_year, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=hdi_low68, ymax=hdi_high68)) +
  geom_errorbar(aes(ymin=hdi_low95, ymax=hdi_high95), color="red", alpha=0.5) +
  coord_flip()  +
  theme(axis.text.y = element_blank()) 


# find skewed cases
skewed_test = hdi_sample %>% 
  mutate(diff = (hdi_high68 - hdi_low68)/2,
         degree_skewness = (hdi_low68 + diff) - mean,
         degree_skewness = abs(degree_skewness)
         ) %>% 
  arrange(-degree_skewness)
skewed_test


# NZL 1919-12-17	
case = "NZL 1919-12-17"

v2elfrfair_wide %>% 
  select(NZL1919 = case) %>% 
  mutate(mean = mean(NZL1919), median = median(NZL1919)) %>% 
  cbind(hdi_sample %>%  filter(country_year == case) %>%  select(hdi_low68,  hdi_high68, eti_low68,  eti_high68)) %>% 
  ggplot(aes(x=NZL1919)) +
  geom_histogram(bins=50, alpha=0.5) +
  geom_vline(aes(xintercept = mean)) +
  geom_vline(aes(xintercept = median)) +
  geom_segment(aes(y=10, yend=10, x = hdi_low68, xend = hdi_high68), size=1.1)  +
  annotate("text",y=12, x = 1.5, label = "HPD") +
  geom_segment(aes(y=5, yend=5, x = eti_low68, xend = eti_high68), size=1.1)  +
  annotate("text",y=7, x = 1.5, label = "ETI") + 
  ggtitle(case) +
  theme_bw()

# Absolute Numbers ####



abs_coder_df =  vdem_main %>% 
  select_at(vars(country_name, year,  ends_with("_nr"))) %>%
  select_at(vars(country_name, year,  starts_with("v2"))) %>%
  select_at(vars(-ends_with("bin_nr"))) %>%
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_nr", "", name)) %>% 
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) %>%   
  na.omit() 

#per year
abs_coder_df %>% 
  group_by(year) %>% 
  summarise(abs_indicator = n()) %>% 
  ggplot(aes(x=year, y=abs_indicator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  ggtitle("Mean number of coders per country-year per Section")
  
#per sections
abs_coder_df %>% 
  left_join(length_varname %>% rename(name = varname), by="name") %>% 
  group_by(sections, name) %>% 
  summarise(abs_indicator = n()) %>% 
  group_by(sections) %>% 
  summarise(abs_indicator = mean(abs_indicator)) %>% 
  mutate(sections = fct_reorder(sections, abs_indicator)) %>% 
  ggplot(aes(x=sections, y=abs_indicator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  ggtitle("Mean number of coders per country-year per Section")

#per name
abs_coder_df %>% 
  left_join(length_varname %>% rename(name = varname), by="name") %>% 
  group_by(name) %>% 
  summarise(abs_indicator = n()) %>% 
  mutate(name = fct_reorder(name, abs_indicator)) %>% 
  ggplot(aes(x=name, y=abs_indicator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  ggtitle("Mean number of coders per country-year per Section")

### Number of coders ####

mean_coder_df = vdem_main %>% 
  select_at(vars(country_name, year,  ends_with("_nr"))) %>%
  select_at(vars(country_name, year,  starts_with("v2"))) %>%
  select_at(vars(-ends_with("bin_nr"))) %>%
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_nr", "", name)) %>% 
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) 

mean_coder_df %>% 
  group_by(year, country_name) %>% 
  summarise(mean_country = mean(value, na.rm=T)) %>% 
  group_by(year) %>% 
  summarise(mean_coder = mean(mean_country, na.rm=T),
            lower_coder = quantile(mean_country, prob=0.25, na.rm=T),
            higher_coder = quantile(mean_country, prob=0.75, na.rm=T)) %>% 
  ggplot(aes(x=year, y=mean_coder)) +
  geom_line(size=1.1) +
  geom_errorbar(aes(ymin=lower_coder, ymax=higher_coder)) +
  scale_y_continuous(breaks=seq(0,20, 2), limit=c(0,13)) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  geom_hline(yintercept = 5) +
  theme_bw() +
  ggtitle("Number of Coders per country per year")


mean_coder_df %>% 
  group_by(name) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>% 
  mutate(name = fct_reorder(name, mean_indicator)) %>% 
  ggplot(aes(x=name, y=mean_indicator)) +
  geom_point() +
  coord_flip() +
  theme_bw() +
  ggtitle(" Number of Coders per country per year, ~ Indicator")


mean_coder_df %>% 
  group_by(name) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>% 
  arrange(mean_indicator) %>% 
  top_n(10)
mean_coder_df%>% 
  group_by(name) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>% 
  arrange(mean_indicator) %>% 
  top_n(-10)




mean_coder_df %>% 
  mutate(name = gsub("_nr", "", name)) %>% 
  left_join(length_varname %>% rename(name = varname), by="name")  %>% 
  group_by(update) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>% 
  na.omit() %>%  
  ggplot(aes(x=update, y=mean_indicator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  ggtitle("Average Number of Coders per Update")


### Average Confidence ####

conf_rater_vdem = vdem_ds %>% 
  select_at(vars(country_text_id, year, starts_with("v2"))) %>% 
  select_at(vars(country_text_id, year, 
                 ends_with("_conf"),
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
                 -matches("_20"))) %>% 
  filter(year >= 1900) %>% 
  pivot_longer(cols=ends_with("_conf")) %>%
  mutate(name = gsub("_conf", "", name)) %>% 
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) 
  

conf_rater_vdem  %>% 
  group_by(country_text_id, year) %>%
  summarise(mean_conf_ctry_year = mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(year) %>%
  summarise(mean_conf_year = mean(mean_conf_ctry_year, na.rm=T),
            lower_conf_year = quantile(mean_conf_ctry_year, prob=0.25, na.rm=T),
            higher_conf_year = quantile(mean_conf_ctry_year, prob=0.75, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=mean_conf_year)) +
  geom_line(size=1) +
  geom_errorbar(aes(ymin=lower_conf_year, ymax=higher_conf_year)) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  theme_bw() +
  ylim(0.5,1) +
  ggtitle("Average Confidence of Coders per year")


conf_rater_vdem %>% 
  left_join(length_varname %>%  rename(name = varname), by="name") %>% 
  group_by(sections) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>%
  na.omit() %>% 
  mutate(sections = fct_reorder(sections, mean_indicator)) %>% 
  ggplot(aes(x=sections, y=mean_indicator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  ggtitle("Average Confidence of Coders per section")


### Value Change due to Coder Change ####
# Multiple Choice
multiplechoice_values = vdem_main %>% 
  select_at(vars(
    country_name, country_text_id, year,
    matches("v2elsnlfc"),
    matches("v2elsnmrfc"),
    matches("v2psbantar"),
    matches("v2exrmhsol"),
    matches("v2exctlhs"),
    matches("v2exrmhgnp"),
    matches("v2exctlhg"),
    matches("v2regsupgroups"), 
    matches("v2clrgstch"),
    matches("v2clrgwkch"),
    matches("v2csanmvch"),
    matches("v2exl_legitideolcr"))
  ) %>% 
  select_at(vars(-ends_with("_codelow"), 
                 -ends_with("_codehigh"), 
                 -ends_with("_nr"), 
                 -ends_with("_sd"), 
                 -ends_with("_mean"),
                 -matches("v2regsupgroupssize"))) %>% 
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2"), names_to = "varname") %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_perc = mean(value, na.rm=T)) %>% 
  group_by(year) %>% 
  summarise(mean_perc = mean(mean_perc, na.rm=T))


multiplechoice_coders = vdem_main %>% 
  select_at(vars(
    country_name, country_text_id, year,
    matches("v2elsnlfc"),
    matches("v2elsnmrfc"),
    matches("v2psbantar"),
    matches("v2exrmhsol"),
    matches("v2exctlhs"),
    matches("v2exrmhgnp"),
    matches("v2exctlhg"),
    matches("v2regsupgroups"), 
    matches("v2clrgstch"),
    matches("v2clrgwkch"),
    matches("v2csanmvch"),
    matches("v2exl_legitideolcr"))) %>% 
  select_at(vars(country_name, country_text_id, year,ends_with("_nr"))) %>% 
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2"), names_to = "varname") %>% 
  group_by(country_text_id, year) %>% 
  summarise(mean_coder = mean(value, na.rm=T)) %>% 
  group_by(year) %>% 
  summarise(mean_coder = mean(mean_coder, na.rm=T))


multiplechoice_values %>% 
  left_join(multiplechoice_coders, by="year") %>% 
  mutate(change_value = abs(scale_this(mean_perc  - dplyr::lag(mean_perc ,1))),
         change_coder = abs(scale_this(mean_coder - dplyr::lag(mean_coder,1)))) %>%  
  pivot_longer(cols=starts_with("change"), names_to = "varname") %>% 
  ggplot(aes(x=year, y=value, linetype=varname)) +
  geom_line(size=1) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) + 
  theme_bw()

value_change_data_multiple = multiplechoice_values %>% 
  left_join(multiplechoice_coders, by="year") %>% 
  mutate(change_value = abs(scale_this(mean_perc  - dplyr::lag(mean_perc ,1))),
         change_coder = abs(scale_this(mean_coder - dplyr::lag(mean_coder,1)))) 

# Measurement Model
bootstrapped_vars = c("v2svstterr", "v2mefemjrn", "v2clsnlpct")
measurement_model_mean = vdem_main %>% 
  select_at(vars(country_name, year,  starts_with("v2"))) %>%
  select(country_name, year,  matches(paste(length_varname %>%  filter(codetype == "C") %>% pull(varname), collapse = "|"))) %>% 
  select_at(vars(-matches("_code"), 
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
                 -matches("_20"), 
                 -matches("v2el"),
                 -bootstrapped_vars)) %>%
  #select_at(vars(country_name, year, matches("v2me"))) %>%
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  group_by(year, country_name) %>% 
  summarise(mean_country = mean(value, na.rm=T)) %>% 
  group_by(year) %>% 
  summarise(mean_year = mean(mean_country, na.rm=T))

measurement_model_coder = vdem_main %>% 
  select_at(vars(country_name, year,  starts_with("v2"))) %>%
  select(country_name, year,  matches(paste(length_varname %>%  filter(codetype == "C") %>% pull(varname), collapse = "|"))) %>% 
  select_at(vars(-matches("_code"), 
                 -matches("_osp"), 
                 -matches("_ord"), 
                 #-matches("_nr"), 
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
                 -matches("_20"), 
                 -matches("v2el"),
                 -bootstrapped_vars)) %>%
  #select_at(vars(country_name, year, matches("v2me"))) %>%
  select_at(vars(country_name, year, matches("_nr"))) %>% 
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  group_by(year, country_name) %>% 
  summarise(mean_coder = mean(value, na.rm=T)) %>% 
  group_by(year) %>% 
  summarise(mean_coder = mean(mean_coder, na.rm=T))


measurement_model_mean %>% 
  left_join(measurement_model_coder, by="year") %>% 
  pivot_longer(cols=starts_with("mean_"), names_to = "varname") %>% 
  ggplot(aes(x=year, y=value, linetype=varname)) +
  geom_line(size=1) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  theme_bw() +
  geom_vline(xintercept = 2012)


measurement_model_mean %>% 
  left_join(measurement_model_coder, by="year") %>% 
  mutate(change_value = abs(scale_this(mean_year  - dplyr::lag(mean_year ,1))),
         change_coder = abs(scale_this(mean_coder - dplyr::lag(mean_coder,1)))) %>% 
  pivot_longer(cols=starts_with("change"), names_to = "varname") %>% 
  ggplot(aes(x=year, y=value, linetype=varname)) +
  geom_line(size=1) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  theme_bw()  

value_change_data_measurement = measurement_model_mean %>% 
  left_join(measurement_model_coder, by="year") %>% 
  mutate(change_value = abs((mean_year  - dplyr::lag(mean_year ,1))),
         change_coder = ((mean_coder - dplyr::lag(mean_coder,1)))) 


### Expert Coders ####

vdem_vars = vdem_ds %>% 
  select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
  select_at(vars(-ends_with("_beta"),
                 -ends_with("_conf"),
                 -ends_with("bin"),
                 -ends_with("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
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
                 -matches("_20"))) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  na.omit() %>% 
  # select only C Variables
  mutate(name = gsub("_0", "", name)) %>% 
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) %>%
  # if multiple ratings per year, select only the latest (31.04.1931 < 31.12.1931)
  arrange(name, country_text_id, historical_date) %>% 
  group_by(name, country_text_id, year, coder_id) %>% 
  top_n(1, historical_date) %>% 
  ungroup() %>%
  select(-historical_date)
  

all_v2_coders = vdem_vars %>% 
  filter(year >= 1900)   %>% 
  na.omit() %>% 
  group_by(name, coder_id, country_text_id) %>% 
  summarise(coded_years_per_country = n()) %>% 
  ungroup()

# How many country-years coded per expert?
ctry_years_per_coder = all_v2_coders %>% 
  select(name, coder_id, country_text_id, coded_years_per_country) %>%
  group_by(coder_id) %>% 
  summarize(coded_year = sum(coded_years_per_country)) %>% 
  mutate(coder_id = as.factor(coder_id),
         coder_id = fct_reorder(coder_id, coded_year))

ctry_years_per_coder %>%  
  ggplot(aes(x=coder_id, y=coded_year)) +
  geom_bar(stat="identity", width=10) +
  geom_point() +
  scale_y_continuous(breaks=seq(0,100000,5000), limit=c(0, 75000)) +
  ylab("")  +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  geom_hline(aes(yintercept = median(coded_year)))

#Cumulative Percentage Plot
ctry_years_per_coder %>% 
  arrange(coded_year) %>% 
  mutate(cumsum = cumsum(coded_year)/sum(coded_year))  %>% 
  ggplot(aes(x=1:length(coded_year), y=cumsum)) +
  geom_line(size=1.1) +
  scale_y_continuous(label=percent) +
  ylab("Cumulative Percentage: Country-Years") +
  xlab("Country Experts") +
  theme_bw()

ctry_years_per_coder %>% 
  arrange(coded_year) %>% 
  mutate(cumsum = cumsum(coded_year)/sum(coded_year)) %>% 
  filter(cumsum > 0.5)

mean(ctry_years_per_coder$coded_year)
median(ctry_years_per_coder$coded_year)

# Top 15 coders
ctry_years_per_coder %>% 
  arrange(-coded_year) %>% 
  top_n(15)

all_v2_coders %>% 
  select(name, coder_id, country_text_id, coded_years_per_country) %>%
  left_join(country_coder_type, by=c("coder_id", "name")) %>% 
  group_by(coder_type) %>% 
  summarize(coded_year_per_type = sum(coded_years_per_country)) 

ctry_years_per_coder %>% 
  arrange(-coded_year) %>% 
  top_n(15) %>% 
  summarise(sum(coded_year))


vdem_vars %>% 
  filter(coder_id == 2348) %>% 
  summarise(n_ctry = n_distinct(country_text_id),
            n_vars = n_distinct(name),
            n_years = n_distinct(year))
vdem_vars %>% 
  filter(coder_id == 2348) %>% 
  distinct(country_text_id)

vdem_vars %>% 
  filter(coder_id == 1590) %>% 
  summarise(n_ctry = n_distinct(country_text_id),
            n_vars = n_distinct(name),
            n_years = n_distinct(year))
vdem_vars %>% 
  filter(coder_id == 1590) %>% 
  distinct(country_text_id)

vdem_vars %>% 
  filter(coder_id == 1901) %>% 
  summarise(n_ctry = n_distinct(country_text_id),
            n_vars = n_distinct(name),
            n_years = n_distinct(year))


# How many countries coded per expert?
all_v2_coders %>% 
  select(coder_id, country_text_id) %>%
  distinct() %>%
  group_by( coder_id) %>%
  summarize(countriespercoder = n()) %>% 
  mutate(coder_id = as.factor(coder_id),
         coder_id = fct_reorder(coder_id, countriespercoder)) %>%  
  ggplot(aes(x=coder_id, y=countriespercoder)) +
  scale_y_continuous(breaks=seq(0,50,5)) +
  geom_bar(stat="identity", width=10) +
  theme(axis.text.y = element_blank()) +
  theme_bw()

all_v2_coders %>% 
  select(coder_id, country_text_id) %>%
  distinct() %>%
  group_by( coder_id) %>%
  summarize(countriespercoder = n()) %>% 
  mutate(coder_id = as.factor(coder_id),
         coder_id = fct_reorder(coder_id, countriespercoder)) %>% 
  arrange(-countriespercoder)

all_v2_coders %>% 
  select(coder_id, country_text_id) %>%
  distinct() %>%
  group_by( coder_id) %>%
  summarize(countriespercoder = n()) %>% 
  ungroup() %>% 
  summarise(mean(countriespercoder), median(countriespercoder))

# How many variables coded per expert?
all_v2_coders %>% 
  select(coder_id, name) %>%
  distinct() %>%
  group_by( coder_id) %>%
  summarize(varspercoder = n()) %>% 
  mutate(coder_id = as.factor(coder_id),
         coder_id = fct_reorder(coder_id, varspercoder)) %>%  
  ggplot(aes(x=coder_id, y=varspercoder)) +
  scale_y_continuous(breaks=seq(0,250,20)) +
  geom_bar(stat="identity", width=10) +
  theme(axis.text.y = element_blank()) +
  xlab("") +
  theme_bw()

all_v2_coders %>% 
  select(coder_id, name) %>%
  distinct() %>%
  group_by( coder_id) %>%
  summarize(varspercoder = n()) %>% 
  ungroup() %>% 
  summarise(mean(varspercoder), median(varspercoder))

all_v2_coders %>% 
  select(coder_id, name) %>%
  distinct() %>%
  group_by( coder_id) %>%
  summarize(varspercoder =n()) %>% 
  ungroup() %>% 
  arrange(-varspercoder)

### Identify Historic/Bridge/Lateral Coder ####
if_historic = function(x) {
  historic = ifelse(any(x > 1920), 0, 1) 
  return(historic)
}

historic_coders_df = vdem_vars  %>% 
  select(name, country_text_id, coder_id, year) %>%
  na.omit() %>%
  group_by(name, coder_id) %>% 
  summarise(historc = if_historic(year)) %>% 
  filter(historc == 1) %>% 
  select(name, coder_id) %>% 
  distinct() %>% 
  mutate(coder_type = "HC")


country_coder_type = all_v2_coders %>% 
  anti_join(historic_coders_df, by=c("coder_id","name")) %>% 
  ungroup() %>% 
  select(name, coder_id, country_text_id, coded_years_per_country) %>%
  #filter(name == "v2elfrcamp") %>% 
  group_by(name, coder_id) %>% 
  summarize(coder_type = get_type(coded_years_per_country))  %>% 
  bind_rows(historic_coders_df) %>% 
  arrange(coder_id)


all_v2_coders  %>% 
  left_join(length_varname %>% rename(name = varname), by="name") %>%
  select(sections, coder_id) %>% 
  distinct() %>% 
  group_by(sections) %>% 
  summarise(no_coders = n()) %>% 
  arrange(no_coders)

# Overall
country_coder_type %>% 
  left_join(length_varname %>% rename(name = varname), by="name") %>% 
  group_by(coder_type) %>% 
  summarise(n())


# Type of Coders per Section
country_coder_type %>% 
  group_by(name,  coder_type) %>% 
  summarise(no = n()) %>% 
  left_join(length_varname %>% rename(name = varname), by="name") %>% 
  group_by(sections,  coder_type) %>% 
  summarise(no = mean(no)) %>%
  ungroup() %>% 
  mutate( coder_type = fct_relevel(coder_type, "HC", "BLC", "LC")) %>% 
  mutate( sections = fct_reorder2( sections, coder_type, -no)) %>% 
  ggplot(aes(x=sections, y=no, fill=coder_type)) +
  geom_bar(stat = "identity", position="dodge")


year_type_df %>% 
  left_join(length_varname %>% rename(name = varname), by="name") %>% 
  group_by(sections) %>% 
  summarise(summe = n()) %>% 
  arrange(summe)

# Type of Coders per Update
country_coder_type %>% 
  group_by(name,  coder_type) %>% 
  summarise(no = n()) %>% 
  left_join(length_varname %>% rename(name = varname), by="name") %>% 
  group_by(update, coder_type) %>% 
  summarise(no = mean(no)) %>%  
  ungroup() %>% 
  mutate( coder_type = fct_relevel(coder_type, "HC", "CC", "LC")) %>% 
  ggplot(aes(x=update , y=no, fill=coder_type)) +
  geom_bar(stat = "identity", position="dodge")


# Type of Coders per variable
country_coder_type %>% 
  group_by(name,  coder_type) %>% 
  summarise(no = n()) %>% 
  group_by(name, coder_type) %>% 
  summarise(no = mean(no)) %>%  
  ungroup() %>% 
  #mutate(coder_type = fct_relevel(coder_type, "HC", "CC", "LC")) %>% 
  mutate(name = fct_reorder2(name, coder_type, -no)) %>% 
  ggplot(aes(x=name , y=no, fill=coder_type)) +
  facet_wrap(coder_type ~ .) +
  geom_bar(stat = "identity", position="dodge")


# Type of Coders per year

year_type_df = vdem_ds %>% 
  select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
  select_at(vars(-ends_with("_beta"),
                 -ends_with("_conf"),
                 -ends_with("bin"),
                 -ends_with("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
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
                 -matches("_20"))) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_0", "", name)) %>% 
  filter(year >= 1900) %>% 
  na.omit() %>% 
  left_join(country_coder_type, by=c("name", "coder_id")) %>% 
  na.omit() %>% 
  # select only C Variables
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) 

frame_df = year_type_df %>% 
  group_by(year) %>% 
  summarise(countries_year = n_distinct(country_text_id),
            names_year = n_distinct(name))

codertype_years = year_type_df %>% 
  mutate(coder_type = as.factor(coder_type)) %>% 
  group_by(name, year, country_text_id, coder_type) %>%
  summarise(no = n())  %>% 
  group_by(name, year, coder_type) %>% 
  summarise(sum_country = sum(no, na.rm=T)) %>% 
  left_join(frame_df %>% select(-names_year), by="year") %>%
  mutate(mean_country = sum_country / countries_year) %>% 
  group_by(year, coder_type) %>% 
  summarise(mean_year = mean(mean_country)) %>% 
  group_by(year) %>% 
  mutate(total_year = sum(mean_year))

codertype_years %>% 
  ggplot(aes(x=year, y=mean_year, fill=coder_type)) +
  geom_area(size=1.1) +
  scale_y_continuous(breaks=seq(0,20, 2)) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  geom_hline(yintercept = 5) +
  theme_bw() +
  ggtitle("Number of Coders per codertype per year")


# Which countries are bridged? ####

bridged_df = year_type_df %>% 
  #filter(name == "v2clacfree") %>% 
  filter(coder_type != "CC",  coder_type != "HC") %>% 
  group_by(year, country_text_id) %>% 
  summarise(times_bridged = n()/163) %>% 
  group_by(country_text_id) %>% 
  summarise(times_bridged = mean(times_bridged)) %>% 
  arrange(times_bridged) 

bridged_df %>% 
  mutate(country_text_id = fct_reorder(country_text_id, times_bridged)) %>% 
  ggplot(aes(x=country_text_id, y=times_bridged)) +
  geom_point() +
  coord_flip() +
  ggtitle("Number of LC/BLC per country per year per variable")

bridged_df %>% 
  top_n(10)
bridged_df %>% 
  top_n(-10)


# Coder Agreement ####
vartype = vdem_ds %>% 
  select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
  select_at(vars(-ends_with("_beta"),
                 -ends_with("_conf"),
                 -ends_with("bin"),
                 -ends_with("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
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
                 -matches("_20"))) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_0", "", name)) %>% 
  filter(year >= 1900) %>% 
  na.omit() %>% 
  group_by(name) %>% 
  summarise(vartype = n_distinct(value)) %>% 
  mutate(percentageVar = ifelse(vartype >= 90, T, F)) %>% 
  filter(percentageVar == F)

multiplechoice_values = c("v2elsnlfc", "v2elsnmrfc", "v2psbantar", "v2exrmhsol",
                          "v2exctlhs", "v2exrmhgnp", "v2exctlhg", "v2regsupgroups", 
                          "v2clrgstch", "v2clrgwkch", "v2csanmvch","v2exl_legitideolcr")
c_disagree = vdem_ds %>% 
  select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
  select_at(vars(-ends_with("_beta"),
                 -ends_with("_conf"),
                 -ends_with("bin"),
                 -ends_with("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
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
                 -matches("_20"))) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_0", "", name)) %>% 
  filter(year >= 1900) %>% 
  na.omit()  %>%
  filter(name %in% vartype$name ) %>%
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) %>% 
  filter(name %!in% multiplechoice_values )%>% 
  group_by(name, country_text_id, year) %>% 
  summarise(disagree = sd(value))


c_disagree %>% 
  group_by(year) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  ggplot(aes(x=year, y=disagree)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  theme_bw()  

c_disagree %>% 
  group_by(country_text_id) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  mutate(country_text_id = fct_reorder(country_text_id, disagree)) %>% 
  ggplot(aes(x=country_text_id, y=disagree)) +
  geom_point() +
  theme(legend.position = "none") +
  theme_bw()  

c_disagree %>% 
  group_by(name) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  mutate(name = fct_reorder(name, disagree)) %>% 
  ggplot(aes(x=name, y=disagree)) +
  geom_point() +
  theme(legend.position = "none") +
  theme_bw()   +
  coord_flip()

c_disagree %>% 
  group_by(name) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  arrange(disagree) %>% 
  top_n(10)
c_disagree %>% 
  group_by(name) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  arrange(disagree) %>% 
  top_n(-10)

c_disagree %>% 
  left_join(length_varname %>%  rename(name = varname), by="name") %>% 
  group_by(sections) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  mutate(sections = fct_reorder(sections, disagree)) %>% 
  ggplot(aes(x=sections, y=disagree)) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  theme_bw()

c_disagree %>% 
  left_join(vartype, by="name") %>% 
  filter(vartype == 5) %>% 
  left_join(length_varname %>%  rename(name = varname), by="name") %>% 
  group_by(sections) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  mutate(sections = fct_reorder(sections, disagree)) %>% 
  ggplot(aes(x=sections, y=disagree)) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  theme_bw() +
  ggtitle("Only Indicators with 5 answer categories")


# Regression Setup ####
# Create Independent Variables
source("Setup/LoadDatasets.R")

set.seed(1234)
regression_vars = length_varname %>% 
  right_join(vartype %>%  rename(varname = name), by="varname") %>% 
  filter(codetype == "C") %>% 
  #filter(vartype == 5) %>% 
  sample_frac(1)

question_diff = vdem_ds %>% 
  select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
  select_at(vars(-ends_with("_beta"),
                 -ends_with("_conf"),
                 -ends_with("bin"),
                 -ends_with("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
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
                 -matches("_20"))) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_0", "", name)) %>% 
  filter(year >= 1900) %>% 
  na.omit()  %>% 
  right_join(regression_vars %>% rename(name = varname), by="name") %>% 
  group_by(name, country_text_id, year) %>% 
  summarise(disagree = sd(value)) %>% 
  group_by(name) %>% 
  summarise(q_diff = sd(disagree, na.rm=T)) 

  
variable_caus = question_diff %>% 
  left_join(vartype %>% select(name, nr_categories = vartype), by="name")
  
OECD_member = fread("Datasets/OECD_member.csv") %>% 
  left_join(vdem_main %>%  select(country_name, country_text_id) %>%  distinct(), by="country_name") %>% 
  select(country_text_id, OECD)
dmx_reg_data = fread("unzip -p Datasets/DemocracyMatrix_v1_1.zip", encoding = "UTF-8")  %>% 
  left_join(fread("unzip -p C:/RTest/V-Dem-CY+Others-v8.zip", encoding = "UTF-8") %>% 
              dplyr::select(country = country_name, country_text_id, year), by=c("country", "year")) %>% 
  select(country_text_id, regions) %>% 
  distinct()

QoC_caus = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_wdi = wdi_gdpcapcur,
         pop_wdi = wdi_pop) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  filter(year >= 1970) %>% 
  group_by(country_text_id) %>% 
  summarise(popsize = log10(mean(pop_wdi, na.rm=T)),
            loggdp = log10(mean(GDP_capita_wdi, na.rm=T))
  )

# QoC_data %>%
#   select(country, country_text_id) %>%
#   filter(country_text_id == "BDI")

vdem_caus = vdem_main %>% 
  select(country_text_id, year, 
         polrights_fh = e_fh_pr, 
         internalconf = e_miinterc,
         coups = e_coups) %>% 
  mutate(polrights_fh = as.numeric(polrights_fh)) %>% 
  group_by(country_text_id) %>% 
  summarise(polrights_mean_fh = mean(polrights_fh, na.rm=T),
            polrights_sd_fh = sd(polrights_fh, na.rm=T),
            coups = sum(coups, na.rm=T)
            )


# Regression Setup: Number Coders ####
# Create Dependent Variable
vartype = vdem_ds %>% 
  select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
  select_at(vars(-ends_with("_beta"),
                 -ends_with("_conf"),
                 -ends_with("bin"),
                 -ends_with("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
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
                 -matches("_20"))) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_0", "", name)) %>% 
  filter(year >= 1900) %>% 
  na.omit() %>% 
  group_by(name) %>% 
  summarise(vartype = n_distinct(value)) %>% 
  mutate(percentageVar = ifelse(vartype >= 90, T, F)) %>% 
  filter(percentageVar == F)


reg_coder_df = vdem_main %>% 
  select_at(vars(country_text_id, year,  ends_with("_nr"))) %>%
  select_at(vars(country_text_id, year,  starts_with("v2"))) %>%
  select_at(vars(-ends_with("bin_nr"))) %>%
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_nr", "", name)) %>% 
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) %>% 
  filter(name %in% vartype$name ) %>% 
  na.omit() 


reg_coder_df %>% 
  group_by(name) %>% 
  summarise(nr_coders = mean(value)) %>% 
  right_join(regression_vars %>% rename(name = varname), by="name") %>% 
  mutate(name  = fct_reorder(name , nr_coders)) %>% 
  ggplot(aes(x=name , y=nr_coders)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Mean number of coders per country-year per Section")

reg_coder_df %>% 
  group_by(name) %>% 
  summarise(abs_indicator = n()) %>% 
  right_join(regression_vars %>% rename(name = varname), by="name") %>% 
  ungroup() %>% 
  mutate(name  = fct_reorder(name , abs_indicator)) %>% 
  ggplot(aes(x=name, y=abs_indicator)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Absoulte number of country-years per Section")



reg_data = reg_coder_df %>% 
  group_by(name, country_text_id) %>%
  mutate(year_count = dplyr::row_number()) %>% 
  summarise(nr_coder = sum(value),
            years_counted = max(year_count)) %>% 
  right_join(regression_vars %>% rename(name = varname), by="name") %>% 
  select(name, country_text_id, nr_coder, years_counted, update, sections)


reg_data %>% 
  ggplot(aes(x=nr_coder)) +
  geom_histogram()

reg_data %>% 
  group_by(name) %>% 
  summarise(nr_coder = mean( nr_coder)) %>%
  mutate(name  = fct_reorder(name, nr_coder)) %>% 
  ggplot(aes(x=name, y=nr_coder)) +
  geom_bar(stat="identity")


# Regression 1: No Coders ####
# merge data
reg_data_caus = reg_data %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  mutate(OECD=ifelse(is.na(OECD) == T, 0, OECD)) %>% 
  left_join(dmx_reg_data, by="country_text_id")  %>% 
  left_join(variable_caus, by="name") %>% 
  ungroup() %>% 
  mutate(obs_effect = 1:nrow(.)) %>% 
  sample_frac(0.25)


reg_data_caus %>% 
  ungroup() %>% 
  bind_cols(fastDummies::dummy_cols(reg_data_caus$regions, ignore_na = T)) %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="number")

# reg_data_caus %>% 
#   ggplot(aes(x=polrights_sd_fh, y=nr_coder)) +
#   geom_point()

m0_noml <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)), 
              family=truncated_poisson(link = "log"), 
              reg_data_caus)
summary(m0_noml)

m0 <- glmmTMB(nr_coder ~ 1 + (1|name), 
              family=truncated_poisson(link = "log"), 
              reg_data_caus)
summary(m0)
anova(m0_noml,m0)

m1 <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
                # regions + 
                popsize + 
                loggdp + 
                polrights_mean_fh + I(polrights_mean_fh^2) +
                polrights_sd_fh +
                coups +
                (1|name) + (1|obs_effect), 
              family=truncated_poisson(link = "log"), 
              reg_data_caus)
summary(m1)
check_overdispersion(m1)
anova(m0,m1)


m2 <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
                #regions + 
                popsize + 
                loggdp + 
                polrights_mean_fh + I(polrights_mean_fh^2) +
                polrights_sd_fh +
                coups +
                q_diff + 
                nr_categories +
                (1|name) + (1|obs_effect), 
              family=truncated_poisson(link = "log"), 
              reg_data_caus)
summary(m2)
anova(m1,m2)

source("Setup/Sig_Tables.R")
make_glmm_tables(m0,m1,m2)


plot(effect("polrights_mean_fh",m2))
interplot(m2)
### Regression 2: No Bridged/Lateral Coders ####

reg_data_bridged = year_type_df %>% 
  mutate(bridged = if_else(coder_type == "LC" |  coder_type == "BLC", 1, 0)) %>% 
  group_by(name, year, country_text_id) %>% 
  summarise(times_bridged = sum(bridged))   %>% 
  group_by(name, country_text_id) %>% 
  mutate(year_count = dplyr::row_number()) %>% 
  summarise(times_bridged = sum(times_bridged),
            years_counted = max(year_count)
            ) 


hist(reg_data_bridged$times_bridged)

reg_data_bridged_caus = reg_data_bridged %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  mutate(OECD=ifelse(is.na(OECD) == T, 0, OECD)) %>% 
  left_join(dmx_reg_data, by="country_text_id")  %>% 
  left_join(variable_caus, by="name") %>% 
  ungroup() %>% 
  mutate(obs_effect = 1:nrow(.)) %>% 
  sample_frac(0.25)


m0 <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) + (1|name), 
              family=nbinom1(link = "log"), 
              reg_data_bridged_caus)
summary(m0)
anova(m0_noml,m0)

m1 <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) +
                regions + 
                popsize + 
                loggdp + 
                polrights_mean_fh + I(polrights_mean_fh^2) +
                polrights_sd_fh +
                coups +
                (1|name), 
              family=nbinom1(link = "log"), 
              reg_data_bridged_caus)
summary(m1)

anova(m0,m1)


m2 <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) +
                #regions + 
                popsize + 
                loggdp + 
                polrights_mean_fh + I(polrights_mean_fh^2) +
                polrights_sd_fh +
                coups +
                q_diff +
                nr_categories +
                (1|name) + (1|obs_effect),
              family=poisson(link = "log"), 
              reg_data_bridged_caus)
summary(m2)
anova(m1,m2)
library(performance)
make_glmm_tables(m1)
icc(m1)
r2(m2)
performance::check_singularity(m2)
check_overdispersion(m2)
check_zeroinflation(m2)

make_glmm_tables(m1, m2)

### Regression 3: Disagreement ####

c_disagree = vdem_ds %>% 
  select_at(vars(country_text_id, coder_id, year, historical_date, starts_with("v2"))) %>% 
  select_at(vars(-ends_with("_beta"),
                 -ends_with("_conf"),
                 -ends_with("bin"),
                 -ends_with("_1"), 
                 -matches("_2"), 
                 -matches("_3"), 
                 -matches("_4"), 
                 -matches("_5"), 
                 -matches("_6"), 
                 -matches("_7"), 
                 -matches("_8"), 
                 -matches("_9"), 
                 -matches("_10"), 
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
                 -matches("_20"))) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  mutate(name = gsub("_0", "", name)) %>% 
  filter(year >= 1900) %>% 
  na.omit()  %>%
  filter(name %in% vartype$name ) %>%
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) %>% 
  filter(name %!in% multiplechoice_values )%>% 
  group_by(name, country_text_id, year) %>% 
    summarise(disagree = sd(value))

disagree_df = c_disagree %>% 
  left_join(vartype, by="name") %>% 
  filter(vartype == 5) %>% 
  left_join(length_varname %>%  rename(name = varname), by="name") %>% 
  group_by(name, country_text_id) %>% 
  summarise(disagree = mean(disagree, na.rm = T))

hist(disagree_df$disagree, breaks=100)

disagree_reg = disagree_df %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  mutate(OECD=ifelse(is.na(OECD) == T, 0, OECD)) %>% 
  left_join(dmx_reg_data, by="country_text_id")  %>% 
  left_join(variable_caus, by="name") %>% 
  left_join(reg_data %>% mutate(coders_p_year = nr_coder/years_counted), by=c("name", "country_text_id")) %>% 
  ungroup()   %>% 
  filter(disagree > 0)
hist(disagree_reg$disagree, breaks=100)


m1 <- glmmTMB(disagree ~ 1 +
                popsize + 
                loggdp + 
                polrights_mean_fh + I(polrights_mean_fh^2) +
                polrights_sd_fh +
                coups +
                coders_p_year +
                (1|name),
              family=gaussian, 
              disagree_reg)
summary(m1)

check_distribution(m1)
hist(predict(m1, type = "response"), breaks=100)


m2 <- glmmTMB(disagree ~ 1 +
                popsize + 
                loggdp + 
                polrights_mean_fh + I(polrights_mean_fh^2) +
                polrights_sd_fh +
                coups +
                q_diff +
                coders_p_year +
                (1|name),
              family=gaussian, 
              disagree_reg)
summary(m2)
check_model(m2)
plot(effect("polrights_mean_fh",m2))
###

rater_beta = vdem_ds %>% 
  select(coder_id, v2clkill_beta) %>% 
  filter(is.na(v2clkill_beta) == F) 


ratings_country = vdem_ds %>% 
  select(country, country_text_id, coder_id, year, v2clkill, v2clkill_conf) %>% 
  left_join(rater_beta, by="coder_id") %>% 
  filter(year >= 1900) %>% 
  na.omit()


test = ratings_country %>% 
  group_by(country) %>% 
  summarise(beta_mean = mean(v2clkill_beta, na.rm=T),
            conf_mean = mean(v2clkill_conf, na.rm=T)) %>% 
  mutate(country = fct_reorder(country, beta_mean))

test %>% 
  ggplot(aes(x=country, y=beta_mean)) +
  geom_point() +
  coord_flip()

test %>% 
  ggplot(aes(x=conf_mean, y=beta_mean)) +
  geom_point() +
  geom_smooth(method="lm", se=F, color="red")

cor(test$beta_mean, test$conf_mean, use="pairwise")


sd_fh = vdem_main %>% 
  select(country = country_name, year, 
         e_polity2, 
         e_civil_war, 
         e_coups,
         e_wb_pop,
         e_migdppcln) %>% 
  filter(year >= 1900) %>% 
  group_by(country) %>% 
  summarise(mean_pol = mean(e_polity2, na.rm=T),
            sd_pol = sd(e_polity2, na.rm=T),
            sum_cw = sum(e_civil_war, na.rm=T),
            sum_coups = sum(e_coups, na.rm=T),
            mean_pop = mean(log(e_wb_pop), na.rm=T),
            mean_gdp = mean(e_migdppcln, na.rm=T))


reg_data = test %>% 
  left_join(sd_fh, by="country")
m1 = lm(beta_mean ~ mean_pol+ sd_pol + sum_cw + sum_coups + mean_pop + mean_gdp, reg_data)
summary(m1)



plot(m1)
library(car)
influencePlot(m1)
cooks.distance(m1)
interplot::interplot(m1, var1="mean_pol", var2="sd_pol")

margins(m1)
cplot(m1, "mean_pol")
persp(m1, "mean_pol", "sd_pol")


