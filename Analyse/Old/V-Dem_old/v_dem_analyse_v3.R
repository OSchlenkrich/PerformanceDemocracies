# V-Dem Chapter 2.1 Script

# Setup #####
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

if_historic = function(x) {
  historic = ifelse(any(x > 1920), 0, 1) 
  return(historic)
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
library(ggeffects)
library(performance)

source("Setup/Packages.R")
source("Setup/Base_Functions.R")
source("Setup/Plotting_Functions.R")
source("Setup/Sig_Tables.R")
source("Setup/LoadDatasets.R")


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


# jureview
Lijphart_ju = read.csv2("country_id_Lijphart.csv") %>% 
  left_join(fread("Datasets/Lijphart.csv") %>%  rename(country_text_id = country) , by="country_text_id" ) %>% 
  select( country,
         index_of_judicial_review_1945_2010,
         index_of_judicial_review_1981_2010)
  

p1 = V_dem_all %>% 
  select(country, year, v2jureview) %>% 
  filter(year >= 1945, year <= 2010) %>% 
  group_by(country) %>% 
  summarise(v2jureview_1945_2010 = mean(v2jureview, na.rm=T)) %>% 
  left_join(Lijphart_ju, by="country") %>%  
  na.omit() %>% 
  ggplot(aes(x=v2jureview_1945_2010, y=index_of_judicial_review_1945_2010)) +
  geom_point() +
  geom_smooth(method=lm, se=F, col="black") +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  theme_bw()

p2 = V_dem_all %>% 
  select(country, year, v2jureview) %>% 
  filter(year >= 1981, year <= 2010) %>% 
  group_by(country) %>% 
  summarise(v2jureview_1981_2010 = mean(v2jureview, na.rm=T)) %>% 
  left_join(Lijphart_ju, by="country") %>%  
  na.omit() %>% 
  ggplot(aes(x=v2jureview_1981_2010, y=index_of_judicial_review_1981_2010)) +
  geom_point() +
  geom_smooth(method=lm, se=F, col="black") +
  stat_cor(method = "pearson", label.x = -1, label.y = 3) +
  theme_bw()

grid.arrange(p1,p2)
  
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


### Multiple Choice and Percentage Variables ####

#v2svstterr: scale direction
vdem_ds %>% 
  select(country, year, coder_id, v2svstterr) %>% 
  filter(country == "Germany",
         year >= 1900) %>% 
  mutate(coder_id = as.factor(coder_id)) %>% 
  ggplot(aes(x=year, y=v2svstterr, col=coder_id)) + 
  geom_line(size=1.2)  + 
  geom_point()  +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none")

vdem_ds %>% 
  select(country, year, coder_id, v2svstterr) %>% 
  filter(country == "Germany",
         year >= 1900) %>% 
  top_n(1, -v2svstterr)


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

# value_change_data_multiple = multiplechoice_values %>% 
#   left_join(multiplechoice_coders, by="year") %>% 
#   mutate(change_value = mean_perc  - dplyr::lag(mean_perc ,1),
#          change_coder = (mean_coder / dplyr::lag(mean_coder,1)) - 1,
#          change_coder = round(abs(change_coder),2)
#   )

multiplechoice_values %>% 
  left_join(multiplechoice_coders, by="year") %>% 
  mutate(change_value = abs(mean_perc  - dplyr::lag(mean_perc ,1)),
         change_coder = (mean_coder / dplyr::lag(mean_coder,1)) - 1,
         change_coder = abs(change_coder), 
         sig_year = ifelse(change_coder > 0.05, year,NA), 
         sig_value = ifelse(change_coder > 0.05, change_value,NA)
         ) %>%  
  ggplot(aes(x=year, y=change_value)) +
  geom_line(size=1) +
  geom_point(aes(x=sig_year, y=sig_value), size=2, color="red") +
  theme_bw() 



# Compare this to the variables subjected to the measurement model
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
  mutate(change_value = abs(mean_year  - dplyr::lag(mean_year ,1)),
         change_coder = (mean_coder / dplyr::lag(mean_coder,1)) - 1,
         change_coder = abs(change_coder), 
         sig_year = ifelse(change_coder > 0.05, year,NA), 
         sig_value = ifelse(change_coder > 0.05, change_value,NA)
  ) %>%  
  ggplot(aes(x=year, y=change_value)) +
  geom_line(size=1) +
  geom_point(aes(x=sig_year, y=sig_value), size=2, color="red") +
  theme_bw() 



value_change_data_measurement = measurement_model_mean %>% 
  left_join(measurement_model_coder, by="year") %>% 
  mutate(change_value = mean_year  - dplyr::lag(mean_year ,1),
           change_coder = (mean_coder / dplyr::lag(mean_coder,1)) - 1,
           change_coder = round(abs(change_coder),2),
         change_value = round(abs(change_value),4)
  )



### !Expert Coders! ####

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

### Identify Historic/Bridge/Lateral Coder

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



year_type_df %>% 
  mutate(coder_type = as.factor(coder_type)) %>% 
  group_by(name, year, country_text_id, coder_type) %>%
  summarise(no = n())  %>% 
  group_by(name,country_text_id, year, coder_type) %>% 
  summarise(sum_country = sum(no, na.rm=T)) %>% 
  group_by(name,country_text_id, year) %>% 
  mutate(total_country = sum(sum_country)) %>% 
  group_by(year,  coder_type) %>% 
  summarise(mean_country = sum(sum_country, na.rm=T)) %>% 
  group_by(year) %>% 
  mutate(total_country = sum(mean_country) ) %>% 
  mutate(perc = mean_country/total_country) %>% 
  left_join(mean_coder_df %>% 
              group_by(year, country_name) %>% 
              summarise(mean_country = mean(value, na.rm=T)) %>% 
              group_by(year) %>% 
              summarise(mean_coder = mean(mean_country, na.rm=T)), by ="year") %>% 
  mutate(part = mean_coder * perc) %>% 
  ggplot(aes(x=year, y=part, fill=coder_type)) +
  geom_area(size=1.1) +
  scale_y_continuous(breaks=seq(0,20, 2)) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  geom_hline(yintercept = 5) +
  theme_bw() +
  ggtitle("Number of Coders per codertype per year")


t2 %>% 
  left_join(t1, by=c("name", "country_text_id", "year")) %>% 
  group_by(year,  coder_type) %>% 
  summarise(mean_country = sum(sum_country, na.rm=T),
            total_country = sum(total_country, na.rm=T)) %>% 
  group_by(year) %>% 
  mutate(total_country = sum(mean_country) ) %>% 
  mutate(perc = mean_country/total_country) %>% 
  left_join(mean_coder_df %>% 
              group_by(year, country_name) %>% 
              summarise(mean_country = mean(value, na.rm=T)) %>% 
              group_by(year) %>% 
              summarise(mean_coder = mean(mean_country, na.rm=T)), by ="year") %>% 
  mutate(part = mean_coder * perc) %>% 
  ggplot(aes(x=year, y=part, fill=coder_type)) +
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
  filter(name %!in% multiplechoice_values ) %>% 
  group_by(name, country_text_id, year) %>% 
  summarise(disagree = sd(value))


c_disagree %>% 
  group_by(year) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  ggplot(aes(x=year, y=disagree)) +
  geom_line(size=1) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  xlab("") +
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


# disareement between historic and other coder types

hc_vs_other = year_type_df %>%
  #filter(name == "v2csantimv") %>% 
  filter(name %in% vartype$name ) %>%
  filter(name %in% (length_varname %>% filter(codetype  == "C") %>% pull(varname))) %>% 
  filter(name %!in% multiplechoice_values ) %>% 
  mutate(CC_coder = ifelse(coder_type == "CC", "CC", NA),
         #year = floor(year/10) * 10
         ) %>%
  #na.omit() %>% 
  group_by(name, year, country_text_id, coder_type) %>% 
  mutate(mean_CC = mean(value, na.rm=T),
         mean_CC = ifelse(CC_coder == "CC", mean_CC, NA)) %>% 
  group_by(name, year, country_text_id)  %>% 
  mutate(mean_CC = mean(mean_CC, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(difference = value - mean_CC) %>% 
  group_by(name, year, country_text_id, coder_type) %>% 
  summarise(difference_mean = mean(difference, na.rm=T)) 
  
hc_vs_other %>% 
  group_by(year, coder_type ) %>% 
  summarise(mean_value = mean(difference_mean, na.rm=T)) %>%  
  ggplot(aes(x=year, y=mean_value, col=coder_type )) + 
  geom_line() +
  geom_point(aes(shape=coder_type ), size=1.7) +
  scale_y_continuous(breaks=seq(0,4,0.2)) +
  scale_color_grey(start = 0.1, end = 0.7) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  xlab("") +
  theme_bw()


# Regression Setup ####
# Independent Variables ####
multiplechoice_values = c("v2elsnlfc", "v2elsnmrfc", "v2psbantar", "v2exrmhsol",
                          "v2exctlhs", "v2exrmhgnp", "v2exctlhg", "v2regsupgroups", 
                          "v2clrgstch", "v2clrgwkch", "v2csanmvch","v2exl_legitideolcr")

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
         pop_wdi = wdi_pop,
         primaryschool_wdi = wdi_gerp) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  filter(year >= 1970) %>% 
  group_by(country_text_id) %>% 
  summarise(popsize = log10(mean(pop_wdi, na.rm=T)),
            loggdp = log10(mean(GDP_capita_wdi, na.rm=T)),
            primaryschool_wdi = mean(primaryschool_wdi, na.rm=T)
  )

# QoC_data %>%
#   select(country, country_text_id) %>%
#   filter(country_text_id == "BDI")

vdem_caus = vdem_main %>% 
  select(country_text_id, year, 
         polrights_fh = e_fh_pr, 
         internalconf = e_miinterc,
         coups = e_coups) %>% 
  group_by(country_text_id) %>% 
  mutate(polrights_rev_fh = 7 - as.numeric(polrights_fh),
         polrights_rev_fh_lag = dplyr::lag(polrights_rev_fh, 1),
         polrights_rev_variability_fh = polrights_rev_fh- polrights_rev_fh_lag) %>% 
  summarise(polrights_rev_mean_fh = mean(polrights_rev_fh, na.rm=T),
            polrights_rev_variability_fh = mean(abs(polrights_rev_variability_fh), na.rm=T),
            coups = sum(coups, na.rm=T)
            ) %>% 
  mutate(no_coups = max(coups, na.rm=T) - coups,
         polrights_rev_variability_fh = max(polrights_rev_variability_fh, na.rm=T) - polrights_rev_variability_fh) %>% 
  select(-coups)


# Dependent Variable ####

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


# Regression 1: Nr Coders ####
# merge data
reg_data_caus = reg_data %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  mutate(OECD=ifelse(is.na(OECD) == T, 0, OECD)) %>% 
  left_join(dmx_reg_data, by="country_text_id")  %>% 
  left_join(variable_caus, by="name") %>% 
  ungroup() %>% 
  na.omit() %>%
  #sample_frac(0.25) %>% 
  mutate(obs_effect = 1:nrow(.))

hist(reg_data_caus$nr_coder)

reg_data_caus %>% 
  mutate(rate = nr_coder/years_counted) %>% 
  group_by(country_text_id) %>% 
  summarise(rate = mean(rate)) %>% 
  mutate(rate = round(rate,0)) %>% 
  group_by(rate) %>% 
  summarise(nr_sum = n()) %>% 
  ggplot(aes(x=rate, y = nr_sum)) +
  geom_point() +
  geom_line()

reg_data_caus %>% 
  mutate(rate = nr_coder/years_counted) %>% 
  group_by(country_text_id) %>% 
  summarise(rate = mean(rate)) %>% 
  mutate(rate = round(rate,0)) %>% 
  ungroup() %>% 
  summarise(mean(rate), var(rate))

reg_data_caus %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="number")

# Analysis
# Null model without ML
m0_noml_p <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)), 
              family=truncated_poisson(link = "log"), 
              reg_data_caus)
summary(m0_noml_p)

m0_noml_nb <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)), 
                      family=truncated_nbinom2(link = "log"), 
                      reg_data_caus)
summary(m0_noml_nb)

# Null model with ML: Poisson
m0_p <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) + 
                   (1|name), 
              family=truncated_poisson(link = "log"), 
              reg_data_caus)
summary(m0_p)

# Null model with ML: Negative Binomial
m0_nb <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) + 
                   (1|name), 
                 family=truncated_nbinom2(link = "log"), 
                 reg_data_caus)
summary(m0_nb)

# Null model with ML: genpois can handle udnerdispersion?
# m0_qp <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
#                    (1|name), disp=~1,
#                  family=truncated_genpois(link = "log"),
#                  reg_data_caus)
# summary(m0_qp)


# favors ML and nbinom
anova(m0_noml_p,  m0_p, m0_noml_nb,m0_nb) %>% 
   make_anova_table()


# data.frame(rate = predict(m0_nc, newdata = reg_data_caus %>%  mutate(years_counted = 1),
#         type = "response", offset=0)) %>% 
#   mutate(rate = round(rate, 0)) %>% 
#   group_by(rate) %>% 
#   summarise(nr_sum = n()) %>% 
#   ggplot(aes(x=rate, y = nr_sum)) +
#   geom_point() +
#   geom_line()
# 
# data.frame(rate = predict(m0_nc, newdata = reg_data_caus %>%  mutate(years_counted = 1),
#                           type = "response", offset=0)) %>% 
#   mutate(rate = round(rate, 0)) %>% 
#   ungroup() %>% 
#   summarise(mean(rate), var(rate))



# Control + Modernization
m1_nc <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
                   loggdp + 
                   primaryschool_wdi + 
                   (1|name), 
                 family=truncated_nbinom2(link = "log"), 
                 reg_data_caus)
summary(m1_nc)
r2(m1_nc)
anova(m0_nc,m1_nc)

# Control + Modernization + Population Size
m2_nc <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
                   loggdp + 
                   primaryschool_wdi + 
                   popsize + 
                   (1|name), 
                 family=truncated_nbinom2(link = "log"), 
                 reg_data_caus)
summary(m2_nc)
r2(m2_nc)

# Control + Modernization + Population Size + DQ
m3_nc <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
                   loggdp + 
                   primaryschool_wdi + 
                   popsize + 
                   polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                   (1|name), 
                 family=truncated_nbinom2(link = "log"), 
                 reg_data_caus)
summary(m3_nc)
r2(m3_nc)

# Control + Modernization + Population Size + DQ + DiffObj
m4_nc <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
                   loggdp + 
                   primaryschool_wdi + 
                   popsize + 
                   polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                   polrights_rev_variability_fh +
                   no_coups +
                   (1|name), 
                 family=truncated_nbinom2(link = "log"), 
                 reg_data_caus)
summary(m4_nc)
r2(m4_nc)

# Control + Modernization + Population Size + DQ + DiffObj + DiffQuestion
m5_nc <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
                loggdp + 
                primaryschool_wdi + 
                popsize + 
                polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                #polrights_rev_variability_fh +
                no_coups +
                q_diff + 
                nr_categories +
                sections +
                regions +
                update +
                (1|country_text_id), 
              family=truncated_nbinom2(link = "log"), 
              reg_data_caus %>%  sample_n(5000))
summary(m5_nc)
r2(m5_nc)

data.frame(rate = predict(m5_nc, newdata = reg_data_caus %>%  mutate(years_counted = 1),
                          type = "response")) %>% 
  bind_cols(reg_data_caus %>%  select(country_text_id)) %>% 
  group_by(country_text_id) %>%
  summarise(rate = mean(rate)) %>%
  mutate(rate = round(rate,0)) %>% 
  group_by(rate) %>% 
  summarise(nr_sum = n()) %>% 
  ggplot(aes(x=rate, y = nr_sum)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(2,10,2))


make_glmm_tables(m0_nb, m1_nc, m2_nc, m3_nc, m4_nc, m5_nc)
anova(m0_nb, m1_nc, m2_nc, m3_nc, m4_nc, m5_nc) %>% 
  make_anova_table()

# For R2
# m6_nc <- glmmTMB(nr_coder ~ 1 + offset(log(years_counted)) +
#                    loggdp + 
#                    primaryschool_wdi + 
#                    popsize + 
#                    polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
#                    polrights_rev_sd_fh +
#                    coups +
#                    q_diff + 
#                    nr_categories +
#                    (1|name), 
#                  family=nbinom2(link = "log"), 
#                  reg_data_caus)
# summary(m6_nc)

# Visualization
eff_m5_nc = ggeffect(m5_nc, offset=0)
get_complete_df(eff_m5_nc) %>% 
  filter(group != "years_counted") %>% 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax = conf.high)) +
  facet_wrap(group ~ ., scales = "free_x") +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  geom_hline(yintercept = 5)

# Own Sample
dmx_reg_vis = fread("unzip -p Datasets/DemocracyMatrix_v1_1.zip", encoding = "UTF-8")  %>% 
  left_join(fread("unzip -p C:/RTest/V-Dem-CY+Others-v8.zip", encoding = "UTF-8") %>% 
              dplyr::select(country = country_name, country_text_id, year), by=c("country", "year")) %>% 
  select(country_text_id, regions, classification_core) %>% 
  filter(classification_core == "Deficient Democracy" | classification_core == "Working Democracy") %>% 
  select(country_text_id) %>% 
  distinct()

new_data = dmx_reg_vis %>% 
  left_join(reg_data_caus, by="country_text_id") %>% 
  select(loggdp, primaryschool_wdi,
         popsize,
         polrights_rev_mean_fh, 
         polrights_rev_variability_fh, no_coups,
         nr_categories, q_diff) %>% 
  pivot_longer(cols=everything()) %>% 
  group_by(name) %>% 
  summarise(conf_lower = quantile(value, 0.25, na.rm=T),
            conf_median = quantile(value, 0.5, na.rm=T),
            conf_upper = quantile(value, 0.75, na.rm=T)) %>% 
  pivot_longer(cols=starts_with("conf_"), names_to = "conf") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(years_counted = 1,
         name = NA,
         q_diff = 0.459,
         nr_categories = 5)

predict(m5_nc, newdata = new_data, type="response", offset=0, 
        se.fit=F)

### Regression 2: Nr Bridged/Lateral Coders ####

reg_data_bridged = year_type_df %>% 
  # after 2005: influx of bridge and lateral coders
  # otherwise one would punish countries with long time series
  filter(year >= 2005) %>% 
  mutate(bridged = if_else(coder_type == "LC" |  coder_type == "BLC", 1, 0)) %>% 
  group_by(name, year, country_text_id) %>% 
  summarise(times_bridged = sum(bridged))   %>% 
  group_by(name, country_text_id) %>% 
  mutate(year_count = dplyr::row_number()) %>% 
  summarise(times_bridged = sum(times_bridged),
            years_counted = max(year_count)
            ) 

reg_data_bridged %>% 
  mutate(rate = times_bridged/years_counted) %>% 
  group_by(country_text_id) %>% 
  summarise(rate = mean(rate)) %>% 
  mutate(rate = round(rate,0)) %>% 
  group_by(rate) %>% 
  summarise(nr_sum = n()) %>% 
  ggplot(aes(x=rate, y = nr_sum)) +
  geom_point() +
  geom_line()

reg_data_bridged %>% 
  mutate(rate = times_bridged/years_counted) %>% 
  group_by(country_text_id) %>% 
  summarise(rate = mean(rate)) %>% 
  mutate(rate = round(rate,0)) %>% 
  ungroup() %>% 
  summarise(mean(rate), var(rate))


reg_data_bridged_caus = reg_data_bridged %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(OECD_member, by="country_text_id") %>% 
  mutate(OECD=ifelse(is.na(OECD) == T, 0, OECD)) %>% 
  left_join(dmx_reg_data, by="country_text_id")  %>% 
  left_join(variable_caus, by="name") %>% 
  ungroup() %>% 
  mutate(obs_effect = 1:nrow(.)) %>% 
  sample_frac(1)



# Analysis
# Null model without ML
m0_noml_bc <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)), 
              family=nbinom2(link = "log"), 
              reg_data_bridged_caus)

# Null model with ML
m0_bc <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) + 
                   (1|country_text_id), 
              family=nbinom2(link = "log"), 
              reg_data_bridged_caus)
summary(m0_bc)
r2(m0_bc)

data.frame(rate = predict(m0_bc, newdata = reg_data_bridged_caus %>%  mutate(years_counted = 1),
                          type = "response")) %>% 
  bind_cols(reg_data_bridged_caus %>%  select(country_text_id)) %>% 
  group_by(country_text_id) %>%
  summarise(rate = mean(rate)) %>%
  mutate(rate = round(rate,0))  %>% 
  group_by(rate) %>% 
  summarise(nr_sum = n()) %>% 
  ggplot(aes(x=rate, y = nr_sum)) +
  geom_point() +
  geom_line()


#favors ML
anova(m0_noml_bc,m0_bc)

# Control + Modernization
m1_bc <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) +
                loggdp + 
                primaryschool_wdi + 
                (1|country_text_id), 
              family=nbinom2(link = "log"), 
              reg_data_bridged_caus)
summary(m1_bc)
anova(m0_bc,m1_bc)
r2(m1_bc)


# Control + Modernization + Population Size
m2_bc <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) +
                   loggdp + 
                   primaryschool_wdi +
                   popsize +
                   (1|country_text_id), 
                 family=nbinom2(link = "log"), 
                 reg_data_bridged_caus)
summary(m2_bc)
anova(m1_bc,m2_bc)
r2(m2_bc)


# Control + Modernization + Population Size + DQ
m3_bc <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) +
                loggdp + 
                primaryschool_wdi +
                popsize + 
                polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                (1|country_text_id),
              family=nbinom2(link = "log"), 
              reg_data_bridged_caus)
summary(m3_bc)
anova(m2_bc, m3_bc)
r2(m3_bc)

# Control + Modernization + Population Size + DQ + DiffObj
m4_bc <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) +
                loggdp + 
                primaryschool_wdi +
                popsize + 
                polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                polrights_rev_sd_fh +
                no_coups +
                (1|country_text_id),
              family=nbinom2(link = "log"), 
              reg_data_bridged_caus)
summary(m4_bc)
anova(m3_bc,m4_bc)
r2(m4_bc)

# Control + Modernization + Population Size + DQ + DiffObj + DiffQuestion
m5_bc <- glmmTMB(times_bridged ~ 1 + offset(log(years_counted)) +
                   loggdp + 
                   primaryschool_wdi + 
                   popsize + 
                   polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                   polrights_rev_variability_fh +
                   no_coups +
                   q_diff + 
                   nr_categories +
                   (1|country_text_id), 
                 family=nbinom2(link = "log"), 
                 reg_data_bridged_caus)
summary(m5_bc)
anova(m4_bc,m5_bc)
r2(m5_bc)

data.frame(rate = predict(m5_bc, newdata = data.frame(m5_bc$frame) %>%  mutate(years_counted = 1),
                          type = "response")) %>% 
  bind_cols(data.frame(m5_bc$frame) %>%  select(country_text_id)) %>% 
  group_by(country_text_id) %>%
  summarise(rate = mean(rate)) %>%
  mutate(rate = round(rate,0))  %>% 
  group_by(rate) %>% 
  summarise(nr_sum = n()) %>% 
  ggplot(aes(x=rate, y = nr_sum)) +
  geom_point() +
  geom_line()


make_glmm_tables(m0_bc, m1_bc, m2_bc, m3_bc, m4_bc, m5_bc, rsquared=F)

# Visualization
eff_m5_bc = ggeffect(m5_bc, offset=0)
get_complete_df(eff_m5_bc) %>% 
  filter(group != "years_counted") %>% 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax = conf.high)) +
  facet_wrap(group ~ ., scales = "free_x") +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.5) +
  theme_bw()

# Own Sample
dmx_reg_vis = fread("unzip -p Datasets/DemocracyMatrix_v1_1.zip", encoding = "UTF-8")  %>% 
  left_join(fread("unzip -p C:/RTest/V-Dem-CY+Others-v8.zip", encoding = "UTF-8") %>% 
              dplyr::select(country = country_name, country_text_id, year), by=c("country", "year")) %>% 
  select(country_text_id, regions, classification_core) %>% 
  filter(classification_core == "Deficient Democracy" | classification_core == "Working Democracy") %>% 
  select(country_text_id) %>% 
  distinct()

new_data_bridge = dmx_reg_vis %>% 
  left_join(reg_data_bridged_caus, by="country_text_id") %>% 
  select(loggdp, primaryschool_wdi,
         popsize,
         polrights_rev_mean_fh, 
         polrights_rev_variability_fh, no_coups,
         nr_categories, q_diff) %>% 
  pivot_longer(cols=everything()) %>% 
  group_by(name) %>% 
  summarise(conf_lower = quantile(value, 0.25, na.rm=T),
            conf_median = quantile(value, 0.5, na.rm=T),
            conf_upper = quantile(value, 0.75, na.rm=T)) %>% 
  pivot_longer(cols=starts_with("conf_"), names_to = "conf") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(years_counted = 1,
         country_text_id = NA,
         q_diff = 0.459,
         nr_categories = 5)

predict(m5_bc, newdata = new_data_bridge, type="response", offset=0, 
        se.fit=F)

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
  filter(disagree > 0) %>% 
  mutate(disagree = scale(disagree)[,1])

hist(disagree_reg$disagree, breaks=100)

# Analysis
# Null model without ML
m0_disagree <- glmmTMB(disagree ~ 1,
                       family=gaussian, 
                       disagree_reg)
summary(m0_disagree)

# Null model with ML
m0_ml_disagree <- glmmTMB(disagree ~ 1 +
                (1|name),
              family=gaussian, 
              disagree_reg)
summary(m0_ml_disagree)

# ML favored
anova(m0_disagree, m0_ml_disagree)


# Control + Modernization
m1_disagree <- glmmTMB(disagree ~ 1 +
                coders_p_year +
                primaryschool_wdi +
                loggdp +
                (1|country_text_id),
              family=gaussian, 
              disagree_reg)
summary(m1_disagree)
r2(m1_disagree)


# Control + Modernization + Population Size
m2_disagree <- glmmTMB(disagree ~ 1 +
                         coders_p_year +
                         loggdp +
                         primaryschool_wdi +
                         popsize +
                         (1|country_text_id),
                       family=gaussian, 
                       disagree_reg)
summary(m2_disagree)
r2(m2_disagree)


# Control + Modernization + Population Size + DQ
m3_disagree <- glmmTMB(disagree ~ 1 +
                         coders_p_year +
                         loggdp +
                         primaryschool_wdi +
                         popsize +
                         polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                         (1|country_text_id),
                       family=gaussian, 
                       disagree_reg)
summary(m3_disagree)
r2(m3_disagree)

# Control + Modernization + Population Size + DQ + DiffObj
m4_disagree <- glmmTMB(disagree ~ 1 +
                         coders_p_year +
                         loggdp +
                         primaryschool_wdi +
                         popsize +
                         polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                         polrights_rev_variability_fh +
                         no_coups +
                         (1|country_text_id),
                       family=gaussian, 
                       disagree_reg)
summary(m4_disagree)
r2(m4_disagree)


# Control + Modernization + Population Size + DQ + DiffObj + DiffQuestion
m5_disagree <- glmmTMB(disagree ~ 1 +
                         coders_p_year +
                         loggdp +
                         primaryschool_wdi +
                         popsize +
                         polrights_rev_mean_fh + I(polrights_rev_mean_fh^2) +
                         polrights_rev_variability_fh +
                         no_coups +
                         q_diff +
                         (1|country_text_id),
                       family=gaussian, 
                       disagree_reg)
summary(m5_disagree)
r2(m5_disagree)

make_glmm_tables(m0_ml_disagree,m1_disagree,m2_disagree,m3_disagree,m4_disagree,m5_disagree)

# Visualization
library(ggeffects)

eff_m5_disaggree = ggeffect(m5_disagree)
get_complete_df(eff_m5_disaggree) %>% 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax = conf.high)) +
  facet_wrap(group ~ ., scales = "free_x") +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  geom_hline(yintercept = 0)

# Own Sample
dmx_reg_vis = fread("unzip -p Datasets/DemocracyMatrix_v1_1.zip", encoding = "UTF-8")  %>% 
  left_join(fread("unzip -p C:/RTest/V-Dem-CY+Others-v8.zip", encoding = "UTF-8") %>% 
              dplyr::select(country = country_name, country_text_id, year), by=c("country", "year")) %>% 
  select(country_text_id, regions, classification_core) %>% 
  filter(classification_core == "Deficient Democracy" | classification_core == "Working Democracy") %>% 
  select(country_text_id) %>% 
  distinct()

new_data = dmx_reg_vis %>% 
  left_join(disagree_reg, by="country_text_id") %>% 
  select(coders_p_year,
         loggdp, primaryschool_wdi,
         popsize,
         polrights_rev_mean_fh, 
         polrights_rev_variability_fh, no_coups,
         nr_categories, q_diff) %>% 
  pivot_longer(cols=everything()) %>% 
  group_by(name) %>% 
  summarise(conf_lower = quantile(value, 0.25, na.rm=T),
            conf_median = quantile(value, 0.5, na.rm=T),
            conf_upper = quantile(value, 0.75, na.rm=T)) %>% 
  pivot_longer(cols=starts_with("conf_"), names_to = "conf") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(country_text_id = NA,
         q_diff = 0.486,
         coders_p_year = 5.55)

predict(m5_disagree, newdata = new_data, type="response", 
        se.fit=F)


# Empirical Inconsistencies ####
# Territory
vdem_ds %>% 
  select(country, year, coder_id, v2svstterr) %>% 
  filter(country == "Germany",
         year >= 1900) %>% 
  mutate(coder_id = as.factor(coder_id)) %>% 
  ggplot(aes(x=year, y=v2svstterr, col=coder_id)) + 
  geom_line(size=1.2)  + 
  geom_point() +
  xlab("") +
  theme_bw() +
  scale_color_grey() +
  theme(legend.position = "none")

vdem_ds %>% 
  select(country, year, coder_id, v2svstterr) %>% 
  filter(country == "Germany",
         year >= 1900) %>% 
  top_n(1, -v2svstterr)
  
  
# MEBIAS
vdem_ds %>% 
  select(country, year, coder_id, v2mebias) %>% 
  filter(country == "Germany",
         year >= 1900) %>% 
  mutate(coder_id = as.factor(coder_id),
         v2mebias = as.factor(v2mebias)) %>% 
  na.omit() %>% 
  ggplot(aes(x=year, y=coder_id, col=v2mebias)) + 
  #geom_line(size=1.2)  + 
  geom_point(size=2)

vdem_main %>% 
  select(country = country_name, year, v2mebias, v2mebias_codehigh, v2mebias_codelow) %>% 
  filter(country == "Germany",
         year >= 1900) %>% 
  ggplot(aes(x=year, y=v2mebias, ymax=v2mebias_codehigh, ymin=v2mebias_codelow)) + 
  geom_line(size=1.2)  + 
  geom_errorbar() +
  geom_point()


test = vdem_ds %>% 
  select(country, year, coder_id, variable = v2clkill) %>% 
  filter(year >= 1900) %>% 
  group_by(country, coder_id, year) %>% 
  arrange(variable) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(year) %>% 
  group_by(country, coder_id) %>% 
  mutate(v2mebias_lag = dplyr::lag(variable, 1),
         Difference = abs(variable - v2mebias_lag),
         Difference = ifelse(Difference == 0, NA, Difference),
         Difference = ifelse(Difference > 0, 1, Difference))  %>% 
  ungroup() %>% 
  mutate(coder_id = ifelse(is.na(variable) == T, NA, coder_id)) %>% 
  group_by(country, year) %>% 
  summarise(nr = n_distinct(coder_id, na.rm = T),
            divergent = sum(Difference, na.rm = T)) %>% 
  #filter(divergent == 1)
  mutate( divergent = ifelse(divergent == 1, 1, 0))

m2 = test %>% 
  filter(divergent == 1, nr < 5) %>%
  group_by(country) %>% 
  summarise(summe = sum(divergent)) %>% 
  filter(summe <= 3)
  
selected_country = "Austria"

p1 = vdem_ds %>% 
  select(country, year, coder_id, v2clkill) %>% 
  filter(country == selected_country,
         year >= 1900) %>% 
  mutate(coder_id = as.factor(coder_id),
         v2clkill = as.factor(v2clkill)) %>% 
  na.omit() %>% 
  ggplot(aes(x=year, y=coder_id, col=v2clkill)) + 
  #geom_line(size=1.2)  + 
  geom_point(size=2) 


p2 = vdem_main %>% 
  select(country = country_name, year, variable = v2clkill, var_high = v2clkill_codehigh, var_low = v2clkill_codelow) %>% 
  filter(country == selected_country,
         year >= 1900) %>% 
  na.omit() %>% 
  left_join(test, by=c("country", "year")) %>% 
  ggplot(aes(x=year, y=variable, ymax=var_high, ymin=var_low)) + 
  geom_line(size=1.2)  + 
  geom_errorbar() +
  geom_point(size=2.5, aes(col=as.factor(divergent))) 

p3 = vdem_ds %>% 
  select(country, year, coder_id, v2clkill_conf) %>% 
  filter(country == selected_country,
         year >= 1900)  %>% 
  na.omit() %>% 
  group_by(country, year) %>% 
  summarise(conf = mean(v2clkill_conf, na.rm=T)) %>% 
  ggplot(aes(x=year, y=conf)) + 
  geom_line(size=1.2, aes(col="conf")) 

grid::grid.draw(rbind(ggplotGrob(p1),ggplotGrob(p2),ggplotGrob(p3)))

