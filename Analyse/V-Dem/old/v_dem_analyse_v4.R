# V-Dem Chapter 2.1 Script

# SETUP

source("Analyse/V-Dem/v_dem_setup.R")


# Descriptive Analysis ####

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

# saveRDS(conf_rater_vdem, file="Analyse/V-Dem/robjects/conf_rater_vdem.RDS")
conf_rater_vdem = readRDS(file="Analyse/V-Dem/robjects/conf_rater_vdem.RDS")


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


all_v2_coders = vdem_vars %>% 
  filter(year >= 1900)   %>% 
  na.omit() %>% 
  group_by(name, coder_id, country_text_id) %>% 
  partition(cluster) %>% 
  summarise(coded_years_per_country = n()) %>% 
  collect() %>% 
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
  mutate(expert = 1,
         cumsum = cumsum(coded_year)/sum(coded_year),
         expert_sum = cumsum(expert)/sum(expert))  %>% 
  ggplot(aes(x=expert_sum, y=cumsum)) +
  geom_line(size=1.1) +
  scale_y_continuous(label=percent, breaks=seq(0,1,0.2)) +
  scale_x_continuous(label=percent, breaks=seq(0,1,0.2)) +
  ylab("Cumulative Percentage: Country-Years") +
  xlab("Cumulative Percentage: Country Experts") +
  theme_bw() +
  geom_abline(intercept = 0, slope=1)

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

# saveRDS(c_disagree, file="Analyse/V-Dem/robjects/c_disagree.RDS")
c_disagree = readRDS(file="Analyse/V-Dem/robjects/c_disagree.RDS")




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

set.seed(1234)
regression_vars = length_varname %>% 
  right_join(vartype %>%  rename(varname = name), by="varname") %>% 
  filter(codetype == "C") %>% 
  filter(varname %!in% multiplechoice_values ) %>% 
  #filter(vartype == 5) %>% 
  sample_frac(1)

# Difficualty of Questions


variable_caus = variable_caus_1 %>% 
  group_by(name, country_text_id, year) %>%
  partition(cluster) %>% 
  summarise(disagree = sd(value)) %>%
  collect() %>% 
  select(-year) %>% 
  distinct() %>% 
  group_by(name, country_text_id) %>% 
  summarise(disagree = mean(disagree, na.rm=T)) %>% 
  group_by(name) %>% 
  summarise(q_diff_caus = mean(disagree, na.rm=T)) %>% 
  left_join(vartype %>% select(name, nr_cat_caus = vartype), by="name")



QoC_caus = QoC_data %>% 
  select(country_text_id, year,
         GDP_capita_wdi = wdi_gdpcapcon2010,
         pop_wdi = wdi_pop,
         area_wdi = wdi_area,
         pssab_wb = wbgi_pve,
         export_gle = gle_exp,
         import_gle = gle_imp
         ) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>% 
  filter(year >= 1974) %>% 
  group_by(country_text_id) %>% 
  summarise(mean_pop_wdi_caus = log(mean(pop_wdi, na.rm=T)),
            mean_export_gle = log(mean(export_gle, na.rm=T)),
            mean_import_gle = log(mean(import_gle, na.rm=T)),
            mean_trade_gle_caus = (mean_export_gle + mean_import_gle)/2,
            mean_pssab_wb_caus = mean(pssab_wb, na.rm=T),
            mean_area_wdi_caus = log(mean(area_wdi, na.rm=T)),
            mean_loggdp_wdi_caus = log(mean(GDP_capita_wdi, na.rm=T))
  ) %>% 
  select_at(vars(country_text_id, ends_with("caus")))

# QoC_data %>%
#   select(country, country_text_id) %>%
#   filter(country_text_id == "BDI")

vdem_caus = vdem_main %>% 
  filter(year >= 1974) %>% 
  select(country_text_id, country_name, year, 
         polrights_fh = e_fh_pr, 
         internalconf_caus = e_miinterc,
         internatconf_caus = e_miinteco,
         coups = e_coups) %>% 
  mutate(
    polrights_rev_fh = 7 - as.numeric(polrights_fh),
    coups = ifelse(coups > 1, 1, coups)
  ) %>% 
  group_by(country_text_id, country_name) %>% 
  summarise(mean_pr_rev_fh_caus = mean(polrights_rev_fh, na.rm=T),
            sum_coups_caus = sum(coups, na.rm=T),
            sum_conf_caus = sum(internalconf_caus, na.rm=T) + sum(internatconf_caus, na.rm=T),
            sum_conf_caus = ifelse(sum_conf_caus > 10, 10, sum_conf_caus)
            ) %>% 
  select_at(vars(country_text_id, country_name, ends_with("caus")))


# Dependent Variable ####


reg_data = reg_coder_df %>% 
  group_by(name, country_text_id) %>%
  mutate(year_count = dplyr::row_number()) %>% 
  summarise(nr_coder = sum(nr_coders),
            years_counted = max(year_count)) %>% 
  right_join(regression_vars %>% rename(name = varname), by="name") %>% 
  select(name, country_text_id, nr_coder, update, sections, years_counted)




# Regression 1: Nr Coders ####
# merge data
reg_data_caus = reg_data %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(variable_caus, by="name") %>% 
  rename(sections_caus = sections,
         update_caus = update) %>% 
  ungroup() %>% 
  #na.omit() %>%
  #sample_frac(0.25) %>% 
  mutate(obs_effect = 1:nrow(.))


# Descriptive Analysis ####
reg_data_caus %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="number")

reg_data_caus %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="circle")

xyplot(reg_data_caus %>% 
         sample_frac(0.25), "nr_coder")

missd_pattern(reg_data_caus %>% 
                rename_all(funs(gsub("_caus", "", .))))

reg_data_caus %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  mutate_if(is.numeric, funs(isna = ifelse(is.na(.) == T, 1, 0))) %>% 
  select_at(vars(matches("mean_"))) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="circle")

reg_data_caus_alyse = reg_data_caus %>% 
  na.omit()

# Analysis ####
# Null model without ML
m0_noml_p <- glmmTMB(nr_coder ~ 1, 
              family=truncated_poisson(link = "log"), 
              reg_data_caus_alyse)
summary(m0_noml_p)


m0_noml_nb <- glmmTMB(nr_coder ~ 1, 
                      family=truncated_nbinom2(link = "log"), 
                      reg_data_caus_alyse)
summary(m0_noml_nb)

# Null model with ML: Poisson
m0_p <- glmmTMB(nr_coder ~ 1 + 
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
              family=truncated_poisson(link = "log"), 
              reg_data_caus_alyse)
# saveRDS(m0_p, file="Analyse/V-Dem/robjects/m0_p.RDS")
m0_p = readRDS(file="Analyse/V-Dem/robjects/m0_p.RDS")

# Simulate from fitted model and compare to real data
simulate(m0_p, nsim=15) %>%
  bind_cols(data.frame(true = m0_p$frame$nr_coder)) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = as.factor(name),
         name = fct_relevel(name, "true")) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram(fill="darkgrey", bins=50) +
  facet_wrap(name ~ .) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  xlab("") +
  ylab("")


# favors ML and nbinom
anova(m0_noml_p, m0_noml_nb, m0_p) %>% 
   make_anova_table()



# Control + Difficulty
# Test linear effect (mean_pr_rev_fh_caus)
m1_ncl <- glmmTMB(nr_coder ~ 1 +
                    update_caus +                   
                    # Difficulty
                    mean_loggdp_wdi_caus + 
                    mean_pr_rev_fh_caus +
                    sum_coups_caus +
                    sum_conf_caus +
                    mean_pssab_wb_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
                 family=truncated_poisson(link = "log"), 
                 reg_data_caus_alyse)
summary(m1_ncl)
#saveRDS(m1_ncl, file="Analyse/V-Dem/robjects/m1_ncl.RDS")
anova(m0_p, m1_ncl) %>% 
  make_anova_table()

data.frame(resid = residuals(m1_ncl)) %>% 
  bind_cols(m1_ncl$frame) %>% 
  ggplot(aes(x=mean_pssab_wb_caus, y=resid)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

data.frame(fitted = fitted(m1_ncl)) %>% 
  bind_cols(m1_ncl$frame) %>% 
  ggplot(aes(x=mean_pr_rev_fh_caus, y=fitted)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

# Test nonlinear effect (mean_pr_rev_fh_caus)
m1_ncnl <- glmmTMB(nr_coder ~ 1 +
                    update_caus +
                     # Difficulty
                    mean_loggdp_wdi_caus + 
                    mean_pr_rev_fh_caus + 
                    I(mean_pr_rev_fh_caus^2) +
                    sum_coups_caus +
                    sum_conf_caus + 
                    mean_pssab_wb_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
                 family=truncated_poisson(link = "log"), 
                 reg_data_caus_alyse)
summary(m1_ncnl)
#saveRDS(m1_ncnl, file="Analyse/V-Dem/robjects/m1_ncnl.RDS")

anova(m0_p, m1_ncl, m1_ncnl) %>% 
  make_anova_table()


# Control + Difficulty + Importance
m2_nc <- glmmTMB(nr_coder ~ 1 +
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus +
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
                 family=truncated_poisson(link = "log"), 
                 reg_data_caus_alyse)
summary(m2_nc)
#saveRDS(m2_nc, file="Analyse/V-Dem/robjects/m2_nc.RDS")

m2_nc_exclarea <- glmmTMB(nr_coder ~ 1 +
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus +
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   # mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
                 family=truncated_poisson(link = "log"), 
                 reg_data_caus_alyse)
summary(m2_nc_exclarea)
#saveRDS(m2_nc_exclarea, file="Analyse/V-Dem/robjects/m2_nc_exclarea.RDS")


anova(m0_p, m1_ncl, m2_nc) %>% 
  make_anova_table()

# Control + Difficulty + Importance + DiffQuestion
m3_nc <- glmmTMB(nr_coder ~ 1 + 
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus +
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   # DifficultyQuestion
                   q_diff_caus +
                   nr_cat_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
                 family=truncated_poisson(link = "log"), 
                 reg_data_caus_alyse)
summary(m3_nc)
#saveRDS(m3_nc, file="Analyse/V-Dem/robjects/m3_nc.RDS")

# Tables ####
anova(m0_p, m1_ncl, m1_ncnl, m2_nc, m2_nc_exclarea, m3_nc) %>% 
  make_anova_table(truenames=F)

make_glmm_tables(m0_p, m1_ncl, m1_ncnl, 
                 m2_nc_exclarea, m2_nc, m3_nc, rsquared=F)



data.frame(fitval = fitted(m4_nc), 
           X = m4_nc$frame$q_diff,
           variable = m4_nc$frame$name) %>%
  group_by(variable) %>% 
  summarise_all(mean) %>% 
  ggplot(aes(x=X, y=fitval)) +
  geom_point() +
  geom_smooth()

# Visualization ####
eff_m5_nc = ggeffect(m3_nc, offset=0)
get_complete_df(eff_m5_nc) %>% 
  mutate(group = gsub("_caus", "", group)) %>% 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax = conf.high)) +
  scale_x_continuous(breaks=seq(-10, 50, 2)) +
  #scale_y_continuous(breaks=seq(0, 30, 2)) +
  facet_wrap(group ~ ., scales = "free") +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  xlab("") +
  ylab("")


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
  select_at(vars(ends_with("caus"), -update_caus, -sections_caus)) %>% 
  pivot_longer(cols=everything()) %>% 
  group_by(name) %>% 
  summarise(conf_lower = quantile(value, 0.25, na.rm=T),
            conf_median = quantile(value, 0.5, na.rm=T),
            conf_upper = quantile(value, 0.75, na.rm=T)) %>% 
  pivot_longer(cols=starts_with("conf_"), names_to = "conf") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(name = NA,
         country_text_id = "DEU",
         update_caus = "V6",
         obs_effect = NA)

# new_data = m3_nc$frame %>% 
#   filter(country_text_id == "PAN") %>% 
#   summarise_all(mean, na.rm=T) %>% 
#   mutate(name = "v2lglegpup",
#          country_text_id = "PAN",
#          update_caus = "V6",
#          obs_effect = 3195)

predict(m3_nc, newdata = new_data, type="response", offset=0, 
        se.fit=F)
#  No Obs_effect ####

m1_ncl_nobs <- glmmTMB(nr_coder ~ 1 +
                    update_caus +                   
                    # Difficulty
                    mean_loggdp_wdi_caus + 
                    mean_pr_rev_fh_caus +
                    sum_coups_caus +
                    sum_conf_caus +
                    mean_pssab_wb_caus +
                    (1|name) + (1|country_text_id), 
                  family=truncated_poisson(link = "log"), 
                  reg_data_caus_alyse)
summary(m1_ncl_nobs)
#saveRDS(m1_ncl_nobs, file="Analyse/V-Dem/robjects/m1_ncl_nobs.RDS")

# Test nonlinear effect (mean_pr_rev_fh_caus)
m1_ncnl_nobs <- glmmTMB(nr_coder ~ 1 +
                     update_caus +
                     # Difficulty
                     mean_loggdp_wdi_caus + 
                     mean_pr_rev_fh_caus + 
                     I(mean_pr_rev_fh_caus^2) +
                     sum_coups_caus +
                     sum_conf_caus + 
                     mean_pssab_wb_caus +
                     (1|name) + (1|country_text_id), 
                   family=truncated_poisson(link = "log"), 
                   reg_data_caus_alyse)
summary(m1_ncnl_nobs)
#saveRDS(m1_ncnl_nobs, file="Analyse/V-Dem/robjects/m1_ncnl_nobs.RDS")

anova(m0_p, m1_ncl, m1_ncnl) %>% 
  make_anova_table()


# Control + Difficulty + Importance
m2_nc_nobs <- glmmTMB(nr_coder ~ 1 +
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus +
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   (1|name) + (1|country_text_id), 
                 family=truncated_poisson(link = "log"), 
                 reg_data_caus_alyse)
summary(m2_nc_nobs)
#saveRDS(m2_nc_nobs, file="Analyse/V-Dem/robjects/m2_nc_nobs.RDS")

m2_nc_exclarea_nobs <- glmmTMB(nr_coder ~ 1 +
                            update_caus +   
                            # Difficulty
                            mean_loggdp_wdi_caus + 
                            mean_pr_rev_fh_caus +
                            sum_coups_caus +
                            sum_conf_caus +
                            mean_pssab_wb_caus + 
                            # Importance
                            mean_pop_wdi_caus +
                            # mean_area_wdi_caus +
                            mean_trade_gle_caus +
                            (1|name) + (1|country_text_id), 
                          family=truncated_poisson(link = "log"), 
                          reg_data_caus_alyse)
summary(m2_nc_exclarea_nobs)
#saveRDS(m2_nc_exclarea_nobs, file="Analyse/V-Dem/robjects/m2_nc_exclarea_nobs.RDS")


# Control + Difficulty + Importance + DiffQuestion
m3_nc_nobs <- glmmTMB(nr_coder ~ 1 + 
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus +
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   # DifficultyQuestion
                   q_diff_caus +
                   nr_cat_caus +
                   (1|name) + (1|country_text_id), 
                 family=truncated_poisson(link = "log"), 
                 reg_data_caus_alyse)
summary(m3_nc_nobs)
#saveRDS(m3_nc_nobs, file="Analyse/V-Dem/robjects/m3_nc_nobs.RDS")

anova(m0_p, m1_ncl_nobs, m1_ncnl_nobs, 
      m2_nc_nobs, m2_nc_exclarea_nobs, m3_nc_nobs) %>% 
  make_anova_table(truenames=F)

make_glmm_tables(m0_p, m1_ncl_nobs, m1_ncnl_nobs, 
                 m2_nc_exclarea_nobs, m2_nc_nobs, m3_nc_nobs, rsquared=F)



### Regression 2: Nr Bridged/Lateral Coders ####

reg_data_bridged = year_type_df %>% 
  select(country_text_id, name, coder_id, coder_type) %>% 
  group_by(name, country_text_id) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(bridged = if_else(coder_type == "LC" |  coder_type == "BLC", 1, 0)) %>%
  group_by(name, country_text_id) %>% 
  summarize(nr_bcoder = sum(bridged, na.rm=T)) %>% 
  right_join(regression_vars %>% rename(name = varname), by="name")


reg_data_bridged_caus = reg_data_bridged %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(variable_caus, by="name") %>% 
  rename(sections_caus = sections,
         update_caus = update) %>% 
  ungroup() %>% 
  #na.omit() %>% 
  #sample_frac(0.25) %>% 
  mutate(obs_effect = 1:nrow(.))

# Descriptive Analysis ####

reg_data_bridged_caus %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="number")

reg_data_bridged_caus %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="circle")

xyplot(reg_data_bridged_caus %>% 
         sample_frac(0.25), "nr_bcoder")

missd_pattern(reg_data_bridged_caus)

reg_data_bridged_caus %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  mutate_if(is.numeric, funs(isna = ifelse(is.na(.) == T, 1, 0))) %>% 
  select_at(vars(matches("mean_"))) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="circle")

reg_data_bridged_caus_alyse = reg_data_bridged_caus %>% 
  na.omit()

# Analysis ####
# Null model without ML
m0_noml_bc <- glmmTMB(nr_bcoder ~ 1, 
              family=poisson(link = "log"), 
              reg_data_bridged_caus_alyse)

# Null model with ML
m0_bc <- glmmTMB(nr_bcoder ~ 1 + 
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
              family=poisson(link = "log"), 
              reg_data_bridged_caus_alyse)
summary(m0_bc)
# saveRDS(m0_bc, file="Analyse/V-Dem/robjects/m0_bc.RDS")
m0_bc = readRDS(file="Analyse/V-Dem/robjects/m0_bc.RDS")



simulate(m0_bc, nsim=15) %>%
  bind_cols(data.frame(true = m0_bc$frame$nr_bcoder)) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = as.factor(name),
         name = fct_relevel(name, "true")) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram(fill="darkgrey", bins=50) +
  facet_wrap(name ~ .) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  xlab("") +
  ylab("")


#favors ML
anova(m0_noml_bc,m0_bc)

# Control + Difficulty
m1_bc <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus +                   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect),
              family=poisson(link = "log"), 
              reg_data_bridged_caus_alyse)
summary(m1_bc)
anova(m0_bc,m1_bc)
# saveRDS(m1_bc, file="Analyse/V-Dem/robjects/m1_bc.RDS")

m1_bc_nl <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus +                   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   I(mean_pr_rev_fh_caus^2) + 
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect),
                 family=poisson(link = "log"), 
                 reg_data_bridged_caus_alyse)
summary(m1_bc_nl)
anova(m0_bc,m1_bc, m1_bc_nl)
# saveRDS(m1_bc_nl, file="Analyse/V-Dem/robjects/m1_bc_nl.RDS")


# Control + Difficulty + Importance
m2_bc <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
                 family=poisson(link = "log"), 
                 reg_data_bridged_caus_alyse)
summary(m2_bc)
anova(m1_bc,m2_bc)
# saveRDS(m2_bc, file="Analyse/V-Dem/robjects/m2_bc.RDS")

m2_bc_exlarea <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   #mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect), 
                 family=poisson(link = "log"), 
                 reg_data_bridged_caus_alyse)
summary(m2_bc_exlarea)
anova(m1_bc,m2_bc, m2_bc_exlarea)
# saveRDS(m2_bc_exlarea, file="Analyse/V-Dem/robjects/m2_bc_exlarea.RDS")

# Control + Difficulty + Importance + DiffQuestion
m3_bc <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus + 
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   # DifficultyQuestion
                   q_diff_caus +
                   nr_cat_caus +
                   (1|name) + (1|country_text_id) + (1|obs_effect),
              family=poisson(link = "log"), 
              reg_data_bridged_caus_alyse)
summary(m3_bc)
anova(m2_bc, m3_bc)
# saveRDS(m3_bc, file="Analyse/V-Dem/robjects/m3_bc.RDS")


# Tables ####

anova(m0_noml_bc, m0_bc, m1_bc, m1_bc_nl, m2_bc_exlarea, 
      m2_bc, m3_bc) %>% 
  make_anova_table(truenames=F)

make_glmm_tables(m0_bc, m1_bc, m1_bc_nl, m2_bc_exlarea, 
                 m2_bc, m3_bc, rsquared=F)



# Visualization ####
eff_m5_bc = ggeffect(m3_bc, offset=0)
get_complete_df(eff_m5_bc) %>% 
  mutate(group = gsub("_caus", "", group)) %>% 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax = conf.high)) +
  scale_x_continuous(breaks=seq(-10, 50, 2)) +
  #scale_y_continuous(breaks=seq(0, 30, 2)) +
  facet_wrap(group ~ ., scales = "free") +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  xlab("") +
  ylab("")

# No Obs_effect ####
# Null model with ML
m0_bc_nobs <- glmmTMB(nr_bcoder ~ 1 + 
                   (1|country_text_id) + (1|name), 
                 family=poisson(link = "log"), 
                 reg_data_bridged_caus_alyse)
summary(m0_bc_nobs)
# saveRDS(m0_bc_nobs, file="Analyse/V-Dem/robjects/m0_bc_nobs.RDS")
m0_bc_nobs = readRDS(file="Analyse/V-Dem/robjects/m0_bc_nobs.RDS")



simulate(m0_bc_nobs, nsim=15) %>%
  bind_cols(data.frame(true = m0_bc$frame$nr_coder)) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = as.factor(name),
         name = fct_relevel(name, "true")) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram(fill="darkgrey", bins=50) +
  facet_wrap(name ~ .) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  xlab("") +
  ylab("")


#favors ML
anova(m0_noml_bc_nobs,m0_bc)

# Control + Difficulty
m1_bc_nobs <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus +                   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus +
                   (1|name) + (1|country_text_id),
                 family=poisson(link = "log"), 
                 reg_data_bridged_caus_alyse)
summary(m1_bc_nobs)
# saveRDS(m1_bc_nobs, file="Analyse/V-Dem/robjects/m1_bc_nobs.RDS")

m1_bc_nl_nobs <- glmmTMB(nr_bcoder ~ 1 +
                      update_caus +                   
                      # Difficulty
                      mean_loggdp_wdi_caus + 
                      mean_pr_rev_fh_caus +
                      I(mean_pr_rev_fh_caus^2) + 
                      sum_coups_caus +
                      sum_conf_caus + 
                      mean_pssab_wb_caus +
                      (1|name) + (1|country_text_id),
                    family=poisson(link = "log"), 
                    reg_data_bridged_caus_alyse)
summary(m1_bc_nl_nobs)
# saveRDS(m1_bc_nl_nobs, file="Analyse/V-Dem/robjects/m1_bc_nl_nobs.RDS")


# Control + Difficulty + Importance
m2_bc_nobs <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus +   
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   (1|name) + (1|country_text_id), 
                 family=poisson(link = "log"), 
                 reg_data_bridged_caus_alyse)

# saveRDS(m2_bc_nobs, file="Analyse/V-Dem/robjects/m2_bc_nobs.RDS")

m2_bc_exlarea_nobs <- glmmTMB(nr_bcoder ~ 1 +
                           update_caus +   
                           # Difficulty
                           mean_loggdp_wdi_caus + 
                           mean_pr_rev_fh_caus +
                           sum_coups_caus +
                           sum_conf_caus + 
                           mean_pssab_wb_caus + 
                           # Importance
                           mean_pop_wdi_caus +
                           #mean_area_wdi_caus +
                           mean_trade_gle_caus +
                           (1|name) + (1|country_text_id), 
                         family=poisson(link = "log"), 
                         reg_data_bridged_caus_alyse)
summary(m2_bc_exlarea_nobs)
# saveRDS(m2_bc_exlarea_nobs, file="Analyse/V-Dem/robjects/m2_bc_exlarea_nobs.RDS")

# Control + Difficulty + Importance + DiffQuestion
m3_bc_nobs <- glmmTMB(nr_bcoder ~ 1 +
                   update_caus + 
                   # Difficulty
                   mean_loggdp_wdi_caus + 
                   mean_pr_rev_fh_caus +
                   sum_coups_caus +
                   sum_conf_caus + 
                   mean_pssab_wb_caus + 
                   # Importance
                   mean_pop_wdi_caus +
                   mean_area_wdi_caus +
                   mean_trade_gle_caus +
                   # DifficultyQuestion
                   q_diff_caus +
                   nr_cat_caus +
                   (1|name) + (1|country_text_id),
                 family=poisson(link = "log"), 
                 reg_data_bridged_caus_alyse)

# saveRDS(m3_bc_nobs, file="Analyse/V-Dem/robjects/m3_bc_nobs.RDS")

anova(m0_bc_nobs, m1_bc_nobs, m1_bc_nl_nobs, m2_bc_exlarea_nobs, 
      m2_bc_nobs, m3_bc_nobs) %>% 
  make_anova_table(truenames=F)

make_glmm_tables(m0_bc_nobs, m1_bc_nobs, m1_bc_nl_nobs, m2_bc_exlarea_nobs, 
                 m2_bc_nobs, m3_bc_nobs, rsquared=F)

### Regression 3: Disagreement ####
c_disagree = readRDS(file="Analyse/V-Dem/robjects/c_disagree.RDS")

disagree_df = c_disagree %>% 
  left_join(vartype, by="name") %>% 
  # filter(vartype == 5)  %>% 
  group_by(name, country_text_id) %>% 
  summarise(disagree = mean(disagree, na.rm = T)) %>% 
  left_join(length_varname %>%  rename(name = varname), by="name")


disagree_reg = disagree_df %>% 
  left_join(vdem_caus, by="country_text_id") %>% 
  left_join(QoC_caus, by="country_text_id") %>% 
  left_join(variable_caus, by="name") %>% 
  rename(sections_caus = sections,
         update_caus = update) %>% 
  ungroup() %>% 
  #na.omit() %>% 
  #sample_frac(0.25) %>% 
  left_join(reg_data %>% mutate(coders_p_year_caus = nr_coder/years_counted), by=c("name", "country_text_id")) %>% 
  ungroup()  %>% 
  #filter(disagree > 0) %>% 
  mutate(disagree = scale(disagree)[,1]) %>% 
  select_at(vars(country_text_id, name, disagree, ends_with("caus")))


# Descriptive Analysis ####

disagree_reg %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="number")

disagree_reg %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="circle")

xyplot(disagree_reg %>% 
         sample_frac(0.25), "disagree")

missd_pattern(disagree_reg)

disagree_reg %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  mutate_if(is.numeric, funs(isna = ifelse(is.na(.) == T, 1, 0))) %>% 
  group_by(mean_loggdp_wdi_isna) %>% 
  summarise(mean(disagree, na.rm=T))
  
disagree_reg %>% 
  rename_all(funs(gsub("_caus", "", .))) %>% 
  mutate_if(is.numeric, funs(isna = ifelse(is.na(.) == T, 1, 0))) %>% 
  select_at(vars(matches("mean_"))) %>% 
  cor(., use="pairwise") %>% 
  corrplot(method="circle")

disagree_reg_caus_alyse = disagree_reg %>% 
  na.omit()


# Analysis ####
# Null model without ML
m0_disagree <- glmmTMB(disagree ~ 1,
                       family=gaussian(), 
                       disagree_reg_caus_alyse)
summary(m0_disagree)
# saveRDS(m0_disagree, file="Analyse/V-Dem/robjects/m0_disagree.RDS")

# Null model with ML
m0_ml_disagree <- glmmTMB(disagree ~ 1 +
                (1|name) + (1|country_text_id),
              family=lognormal(), 
              disagree_reg_caus_alyse)
summary(m0_ml_disagree)
#saveRDS(m0_ml_disagree, file="Analyse/V-Dem/robjects/m0_ml_disagree.RDS")

simulate(m0_ml_disagree, nsim=15) %>%
  bind_cols(data.frame(true = m0_ml_disagree$frame$disagree)) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(name = as.factor(name),
         name = fct_relevel(name, "true")) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram(fill="darkgrey", bins=75) +
  facet_wrap(name ~ .) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  xlab("") +
  ylab("")


# ML favored
anova(m0_disagree, m0_ml_disagree)


# Control + Difficulty
m1_disagree <- glmmTMB(disagree ~ 1 +
                coders_p_year_caus +
                update_caus +                   
                # Difficulty
                mean_loggdp_wdi_caus + 
                mean_pr_rev_fh_caus +
                sum_coups_caus +
                sum_conf_caus +
                mean_pssab_wb_caus + 
                (1|name) + (1|country_text_id),
              family=gaussian(), 
              disagree_reg_caus_alyse)
summary(m1_disagree)
r2(m1_disagree)
# saveRDS(m1_disagree, file="Analyse/V-Dem/robjects/m1_disagree.RDS")
m1_disagree = readRDS(file="Analyse/V-Dem/robjects/m1_disagree.RDS")



m1_disagree_2 <- glmmTMB(disagree ~ 1 +
                              coders_p_year_caus +
                              update_caus +   
                              # Difficulty
                              mean_loggdp_wdi_caus + 
                              mean_pr_rev_fh_caus + 
                              sum_coups_caus +
                              sum_conf_caus +
                              mean_pssab_wb_caus + 
                              (1|name) + (1|country_text_id),
                            family=gaussian(), 
                            disagree_reg_caus_alyse)
summary(m1_disagree_2)
# saveRDS(m1_disagree_2, file="Analyse/V-Dem/robjects/m1_disagree_2.RDS")

m1_disagree_2_nl <- glmmTMB(disagree ~ 1 +
                           coders_p_year_caus +
                           update_caus +   
                           # Difficulty
                           mean_loggdp_wdi_caus + 
                           mean_pr_rev_fh_caus + 
                           I(mean_pr_rev_fh_caus^2) +
                           sum_coups_caus +
                           sum_conf_caus +
                           mean_pssab_wb_caus + 
                           (1|name) + (1|country_text_id),
                         family=gaussian(), 
                         disagree_reg_caus_alyse)
summary(m1_disagree_2_nl)
# saveRDS(m1_disagree_2_nl, file="Analyse/V-Dem/robjects/m1_disagree_2_nl.RDS")

anova(m1_disagree, m1_disagree_2)#
# Control + Difficulty + Importance
m2_disagree <- glmmTMB(disagree ~ 1 +
                         coders_p_year_caus +
                         update_caus +   
                         # Difficulty
                         mean_loggdp_wdi_caus + 
                         mean_pr_rev_fh_caus +
                         sum_coups_caus +
                         sum_conf_caus +
                         mean_pssab_wb_caus + 
                         # Importance
                         mean_pop_wdi_caus +
                         mean_area_wdi_caus +
                         mean_trade_gle_caus +
                         (1|name) + (1|country_text_id),
                       family=gaussian(), 
                       disagree_reg_caus_alyse)
summary(m2_disagree)
r2(m2_disagree)
# saveRDS(m2_disagree, file="Analyse/V-Dem/robjects/m2_disagree.RDS")


# Control + Difficulty + Importance + DifficultyQuestion
m3_disagree <- glmmTMB(disagree ~ 1 +
                         coders_p_year_caus +
                         update_caus + 
                         # Difficulty
                         mean_loggdp_wdi_caus + 
                         mean_pr_rev_fh_caus + I(mean_pr_rev_fh_caus^2) +
                         sum_coups_caus +
                         sum_conf_caus + 
                         mean_pssab_wb_caus + 
                         # Importance
                         mean_pop_wdi_caus +
                         mean_area_wdi_caus +
                         mean_trade_gle_caus +
                         # DifficultyQuestion
                         q_diff_caus +
                         nr_cat_caus +
                         (1|name) + (1|country_text_id),
                       family=gaussian, 
                       disagree_reg_caus_alyse)
summary(m3_disagree)
r2(m3_disagree)
# saveRDS(m3_disagree, file="Analyse/V-Dem/robjects/m3_disagree.RDS")

make_glmm_tables(m0_ml_disagree,m1_disagree,m2_disagree,m3_disagree)

# Visualization ####

eff_m5_disaggree = ggeffect(m5_disagree)
get_complete_df(eff_m5_disaggree) %>% 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax = conf.high)) +
  facet_wrap(group ~ ., scales = "free_x") +
  geom_line(size=1.1) +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  geom_hline(yintercept = 0)


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

