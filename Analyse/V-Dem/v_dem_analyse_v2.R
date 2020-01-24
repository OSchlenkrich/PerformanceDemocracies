# V-Dem Chapter 2.1 Script

# Setup #####
library(bayestestR)

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

vdemcontent = fread("Datasets/VDem_content.csv", sep=";")

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
                 -matches("_10")
                 )) %>% 
  colnames() 
vdem_varnames = gsub("_0", "", vdem_varnames)


# Extract Variables Types from the Codebook
cod_type = sub("\\).*", "", vdemcontent$names) 
cod_type_2 = sub(".*\\(", "", cod_type)

cod_name = sub(".*\\) \\(", "", vdemcontent$names)
cod_name_2 = sub("\\).*", "", cod_name)

var_data = data.frame(varname = cod_name_2, codetype = cod_type_2) %>% 
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
         codetype = as.factor(codetype))



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

### Number of coders ####

mean_coder_df = vdem_main %>% 
  select_at(vars(country_name, year,  ends_with("_nr"))) %>%
  select_at(vars(country_name, year,  starts_with("v2"))) %>%
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  group_by(year, country_name) %>% 
  summarise(mean_country = mean(value, na.rm=T)) %>% 
  group_by(year) %>% 
  summarise(mean_coder = mean(mean_country, na.rm=T),
            lower_coder = quantile(mean_country, prob=0.25, na.rm=T),
            higher_coder = quantile(mean_country, prob=0.75, na.rm=T))

mean_coder_df %>% 
  ggplot(aes(x=year, y=mean_coder)) +
  geom_line(size=1.1) +
  geom_errorbar(aes(ymin=lower_coder, ymax=higher_coder)) +
  scale_y_continuous(breaks=seq(0,20, 2)) +
  scale_x_continuous(breaks=seq(1900,2020, 20)) +
  geom_hline(yintercept = 5) +
  theme_bw() +
  ggtitle("Number of Coders per country per year")


vdem_main %>% 
  select_at(vars(country_name, year,  ends_with("_nr"))) %>%
  select_at(vars(country_name, year,  starts_with("v2"))) %>%
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2")) %>% 
  group_by(name) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>% 
  mutate(name = fct_reorder(name, mean_indicator)) %>% 
  ggplot(aes(x=name, y=mean_indicator)) +
  geom_point() +
  coord_flip() +
  theme_bw() +
  ggtitle("Average Number of Coders per Indicator")


vdem_main %>% 
  select_at(vars(country_name, year,  ends_with("_nr"))) %>%
  select_at(vars(country_name, year,  starts_with("v2"))) %>%
  filter(year >= 1900) %>% 
  pivot_longer(cols=starts_with("v2"), names_to = "varname") %>% 
  group_by(varname) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>% 
  mutate(varname = gsub("_nr", "", varname)) %>% 
  left_join(length_varname, by="varname")  %>% 
  mutate(sections = fct_reorder(sections, mean_indicator)) %>% 
  ggplot(aes(x=sections, y=mean_indicator)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Average Number of Coders per Section")


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
                 -matches("_10"))) %>% 
  filter(year >= 1900) 
  
conf_rater_vdem %>% 
  pivot_longer(cols=ends_with("_conf")) %>% 
  #filter(name %in% paste(length_varname$varname, "_conf", sep="")) %>% 
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
  pivot_longer(cols=ends_with("_conf"), names_to = "varname") %>% 
  group_by(varname) %>% 
  summarise(mean_indicator = mean(value, na.rm=T)) %>% 
  mutate(varname = gsub("_conf", "", varname)) %>% 
  left_join(length_varname, by="varname") %>% 
  mutate(sections = fct_reorder(sections, mean_indicator)) %>% 
  ggplot(aes(x=sections, y=mean_indicator)) +
  geom_boxplot() +
  # geom_errorbar(aes(ymin=lower_conf_year, ymax=higher_conf_year)) +
  theme_bw() +
  ggtitle("Average Confidence of Coders per section")




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


