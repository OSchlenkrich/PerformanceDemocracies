# V-Dem Chapter 2.1 Script

# Setup #####
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
###


names(vdem_main)

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


cod_type = sub("\\).*", "", vdemcontent$names)
cod_type_2 = sub(".*\\(", "", cod_type)

test = sub(".*\\) \\(", "", vdemcontent$names)
test_2 = sub("\\).*", "", test)

var_data = data.frame(varname = test_2, codetype = cod_type_2) %>% 
  filter(grepl("v2", varname))



length_varname = var_data %>% 
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

test = length_varname %>% 
  group_by(sections, codetype)  %>% 
  summarise(nr_codtype = n())  %>% 
  group_by(sections) %>% 
  mutate(total = sum(nr_codtype)) %>% 
  # filter(codetype == "C") %>% 
  mutate(non_coders = total - nr_codtype) %>% 
  tidyr::complete(codetype, fill = list(non_coders = 0))


length_varname %>% 
  filter(codetype == "C") %>% 
  group_by(sections) %>% 
  summarise(nr_section = n()) %>% 
  mutate(sections = fct_reorder(sections, -nr_section)) %>% 
  ggplot(aes(x=sections, y=nr_section)) +
  geom_bar(stat="identity")

length_varname %>% 
  group_by(sections) %>% 
  summarise(nr_section = n()) %>% 
  mutate(sections = fct_reorder(sections, -nr_section)) %>% 
  ggplot(aes(x=sections, y=nr_section)) +
  geom_bar(stat="identity")


length_varname %>% 
  group_by(sections, codetype)  %>% 
  summarise(nr_codtype = n())  %>% 
  tidyr::complete(codetype, fill = list(nr_codtype = 0)) %>% 
  group_by(sections) %>% 
  mutate(total = sum(nr_codtype)) %>% 
  filter(codetype == "C") %>% 
  mutate(non_coders = total - nr_codtype) %>% 
  ungroup() %>% 
  mutate(sections = fct_reorder(sections, -total)) %>% 
  pivot_longer(cols=c("non_coders", "nr_codtype")) %>% 
  ggplot(aes(x=sections , y=value, fill=name)) +
  geom_bar(stat="identity")


varnames_C_length = data.frame(
  elections = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2el", vdemcontent$names))),
  parties =  length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2ps", vdemcontent$names))),
  DD =  length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2dd", vdemcontent$names))),
  exec = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2ex", vdemcontent$names))),
  legis = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2lg", vdemcontent$names))),
  delib = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2dl", vdemcontent$names))),
  jud = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2ju", vdemcontent$names))),
  clib = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2cl", vdemcontent$names))),
  state = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2sv", vdemcontent$names))) + length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2st", vdemcontent$names))),
  csociety = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2cs", vdemcontent$names))),
  media = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2me", vdemcontent$names))),
  equal = length(which(grepl("\\(C\\)", vdemcontent$names) & grepl("v2pe", vdemcontent$names)))
) %>% 
  mutate(total = rowSums(.)) %>% 
  # mutate_all(funs(./total)) %>% 
  select(-total) %>% 
  pivot_longer(cols=everything(), values_to = "coders_vars", names_to = "varname")

varnames_C_length %>% 
  mutate(varname = fct_reorder(varname, -coders_vars)) %>% 
  ggplot(aes(x=varname, y=coders_vars)) +
  geom_bar(stat="identity")

varnames_length = data.frame(
  #indices =  length(which(grepl("v2x", vdem_varnames))),
  elections = length(which(grepl("v2el", vdem_varnames))),
  parties =  length(which(grepl("v2ps", vdem_varnames))),
  DD =  length(which(grepl("v2dd", vdem_varnames))),
  exec = length(which(grepl("v2ex", vdem_varnames))),
  legis = length(which(grepl("v2lg", vdem_varnames))),
  delib = length(which(grepl("v2dl", vdem_varnames))),
  jud = length(which(grepl("v2ju", vdem_varnames))),
  clib = length(which(grepl("v2cl", vdem_varnames))),
  state = length(which(grepl("v2sv", vdem_varnames))) + length(which(grepl("v2st", vdem_varnames))),
  csociety = length(which(grepl("v2cs", vdem_varnames))),
  media = length(which(grepl("v2me", vdem_varnames))),
  equal = length(which(grepl("v2pe", vdem_varnames)))
) %>% 
  mutate(total = rowSums(.)) %>% 
  # mutate_all(funs(./total)) %>% 
  select(-total) %>% 
  pivot_longer(cols=everything(), values_to = "all_vars", names_to = "varname")

varnames_length %>% 
  left_join(varnames_C_length, by="varname") %>% 
  mutate(non_coders_vars = all_vars - coders_vars) %>% 
  pivot_longer(cols=c("non_coders_vars", "coders_vars")) %>% 
  mutate(varname = fct_reorder(varname, -value)) %>% 
  ggplot(aes(x=varname, y=value, fill=name)) +
  geom_bar(stat="identity")



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


