source("Analyse/CreateDatasets.R")
source("Setup/Sig_Tables.R")
source("Setup/Simulation_Dirichlet.R")


# Structural Performance

# library(gmodels)
# library(vcd)
# library(rcompanion)

dmx_performance = performance_all %>% 
  left_join(dmx_trade_cluster %>% 
              select(-country_text_id), by=c("country", "year"))

Welfare_types = fread("Datasets/WelfareStates.csv", sep=";", encoding="UTF-8") %>% 
  select_at(vars(country, starts_with("cl_"))) %>% 
  filter(is.na(cl_Esping_Andersen_1990) == F) %>% 
  mutate_at(vars(starts_with("cl")), funs(fct_recode(., 
                                                     "Con" = "Cor",
                                                     NULL = "CEE",
                                                     NULL = "Rad",
                                                     NULL = "Sou",
                                                     NULL = "Eur"))) %>% 
  melt(id.vars="country") %>% 
  na.omit() %>% 
  group_by(country, value) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "value", values_from = "count", values_fill = list(count=0), names_prefix = "cnt_") %>% 
  ungroup() %>% 
  filter(country != "Greece", country != "Iceland")

test_data = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  select(country, cluster_label_1st) %>% 
  group_by(country, cluster_label_1st) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  pivot_wider(names_from = cluster_label_1st, 
              values_from = count, 
              values_fill = list(count = 0), 
              names_prefix ="count_") 

#### Compare to Welfare and VoC ####

table_data = dmx_performance %>% 
  filter(year > 1950, year<=1970) %>% 
  group_by(country) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st)) %>% 
  left_join(VoC_Welfare_types, by="country") %>% 
  mutate(Pool = fct_recode(cluster_label_1st_mode, 
                           "Fec"= "FeC",
                           "fEc" = "fEC"
                           ),
         Pool_W = fct_recode(welfare_E,
                             "Soc.Dem." = "Conservative")
         )


mytable = table(table_data$Pool, table_data$VoC_HS)
library(vcd)
mosaicplot(x = mytable, color=T)
mytable = table(table_data$Pool, table_data$welfare_E)
library(vcd)
mosaicplot(x = mytable, color=c("darkblue", "darkred", "darkgreen"))
library(ggmosaic)
ggplot(data = table_data) +
  geom_mosaic(aes(x = product(VoC_HS, Pool), fill=VoC_HS), na.rm=TRUE) 


table(table_data$cluster_label_1st_mode, table_data$welfare_E)
table(table_data$Pool, table_data$VoC_HS)
table(table_data$Pool, table_data$Pool_W)

CrossTable(table_data$cluster_label_1st_mode, table_data$welfare_E, 
           chisq = T)


chisq.test(table(table_data$cluster_label_1st_mode, table_data$VoC_HS), simulate.p.value = TRUE)
chisq.test(table(table_data$cluster_label_1st_mode, table_data$welfare_E), simulate.p.value = TRUE)

fisher.test(table(table_data$Pool, table_data$VoC_HS))
fisher.test(table(table_data$Pool, table_data$Pool_W))




assocstats(table(table_data$cluster_label_1st_mode, table_data$welfare_E))
assocstats(table(table_data$cluster_label_1st_mode, table_data$VoC_HS))


# Profile - Welfare ------------
# Count
table_data %>% 
  group_by(cluster_label_1st_mode,  welfare_E) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  group_by(cluster_label_1st_mode) %>% 
  ggplot(aes(x=cluster_label_1st_mode, y=count, fill=welfare_E)) +
  geom_bar(stat = "identity", position = "dodge")

# Percent
table_data %>% 
  group_by(cluster_label_1st_mode,  welfare_E) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  group_by(cluster_label_1st_mode) %>% 
  mutate(count = count/sum(count)) %>% 
  ggplot(aes(x=cluster_label_1st_mode, y=count, fill=welfare_E)) +
  geom_bar(stat = "identity", position = "dodge")


# Profile - VoC ------------
# Count
table_data %>% 
  group_by(cluster_label_1st_mode,  VoC_HS) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  group_by(cluster_label_1st_mode) %>% 
  ggplot(aes(x=cluster_label_1st_mode, y=count, fill=VoC_HS)) +
  geom_bar(stat = "identity", position = "dodge")

# Percent
table_data %>% 
  group_by(cluster_label_1st_mode,  VoC_HS) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  group_by(cluster_label_1st_mode) %>% 
  mutate(count = count/sum(count)) %>% 
  ggplot(aes(x=cluster_label_1st_mode, y=count, fill=VoC_HS)) +
  geom_bar(stat = "identity", position = "dodge")


# VoC - Welfare ------------
# Count
table_data %>% 
  group_by(VoC_HS,  welfare_E) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  group_by(VoC_HS) %>% 
  ggplot(aes(x=VoC_HS, y=count, fill=welfare_E)) +
  geom_bar(stat = "identity", position = "dodge")

# Percent
table_data %>% 
  group_by(VoC_HS,  welfare_E) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  group_by(VoC_HS) %>% 
  mutate(count = count/sum(count)) %>% 
  ggplot(aes(x=VoC_HS, y=count, fill=welfare_E)) +
  geom_bar(stat = "identity", position = "dodge")

##
# Weighting for 
# Western Germany: 1,19295753*; weighting factor for Eastern Germany: 0,57980091*;

library(haven)
ISSP_SE = read_spss("Datasets/ISSP_ZA5400_v4-0-0.sav")
ISSP_G =  read_spss("Datasets/ISSP_ZA6900_v2-0-0.sav")
ISSP_C =  read_spss("Datasets/ISSP_ZA6670_v2-0-0.sav")


ISSP_SE_sub = ISSP_SE %>% 
  select(V5, V4, reduceDiff = V33, typesociety = V55) %>% 
  mutate(weight = if_else(V4 == 27601, 1.19295753, 
                          if_else(V4 == 27602, 0.57980091, 1)))

ISSP_G_sub = ISSP_G %>%
  select(V5 = country, V4 = c_sample, promoteE = v56) %>% 
  mutate(weight = if_else(V4 == 27601, 1.19295753, 
                          if_else(V4 == 27602, 0.57980091, 1)))

ISSP_C_sub = ISSP_C %>% 
  select(V5 = V4, V4 = V3, Minorities = V33) %>% 
  mutate(weight = if_else(V4 == 27601, 1.19295753, 
                          if_else(V4 == 27602, 0.57980091, 1)))


country_names_ISSP = data.frame(V5 = attributes(ISSP_SE_sub$V5)$labels, 
                                country = names(attributes(ISSP_SE_sub$V5)$labels), 
                                row.names = NULL) %>% 
  bind_rows(data.frame(V5 = attributes(ISSP_G_sub$V5)$labels, 
                       country = names(attributes(ISSP_G_sub$V5)$labels), 
                       row.names = NULL)) %>% 
  bind_rows(data.frame(V5 = attributes(ISSP_C_sub$V5)$labels, 
                       country = names(attributes(ISSP_C_sub$V5)$labels), 
                       row.names = NULL)) %>% 
  distinct() %>% 
  mutate(country = gsub(".*?-", "", country),
         country = fct_recode(country, 
                              "United Kingdom" = "Great Britain and/or United Kingdom",
                              "United States of America" = "United States",
                              "Korea, South" = "Korea (South)"
         )) 

ISSP_sub_dmx = ISSP_SE_sub %>% 
  left_join(country_names_ISSP, by="V5") %>% 
  left_join(V_dem %>% select(country, country_text_id) %>%  distinct(), by="country") %>% 
  left_join(VoC_Welfare_types %>%  select(-country_text_id), by="country") %>% 
  #(is.na(welfare_E) == F)  %>% 
  left_join(modes_cluster, by="country_text_id")



ISSP_sub_dmx_mean = ISSP_sub_dmx %>% 
  group_by(country_text_id) %>% 
  summarise(mean_equal = weighted.mean(reduceDiff, w =weight, na.rm=T)) %>% 
  left_join(VoC_Welfare_types, by="country_text_id") %>% 
  filter(is.na(welfare_E) == F)  %>% 
  left_join(modes_cluster, by="country_text_id") 




ISSP_sub_dmx_mean %>% 
  ggplot(aes(x=cluster_label_1st_mode, y=mean_equal)) + 
  geom_boxplot() +
  geom_point()
test %>% 
  filter(country != "Japan") %>% 
  #filter(country != "Italy", country !="France") %>% 
  ggplot(aes(x=Pool_W, y=mean_equal)) + 
  geom_boxplot()
test %>% 
  filter(country != "Japan") %>% 
  ggplot(aes(x=VoC_HS, y=mean_equal)) + 
  geom_boxplot()


#### Compare to Centripetalism ####


centripetalism = fread("centripetalism_frame.csv", encoding="UTF-8")

modes_cluster_cent = dmx_performance %>% 
  group_by(country) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st)) %>% 
  left_join(centripetalism, by="country")

modes_cluster_cent %>% 
  ggplot(aes(x=cluster_label_1st_mode, y=cent)) +
  geom_boxplot()


####


library(rjags)

model_string <- "model{

  # Likelihood
  for(n in 1:N){
    Y[n]   ~ dnorm(mu[zeta[n]],inv.var) 
    zeta[n] ~ dcat(pi[n,])
  }

  # Priors

  for (cat in 1:categories) {
    mu[cat] ~ dnorm(3,0.1)T(1,5)
  }
  inv.var   ~ dgamma(0.001, 0.001)
  sigma   <- 1/sqrt(inv.var)

  diff14 = mu[1] - mu[4]
  diff24 = mu[2] - mu[4]
  diff15 = mu[1] - mu[5]

}"



test_data = dmx_trade_cluster %>% 
  filter(year >= 1950) %>% 
  select(country_text_id, cluster_label_1st) %>% 
  group_by(country_text_id, cluster_label_1st) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  pivot_wider(names_from = cluster_label_1st, 
              values_from = count, 
              values_fill = list(count = 0), 
              names_prefix ="count_") %>% 
  right_join(ISSP_sub_dmx_mean, by="country_text_id") %>% 
  ungroup() %>% 
  select_at(vars(country_text_id, mean_equal, starts_with("count_"))) %>% 
  na.omit()


data_jags = list(
  pi = test_data %>% select_at(vars(starts_with("count_"))),
  categories = 5,
  Y = as.numeric(test_data$mean_equal),
  N = dim(test_data)[1]
)


library(jagsUI)


model <- jagsUI::jags(model.file = textConnection(model_string), 
                      data = data_jags,
                      inits=NULL,
                      n.iter = 15000,
                      n.burnin = 10000,
                      n.thin=1,
                      n.chains=4,
                      #n.cores = 2,
                      parallel = F,
                      parameters.to.save	=c("mu",
                                            "sigma",
                                            "diff14",
                                            "diff15",
                                            "diff24")
)

library(MCMCvis)

MCMCsummary(model, round = 3, HPD=T, hpd_prob = 0.95)

####################


model_string <- "model{

  for ( i in 1:N ) {
    pi_welfare[i,1:categories_1] ~ ddirch( explambda[1:categories_1,i] )
    
    zeta[i] ~ dcat(pi_profile[i,])

    explambda[1,i] = 1
    for ( k in 2:categories_1 ) {
      explambda[k,i] <- exp(mu[k, zeta[i]] )
    }
  }
  
  # priors
  
  for ( k in 2:categories_1 ) {
    for ( j in 1:categories_profiles ) {
      mu[k,j] ~ dnorm( 0 , 1 )
    }
  }
}"

model_string <- "model{

  for ( i in 1:N ) {
    Y[i] ~ dcat( explambda[1:categories_1,i] )
    
    zeta[i] ~ dcat(pi_profile[i,])

    explambda[1,i] = 1
    for ( k in 2:categories_1 ) {
      explambda[k,i] <- exp(mu[k, zeta[i]] )
    }
  }
  
  # priors
  
  for ( k in 2:categories_1 ) {
    for ( j in 1:categories_profiles ) {
      mu[k,j] ~ dnorm( 0 , 0.1 )
    }
  }
}"

table_data = dmx_performance %>% 
  group_by(country) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st)) %>% 
  right_join(Welfare_types, by="country")  %>% 
  mutate(cluster_label_1st_mode = fct_recode(cluster_label_1st_mode,
                                             "Fec"= "FeC",
                                             "fEc" = "fEC"),
         cl_Esping_Andersen_1990 = fct_recode(cl_Esping_Andersen_1990,
                                              "SD"= "Con"),
         cl_Esping_Andersen_1990 = relevel(cl_Esping_Andersen_1990, ref="Lib")
  ) %>%
  left_join(test_data, by="country")  %>% 
  mutate(count_agg_F = count_FeC + count_Fec,
         count_agg_E = count_fEC + count_fEc,
         count_agg_B = count_FEC)


test_data = dmx_trade_cluster %>% 
  filter(year >= 1950, year <= 2000) %>% 
  select(country, cluster_label_1st) %>% 
  group_by(country, cluster_label_1st) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  pivot_wider(names_from = cluster_label_1st, 
              values_from = count, 
              values_fill = list(count = 0), 
              names_prefix ="count_") 


table_data_2 = dmx_performance %>% 
  group_by(country) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st)) %>% 
  # right_join(Welfare_types, by="country")  %>% 
  mutate(cluster_label_1st_mode = fct_recode(cluster_label_1st_mode,
                                             "Fec"= "FeC",
                                             "fEc" = "fEC")
  ) %>%
  left_join(test_data, by="country")  %>% 
  right_join(table_data %>% select(country,cl_Esping_Andersen_1990), by="country")  %>% 
  mutate(count_agg_F = count_FeC + count_Fec,
         count_agg_E = count_fEC + count_fEc,
         count_agg_B = count_FEC) %>% 
  filter(is.na(count_agg_F)==F)

# sub_data_profile = table_data %>% 
#   select_at(vars(starts_with("count_"))) %>% 
#   as.matrix()

sub_data_profile = table_data_2 %>% 
  select_at(vars(starts_with("count_agg"))) %>% 
  mutate(sum =  count_agg_F + count_agg_E + count_agg_B) %>% 
  mutate_at(vars(starts_with("count_agg")), funs(./sum)) %>% 
  select_at(vars(starts_with("count_agg"))) %>% 
  as.matrix()

# test_diri = Welfare_types %>%  
#   select_at(vars(starts_with("cnt_"))) %>% 
#   mutate(sum =  cnt_Con + cnt_Lib + cnt_SD) %>% 
#   mutate_at(vars(starts_with("cnt")), funs(./sum)) %>% 
#   select(cnt_Lib, cnt_Con, cnt_SD) %>% 
#   as.matrix()


data_jags = list(
  pi_profile = sub_data_profile,
  #pi_welfare = DirichletReg::DR_data(test_diri),
  categories_profiles = 3,
  categories_1 = 2,
  Y = as.numeric(table_data_2$cl_Esping_Andersen_1990),
  N = dim(sub_data_profile)[1]
)


model <- jagsUI::jags(model.file = textConnection(model_string), 
                      data = data_jags,
                      inits=NULL,
                      n.iter = 15000,
                      n.burnin = 5000,
                      n.thin=1,
                      n.chains=4,
                      #n.cores = 2,
                      parallel = F,
                      parameters.to.save	=c("mu")
)


library(MCMCvis)
library(coda)

MCMCsummary(model, round = 3, HPD=T, hpd_prob = 0.95)
lm1_mcmc <- as.mcmc.list(model$samples)
plot(lm1_mcmc)


# SIMULATION
N = 100
categories_1 = 3
categories_profiles = 5
pi = brms::rdirichlet(N, rep(0.1, categories_profiles))
zeta = array(NA, N)
Y = array(NA, N)
explambda = array(1, c(categories_1, N))
z_beta = array(0, c(categories_1, categories_profiles))


for (i in 1:N) {
  zeta[i] = sample(1:categories_profiles, size = 1, prob=pi[i,])
}
for (k in 2:categories_1) {
  z_beta[k,2:categories_profiles] = runif(categories_profiles-1, -1,1)
  mu[k] = rnorm(1,0,1)
  explambda[k,] =  exp(z_beta[k,zeta])
}
for (i in 1:N) {
  explambda[,i] = explambda[,i]/sum(explambda[,i])
  Y[i] = sample(1:categories_1, size = 1, prob=explambda[,i])
}



data_jags = list(
  pi = pi,
  categories_profiles = categories_profiles,
  categories_1 = categories_1,
  Y = Y,
  N = N
)
round(pi,2)
table(Y)
table(zeta)
library(jagsUI)


model <- jagsUI::jags(model.file = textConnection(model_string), 
                      data = data_jags,
                      inits=NULL,
                      n.iter = 15000,
                      n.burnin = 5000,
                      n.thin=1,
                      n.chains=2,
                      #n.cores = 2,
                      parallel = F,
                      parameters.to.save	=c("mu")
)


library(MCMCvis)
library(coda)
MCMCsummary(model, round = 3, HPD=T, hpd_prob = 0.95)
lm1_mcmc <- as.mcmc.list(model$samples)
plot(lm1_mcmc)


library(brms)
m1 = brm(Y ~ -1 + zeta, data = data.frame(Y, zeta = as.factor(zeta)), family=categorical())
summary(m1)

make_stancode(Y ~ zeta, data = data.frame(Y, zeta = as.factor(zeta)), family=categorical())
