# Fake Data Simulation For Model Testing ####
source("Analyse/CreateDatasets.R")
source("Analyse/Performance/SpecificP/WorkFlow_v2.R")
source("Setup/brms_tables.R")


G = 20
N = 20

X = data.frame(array(rnorm(N*G, 0, 1), dim=c(N,G)))
Time = data.frame(array((1-(N/2)):(N/2), dim=c(N,G))) 
int_T = rnorm(N, 1, 1)
int_G = rnorm(G, 2, 1)
Y = data.frame(array(NA, dim=c(N, G))) 
Y[1,] = 0

# Unit Heterogeneity: Varying Country Intercept ####

for (g in 1:G) {
  for (i in 2:N) {
    Y[i, g] = int_G[g] + 0.6 * X[i, g] + rnorm(1, 0, 1)
  }  
}

fake_unit = Y %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  select(country_text_id = name, Y = value) %>% 
  bind_cols(X %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(X = value)) %>% 
  bind_cols(Time %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(year_0 = value)) %>% 
  group_by(country_text_id ) %>% 
  mutate(Y_lag = dplyr::lag(Y, 1)) %>% 
  na.omit()


model_simple = brm(Y ~ 1 + X, fake_unit, 
                   family = gaussian(),
                   prior = set_prior("normal(0,10)", class="b") ,
                   warmup = 2000, iter = 4000,
                   chains = 5)
model_unit = brm(Y ~ 1 + X + (1|country_text_id), 
                 fake_unit, 
                 family = gaussian(),
                 prior = c(set_prior("normal(0,10)", class="b"),
                           set_prior("cauchy(0,5)", class="sd")) ,
                 warmup = 2000, iter = 4000,
                 chains = 5)

modelnames = c("non-hierarchical",
               "random country")

model_simple <- add_criterion(model_simple, "loo", reloo=T)
model_unit <- add_criterion(model_unit, "loo", reloo=T)



generateTableBrms(model_simple, model_unit, modelnames = modelnames)

loo_compare(model_simple, model_unit,
            criterion = "loo",
            model_names = modelnames) %>% 
  loo_table()
  

ppc_unithet(model_simple, model_unit, dataset = fake_unit, "Y", unit = "country")


# Contemporaneous Correlation: Varying Time Intercept ####


for (g in 1:G) {
  for (i in 2:N) {
    Y[i, g] = int_G[g] + int_T[i] + 0.6 * X[i, g] + rnorm(1, 0, 1)
  }  
}

fake_contcorr = Y %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  select(country_text_id = name, Y = value) %>% 
  bind_cols(X %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(X = value)) %>% 
  bind_cols(Time %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(year_0 = value)) %>% 
  group_by(country_text_id ) %>% 
  mutate(Y_lag = dplyr::lag(Y, 1)) %>% 
  na.omit()

model_unit_simple = brm(Y ~ 1 + X + (1|country_text_id), 
                 fake_contcorr, 
                 family = gaussian(),
                 prior = c(set_prior("normal(0,10)", class="b"),
                           set_prior("cauchy(0,5)", class="sd")) ,
                 warmup = 2000, iter = 4000,
                 chains = 5)


model_contcorr = brm(Y ~ 1 + X + (1|country_text_id) + (1|year_0), 
                 fake_contcorr, 
                 family = gaussian(),
                 prior = c(set_prior("normal(0,10)", class="b"),
                           set_prior("cauchy(0,5)", class="sd")) ,
                 warmup = 2000, iter = 4000,
                 chains = 5)
model_contcorr

model_unit_simple <- add_criterion(model_unit_simple, "loo", reloo=T)
model_contcorr <- add_criterion(model_contcorr, "loo", reloo=T)

modelnames = c("random country",
               "random country and time")

generateTableBrms(model_unit_simple, model_contcorr, modelnames = modelnames)


loo_compare(model_unit_simple, model_contcorr, criterion = "loo")
loo_compare(model_unit_simple, model_contcorr,
            criterion = "loo",
            model_names = modelnames) %>% 
  loo_table()


ppc_unithet(model_unit_simple, model_contcorr, dataset = fake_unit, "Y", unit = "year_0")

# Panel Heteroscedasticity ####

sigma_G = exp(1 + rnorm(G, 0, 0.5))

for (g in 1:G) {
  for (i in 2:N) {
    Y[i, g] = int_G[g] + int_T[i] + 0.6 * X[i, g] + rnorm(1, 0, sigma_G[g])
  }  
}

fake_panelhet = Y %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  select(country_text_id = name, Y = value) %>% 
  bind_cols(X %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(X = value)) %>% 
  bind_cols(Time %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(year_0 = value)) %>% 
  group_by(country_text_id ) %>% 
  mutate(Y_lag = dplyr::lag(Y, 1)) %>% 
  na.omit()

model_contcorr_simple = brm(bf(Y ~ 1 + X + (1|country_text_id) + (1|year_0),
                               sigma ~ 1), 
                            fake_panelhet, 
                        family = gaussian(),
                        prior = c(set_prior("normal(0,10)", class="b"),
                                  set_prior("cauchy(0,5)", class="sd")) ,
                        warmup = 2000, iter = 4000,
                        chains = 5)

#model_contcorr_simple = update(model_contcorr_simple, newdata=fake_panelhet)

model_panelhet = brm(bf(Y ~ 1 + X + (1|country_text_id) + (1|year_0),
                        sigma ~ (1|country_text_id)),
                     fake_panelhet, 
                     family = gaussian(),
                     prior = c(set_prior("normal(0,10)", class="b"),
                               set_prior("cauchy(0,5)", class="sd"),
                               set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                     warmup = 2000, iter = 4000,
                     chains = 5)

#model_panelhet = update(model_panelhet, newdata=fake_panelhet)
model_panelhet

model_contcorr_simple <- add_criterion(model_contcorr_simple, "loo", reloo=T)
model_panelhet <- add_criterion(model_panelhet, "loo", reloo=T)

modelnames = c("Panel homoscedastic",
               "Panel Heteroscedastic")

generateTableBrms(model_contcorr_simple, model_panelhet, modelnames = modelnames)



loo_compare(model_contcorr_simple, model_panelhet, criterion = "loo")
loo_compare(model_contcorr_simple, model_panelhet,
            criterion = "loo",
            model_names = modelnames) %>% 
  loo_table()

ppc_panelhet(model_contcorr_simple, model_panelhet, fake_panelhet, "Y") 


# Autocorrelation and Trend ####

for (g in 1:G) {
  for (i in 2:N) {
    Y[i, g] = int_G[g] + int_T[i] + 0.6 * X[i, g] + rnorm(1, 0, sigma_G[g]) + 
      0.8 * Y[i-1, g] + 1 *  Time[i, g]
  }  
}

fake_autotrend = Y %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  select(country_text_id = name, Y = value) %>% 
  bind_cols(X %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(X = value)) %>% 
  bind_cols(Time %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(year_0 = value)) %>% 
  group_by(country_text_id ) %>% 
  mutate(Y_lag = dplyr::lag(Y, 1)) %>% 
  na.omit()

model_panelhet_simple = brm(bf(Y ~ 1 + X + (1|country_text_id) + (1|year_0),
                        sigma ~ (1|country_text_id)),
                     fake_autotrend, 
                     family = gaussian(),
                     prior = c(set_prior("normal(0,10)", class="b"),
                               set_prior("cauchy(0,5)", class="sd"),
                               set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                     warmup = 2000, iter = 4000,
                     chains = 5)


# both autoregression and trend
model_autotrend = brm(bf(Y ~ 1 + Y_lag + year_0 +  X + (1|country_text_id) + (1|year_0),
                               sigma ~ (1|country_text_id)),
                            fake_autotrend, 
                            family = gaussian(),
                            prior = c(set_prior("normal(0,10)", class="b"),
                                      set_prior("cauchy(0,5)", class="sd"),
                                      set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                            warmup = 2000, iter = 4000,
                            chains = 5)

model_autotrend
# model_autotrend = update(model_autotrend, newdata=fake_autotrend)




# only autoregression
model_auto = brm(bf(Y ~ 1 + Y_lag +  X + (1|country_text_id) + (1|year_0),
                         sigma ~ (1|country_text_id)),
                      fake_autotrend, 
                      family = gaussian(),
                      prior = c(set_prior("normal(0,10)", class="b"),
                                set_prior("cauchy(0,5)", class="sd"),
                                set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                      warmup = 2000, iter = 4000,
                      chains = 5)

# model_auto = update(model_auto, newdata=fake_autotrend)
model_auto


# only trend
model_trend = brm(bf(Y ~ 1 + year_0 +  X + (1|country_text_id) + (1|year_0),
                         sigma ~ (1|country_text_id)),
                      fake_autotrend, 
                      family = gaussian(),
                      prior = c(set_prior("normal(0,10)", class="b"),
                                set_prior("cauchy(0,5)", class="sd"),
                                set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                      warmup = 2000, iter = 4000,
                      chains = 5)
# model_trend = update(model_trend, newdata=fake_autotrend)
model_trend


model_autotrend_simple <- add_criterion(model_panelhet_simple, "loo", reloo=T)
model_autotrend <- add_criterion(model_autotrend, "loo", reloo=T)
model_auto <- add_criterion(model_auto, "loo", reloo=T)
model_trend <- add_criterion(model_trend, "loo", reloo=T)

modelnames = c("No auto, no trend",
               "Only auto",
               "Only trend",
               "Both auto and trend")

generateTableBrms(model_autotrend_simple,
                  model_auto, 
                  model_trend,
                  model_autotrend, modelnames = modelnames)

loo_compare(model_autotrend_simple,
            model_auto, 
            model_trend,
            model_autotrend)

loo_compare(model_autotrend_simple,
            model_auto, 
            model_trend,
            model_autotrend,
            criterion = "loo",
            model_names = modelnames) %>% 
  loo_table()


p1 = ppc_resid_time(model_panelhet_simple, country_sample = c("X10", "X15", "X16", "X5"), fake_autotrend, title = modelnames[1])
p2 = ppc_resid_time(model_auto, country_sample = c("X10", "X15", "X16", "X5"), fake_autotrend, title = modelnames[2])
p3 = ppc_resid_time(model_trend, country_sample = c("X10", "X15", "X16", "X5"), fake_autotrend, title = modelnames[3])
p4 = ppc_resid_time(model_autotrend, country_sample = c("X10", "X15", "X16", "X5"), fake_autotrend, title = modelnames[4])

ggarrange(p1,p2,p3,p4, nrow=2, ncol=2)



# Autocorrelation and Trend: Random Coefficient ####
int_Y = rnorm(G, 0.6, 0.2)
min(int_Y); max(int_Y)
int_T = rnorm(G, 1, 1)


for (g in 1:G) {
  for (i in 2:N) {
    Y[i, g] = int_G[g] + int_T[i] + 0.6 * X[i, g] + rnorm(1, 0, sigma_G[g]) + 
      int_Y[g] * Y[i-1, g] + int_T[g] *  Time[i, g]
  }  
}

fake_autotrend_random = Y %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  select(country_text_id = name, Y = value) %>% 
  bind_cols(X %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(X = value)) %>% 
  bind_cols(Time %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(year_0 = value)) %>% 
  group_by(country_text_id ) %>% 
  mutate(Y_lag = dplyr::lag(Y, 1)) %>% 
  na.omit()


# no rc
model_random_simple = brm(bf(Y ~ 1 + Y_lag + year_0 +  X + (1|country_text_id) + (1|year_0),
                         sigma ~ (1|country_text_id)),
                      fake_autotrend_random, 
                      family = gaussian(),
                      prior = c(set_prior("normal(0,10)", class="b"),
                                set_prior("cauchy(0,5)", class="sd"),
                                set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                      warmup = 2000, iter = 4000,
                      chains = 5)

model_random_simple
# model_random_simple = update(model_random_simple, newdata=fake_autotrend_random)


# rc
model_random_coefficient = brm(bf(Y ~ 1 + Y_lag + year_0 +  X + (1 + Y_lag + year_0|country_text_id) + (1|year_0),
                             sigma ~ (1|country_text_id)),
                          fake_autotrend_random, 
                          family = gaussian(),
                          prior = c(set_prior("normal(0,10)", class="b"),
                                    set_prior("cauchy(0,5)", class="sd"),
                                    set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                          warmup = 2000, iter = 4000,
                          chains = 5)

model_random_coefficient
# model_random_coefficient = update(model_random_coefficient, newdata=fake_autotrend_random)

model_random_simple <- add_criterion(model_random_simple, "loo", reloo=T)
model_random_coefficient <- add_criterion(model_random_coefficient, "loo", reloo=T)

modelnames = c("No Random Coefficient",
               "Random Coefficient")

generateTableBrms(model_random_simple,
                  model_random_coefficient, modelnames = modelnames)


loo_compare(model_random_simple,
            model_random_coefficient,
            criterion = "loo",
            model_names = modelnames) %>% 
  loo_table()


p1 = ppc_resid_time(model_random_simple, country_sample = c("X1", "X12", "X13", "X14"), fake_autotrend_random, title = modelnames[1])
p2 = ppc_resid_time(model_random_coefficient, country_sample = c("X1", "X12", "X13", "X14"), fake_autotrend_random, title = modelnames[2])        

ggarrange(p1,p2, ncol=2)


# Linearity ####


for (g in 1:G) {
  for (i in 2:N) {
    Y[i, g] = int_G[g] + int_T[i] + 0.5 * X[i, g] + 1 * (X[i, g]^2) + rnorm(1, 0, sigma_G[g]) + 
      int_Y[g] * Y[i-1, g] + int_T[g] *  Time[i, g]
  }  
}

fake_autotrend_linear = Y %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  select(country_text_id = name, Y = value) %>% 
  bind_cols(X %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(X = value)) %>% 
  bind_cols(Time %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(year_0 = value)) %>% 
  group_by(country_text_id ) %>% 
  mutate(Y_lag = dplyr::lag(Y, 1)) %>% 
  na.omit()


# no linearity
model_linear_simple = brm(bf(Y ~ 1 + Y_lag + year_0 +  X + (1 + Y_lag + year_0|country_text_id) + (1|year_0),
                             sigma ~ (1|country_text_id)),
                          fake_autotrend_linear, 
                          family = gaussian(),
                          prior = c(set_prior("normal(0,10)", class="b"),
                                    set_prior("cauchy(0,5)", class="sd"),
                                    set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                          warmup = 2000, iter = 6000,
                          chains = 5, 
                          control = list(adapt_delta = 0.99))

model_linear_simple
# model_linear_simple = update(model_linear_simple, newdata=fake_autotrend_linear)

# no linearity
model_nonlinear = brm(bf(Y ~ 1 + Y_lag + year_0 +  X + I(X^2) + (1 + Y_lag + year_0|country_text_id) + (1|year_0),
                             sigma ~ (1|country_text_id)),
                          fake_autotrend_linear, 
                          family = gaussian(),
                          prior = c(set_prior("normal(0,10)", class="b"),
                                    set_prior("cauchy(0,5)", class="sd"),
                                    set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                          warmup = 2000, iter = 6000,
                          chains = 5, 
                      control = list(adapt_delta = 0.99))

model_nonlinear
# model_nonlinear = update(model_nonlinear, newdata=fake_autotrend_linear)


model_linear_simple <- add_criterion(model_linear_simple, "waic", reloo=T)
model_nonlinear <- add_criterion(model_nonlinear, "waic", reloo=T)


modelnames = c("Linear Model",
               "Non-Linear Model")

generateTableBrms(model_linear_simple,
                  model_nonlinear, modelnames = modelnames)

loo_compare(model_linear_simple,
            model_nonlinear,
            criterion = "waic",
            model_names = modelnames) %>% 
  loo_table()

# p1 = fitted_var_plot(model_linear_simple, independent_var = "X", country_sample = c("X19", "X5", "X1", "X20"), title = modelnames[1])
# p2 = fitted_var_plot(model_nonlinear, independent_var = "X", country_sample = c("X19", "X5", "X1", "X20"), title =  modelnames[2])
p1 = fitted_var_alldata_plot(model_linear_simple, independent_var = "X", title = modelnames[1])
p2 = fitted_var_alldata_plot(model_nonlinear, independent_var = "X", title =  modelnames[2])

ggarrange(p1,p2, ncol=2)

# Homoscedasticity with respect to covariates ####

sigma_G_heterosc= data.frame(array(NA, dim=c(N,G)))

for (g in 1:G) {
  sigma_G_heterosc[,g] = exp(1 + 0.5 * X[,g] + rnorm(1, 0, 0.5))
  
  for (i in 2:N) {
    
    Y[i, g] = int_G[g] + int_T[i] + 0.6 * X[i, g] + rnorm(1, 0, sigma_G_heterosc[i,g]) + 
      int_Y[g] * Y[i-1, g] + int_T[g] *  Time[i, g]
  }  
}

fake_heteroscedastic = Y %>% 
  pivot_longer(cols=starts_with("X")) %>% 
  select(country_text_id = name, Y = value) %>% 
  bind_cols(X %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(X = value)) %>% 
  bind_cols(Time %>% 
              pivot_longer(cols=starts_with("X")) %>% 
              select(year_0 = value)) %>% 
  group_by(country_text_id ) %>% 
  mutate(Y_lag = dplyr::lag(Y, 1)) %>% 
  na.omit()


# no heteroscedastic
model_heterosc_simple = brm(bf(Y ~ 1 + Y_lag + year_0 +  X + (1 + Y_lag + year_0|country_text_id) + (1|year_0),
                             sigma ~ (1|country_text_id)),
                            fake_heteroscedastic, 
                          family = gaussian(),
                          prior = c(set_prior("normal(0,10)", class="b"),
                                    set_prior("cauchy(0,5)", class="sd"),
                                    set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                          warmup = 2000, iter = 6000,
                          chains = 5, 
                          control = list(adapt_delta = 0.99))

model_heterosc_simple
# model_heterosc_simple = update(model_heterosc_simple, newdata=fake_heteroscedastic)

# heteroscedastic
model_heterosc = brm(bf(Y ~ 1 + Y_lag + year_0 +  X + (1 + Y_lag + year_0|country_text_id) + (1|year_0),
                      sigma ~ X + (1|country_text_id)),
                     fake_heteroscedastic, 
                   family = gaussian(),
                   prior = c(set_prior("normal(0,10)", class="b"),
                             set_prior("cauchy(0,5)", class="sd"),
                             set_prior("cauchy(0,5)", class="sd", dpar="sigma")) ,
                   warmup = 2000, iter = 6000,
                   chains = 5, 
                   control = list(adapt_delta = 0.99))

model_heterosc


# model_heterosc = update(model_heterosc, newdata=fake_heteroscedastic , 
#                         control = list(adapt_delta = 0.99))


model_heterosc_simple <- add_criterion(model_heterosc_simple, "waic", reloo=T)
model_heterosc <- add_criterion(model_heterosc, "waic", reloo=T)

modelnames = c("Covariate homoscedastic",
               "Covariate heteroscedastic")

generateTableBrms(model_heterosc_simple,
                  model_heterosc, modelnames = modelnames)

loo_compare(model_heterosc_simple,
            model_heterosc,
            criterion = "waic",
            model_names = modelnames) %>% 
  loo_table()


p1 = fitted_var_alldata_plot(model_heterosc_simple, independent_var = "X", title = modelnames[1]) 
p2 = fitted_var_alldata_plot(model_heterosc, independent_var = "X", title =  modelnames[2])

ggarrange(p1,p2, ncol=2)

# Distributional Assumption ####
# distribution_plot(model_contcorr_simple)
# distribution_plot(model_heterosc)
# 
# generateTableBrms(model_simple, model_heterosc_simple, model_heterosc, prob_interval = 0.95)
