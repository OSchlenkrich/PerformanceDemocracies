# TSCS Economy

lag_f = function(x, nr_lag = 1) {
  dplyr::lag(x, nr_lag)
}



soc_raw_scores = fread(file="Datasets/performance_data/ImputedDatasets/soc_scores.csv", encoding = "UTF-8") %>% 
  melt(id.vars=c("country_text_id", "year")) %>% 
  group_by(country_text_id, year) %>% 
  summarise(soc_raw = mean(value, na.rm=T))


dmx_performance = performance_all %>% 
  left_join(dmx_trade_cluster %>% 
              dplyr::select(-country_text_id), by=c("country", "year"))  %>% 
  mutate(cluster_label_1st = relevel(as.factor(cluster_label_1st), ref="FeC"),
         cluster_label_2nd = relevel(as.factor(cluster_label_2nd), ref="FeC")
  )

longdemocracies = dmx_performance %>% 
  dplyr::select(country, classification_context) %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(nr = n()) %>% 
  filter(nr > 1) %>% 
  pull(country)


roll_cluster = dmx_performance %>% 
  filter(country %in% longdemocracies) %>% 
  group_by(country) %>%
  mutate(cluster_label_1st_roll = rollapply(cluster_label_1st, 30, FUN = function(x) getmode(x),
                                     fill=NA, align="right", partial=T)) %>% 
  ungroup() %>% 
  mutate(cluster_label_1st_roll = factor(cluster_label_1st_roll))

modes_cluster = dmx_performance %>% 
  filter(country %in% longdemocracies) %>%
  filter(year >= 1940) %>% 
  
  group_by(country_text_id) %>% 
  summarise(cluster_label_1st_mode = getmode(cluster_label_1st)) 

modes_cluster %>% 
  group_by(cluster_label_1st_mode) %>% 
  summarise(n())

test = dmx_performance %>% 
  filter(country %in% longdemocracies) %>% 
  filter(year >= 1970) %>% 
  
  group_by(country, cluster_label_1st) %>% 
  summarise(nr1 = n()) %>%
  
  left_join(
    dmx_performance %>% 
      filter(country %in% longdemocracies) %>% 
      filter(year >= 1970) %>% 
      
      group_by(country, cluster_label_2nd) %>% 
      summarise(nr2 = n()) %>% 
      rename(cluster_label_1st = cluster_label_2nd),
    by=c("country", "cluster_label_1st")
  ) %>% 
  na.omit() %>% 
  mutate(cluster_label_1st = paste(cluster_label_1st, "_nr", sep=""),
         sum_nr = nr1 + nr2) %>%
  dplyr::select(-nr1, -nr2) %>% 
  pivot_wider(names_from =cluster_label_1st,  values_from="sum_nr") %>% 
  mutate_at(vars(ends_with("nr")), funs(ifelse(is.na(.) ==T, 0, .)))

control_vars = QoC_data %>% 
  dplyr::select(country_text_id, year,
         wdi_pop, 
         wdi_pop65,
         lp_protmg80,
         lp_catho80,
         legal_ctl = lp_legor,
         ht_colonial,
         muslim_ctl = lp_muslim80,
         ethno_frac_ctl = r_atlas,
         pop_density_ctl = wdi_popden) %>% 
  mutate(pop_total_ctl = log10(wdi_pop),
         pop_65_ctl = log10(wdi_pop65),
         protestant_ctl = lp_protmg80
  ) %>% 
  filter(country_text_id %in% dmx_cluster_names) %>% 
  filter_if(is.double, any_vars(!is.na(.))) %>%
  group_by(country_text_id, year) %>% 
  summarise_all(sum, na.rm=T) %>% 
  mutate(colonial_ctl = as.factor(ht_colonial))




names(eco_tscs)
names(performance_all)
eco_tscs = performance_all %>% 
  dplyr::select(country, country_text_id, year, 
         DV = eco_oecd_index,
         eco_wdi_index,
         environment_oecd_index,
         environment_wdi_index,
         soc_index,
         ds_life_index,
         ds_order_index, 
         conf_index) %>% 
  left_join(modes_cluster, by=c("country_text_id")) %>%
  left_join(test, by=c("country")) %>% 
  
  left_join(roll_cluster %>%  select(country, year, cluster_label_1st_roll), by=c("country", "year")) %>%
  left_join(dmx_data %>%  select(country, year, regions, classification_context), by=c("country", "year")) %>%
  left_join(soc_raw_scores, by=c("country_text_id", "year")) %>% 
  
  left_join(dmx_performance %>%  select(country, year, cluster_label_1st, X1, X2, X3, X4), by=c("country", "year")) %>% 
  
  left_join(VoC_Welfare_types %>%  select(-country), by=c("country_text_id")) %>% 
   
  # Control Vars
  left_join(control_vars %>%  select_at(vars(country_text_id, year, ends_with("ctl"))), by=c("country_text_id", "year")) %>%
  
  filter(classification_context == "Deficient Democracy" | classification_context == "Working Democracy") %>% 
  arrange(country_text_id, year) %>% 
  group_by(country_text_id, .drop=FALSE) %>% 
  mutate(
    lag_DV = lag_f(DV),
    lag2_DV = lag_f(DV, 2),
    lag3_DV = lag_f(DV, 3),
    lag4_DV = lag_f(DV, 4),
    lag5_DV = lag_f(DV, 5),
    #b_lagDV = mean(lag_DV, na.rm=T),
    w_lagDV = lag_DV - mean(lag_DV, na.rm=T),
    soc_raw_lag = lag_f(soc_raw),
    soc_raw2_lag = lag_f(soc_raw,2),
    soc_raw3_lag = lag_f(soc_raw,3),
    # soc_index_lag = lag_f(soc_index),
    # eco_oecd_index_lag = lag_f(eco_oecd_index),
    #e co_oecd_index2_lag = lag_f(eco_oecd_index,2),
    
    #conf_index_lag = lag_f(conf_index),
    
    # cluster_label_1st_lag = lag_f(cluster_label_1st),
    classification_context_lag = lag_f(classification_context),
    classification_context2_lag = lag_f(classification_context,2),
    
    #pop_65_ctl_lag = lag_f(pop_65_ctl),
    
    trend = year - median(year)
  )  %>%
  ungroup() %>% 
  filter(country %in% longdemocracies) %>% 
  unite("id",c(country,year), sep = "-", remove = F) %>% 
  as.data.frame() 

outliers = c("Argentina-2000",
             "Argentina-2003")

# NMLE
library(brms)
options(mc.cores = parallel::detectCores())

bmod3 <- brm(
  DV ~ 1 + lag_DV + lag2_DV + lag3_DV + 
    poly(trend, 2) + 
    classification_context +
    protestant_ctl +
    muslim_ctl +
    legal_ctl +
    colonial_ctl + 
    ethno_frac_ctl + 
    cluster_label_1st_mode +
    (1|country) + (1|year),
  data = eco_tscs, family = gaussian(),
  #prior = prior3,
  warmup = 2000, iter = 10000
)

summary(bmod3)
marginal_effects(bmod3)
brms::loo(bmod3)
bayes_R2(fit1)

bmod4 <- brm(
  bf(soc_raw ~ soc_raw_lag + year + classification_context + protestant_ctl +
    (1|country_num) + (1|year_num)),
  data = JAGS_DATA, 
  family = gaussian(),
  #prior = prior3,
  warmup = 2000, iter = 5000
)
summary(bmod4)
prior_summary(bmod4)


my_formula = bf(DV ~ 1 + 
                  trend +
                  classification_context +
                  protestant_ctl +
                  cluster_label_1st_mode +
                  (1 | country),
                sigma ~ 0 + country)
get_prior(my_formula, data = eco_tscs, family = student())

prior1 <- c(set_prior("normal(0,10)", class = "Intercept"),
            set_prior("normal(0,10)", class = "b"),
            #set_prior("normal(0,1)", class = "b", coef = "soc_raw_lag"),
            set_prior("cauchy(0,5)", class = "sd"),
            set_prior("cauchy(0,5)", dpar = "sigma")
            #set_prior("cauchy(0,5)", class = "sigma")
            )

make_stancode(my_formula, data = eco_tscs, prior = prior1)


bmod4 <- brm(my_formula,
  data = eco_tscs, family = student(),
  prior = prior1,
  warmup = 2000, iter = 5000,
  # sample_prior = 'only',
  chains = 5
)

prior_summary(bmod4)
stancode(bmod4)

summary(bmod4)
posterior_samples(bmod4, "sigma")




bmod3 <- add_criterion(bmod3, "loo")
bmod4 <- add_criterion(bmod4, "loo")

loo_compare(bmod3, bmod4, criterion = "loo")

brms::pp_check(bmod3)
brms::pp_check(bmod4)
pp_check(bmod5, type = "dens_overlay")

pp_check(bmod3, type = "loo_pit_qq")
pp_check(bmod5, type = "error_hist", nsamples = 11)
pp_check(bmod3, type = "scatter_avg", nsamples = 100)
pp_check(bmod3, type = "stat_2d")
pp_check(bmod3, type = "rootogram")
pp_check(bmod3, type = "loo_pit")
pp_check(bmod5, type = "ecdf_overlay")
bayesplot::mcmc_scatter(bmod5)
stanplot(bmod5, type = "scatter")

mcmc_scatter(bmod5, pars = c("intercept", "sigma"), 
             size = 1.5, alpha = 0.5)
mcmc_parcoord(bmod5, regex_pars = c("sd", "Intercept"))
mcmc_parcoord_data(bmod5, regex_pars = c("sd", "Intercept"))

np <- nuts_params(bmod5)
test = mcmc_parcoord_data(bmod5, 
                          regex_pars = c("b", "Intercept"))
mcmc_parcoord_data(bmod5, 
                   regex_pars = c("b", "Intercept"),
                   transform = function(x) {(x - mean(x)) / sd(x)},
                   np = np) %>% 
  filter(Draw%in%sample(max(test$Draw), 100)) %>% 
  ggplot(aes(x=Parameter, y = Value, group =factor(Draw))) +
  geom_line(color=get_color("dh")) +
  bayesplot_theme_get()
np <- nuts_params(bmod5)

test = mcmc_parcoord_data(bmod5, 
                          #regex_pars = c("b", "Intercept"),
                          transform = function(x) {(x - mean(x)) / sd(x)},
                          np = np) %>% 
  filter(Draw%in%sample(max(test$Draw), 1000))


mcmc_parcoord(test)

loo1 <- loo::loo(bmod3, save_psis = TRUE)
yrep <- posterior_predict(bmod5)
dim(yrep)
yrep[1,]
brms::bayes_R2(bmod5)

my_formula = bf(soc_raw ~ 1 + soc_raw_lag +
                  (1 | country) + (1 | year),
                sigma ~ 0 + country)

get_prior(my_formula, data = eco_tscs, family = student())
make_stancode(my_formula, data = eco_tscs, family = student())

prior1 <- c(set_prior("normal(0,10)", class = "Intercept"),
            #set_prior("normal(0,10)", class = "b"),
            #set_prior("normal(0,1)", class = "b", coef = "soc_raw_lag"),
            set_prior("cauchy(0,5)", class = "sd"),
            set_prior("cauchy(0,5)", dpar = "sigma")
            #set_prior("cauchy(0,5)", class = "sigma")
)


bmod5 <- brm(my_formula,
             data = eco_tscs %>% 
               filter(country != "India" & country != "Colombia"), family = gaussian(),
             prior = prior1,
             warmup = 1000, iter = 2500,
             chains = 8)

prior_summary(bmod5)

library(shinystan)
shinystan::launch_shinystan(bmod5)

test = predict(bmod5)
plot(test[,1], bmod5$data$soc_raw)
summary(bmod5)

test = mcmc_parcoord_data(bmod5, regex_pars = c("sigma"), np=np)

predictions = data.frame(prediction = test[,1], 
                         DV = bmod4$data$DV, 
                         country = bmod4$data$country, 
                         year = bmod4$data$year,
                         resid = bmod4$data$DV-test[,1])
loo2 <- loo::loo(bmod5, save_psis = TRUE, reloo = F)
plot(loo2)
loo::compare(loo1, loo2)

loo2$diagnostics$pareto_k
loo2$pointwise
library(bayesplot)
# requires bayesplot version >= 1.5.0
ppc_loo_pit_overlay(
  y = bmod5$data$soc_raw,
  yrep = yrep,
  lw = weights(loo2$psis_object),
  samples = 200
) 


ppc_loo_pit_qq(
  y = bmod5$data$DV,
  yrep = yrep,
  lw = weights(loo2$psis_object),
  compare = "uniform"
)

keep_obs <- sample(length(bmod5$data$soc_raw), 100)
ppc_loo_intervals(y = bmod5$data$soc_raw, 
                  yrep = yrep,
                  psis_object = loo2$psis_object, 
                  #subset = keep_obs,
                  order = "median")


residuals(bmod3)

## Prais Winston

eco_tscs_PW = panelAR(DV  ~ 
                        poly(trend,1) + 
                        classification_context_lag +
                        regions +
                        cluster_label_1st_mode, 
                      data=eco_tscs %>% 
                        mutate(cluster_label_1st_mode = relevel(cluster_label_1st_mode, ref="FeC")), 
                                    panelVar = "country", timeVar = "year", 
                                    rho.na.rm=T,
                                    panelCorrMethod ="pcse", 
                                    bound.rho=T,
                                    rhotype = "scorr",
                                    autoCorr='psar1'
                      )

summary(eco_tscs_PW)

# acf(residuals(eco_tscs_PW))
test = data.frame(country = eco_tscs_PW$model$country,
           year = eco_tscs_PW$model$year,
           y = eco_tscs_PW$model$DV,
           y_pred = eco_tscs_PW$fitted.values,
           resid = residuals(eco_tscs_PW)) %>%
  filter(country=="Germany")

acf(test$resid)

data.frame(country = eco_tscs_PW$model$country,
           year = eco_tscs_PW$model$year,
           y = eco_tscs_PW$model$DV,
           y_pred = eco_tscs_PW$fitted.values,
           resid = residuals(eco_tscs_PW)) %>% 
  ggplot(aes(x=y, y=y_pred)) +
  geom_point(alpha=0.9) +
  geom_abline(intercept=0, slope=1, size=1)

data.frame(y = eco_tscs_PW$model$DV,
           y_pred = eco_tscs_PW$fitted.values,
           resid = residuals(eco_tscs_PW)) %>% 
  ggplot(aes(x=y_pred, y=resid)) +
  geom_point(alpha=0.9) +
  geom_hline(yintercept = 0, size=1)



## Random Effects Model ####

eco_tscs_plm <- pdata.frame(eco_tscs , index=c("country", "year")) 

# Unit Root Test

BeckTest = plm(DV ~ lag_DV, eco_tscs_plm,
               model="pooling")
summary(BeckTest)



BeckTest = plm(DV ~ lag_DV, eco_tscs_plm,
               model="pooling")
summary(BeckTest)
test = data.frame(
  country = index(BeckTest$model)$country,
  year = index(BeckTest$model)$year,
  y = BeckTest$model$DV,
  y_pred = predict(BeckTest),
  resid = residuals(BeckTest)) %>%
  group_by(country) %>% 
  mutate(resid_lag = lag_f(resid))
test_plm <- pdata.frame(test, index=c("country", "year")) 

BeckTest_resid = plm(resid ~ resid_lag, test_plm,
               model="pooling")
summary(BeckTest_resid)

table(eco_tscs_plm$VoC_HS , is.na(eco_tscs_plm$DV) )
eco_tscs_plm$year

eco_RE <- plm::plm(soc_raw ~  soc_raw_lag + year + classification_context + protestant_ctl +
                    cluster_label_1st_mode, 
                   data=eco_tscs_plm,
                   effect="individual",
                   model ="random",
                   random.method = "walhus")
round(coeftest(eco_RE, .vcov=vcovBK(eco_RE, cluster="time")),6)
summary(eco_RE)
#pdwtest(eco_RE)
pbgtest(eco_RE)
#pbltest(eco_RE)

pcdtest(eco_RE, test = c("lm"))

summary(eco_RE)
ranef(eco_RE, effect="individual")
ranef(eco_RE, effect="time")
coef(eco_RE)

eco_RE_BG <- plm::plm(DV ~ lag_DV + lag2_DV + lag3_DV, 
                   data=eco_tscs_plm,
                   effect="individual",
                   model ="random",
                   random.method = "walhus")


pbgtest(eco_RE_BG)


X <- model.matrix(eco_RE)
P <- X %*% solve(t(X) %*% X) %*% t(X)
sigma.sq <- (1 / eco_RE$df.residual) * sum(residuals(eco_RE)^2)
student.resids <- residuals(eco_RE) / (sigma.sq * (1 - diag(P)))

test = data.frame(
           country = index(eco_RE$model)$country,
           year = index(eco_RE$model)$year,
           y = eco_RE$model$DV,
           y_pred = predict(eco_RE),
           resid = residuals(eco_RE)) %>%
  filter(country=="Germany")
acf(test$resid)



test = data.frame(y = eco_RE$model$DV,
                  y_pred = predict(eco_RE),
                  resid = residuals(eco_RE),
                  resid_stud = student.resids)


data.frame(y = eco_RE$model$DV,
           y_pred = eco_RE$model$DV - residuals(eco_RE),
           resid = residuals(eco_RE)) %>% 
  ggplot(aes(x=y, y=y_pred)) +
  geom_point(alpha=0.9) +
  geom_abline(intercept=0, slope=1, size=1)

require(faraway)
par(mfrow=c(3,2))
halfnorm(diag(P), labs = 1:length(eco_RE$model$DV), ylab = 'Leverages', nlab = 1)
eco_RE$model[733,]


data.frame(y = eco_RE$model$DV,
           y_pred = predict(eco_RE),
           resid = residuals(eco_RE),
           resid_stud = student.resids,
           label = paste(index(eco_RE$model)$country, index(eco_RE$model)$year)) %>% 
  ggplot(aes(x=y_pred, y=resid_stud)) +
  geom_point(alpha=0.9) +
  geom_hline(yintercept=0, size=1) +
  geom_text(aes(label = label))


dmx_performance %>% 
  ggplot(aes(x=X2, y=X3)) + 
  geom_point()

unite("ads", index(eco_RE$model))
predict(eco_RE, eco_RE$model[1,])
index()
