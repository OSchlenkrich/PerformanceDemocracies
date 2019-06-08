library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



G = 30
N = 20
gmean = rnorm(1, 0,2)
int_r = array(NA, G)
sigma_r = array(NA, G)

# time = seq(-(N/2), (N/2)-1, 1) 
time = seq(1, N, 1) 

y_df = data.frame()
x_gm = runif(G,-2,2)

sigma_r = runif(G,0.5,1)

for (g in 1:G) {
  int_r[g] = gmean + rnorm(1,0,2)

  x_mean = x_gm[g]
  x = runif(N , x_mean, 2)
  x_group = x - x_mean
  
  y_g = data.frame(
    y = int_r[g] + 0.1 * time + rnorm(N, 0, (sigma_r[g])^2), 
    G = g, 
    time,
    time_index =1:N,
    x)
  y_df = bind_rows(y_df, y_g)
}

y_df %>% 
  ggplot(aes(x=time, y=y, col=as.factor(G))) +
  geom_point() +
  facet_wrap(G ~. )

library(plm)
library(lmtest)
plm_1 = plm(y ~ time, index = c("G", "time_index"), y_df, model="random")
summary(plm_1)
coeftest(plm_1, vcov.=function(x) vcovHC(plm_1, cluster="group"))
coeftest(plm_1, vcov.=function(x) vcovBK(plm_1, cluster="time"))

# y_df$time[42] = NA

library(lme4)
library(lmerTest)
m1 = lmer(y ~ time + x + (1 + time|G), y_df)

summary(m1)
m1@frame
m1@vcov_beta
m1@vcov_varpar


# WBRE
y_df_WB = y_df %>% 
  group_by(G) %>% 
  summarise(x_mean = mean(x)) %>% 
  right_join(y_df, by="G") %>% 
  mutate(x_group = x - x_mean)

m1 = lmer(y ~ time + x_group + x_mean + (1 + time|G), y_df_WB)
summary(m1)



setwd("C:/RTest")
library(brms)
m1_bay = brm(bf(y ~ time + (1|G), sigma ~ 0 + (1|G)), 
             family = "gaussian", 
             cores=4, 
             data=y_df_WB)
summary(m1_bay)
marginal_effects(m1_bay)

sigmas <- exp(posterior_samples(m1_bay, "b_sigma_"))
ggplot(stack(sigmas), aes(values)) +
  geom_density(aes(fill = ind))

# STAN MODEL
stan_Data = list( 
  Obs = dim(y_df)[1],
  T = max(y_df$time_index),
  G = max(y_df$G),
  index = y_df$G,
  time_index = y_df$time_index,
  
  y = y_df$y,
  time = y_df$time
  )


M = stan(file = 'Heteroscedasticity/Heteroscedasticity.stan', 
         chains = 4,
         warmup = 1000,
         iter = 2000,
         cores = 4,
         data = stan_Data,
         seed=194838)
print(M, "sigma_lvl1")
print(M, "beta")


m1_bay = brm(bf(y ~ time + (1|G)), 
             family = "gaussian", 
             cores=4, 
             data=y_df)
summary(m1_bay)

m2_bay = brm(bf(y ~ time + (1|G), sigma ~ 0 + (1|G)), 
             family = "gaussian", 
             cores=4, 
             data=y_df)
summary(m2_bay)

m3_bay = brm(bf(y ~ time + (1|G), sigma ~ 0 + time + (1|G)), 
             family = "gaussian", 
             cores=4, 
             data=y_df)
summary(m3_bay)

sigmas <- exp(posterior_samples(m3_bay, "b_sigma_"))

ggplot(stack(sigmas), aes(values)) +
  geom_density(aes(fill = ind))

