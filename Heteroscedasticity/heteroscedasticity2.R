library(dplyr)
library(tidyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(mvtnorm)


G = 10
N = 15
gmean = rnorm(1, 0,0)
int_r = array(NA, G)
sigma_r = array(NA, G)

#time = seq(-(N/2), (N/2)-1, 1)
time = seq(1, N, 1) -1

y_df = data.frame()

sigma_r = runif(G,3,4)


A <- matrix(rnorm(G^2,0,0.01), ncol=G) 
diag(A) = runif(G, 1,2)
cov_matrix <- t(A) %*% A


int_r = gmean + rnorm(G,0,2)

means = array(NA, dim = c(N,G))

for (g in 1:G) {
  means[,g] = int_r[g] + 0.1 * time
}

final_data = array(NA, dim = c(N,G))

for (t in 1:N) {
  final_data[t,] = rmvnorm(1, means[t,], cov_matrix)
  
}
time_index = rep(1:N, G)
y_df = data.frame(final_data) %>% 
  gather("G","y") %>% 
  mutate(time = time_index,
         time0 = time_index-1,
         time_index = time_index)

# y_df %>%
#   ggplot(aes(x=time, y=y, col=as.factor(G))) +
#   geom_point() +
#   facet_wrap(G ~. )

library(plm)
library(lmtest)
plm_1 = plm(y ~ time0, index = c("G", "time_index"), y_df, model="random")
summary(plm_1)
# coeftest(plm_1, vcov.=function(x) vcovHC(plm_1, cluster="group"))
coeftest(plm_1, vcov.=function(x) vcovBK(plm_1, cluster="time"))
ranef(plm_1)

BeckTest = plm(y ~ time, index = c("G", "time_index"), y_df, model="random")


Epsilon = data.frame(index(BeckTest), BeckTest$residuals) %>% 
  spread("time_index", "BeckTest.residuals") %>% 
  select(-G) %>% 
  as.matrix()


mylist = list()
for (tt in 1:N) {
  E_hat = array(NA, dim=c(G,G))
  for (i in 1:G) {
    for (j in 1:G) {
      summe = 0 
      for (t in 1:N) {
        summe = summe + (Epsilon[i,t]  * Epsilon[j,t])
      }
      E_hat[i,j]= summe/N
    }
  }
  mylist[[tt]] =  E_hat
}

E_hat
cov_matrix - E_hat
# y_df$time[42] = NA

library(lme4)
library(lmerTest)
m1 = lmer(y ~ time0 + (1 |G), y_df)

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
y_df %>% 
  spread("G", "y")
final_data
time_index

stan_Data = list( 
  T = N,
  G = G,

  y = final_data,
  time = 0:(N-1)
  )


M = stan(file = 'Heteroscedasticity/Heteroscedasticity2.stan', 
         chains = 4,
         warmup = 1000,
         iter = 2000,
         cores = 4,
         data = stan_Data,
         seed=194838)
print(M, "beta")
print(M, "Omega")
print(M, "sigma")
print(M, "mu_lvl1")
print(M, "mu_lvl2")
print(M, "sigma_lvl2")
print(M, "Sigma")

shinystan::launch_shinystan(M)

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


