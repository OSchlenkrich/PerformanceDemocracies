library(tidyr)
NA_maker = function(x) {
  x = if_else(is.na(x)==T, -99, x)
}


NA_identifier = function(x) {
  x = if_else(x==-99, 0, 1)
}



###

G = 10
Time = 10

mu = rnorm(G, 0, 3)
sigma = diag(runif(G,0.8,1.2)^2,G,G)
stan_data = rmvnorm(Time, mu,  sigma)

stan_data[1,1:8] = -99

stan_data_ident = as.data.frame(stan_data) %>% 
  mutate_all(NA_identifier) 

length_obs = apply(stan_data_ident, 1, sum)


input_stan = list( 
  T = Time,
  G = G,
  
  y = stan_data,
  y_obs_id = stan_data_ident,
  length_obs = length_obs
)


M = stan(file = 'MV_Missings/mv_missing_chol.stan', 
         chains = 1,
         warmup = 1000,
         iter = 2000,
         cores = 1,
         data = input_stan,
         init=0)
print(M)
print(M, "Sigma")
data.frame(summary(M))

traceplot(M, pars="mu_g[1]")

c(8,5) %*% t(c(8,5))
