
############
library(Compositional)

N <- 300
df <- data.frame(
  x = rnorm(N)
) %>%
  mutate(
    y1 = 1,
    y2 = 0 - 0.1*x,
    y3 = 1 - 0.5*x,
    
    y1 = 1/(1 + exp(y2) + exp(y3)),
    y2 = exp(y2)/(1 + exp(y2) + exp(y3)),
    y3 = exp(y3) /(1 + exp(y2) + exp(y3))
  )


diri_sample = DirichletReg::rdirichlet(N, cbind(df$y1, df$y2, df$y3)*exp(2))
diri_sample[1:50, 1] <- 0
diri_sample[100:150, 3] <- 0


df = df %>% 
  mutate(y1 = diri_sample[,1],
         y2 = diri_sample[,2],
         y3 = diri_sample[,3],
         sum = y1 + y2+ y3,
         y1 = y1/sum,
         y2 = y2/sum,
         y3 = y3/sum,
  ) %>% 
  select(-sum)


# df$Y = DirichletReg::DR_data(df$y1, df$y2, df$y3)
# plot(df$Y)




zadr(cbind(df$y1, df$y2, df$y3), x=df$x)


m1  = DirichReg(Y ~  x, df, "alternative")
summary(m1)



library(rstan)
options(mc.cores = parallel::detectCores())


slicing_matrix = df %>% 
  mutate(ind1  = if_else(y1 == 0, 0, 1),
         ind2  = if_else(y2 == 0, 0, 2),
         ind3  = if_else(y3 == 0, 0, 3),
         nr_cols = rowSums(1-apply(cbind(df$y1, df$y2, df$y3) == 0, 2, as.numeric))) %>% 
  select(nr_cols, ind1,ind2,ind3) %>%
  mutate(id = 1:N) %>% 
  pivot_longer(cols=starts_with("ind")) %>% 
  filter(value != 0) %>% 
  group_by(id) %>% 
  mutate(name_value = 1:nr_cols) %>%
  ungroup() %>% 
  select(id, value, name_value) %>% 
  pivot_wider(names_from = name_value, names_prefix = "ind_", values_fill=list(value = 0)) %>% 
  select_at(vars(starts_with("ind"))) %>% 
  as.matrix()


slicing_index = rowSums(1-apply(cbind(df$y1, df$y2, df$y3) == 0, 2, as.numeric))


stan_data = list(
  N = N,
  ncat = 3,
  isnull = rowSums(df == 0),
  Y = cbind(df$y1, df$y2, df$y3),
  x = df$x,
  slicing_m = slicing_matrix,
  slicing_i = slicing_index
)

fit <- stan(file = 'Dirichlet/zero_adjusted_dirichlet.stan', data = stan_data, chains=4)
print(fit)

##
glass_data = read.csv(file="https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data", header=F)
glass_data$V2_scale = glass_data$V2/1000
y = data.frame(glass_data[,3:10]) %>% 
  mutate(sum = rowSums(.)) %>% 
  mutate_all(funs(./sum)) %>% 
  select(-sum)
  

zadr(cbind(y$V3, y$V4, y$V5 , y$V6  , y$V7  , y$V8 ,   y$V9 , y$V10) , x=scale(glass_data$V2))


N = 214
slicing_matrix = y %>% 
  mutate(ind1  = if_else(V3 == 0, 0, 1),
         ind2  = if_else(V4 == 0, 0, 2),
         ind3  = if_else(V5 == 0, 0, 3),
         ind4  = if_else(V6 == 0, 0, 4),
         ind5  = if_else(V7 == 0, 0, 5),
         ind6  = if_else(V8 == 0, 0, 6),
         ind7  = if_else(V9 == 0, 0, 7),
         ind8  = if_else(V10 == 0, 0, 8),
         nr_cols = rowSums(1-apply(y == 0, 2, as.numeric))) %>% 
  select_at(vars(nr_cols, starts_with("ind"))) %>%
  mutate(id = 1:N) %>% 
  pivot_longer(cols=starts_with("ind")) %>% 
  filter(value != 0) %>% 
  group_by(id) %>% 
  mutate(name_value = 1:nr_cols) %>%
  ungroup() %>% 
  select(id, value, name_value) %>% 
  pivot_wider(names_from = name_value, names_prefix = "ind_", values_fill=list(value = 0)) %>% 
  select_at(vars(starts_with("ind"))) %>% 
  as.matrix()


slicing_index = rowSums(1-apply(y == 0, 2, as.numeric))


stan_data = list(
  N = N,
  ncat = 8,
  Y = y,
  x = scale(glass_data$V2)[,1],
  slicing_m = slicing_matrix,
  slicing_i = slicing_index
)

fit <- stan(file = 'Dirichlet/zero_adjusted_dirichlet.stan', data = stan_data, chains=4)
print(fit)

