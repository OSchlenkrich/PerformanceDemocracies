
// The input data is a vector 'y' of length 'N'.
data {
  
  int<lower=0> Obs;
  int<lower=0> G;
  int<lower=0> T;
  
  int index[Obs];
  int time_index[Obs];
  
  vector[Obs] y;
  vector[Obs] time;
}


parameters {
  
  vector[G] mu_lvl1_scaled;
    
  real mu_lvl2;
  
  real beta;
  
  vector[G] sigma_intercept;

  vector[T] sigma_time;

  real<lower=0> sigma_lvl2;
}

transformed parameters {
  vector[G] mu_lvl1;
  vector<lower=0>[Obs] sigma_lvl1;

  mu_lvl1 = mu_lvl2 + sigma_lvl2 * mu_lvl1_scaled;
  
  sigma_lvl1 = exp(sigma_intercept[index] + sigma_time[time_index]);

}
model {
  
  mu_lvl1_scaled ~ std_normal();
  sigma_intercept ~ normal(0,2);
  
  y ~ normal(mu_lvl1[index] + beta * time, sigma_lvl1);
  
}

