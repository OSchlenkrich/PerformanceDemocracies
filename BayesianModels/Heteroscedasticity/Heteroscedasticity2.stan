
// The input data is a vector 'y' of length 'N'.
data {
  
  int<lower=0> G;
  int<lower=0> T;
  
  matrix[T,G] y;
  vector[T] time;
}


parameters {
  
  vector[G] mu_lvl1_scaled;
  real mu_lvl2;
  real beta;
  real<lower=0> sigma_lvl2;
  
  cholesky_factor_corr[G] Lcorr;  
  vector<lower=0>[G] sigma; 
}

transformed parameters {
  vector[G] mu_lvl1;
  matrix[T,G] mu;

  mu_lvl1 = mu_lvl2 + sigma_lvl2 * mu_lvl1_scaled;
  
  for (g in 1:G) {
    mu[,g] = mu_lvl1[g] + beta * time;
  }


}
model {
  
  mu_lvl1_scaled ~ std_normal();
  Lcorr ~ lkj_corr_cholesky(2);
  for (t in 1:T) {
    y[t] ~ multi_normal_cholesky(mu[t], diag_pre_multiply(sigma, Lcorr));
  }
  
}
generated quantities {
  matrix[G,G] Omega;
  matrix[G,G] Sigma;
  Omega = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(Omega, sigma); 
}
