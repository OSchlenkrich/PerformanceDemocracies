// Missings MV

functions{
   matrix get_sigma_obs(vector y_obs_id_t, matrix Lcorr, vector sigma, int T, int G) {
    
    matrix[G,G] Lcorr_obs = rep_matrix(0, G,G);
    vector[G] sigma_obs;
    
    int g_count = 1;
    int k_count = 1;
    
    for (g in 1:G) {
      if (y_obs_id_t[g] == 1) {
        sigma_obs[g_count] =  sigma[g];
        
        for (k in 1:g) {
          if (y_obs_id_t[k] == 1) {
            Lcorr_obs[g_count,k_count] = Lcorr[g , k];
            k_count += 1;
          }
        }
        g_count += 1;
        k_count = 1;
      }
    }

   return diag_pre_multiply(sigma_obs[1:(g_count-1)], Lcorr_obs[1:(g_count-1), 1:(g_count-1)]); 
  } 

}

data {
  
  int<lower=0> G;
  int<lower=0> T;
  int<lower=0> N;
  
  int beta_nr;

  vector[N] y;
  
  vector[G] y_obs_id_vec[T];
  
  int time_index[T, G];
  
  int length_obs[T];

  int country_index[N];
  
  matrix[N,beta_nr] X;

}


parameters {
  cholesky_factor_corr[G] Lcorr; 
  vector<lower=0>[G] sigma;
  
  vector[beta_nr] beta;
  
  real mu_lvl2;
  real<lower=0> sigma_lvl2;
  vector[G] mu_lvl1_scaled;

}

transformed parameters {
  vector[G] mu_lvl1;
  vector[N] mu;

  
  mu_lvl1 = mu_lvl2 + sigma_lvl2 * mu_lvl1_scaled;
  
  mu = X * beta + mu_lvl1[country_index];

}

model {
  matrix[G,G] Sigma_obs[T]; 


  mu_lvl1_scaled ~ std_normal();
  sigma_lvl2 ~ cauchy(0, 5);
  mu_lvl2 ~ normal(0, 10);
    
  sigma ~ cauchy(0, 5);
  Lcorr ~ lkj_corr_cholesky(10);
  
  for (t in 1:T) {
    Sigma_obs[t] = get_sigma_obs(y_obs_id_vec[t], Lcorr, sigma, T, G);
    
    y[time_index[t, 1:length_obs[t]]] ~ multi_normal_cholesky(mu[time_index[t, 1:length_obs[t]]], block(Sigma_obs[t], 1, 1, length_obs[t], length_obs[t]));

  }
  
}
generated quantities {
  matrix[G,G] Omega;
  matrix[G,G] Sigma;
  Omega = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(Omega, sigma); 

}
