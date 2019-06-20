// Missings MV

functions{
  matrix get_y_obs(matrix y_obs_id, matrix y, int T, int G) {
    matrix[T,G] y_obs;
    int g_count = 1;
  
    for (t in 1:T) {
      for (g in 1:G) {
        if (y_obs_id[t,g]) {
          y_obs[t,g_count] = y[t,g]; 
          g_count += 1;
        }
      }
      g_count = 1;
    }
    
   return y_obs; 
  }
  
  matrix get_sigma_obs(vector y_obs_id_t, matrix Sigma, int T, int G) {
    
    matrix[G,G] Sigma_obs;
    int g_count = 1;
    int k_count = 1;
    
    for (g in 1:G) {
      if (y_obs_id_t[g] == 1) {
        for (k in 1:G) {
          if (y_obs_id_t[k] == 1) {
            Sigma_obs[g_count,k_count] = Sigma[g , k];
            k_count += 1;
          }
        }
        g_count += 1;
        k_count = 1;
      }
    }

   return Sigma_obs; 
  } 

}

data {
  
  int<lower=0> G;
  int<lower=0> T;
  
  matrix[T,G] y;
  
  matrix[T,G] y_obs_id;
  int length_obs[T];

}


parameters {
  vector[G] mu_g;

  cov_matrix[G] Sigma; 
}

transformed parameters {
  matrix[T,G] mu;

  for (g in 1:G)
    mu[,g] = rep_vector(mu_g[g], T);
}

model {
  matrix[T,G] y_obs = get_y_obs(y_obs_id, y, T, G);
  matrix[G,G] Sigma_obs[T]; 
  
  for (t in 1:T) {
    Sigma_obs[t] = get_sigma_obs(to_vector(y_obs_id[t]), Sigma, T, G);
  }

  mu_g ~ normal(0,10);

  for (t in 1:T) {
    y_obs[t, 1:length_obs[t]] ~ multi_normal(mu[t, 1:length_obs[t]], Sigma_obs[t, 1:length_obs[t], 1:length_obs[t]]);
  }
  
}
generated quantities {

}
