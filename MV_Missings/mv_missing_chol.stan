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
  
  matrix get_x_obs(matrix y_obs_id, vector mu_g, int T, int G) {
    matrix[T,G] x;
    matrix[T,G] x_obs;
    
    int g_count = 1;
    
    for (g in 1:G)
      x[,g] = rep_vector(mu_g[g], T);
    
    for (t in 1:T) {
      for (g in 1:G) {
        if (y_obs_id[t,g]) {
          x_obs[t,g_count] = x[t,g]; 
          g_count += 1;
        }
      }
      g_count = 1;
    }
    
   return x_obs; 
  }
  
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
  
  matrix[T,G] y;
  
  matrix[T,G] y_obs_id;
  int length_obs[T];

}


parameters {
  vector[G] mu_g;

  cholesky_factor_corr[G] Lcorr; 
  vector<lower=0>[G] sigma;
}

transformed parameters {
  
}

model {
  matrix[T,G] y_obs = get_y_obs(y_obs_id, y, T, G);
  matrix[T,G] mu_obs = get_x_obs(y_obs_id, mu_g, T, G);
  
  matrix[G,G] Sigma_obs[T]; 
  
  for (t in 1:T) {
    Sigma_obs[t] = get_sigma_obs(to_vector(y_obs_id[t]), Lcorr, sigma, T, G);
  }

  mu_g ~ normal(0,10);
  sigma ~ cauchy(0, 5);
  Lcorr ~ lkj_corr_cholesky(10);
  
  
  for (t in 1:T) {
    y_obs[t, 1:length_obs[t]] ~ multi_normal_cholesky(mu_obs[t, 1:length_obs[t]], Sigma_obs[t, 1:length_obs[t], 1:length_obs[t]]);
  }
  
}
generated quantities {
  matrix[G,G] Omega;
  matrix[G,G] Sigma;
  Omega = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(Omega, sigma); 

}
