functions {

  /* dirichlet-logit log-PDF
   * Args: 
   *   y: vector of real response values
   *   mu: vector of category logit probabilities
   *   phi: precision parameter
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
   real dirichlet_logit_lpdf(vector y, vector mu, real phi) {
     return dirichlet_lpdf(y | softmax(mu) * phi);
   }
}

data {
  int<lower=1> N;  // number of observations
  int<lower=2> ncat;  // number of categories
  
  int slicing_m[N, ncat];
  int slicing_i[N];
  
  vector[N] x;
  vector[ncat] Y[N];  // response array
}

parameters {
  real Intercept_muv[ncat-1];
  real b_muv[ncat-1];
  

  real<lower=0> phi;  // precision parameter
}

model {
  vector[N] muv[ncat];
  vector[ncat] mu[N];
  
  for (c in 1:(ncat-1)) {
   muv[c,] = Intercept_muv[c] + x * b_muv[c];
  }
  
  for (n in 1:N) {
    // mu[n] = [0, muv[1,n], muv[2,n], muv[3,n]]';
    mu[n] = [0, muv[1,n], muv[2,n], muv[3,n], muv[4,n], muv[5,n], muv[6,n], muv[7,n]]';

  }
  
  for (n in 1:N) {
    target += dirichlet_logit_lpdf(Y[n, slicing_m[n, 1:slicing_i[n]]] | mu[n, slicing_m[n, 1:slicing_i[n]]], phi);
  }
}

