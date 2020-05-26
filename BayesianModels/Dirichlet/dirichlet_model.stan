// generated with brms 2.10.3
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

  /* multi-normal log-PDF for special residual covariance structures 
   * assuming homogoneous variances
   * Args: 
   *   y: response vector 
   *   mu: mean parameter vector
   *   sigma: residual standard deviation
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real normal_cov_hom_lpdf(vector y, vector mu, real sigma, matrix chol_cor, 
                           vector se2, int[] nobs, int[] begin, int[] end) {
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] L;
      L = sigma * chol_cor[1:nobs[i], 1:nobs[i]];
      if (has_se) {
        // need to add 'se' to the correlation matrix itself
        L = multiply_lower_tri_self_transpose(L);
        L += diag_matrix(se2[begin[i]:end[i]]);
        L = cholesky_decompose(L);
      }
      lp[i] = multi_normal_cholesky_lpdf(
        y[begin[i]:end[i]] | mu[begin[i]:end[i]], L
      );
    }                        
    return sum(lp); 
  }
  /* multi-normal log-PDF for special residual covariance structures 
   * assuming heterogenous variances
   * Args: 
   *   y: response vector 
   *   mu: mean parameter vector
   *   sigma: residual standard deviation vector
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real normal_cov_het_lpdf(vector y, vector mu, vector sigma, matrix chol_cor, 
                           vector se2, int[] nobs, int[] begin, int[] end) {
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] L;
      L = diag_pre_multiply(sigma[begin[i]:end[i]], 
                            chol_cor[1:nobs[i], 1:nobs[i]]);
      if (has_se) {
        // need to add 'se' to the correlation matrix itself
        L = multiply_lower_tri_self_transpose(L);
        L += diag_matrix(se2[begin[i]:end[i]]);
        L = cholesky_decompose(L);
      }
      lp[i] = multi_normal_cholesky_lpdf(
        y[begin[i]:end[i]] | mu[begin[i]:end[i]], L
      );
    }                        
    return sum(lp); 
  }

  /* multi-student-t log-PDF for special residual covariance structures 
   * assuming homogoneous variances
   * Args: 
   *   y: response vector 
   *   nu: degrees of freedom parameter 
   *   mu: mean parameter vector
   *   sigma: scale parameter
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real student_t_cov_hom_lpdf(vector y, real nu, vector mu, real sigma, 
                              matrix chol_cor, vector se2, int[] nobs, 
                              int[] begin, int[] end) { 
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] Cov; 
      Cov = sigma * chol_cor[1:nobs[i], 1:nobs[i]];
      Cov = multiply_lower_tri_self_transpose(Cov);
      if (has_se) {
        Cov += diag_matrix(se2[begin[i]:end[i]]);
      }
      lp[i] = multi_student_t_lpdf(
        y[begin[i]:end[i]] | nu, mu[begin[i]:end[i]], Cov
      );
    }                        
    return sum(lp); 
  }
  /* multi-student-t log-PDF for special residual covariance structures 
   * assuming heterogenous variances
   * Args: 
   *   y: response vector 
   *   nu: degrees of freedom parameter 
   *   mu: mean parameter vector
   *   sigma: scale parameter vector
   *   chol_cor: cholesky factor of the correlation matrix
   *   se2: square of user defined standard errors 
   *     should be set to zero if none are defined 
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   sum of the log-PDF values of all observations 
   */ 
  real student_t_cov_het_lpdf(vector y, real nu, vector mu, vector sigma, 
                              matrix chol_cor, vector se2, int[] nobs, 
                              int[] begin, int[] end) { 
    int I = size(nobs);
    int has_se = max(se2) > 0;
    vector[I] lp; 
    for (i in 1:I) { 
      matrix[nobs[i], nobs[i]] Cov; 
      Cov = diag_pre_multiply(sigma[begin[i]:end[i]], 
                              chol_cor[1:nobs[i], 1:nobs[i]]);
      Cov = multiply_lower_tri_self_transpose(Cov);
      if (has_se) {
        Cov += diag_matrix(se2[begin[i]:end[i]]);
      }
      lp[i] = multi_student_t_lpdf(
        y[begin[i]:end[i]] | nu, mu[begin[i]:end[i]], Cov
      );
    }                        
    return sum(lp); 
  }

  /* scale and correlate residuals 
   * Args: 
   *   zerr: standardized and independent residuals
   *   sderr: standard deviation of the residuals
   *   chol_cor: cholesky factor of the correlation matrix
   *   nobs: number of observations in each group 
   *   begin: the first observation in each group 
   *   end: the last observation in each group 
   * Returns: 
   *   vector of scaled and correlated residuals
   */ 
   vector scale_cov_err(vector zerr, real sderr, matrix chol_cor, 
                        int[] nobs, int[] begin, int[] end) { 
     vector[rows(zerr)] err; 
     for (i in 1:size(nobs)) { 
       err[begin[i]:end[i]] = 
         sderr * chol_cor[1:nobs[i], 1:nobs[i]] * zerr[begin[i]:end[i]];
     }                        
     return err; 
   }

  /* compute the cholesky factor of an AR1 correlation matrix
   * Args: 
   *   ar: AR1 autocorrelation 
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows matrix 
   */ 
   matrix cholesky_cor_ar1(real ar, int nrows) { 
     matrix[nrows, nrows] mat; 
     vector[nrows - 1] gamma; 
     mat = diag_matrix(rep_vector(1, nrows)); 
     for (i in 2:nrows) { 
       gamma[i - 1] = pow(ar, i - 1); 
       for (j in 1:(i - 1)) { 
         mat[i, j] = gamma[i - j]; 
         mat[j, i] = gamma[i - j]; 
       } 
     } 
     return cholesky_decompose(1 / (1 - ar^2) * mat); 
   }

  /* compute the cholesky factor of a MA1 correlation matrix
   * Args: 
   *   ma: MA1 autocorrelation 
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows MA1 covariance matrix 
   */ 
   matrix cholesky_cor_ma1(real ma, int nrows) { 
     matrix[nrows, nrows] mat; 
     mat = diag_matrix(rep_vector(1 + ma^2, nrows)); 
     if (nrows > 1) { 
       mat[1, 2] = ma; 
       for (i in 2:(nrows - 1)) { 
         mat[i, i - 1] = ma; 
         mat[i, i + 1] = ma; 
       } 
       mat[nrows, nrows - 1] = ma; 
     } 
     return cholesky_decompose(mat); 
   }

  /* compute the cholesky factor of an ARMA1 correlation matrix
   * Args: 
   *   ar: AR1 autocorrelation 
   *   ma: MA1 autocorrelation 
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows matrix 
   */ 
   matrix cholesky_cor_arma1(real ar, real ma, int nrows) { 
     matrix[nrows, nrows] mat; 
     vector[nrows] gamma; 
     mat = diag_matrix(rep_vector(1 + ma^2 + 2 * ar * ma, nrows)); 
     gamma[1] = (1 + ar * ma) * (ar + ma); 
     for (i in 2:nrows) { 
       gamma[i] = gamma[1] * pow(ar, i - 1); 
       for (j in 1:(i - 1)) { 
         mat[i, j] = gamma[i - j]; 
         mat[j, i] = gamma[i - j]; 
       } 
     } 
     return cholesky_decompose(1 / (1 - ar^2) * mat); 
   }

  /* compute the cholesky factor of a compound symmetry correlation matrix
   * Args: 
   *   cosy: compound symmetry correlation
   *   nrows: number of rows of the covariance matrix 
   * Returns: 
   *   A nrows x nrows covariance matrix 
   */ 
   matrix cholesky_cor_cosy(real cosy, int nrows) { 
     matrix[nrows, nrows] mat; 
     mat = diag_matrix(rep_vector(1, nrows)); 
     for (i in 2:nrows) { 
       for (j in 1:(i - 1)) { 
         mat[i, j] = cosy; 
         mat[j, i] = mat[i, j];
       } 
     } 
     return cholesky_decompose(mat); 
   }
}
data {
  int<lower=1> N;  // number of observations
  int<lower=2> ncat;  // number of categories
  vector[ncat] Y[N];  // response array
  // data needed for ARMA correlations
  int<lower=0> Kar;  // AR order
  int<lower=0> Kma;  // MA order
  // see the functions block for details
  int<lower=1> N_tg;
  int<lower=1> begin_tg[N_tg];
  int<lower=1> end_tg[N_tg];
  int<lower=1> nobs_tg[N_tg];
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_muyd2_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_muyd2_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> J_3[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_muyd3_1;
  // data for group-level effects of ID 4
  int<lower=1> N_4;  // number of grouping levels
  int<lower=1> M_4;  // number of coefficients per level
  int<lower=1> J_4[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_4_muyd3_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int max_lag = max(Kar, Kma);
  // no known standard errors specified by the user
  vector[N] se2 = rep_vector(0, N);
}
parameters {
  // temporary intercept for centered predictors
  real Intercept_muyd2;
  // temporary intercept for centered predictors
  real Intercept_muyd3;
  real<lower=0> phi;  // precision parameter
  vector<lower=-1,upper=1>[2] ar;  // autoregressive effects
  vector[N] zerryd2;  // unscaled residuals
  vector[N] zerryd3;  // unscaled residuals

  real<lower=0> sderryd2;  // SD of residuals
  real<lower=0> sderryd3;  // SD of residuals

  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  // standardized group-level effects
  vector[N_1] z_1[M_1];
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  // standardized group-level effects
  vector[N_2] z_2[M_2];
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  // standardized group-level effects
  vector[N_3] z_3[M_3];
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations
  // standardized group-level effects
  vector[N_4] z_4[M_4];
}
transformed parameters {
  // cholesky factor of the autocorrelation matrix
  matrix[max(nobs_tg), max(nobs_tg)] chol_coryd2;
  matrix[max(nobs_tg), max(nobs_tg)] chol_coryd3;

  vector[N] erryd2;  // actual residuals
  vector[N] erryd3;  // actual residuals

  // actual group-level effects
  vector[N_1] r_1_muyd2_1 = (sd_1[1] * (z_1[1]));
  // actual group-level effects
  vector[N_2] r_2_muyd2_1 = (sd_2[1] * (z_2[1]));
  // actual group-level effects
  vector[N_3] r_3_muyd3_1 = (sd_3[1] * (z_3[1]));
  // actual group-level effects
  vector[N_4] r_4_muyd3_1 = (sd_4[1] * (z_4[1]));
  // compute residual covariance matrix
  chol_coryd2 = cholesky_cor_ar1(ar[1], max(nobs_tg));
  chol_coryd3 = cholesky_cor_ar1(ar[2], max(nobs_tg));

  // compute correlated residuals
  erryd2 = scale_cov_err(zerryd2, sderryd3, chol_coryd2, nobs_tg, begin_tg, end_tg);
  erryd3 = scale_cov_err(zerryd3, sderryd3, chol_coryd3, nobs_tg, begin_tg, end_tg);

}
model {
  // initialize linear predictor term
  vector[N] muyd2 = Intercept_muyd2 + rep_vector(0, N) + erryd2;
  // initialize linear predictor term
  vector[N] muyd3 = Intercept_muyd3 + rep_vector(0, N) + erryd3;
  // linear predictor matrix
  vector[ncat] mu[N];
  for (n in 1:N) {
    // add more terms to the linear predictor
    muyd2[n] += r_1_muyd2_1[J_1[n]] * Z_1_muyd2_1[n] + r_2_muyd2_1[J_2[n]] * Z_2_muyd2_1[n];
  }
  for (n in 1:N) {
    // add more terms to the linear predictor
    muyd3[n] += r_3_muyd3_1[J_3[n]] * Z_3_muyd3_1[n] + r_4_muyd3_1[J_4[n]] * Z_4_muyd3_1[n];
  }
  for (n in 1:N) {
    mu[n] = [0, muyd2[n], muyd3[n]]';
  }
  // priors including all constants
  target += normal_lpdf(Intercept_muyd2 | 0,100);
  target += normal_lpdf(Intercept_muyd3 | 0,100);
  target += gamma_lpdf(phi | 0.01, 0.01);
  target += normal_lpdf(zerryd2 | 0, 1);
  target += normal_lpdf(zerryd3 | 0, 1);

  target += student_t_lpdf(sderryd2 | 3, 0, 10);
  target += student_t_lpdf(sderryd3 | 3, 0, 10);

  target += cauchy_lpdf(sd_1 | 0,5)
    - 1 * cauchy_lccdf(0 | 0,5);
  target += normal_lpdf(z_1[1] | 0, 1);
  target += cauchy_lpdf(sd_2 | 0,5)
    - 1 * cauchy_lccdf(0 | 0,5);
  target += normal_lpdf(z_2[1] | 0, 1);
  target += cauchy_lpdf(sd_3 | 0,5)
    - 1 * cauchy_lccdf(0 | 0,5);
  target += normal_lpdf(z_3[1] | 0, 1);
  target += cauchy_lpdf(sd_4 | 0,5)
    - 1 * cauchy_lccdf(0 | 0,5);
  target += normal_lpdf(z_4[1] | 0, 1);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += dirichlet_logit_lpdf(Y[n] | mu[n], phi);
    }
  }
}
generated quantities {
  // actual population-level intercept
  real b_muyd2_Intercept = Intercept_muyd2;
  // actual population-level intercept
  real b_muyd3_Intercept = Intercept_muyd3;
}
