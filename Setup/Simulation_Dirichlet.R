# Simulation for Dirichlet


simu_diri_function_expected = function(model, simu, draws = 100, N = 100, selected_variable, categorical = F ) {
  # model = Dirichletreg model
  # simu = Simulated Data
  # N = number of simulations
  library(mvtnorm)
  
  # get coefs
  raw_coefs = coef(model)
  raw_vcov = vcov(model)
  
  length_simu = dim(simu)[1]
  container = data.frame(array(NA, dim =c(0,4)))
  names(container) =  colnames(model$Y)
  
  # Fec is reference
  coef_data = data.frame(rmvnorm(N, unlist(raw_coefs), raw_vcov))
  coef_1 = coef_data %>% 
    select_at(vars(starts_with("beta.X_fEC", ignore.case = F)))
  coef_2 = coef_data %>% 
    select_at(vars(starts_with("beta.X_fEc", ignore.case = F)))
  coef_3 = coef_data %>% 
    select_at(vars(starts_with("beta.X_FeC", ignore.case = F)))
  
  coef_gamma = coef_data %>% 
    select_at(vars(starts_with("gamma.gamma", ignore.case = F))) 
  
  
  for (i in 1:length_simu) {
    pred_2 = data.frame(intercept = rep(1, N)) %>% 
      cbind(simu %>% slice(i))
    
    betas = data.frame(
      a1 = 1,
      a2 = rowSums(pred_2 * coef_1),
      a3 = rowSums(pred_2 * coef_2),
      a4 = rowSums(pred_2 * coef_3),
      phi = exp(coef_gamma$gamma.gamma..Intercept)
    ) %>% 
      mutate(
        a1_exp = 1/(1 + exp(a2) + exp(a3) + exp(a4)),
        a2_exp = exp(a2)/(1 + exp(a2) + exp(a3) + exp(a4)),
        a3_exp = exp(a3)/(1 + exp(a2) + exp(a3) + exp(a4)),
        a4_exp = exp(a4)/(1 + exp(a2) + exp(a3) + exp(a4))
      ) %>% 
      select_at(vars(ends_with("exp"), phi)) %>% 
      mutate_all(funs(.*phi)) %>% 
      select_at(vars(ends_with("exp"))) %>% 
      as.matrix()
    
    for (d in 1:draws) {
    
      simulated_Data = as.data.frame(DirichletReg::rdirichlet(N, betas))
      names(simulated_Data)  = colnames(model$Y)
      
      mean_values = simulated_Data %>%
        pivot_longer(cols=everything()) %>%
        group_by(name) %>%
        summarise(mean = mean(value)) %>% 
        pivot_wider(names_from = name, values_from = mean) %>% 
        bind_cols(simu %>% select(selected_variable) %>% slice(i))
      
      container = container %>% 
        bind_rows(mean_values) 
    }
    
  }
  # Plot
  
  if (categorical == T) {
    container %>% 
      select_at(vars(select_x = selected_variable, starts_with("X_"))) %>% 
      pivot_longer(cols=c(starts_with("X_"))) %>% 
      group_by(select_x, name) %>% 
      summarise(mean = mean(value),
                lower = quantile(value, 0.05),
                upper = quantile(value, 0.95)) %>% 
      ungroup() %>% 
      mutate(select_x = as.factor(select_x)) %>%
      ggplot(aes(x=select_x, y=mean, ymin = lower, ymax = upper, col=name)) + 
      geom_errorbar() +
      facet_wrap(name~.) +
      scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0,1)) +
      xlab(selected_variable)
  } else {
    container %>% 
      select_at(vars(select_x = selected_variable, starts_with("X_"))) %>% 
      pivot_longer(cols=c(starts_with("X_"))) %>% 
      group_by(select_x, name) %>% 
      summarise(mean = mean(value),
                lower = quantile(value, 0.05),
                upper = quantile(value, 0.95)) %>% 
      ggplot(aes(x=select_x, y=mean, ymin = lower, ymax = upper, fill=name)) + 
      geom_line() + 
      geom_ribbon(alpha= 0.5) +
      facet_wrap(name~.) +
      scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0,1)) +
      xlab(selected_variable)
    
  }
  
}



simu_diri_function_predicted = function(model, simu, N = 1000) {
  # model = Dirichletreg model
  # simu = Simulated Data
  # N = number of simulations
  library(mvtnorm)
  
  # get coefs
  raw_coefs = coef(model)
  raw_vcov = vcov(model)
  
  # Create DataFrame for Prediction
  simu = model.matrix(model)$X
  
  
  # Fec is reference
  coef_data = data.frame(rmvnorm(N, unlist(raw_coefs), raw_vcov))
  coef_1 = coef_data %>% 
    select_at(vars(starts_with("beta.X_fEC", ignore.case = F)))
  coef_2 = coef_data %>% 
    select_at(vars(starts_with("beta.X_fEc", ignore.case = F)))
  coef_3 = coef_data %>% 
    select_at(vars(starts_with("beta.X_FeC", ignore.case = F)))
  
  coef_gamma = coef_data %>% 
    select_at(vars(starts_with("gamma.gamma", ignore.case = F))) 
  
  betas = data.frame(
    a1 = 1,
    a2 = rowSums(simu[[1]] * coef_1),
    a3 = rowSums(simu[[1]]  * coef_2),
    a4 = rowSums(simu[[1]]  * coef_3),
    phi = coef_gamma$gamma.gamma..Intercept.
  ) %>% 
    mutate(
      a1_exp = 1/(1 + exp(a2) + exp(a3) + exp(a4)),
      a2_exp = exp(a2)/(1 + exp(a2) + exp(a3) + exp(a4)),
      a3_exp = exp(a3)/(1 + exp(a2) + exp(a3) + exp(a4)),
      a4_exp = exp(a4)/(1 + exp(a2) + exp(a3) + exp(a4))
    ) %>% 
    select_at(vars(ends_with("exp"), phi)) %>% 
    mutate_all(funs(.*phi)) %>% 
    select_at(vars(ends_with("exp"))) %>% 
    as.matrix()
  
  
  simulated_Data = as.data.frame(DirichletReg::rdirichlet(N, betas))
  names(simulated_Data)  = colnames(model$Y)
  
  simulated_Data %>% 
    pivot_longer(cols=everything()) %>% 
    ggplot(aes(x=name, y=value)) + 
    geom_boxplot(fatten = NULL) +
    stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 0.75, size = 1, linetype = "solid") +
    scale_y_continuous(breaks=seq(0,1,0.2))
  
}


# simu_diri_function_ppd = function(model, simu, N = 1000) {
#   # model = Dirichletreg model
#   # simu = Simulated Data
#   # N = number of simulations
#   library(mvtnorm)
#   
#   # get coefs
#   raw_coefs = coef(model)
#   raw_vcov = vcov(model)
#   
#   # Create DataFrame for Prediction
#   simu = model.matrix(model)$X
#   
#   
#   # Fec is reference
#   coef_data = data.frame(rmvnorm(N, unlist(raw_coefs), raw_vcov))
#   coef_1 = coef_data %>% 
#     select_at(vars(starts_with("beta.X_fEC", ignore.case = F))) %>% 
#     as.matrix()
#   coef_2 = coef_data %>% 
#     select_at(vars(starts_with("beta.X_fEc", ignore.case = F))) %>% 
#     as.matrix()
#   coef_3 = coef_data %>% 
#     select_at(vars(starts_with("beta.X_FeC", ignore.case = F))) %>% 
#     as.matrix()
#   
#   coef_gamma = coef_data %>% 
#     select_at(vars(starts_with("gamma.gamma", ignore.case = F)))
#   
#   
#   a2 = exp(coef_1 %*% t(simu[[2]]))
#   a3 = exp(coef_2 %*% t(simu[[3]]))
#   a4 = exp(coef_3 %*% t(simu[[4]]))
#   
#   lin_predict = array(NA, dim = c(101, N, 4))
#   for (i in 1:101) {
#     betas = data.frame(
#       a1_exp = 1/(1+a2[,i]+a3[,i]+a4[,i]),
#       a2_exp = a2[,i]/(1+a2[,i]+a3[,i]+a4[,i]),
#       a3_exp = a3[,i]/(1+a2[,i]+a3[,i]+a4[,i]),
#       a4_exp = a4[,i]/(1+a2[,i]+a3[,i]+a4[,i]),
#       phi = coef_gamma$gamma.gamma..Intercept.
#     ) %>% 
#       mutate_all(funs(.*phi)) %>% 
#       select_at(vars(ends_with("exp"))) %>% 
#       as.matrix()
#     lin_predict[i,,] = betas
#   }
# 
#   sims <- array(NA, dim = c(N*N,101, 4))
#   for (i in 1:101) {
#     sims[,i,] = DirichletReg::rdirichlet(N*N, matrix(rep(lin_predict[i,,], each = 1000), ncol=4))
#   }
#   
#   
#   simulated_Data %>% 
#     pivot_longer(cols=everything()) %>% 
#     ggplot(aes(x=name, y=value)) + 
#     geom_boxplot(fatten = NULL) +
#     stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
#                  width = 0.75, size = 1, linetype = "solid") +
#     scale_y_continuous(breaks=seq(0,1,0.2))
#   
# }
# 
# hist(sims[,,3])
# 
# 
# hist(sims[96,,2])
# hist(sims[96,,3])
# hist(sims[96,,4])
# 
# matrix(rep(lin_predict[2,,], 1000), ncol=4)
# hist(model$data$X_fEc)
# library(bayesplot)
# bayesplot::ppc_dens_overlay(model$data$X_fEc, sims[sample(1:1000000,20),,4])
# bayesplot::ppc_dens_overlay(model$data$X_FeC, sims[sample(1:1000000,100),,4])
