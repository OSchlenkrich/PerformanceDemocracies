### Simulate Functions
# m8$d %>% 
#   summarise_all(funs(mean, max))
# simu = data.frame(
#   gdp_caus = 3.387582      ,
#   pop_size_caus = 8.911705            ,
#   englegal_centr_caus = 0,
#   diverse_caus =  0.9061         ,
#   rel_cath_wrp_caus =  0.3609732                 ,
#   time_democratic_perc_caus = 0.5517898               ,
#   def_democracy_perc_caus =  0.5963493   
# )
# 
# simu_diri_function_expected(m8, simu, draws = 100, N = 100, selected_variable = "englegal_centr_caus", categorical = T )
# simu_diri_function_expected2(m8, simu, draws = 100, N = 10000, selected_variable = "englegal_centr_caus", categorical = T )


# simu_diri_function_expected = function(model, simu, N = 1000) {
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
#   pred_2 = data.frame(intercept = rep(1, N)) %>% 
#     cbind(simu)
#   
#   # Fec is reference
#   coef_data = data.frame(rmvnorm(N, unlist(raw_coefs), raw_vcov))
#   coef_1 = coef_data %>% 
#     select_at(vars(starts_with("beta.X_fEC", ignore.case = F)))
#   coef_2 = coef_data %>% 
#     select_at(vars(starts_with("beta.X_fEc", ignore.case = F)))
#   coef_3 = coef_data %>% 
#     select_at(vars(starts_with("beta.X_FeC", ignore.case = F)))
#   
#   coef_gamma = coef_data %>% 
#     select_at(vars(starts_with("gamma.gamma", ignore.case = F))) 
#   
#   betas = data.frame(
#     a1 = 1,
#     a2 = rowSums(pred_2 * coef_1),
#     a3 = rowSums(pred_2 * coef_2),
#     a4 = rowSums(pred_2 * coef_3),
#     phi = coef_gamma$gamma.gamma..Intercept.
#   ) %>% 
#     mutate(
#       a1_exp = 1/(1 + exp(a2) + exp(a3) + exp(a4)),
#       a2_exp = exp(a2)/(1 + exp(a2) + exp(a3) + exp(a4)),
#       a3_exp = exp(a3)/(1 + exp(a2) + exp(a3) + exp(a4)),
#       a4_exp = exp(a4)/(1 + exp(a2) + exp(a3) + exp(a4))
#     ) %>% 
#     select_at(vars(ends_with("exp"), phi)) %>% 
#     mutate_all(funs(.*phi)) %>% 
#     select_at(vars(ends_with("exp"))) %>% 
#     as.matrix()
#   
#   
#   simulated_Data = as.data.frame(DirichletReg::rdirichlet(N, betas))
#   names(simulated_Data)  = colnames(model$Y)
#   
#   # simulated_Data %>%
#   #   pivot_longer(cols=everything()) %>%
#   #   group_by(name) %>%
#   #   summarise(mean = mean(value),
#   #             lower = quantile(value, probs=0.025),
#   #             upper = quantile(value, probs=0.975))
#   
#   simulated_Data %>% 
#     pivot_longer(cols=everything()) %>% 
#     ggplot(aes(x=name, y=value)) + 
#     geom_boxplot(fatten = NULL) +
#     stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
#                  width = 0.75, size = 1, linetype = "solid") +
#     scale_y_continuous(breaks=seq(0,1,0.2))
# }



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
  
  for (i in 1:length_simu) {
    pred_2 = data.frame(intercept = rep(1, N)) %>% 
      cbind(simu %>% slice(i))
    
    
    for (d in 1:draws) {
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
        a2 = rowSums(pred_2 * coef_1),
        a3 = rowSums(pred_2 * coef_2),
        a4 = rowSums(pred_2 * coef_3),
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
      names(simulated_Data)  = colnames(m2$Y)
      
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
  # Create DataFrame for Prediction
  
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
      xlab(selected_variable)  +
      theme_bw()
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
      #facet_wrap(name~.) +
      scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0,1)) +
      xlab(selected_variable)  +
      theme_bw()
    
  }
  
}


simu_diri_function_expected2 = function(model, simu, draws = 100, N = 1000, selected_variable, categorical = F ) {
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
  coef_data = data.frame(rmvnorm(draws, unlist(raw_coefs), raw_vcov))
  coef_1 = coef_data %>% 
    select_at(vars(starts_with("beta.X_fEC", ignore.case = F)))
  coef_2 = coef_data %>% 
    select_at(vars(starts_with("beta.X_fEc", ignore.case = F)))
  coef_3 = coef_data %>% 
    select_at(vars(starts_with("beta.X_FeC", ignore.case = F)))
  
  coef_gamma = coef_data %>% 
    select_at(vars(starts_with("gamma.gamma", ignore.case = F))) 
  
  for (i in 1:length_simu) {
    pred_2 = data.frame(intercept = rep(1, draws)) %>% 
      cbind(simu %>% slice(i))
    
    betas = data.frame(
      a1 = 1,
      a2 = rowSums(pred_2 * coef_1),
      a3 = rowSums(pred_2 * coef_2),
      a4 = rowSums(pred_2 * coef_3),
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
    
    
    for (d in 1:draws) {
    
      simulated_Data = as.data.frame(DirichletReg::rdirichlet(N, betas[d,]))
      names(simulated_Data)  = colnames(m2$Y)
      
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
  # Create DataFrame for Prediction
  
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
