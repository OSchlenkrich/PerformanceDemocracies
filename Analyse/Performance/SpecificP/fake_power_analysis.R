# Power Analysis of Compositional Data ####


power_data = wealth_list[[1]] %>% 
  select(country_text_id, year_0,
         wealth_eco,
         wealth_eco_lag_wi,
         trade_wdi_num_ctl_wi,
         trade_wdi_num_ctl_wi_lag, 
         trade_wdi_num_ctl_bw,
         FKM_5_mb_fEc.FKM_5_mb_tot_fEcT,
         FKM_5_mb_Fec.FKM_5_mb_tot_FecT,
         FKM_5_mb_FEC.FKM_5_mb_tot_FECT,
         FKM_5_mb_FeC.FKM_5_mb_tot_FeCT,
         FKM_5_mb_fEC.FKM_5_mb_tot_fECT,
         FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_wi,
         FKM_5_mb_Fec.FKM_5_mb_tot_FecT_wi,
         FKM_5_mb_FEC.FKM_5_mb_tot_FECT_wi,
         FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_wi,
         FKM_5_mb_fEC.FKM_5_mb_tot_fECT_wi,
         FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_wi_lag,
         FKM_5_mb_Fec.FKM_5_mb_tot_FecT_wi_lag,
         FKM_5_mb_FEC.FKM_5_mb_tot_FECT_wi_lag,
         FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_wi_lag,
         FKM_5_mb_fEC.FKM_5_mb_tot_fECT_wi_lag,
         FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_bw,
         FKM_5_mb_Fec.FKM_5_mb_tot_FecT_bw,
         FKM_5_mb_FEC.FKM_5_mb_tot_FECT_bw,
         FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_bw,
         FKM_5_mb_fEC.FKM_5_mb_tot_fECT_bw
         )


# FKM_5_mb_fEc.FKM_5_mb_tot_fEcT
# FKM_5_mb_Fec.FKM_5_mb_tot_FecT
# FKM_5_mb_FEC.FKM_5_mb_tot_FECT
# FKM_5_mb_FeC.FKM_5_mb_tot_FeCT
# FKM_5_mb_fEC.FKM_5_mb_tot_fECT


# there is some variance over time
power_data %>% 
  select(country_text_id, year_0,
         FKM_5_mb_fEc.FKM_5_mb_tot_fEcT,
         FKM_5_mb_Fec.FKM_5_mb_tot_FecT,
         FKM_5_mb_FEC.FKM_5_mb_tot_FECT,
         FKM_5_mb_FeC.FKM_5_mb_tot_FeCT,
         FKM_5_mb_fEC.FKM_5_mb_tot_fECT) %>% 
  pivot_longer(cols=starts_with("FKM")) %>% 
  ggplot(aes(x=year_0, y=value, col=name)) +
  geom_line() +
  facet_wrap(country_text_id ~ .) +
  theme_bw() +
  theme(legend.position = "bottom")

# true values

true_model = glmmTMB(wealth_eco ~ wealth_eco_lag_wi + (1|country_text_id), power_data)
summary(true_model)

true_lvl2_int = -0.127447
true_lvl2_sd = sqrt(0.82297)
true_lvl1_sd = sqrt(0.00136)


##
multicor_analyse = power_data %>% 
  na.omit() %>% 
  mutate(y = NA) 


# number of countries
G = length(unique(multicor_analyse$country_text_id))
N = dim(multicor_analyse)[2]

startend_frame = multicor_analyse %>% 
  group_by(country_text_id) %>% 
  summarise(nr = n()) %>% 
  ungroup() %>% 
  mutate(start = dplyr::lag(cumsum(nr), 1) + 1,
         end = cumsum(nr))
startend_frame$start[1] = 2

int_G = rnorm(G, true_lvl2_int, true_lvl2_sd)


# Simulation ####

power_data_analyse = power_data  %>% 
  na.omit() %>% 
  mutate(y = 0,
         y_lag = NA)


startend_frame = power_data_analyse %>% 
  group_by(country_text_id) %>% 
  summarise(nr = n()) %>% 
  ungroup() %>% 
  mutate(start = dplyr::lag(cumsum(nr), 1) + 1,
         end = cumsum(nr))
startend_frame$start[1] = 2

int_G = rnorm(G, 2, 1)

nruns = 100
nr_coefs = 11

sim_coefs = seq(0.01, 0.31, length.out = nr_coefs)


effect_names_wi = c("FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_wi",
                 "FKM_5_mb_Fec.FKM_5_mb_tot_FecT_wi",
                 "FKM_5_mb_FEC.FKM_5_mb_tot_FECT_wi",
                 "FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_wi",
                 "FKM_5_mb_fEC.FKM_5_mb_tot_fECT_wi")
effect_names_bw = c("FKM_5_mb_fEc.FKM_5_mb_tot_fEcT_bw",
                    "FKM_5_mb_Fec.FKM_5_mb_tot_FecT_bw",
                    "FKM_5_mb_FEC.FKM_5_mb_tot_FECT_bw",
                    "FKM_5_mb_FeC.FKM_5_mb_tot_FeCT_bw",
                    "FKM_5_mb_fEC.FKM_5_mb_tot_fECT_bw")

# dimension of array: 
# [1,,,] = number of runs
# [,1,,] = coefficients of _wi and _bw
# [,,1,] = strength of coefficients
# [,,,1] = names of effects

mycoef = array(NA, dim=c(nruns, 2, nr_coefs, length(effect_names_wi)))

for (effectnames in 1:length(effect_names_wi)) { 
  print(effect_names_wi[effectnames])
  
  for (coefs in 1:nr_coefs) {
    print(paste(coefs, "from", nr_coefs))
    
    for (run in 1:nruns)  {
      # select data
      run_data = power_data_analyse %>% 
        select_at(vars(country_text_id, year_0, y, 
                       wi = effect_names_wi[effectnames],
                       bw = effect_names_bw[effectnames]))
      
      
      # simulate data
      for (g in 1:G) {
        for (i in startend_frame$start[g]:startend_frame$end[g]) {
          run_data$y[i] = int_G[g] +  0.5 * run_data$y[i-1] +
            sim_coefs[coefs] * run_data$wi[i] +
            sim_coefs[coefs] * run_data$bw[i] + 
            rnorm(1, 0, 1)
          
          run_data$y_lag[i] = run_data$y[i-1]
        }  
      }
      
      # calculate model
      m1 = glmmTMB(y ~ y_lag + wi + bw +
                     (1|country_text_id),
                   run_data)
      
      #save dat in array
      mycoef[run, ,coefs, effectnames] = rmvn(1, mu = fixef(m1)$cond, V = vcov(m1)$cond)[3:4]
      
    }
  }
}
  

#save(mycoef, file="Analyse/Performance/SpecificP/SimulatedData/mycoef.Rdata")
load(file="Analyse/Performance/SpecificP/SimulatedData/mycoef.Rdata")

get_hdi_plot = function(effectvariable) {
  plot_data = data.frame()
  for (coefs in 1:length(sim_coefs)) {
    
    
    # wi HDI
    hdi_wi = bayestestR::hdi(mycoef[,1,coefs,effectvariable], ci = 0.95, verbose=F)[2:3] %>% 
      unlist() %>% 
      matrix(ncol=1) %>% 
      as.data.frame() %>% 
      rename(FKM_wi = V1)
    # bw HDI
    hdi_bw = bayestestR::hdi(mycoef[,2,coefs,effectvariable], ci = 0.95, verbose=F)[2:3] %>% 
      unlist() %>% 
      matrix(ncol=1) %>% 
      as.data.frame() %>% 
      rename(FKM_bw = V1)
    # mean
    hdi_mean = matrix(colMeans(mycoef[,,coefs,effectvariable]), nrow=1) %>% 
      as.data.frame() %>% 
      rename(FKM_wi = V1, FKM_bw = V2)
    
    hdi_data = hdi_wi %>% 
      bind_cols(hdi_bw) %>% 
      bind_rows(hdi_mean) %>% 
      mutate(hdi = c("lower", "upper", "mean"),
             coef = sim_coefs[coefs]) %>% 
      pivot_longer(cols=starts_with("FKM")) %>% 
      pivot_wider(names_from = hdi, values_from = value) 
    
    
    plot_data = bind_rows(plot_data, hdi_data)
  }
  plot_data = plot_data %>% 
    mutate(effect = effect_names_wi[effectvariable])
  plot_data$effect = gsub("\\..*", "", plot_data$effect)

  
  myplot = plot_data %>% 
    ggplot(aes(x=name, ymin=lower, y=mean, ymax=upper)) + 
    geom_point() +
    geom_errorbar() +
    geom_hline(yintercept = 0, linetype="longdash", size=1.1) +
    coord_flip() +
    facet_wrap(coef ~ .)  +
    theme_bw() +
    scale_x_discrete(labels=c("bw", "wi"), name="") +
    ylab("") +
    ggtitle(plot_data$effect[1])
  
  return(myplot)
}

p1 = get_hdi_plot(1)
p2 = get_hdi_plot(2)
p3 = get_hdi_plot(3)
p4 = get_hdi_plot(4)
p5 = get_hdi_plot(5)

ggarrange(p1,p2,p3,p4,p5, nrow=3, ncol=2)



