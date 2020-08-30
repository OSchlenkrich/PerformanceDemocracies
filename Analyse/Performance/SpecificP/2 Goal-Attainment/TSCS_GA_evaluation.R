# Evaluation GA ####
source("Analyse/Performance/SpecificP/LoadTSCSData.R")
load("Analyse/Performance/SpecificP/Datasets/tscs_data_GA_Lutz_trans_v2.Rdata")
source("Analyse/Performance/SpecificP/2 Goal-Attainment/table_GA.R")
source("Analyse/Performance/SpecificP/WorkFlow_v3.R")

# Load All Models

Lutz_m1_normal = loadBRMS("Lutz_m1_normal", "GoalAttainment")
Lutz_m1_student = loadBRMS("Lutz_m1_student", "GoalAttainment")
model_list_lutz_1 = loadBRMS("model_list_lutz_1", "GoalAttainment")

Lutz_m2_cab = loadBRMS("Lutz_m2_cab", "GoalAttainment")
model_list_lutz_2 = loadBRMS("model_list_lutz_2", "GoalAttainment")




ccp_m1_normal = loadBRMS("ccp_m1_normal", "GoalAttainment")
ccp_m1_student = loadBRMS("ccp_m1_student", "GoalAttainment")
model_list_ccp_1 = loadBRMS("model_list_ccp_1", "GoalAttainment")

ccp_m2_cab = loadBRMS("ccp_m2_cab", "GoalAttainment")
model_list_ccp_2 = loadBRMS("model_list_ccp_2", "GoalAttainment")

# Lutz ####
# Tables ####
generateTableBrms_GA(Lutz_m1_normal, 
                     model_list_lutz_1[[1]], 
                     model_list_lutz_1[[2]], 
                     model_list_lutz_1[[3]], 
                     model_list_lutz_1[[4]], 
                     model_list_lutz_1[[5]], 
                     model_list_lutz_1[[6]], 
                     model_list_lutz_1[[7]], prob_interval = 0.95)
generateTableBrms_GA(Lutz_m1_normal, 
                     model_list_lutz_1[[1]], 
                     model_list_lutz_1[[2]], 
                     model_list_lutz_1[[3]], 
                     model_list_lutz_1[[4]], 
                     model_list_lutz_1[[5]], 
                     model_list_lutz_1[[6]], 
                     model_list_lutz_1[[7]], prob_interval = 0.9)

generateTableBrms_GA(Lutz_m2_cab, 
                     model_list_lutz_2[[1]], 
                     model_list_lutz_2[[2]], 
                     model_list_lutz_2[[3]], 
                     model_list_lutz_2[[4]], 
                     model_list_lutz_2[[5]], 
                     model_list_lutz_2[[6]], 
                     model_list_lutz_2[[7]], prob_interval = 0.95)
generateTableBrms_GA(Lutz_m2_cab, 
                     model_list_lutz_2[[1]], 
                     model_list_lutz_2[[2]], 
                     model_list_lutz_2[[3]], 
                     model_list_lutz_2[[4]], 
                     model_list_lutz_2[[5]], 
                     model_list_lutz_2[[6]], 
                     model_list_lutz_2[[7]], prob_interval = 0.9)

p1 = fitted_res_plot(model_list_lutz_2[[1]])
fitted_res_plot(model_list_lutz_1[[8]])


# Predictions ####
p0 = conditional_effects(model_list_lutz_1[[1]], effects = "FKM5_Fec", plot=F) 
p0$FKM5_Fec %>% 
  ggplot(aes(x=FKM5_Fec, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_lutz")


p1 = conditional_effects(model_list_lutz_1[[4]], effects = "FKM5_fEc", plot=F) 
p1$FKM5_fEc %>% 
  ggplot(aes(x=FKM5_fEc, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_lutz")

p2 = conditional_effects(model_list_lutz_1[[5]], effects = "FKM5_FEC", plot=F) 
p2$FKM5_FEC %>% 
  ggplot(aes(x=FKM5_FEC, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_lutz")

p3 = conditional_effects(model_list_lutz_1[[7]], effects = "FKM4_c", plot=F) 
p3$FKM4_c %>% 
  ggplot(aes(x=FKM4_c, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_lutz")

p4 = conditional_effects(model_list_lutz_1[[6]], effects = "FKM4_E", plot=F) 
p4$FKM4_E %>% 
  ggplot(aes(x=FKM4_E, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_lutz")

p5 = conditional_effects(model_list_lutz_1[[10]], effects = "centrip_odempr", plot=F) 
p5$centrip_odempr %>% 
  ggplot(aes(x=centrip_odempr, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_lutz")


# CCP ####
#Tables ####

generateTableBrms_GA(ccp_m1_normal, 
                     model_list_ccp_1[[1]], 
                     model_list_ccp_1[[2]], 
                     model_list_ccp_1[[3]], 
                     model_list_ccp_1[[4]], 
                     model_list_ccp_1[[5]], 
                     model_list_ccp_1[[6]], 
                     model_list_ccp_1[[7]], prob_interval = 0.95)

generateTableBrms_GA(ccp_m1_normal, 
                     model_list_ccp_1[[1]], 
                     model_list_ccp_1[[2]], 
                     model_list_ccp_1[[3]], 
                     model_list_ccp_1[[4]], 
                     model_list_ccp_1[[5]], 
                     model_list_ccp_1[[6]], 
                     model_list_ccp_1[[7]], prob_interval = 0.9)

generateTableBrms_GA(ccp_m2_cab, 
                     model_list_ccp_2[[1]], 
                     model_list_ccp_2[[2]], 
                     model_list_ccp_2[[3]], 
                     model_list_ccp_2[[4]], 
                     model_list_ccp_2[[5]], 
                     model_list_ccp_2[[6]], 
                     model_list_ccp_2[[7]], prob_interval = 0.95)

generateTableBrms_GA(ccp_m2_cab, 
                     model_list_ccp_2[[1]], 
                     model_list_ccp_2[[2]], 
                     model_list_ccp_2[[3]], 
                     model_list_ccp_2[[4]], 
                     model_list_ccp_2[[5]], 
                     model_list_ccp_2[[6]], 
                     model_list_ccp_2[[7]], prob_interval = 0.9)


p2 = fitted_res_plot(model_list_ccp_1[[1]])
fitted_res_plot(model_list_ccp_2[[2]])

ggarrange(p1, p2)

# Predictions ####
p0 = conditional_effects(model_list_ccp_2[[1]], effects = "FKM5_Fec", plot=F) 
p0$FKM5_Fec %>% 
  ggplot(aes(x=FKM5_Fec, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_lutz")

p1 = conditional_effects(model_list_ccp_2[[2]], effects = "FKM5_FeC", plot=F) 
p1$FKM5_FeC %>% 
  ggplot(aes(x=FKM5_FeC, y=estimate__, ymin=lower__, ymax = upper__)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  theme_bw() +
  ylab("arate_ccp")



hist(model_list_ccp_1[[1]]$data$arate_ccp_ga)


model_list_ccplutz_1
model_list_ccplutz_2