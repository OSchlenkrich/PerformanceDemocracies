# Quality of Government

StatN <- ggproto("StatN", Stat,
                 required_aes = c("x", "y"), 
                 compute_group = function(data, scales) {
                   y <- data$y
                   y <- y[!is.na(y)]
                   n <- length(y)
                   data.frame(x = data$x[1], y = min(y), label = paste0("n=", n))
                 }
)

stat_n <- function(mapping = NULL, data = NULL, geom = "text", 
                   position = "identity", inherit.aes = TRUE, show.legend = NA, 
                   na.rm = FALSE, ...) {
  ggplot2::layer(stat = StatN, mapping = mapping, data = data, geom = geom, 
                 position = position, inherit.aes = inherit.aes, show.legend = show.legend, 
                 params = list(na.rm = na.rm, ...))
}


QoC_data = fread("C:/RTest/qog_std_ts_jan19.csv", encoding = "UTF-8") %>% 
  rename(country = cname,
         country_text_id = ccodealp)


Origin = QoC_data %>% 
  select(country_text_id, year, ht_colonial, r_muller, lp_catho80, lp_protmg80, lp_legor, variable =  top_top1_income_share   ) %>% 
  mutate(ht_colonial = as.factor(ht_colonial),
         lp_legor = as.factor(lp_legor)) %>% 
  group_by(country_text_id) %>% 
  mutate(ht_colonial = as.factor(ht_colonial),
         lp_legor = as.factor(lp_legor),
         variable_lag = dplyr::lag(variable))


dmx_origin = dmx_trade_cluster %>% 
  left_join(Origin, by=c("country_text_id", "year"))


dmx_origin %>% 
  group_by(cluster_label_1st) %>% 
  summarise(mean(variable , na.rm=T))

dmx_origin %>% 
  ggplot(aes(x=cluster_label_1st, y=variable)) +
  geom_boxplot() +
  geom_label(aes(label=country))


require(nnet)

my_mnom = multinom(cluster_label_1st ~  1 + r_muller + lp_catho80 + lp_legor , dmx_origin)


names(dmx_origin)
test = plm(variable ~ variable_lag + cluster_label_1st + classification_context, 
           pdata.frame(dmx_origin, index=c("country", "year")), 
           model ="random")
summary(test, vcov = vcovBK(test))


summary(my_mnom)


b <- summary(my_mnom)$coefficients
se <- summary(my_mnom)$standard.errors
p.vals <- 2*(1 - pnorm(abs(b/se)) ) # compute p-values (2-tailed test), z = b/se
p.vals

table(QoC_data$country)
Origin = QoC_data %>% 
  select(country_text_id, year, 
         life_exp =  who_halet,
         hdi = undp_hdi,
         gini = wdi_gini,
         oecd_socexpnd_t1a,
         test_variable = top_top1_income_share 
         ) %>% 
  right_join(dmx_trade_cluster, by=c("country_text_id", "year"))

Origin %>% 
  ggplot(aes(x=test_variable)) +
  geom_histogram()

Origin %>% 
  ggplot(aes(x=year, y=test_variable, grp=as.factor(year))) +
  geom_boxplot() +
  stat_n()


create_world_map(Origin, "test_variable", "2010", 
                 "Spatial Distribution of Democracy Profiles \n", mode=F)



Origin %>% 
  select(life_exp,
         hdi,
         gini,
         oecd_socexpnd_t1a,
         test_variable
  ) %>% 
  fa(1, fm="ml")

library(lavaan)

model = ' LV =~ life_exp + hdi + gini + oecd_socexpnd_t1a'
mym = cfa(model, Origin %>% 
      select(life_exp,
             hdi,
             gini,
             oecd_socexpnd_t1a
      ))
summary(mym)


test = QoC_data %>% 
  select_at(vars(
    country_text_id, 
    year, 
    starts_with("eu_heah"), 
    starts_with("ihme_lifexp"), 
    starts_with("oecd_doctor")
    )
  )
  
test %>% 
  melt(id.vars=c("country_text_id","year")) %>% 
  group_by(variable) %>% 
  mutate(value = scale(value)) %>% 
  ggplot(aes(x=year, y=value, grp=as.factor(year))) +
  geom_boxplot() +
  stat_n() +
  facet_wrap(variable~.)


test %>% 
  filter(year > 1990) %>% 
  na.omit() %>% 
  select(-country_text_id, - year) %>% 
  mutate_all(all_vars(scale(.))) %>% 
  fa(3)
  