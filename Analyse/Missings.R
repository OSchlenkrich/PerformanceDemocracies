test = data.frame(x = rbinom(1000,1, 0.5))
table(test)

library(dplyr)

test %>%
  mutate(x_SGI =  SGI_fun(x))
iqrange = IQR(test$x, na.rm=T)


pMiss <- function(x){sum(is.na(x))/length(x)*100}
pMiss_Abs <- function(x){sum(is.na(x))}

apply(Economy_IQR_NA  %>% filter(year >= 1990) %>%  select_at(vars(ends_with("sgi"))),2,pMiss)
apply(Economy_IQR_NA  %>% filter(year < 1990) %>%  select_at(vars(ends_with("oecd_sgi"))),2,pMiss)
apply(Economy_IQR_NA  %>% filter(year >= 1990) %>%  select_at(vars(ends_with("wdi"))),2,pMiss)


Economy_IQR_NA %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("wdi"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw()  +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - WDI")

Economy_IQR_NA %>% 
  group_by(year) %>% 
  select_at(vars(ends_with("oecd"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="year") %>% 
  ggplot(aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10))  +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") +
  ggtitle("Missings in Democracy Profile Sample - OECD")


Economy_IQR_NA %>% 
  filter(year>=1990) %>% 
  group_by(cluster_label_1st) %>% 
  select_at(vars(ends_with("oecd_sgi"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="cluster_label_1st") %>% 
  ggplot(aes(x=cluster_label_1st, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10))  +
  theme_bw() +
  ggtitle("Missings")

Economy_IQR_NA %>% 
  #filter(year>=1990) %>% 
  group_by(cluster_label_1st) %>% 
  select_at(vars(ends_with("wdi_sgi"))) %>% 
  summarise_all(pMiss) %>% 
  melt(id.vars="cluster_label_1st") %>% 
  ggplot(aes(x=cluster_label_1st, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  scale_y_continuous(breaks=seq(0,100, 10))  +
  theme_bw() +
  ggtitle("Missings")


Economy_IQR_NA %>% 
  #filter(year>=1990) %>% 
  group_by(cluster_label_1st) %>% 
  select_at(vars(ends_with("oecd_sgi"))) %>% 
  summarise_all(list(pMiss_Abs = pMiss_Abs, all= length)) %>% 
  melt(id.vars="cluster_label_1st") %>% 
  ggplot(aes(x=cluster_label_1st, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  facet_wrap(variable~.) +
  theme_bw() +
  ggtitle("Missings")
