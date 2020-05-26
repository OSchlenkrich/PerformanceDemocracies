
BeckTest = plm(inv ~ value, Grunfeld %>% filter(firm==1 | firm==2 | firm==3, year<1937 ), index=c("firm", "year"),
               model="pooling")

BeckTest = plm(y ~ time, index = c("G", "time_index"), y_df, model="random")

Epsilon = data.frame(index(BeckTest), BeckTest$residuals) %>% 
  spread("time_index", "BeckTest.residuals") %>% 
  select(-G) %>% 
  as.matrix()


mylist = list()
for (tt in 1:15) {
  E_hat = array(NA, dim=c(5,5))
  for (i in 1:5) {
    for (j in 1:5) {
      summe = 0 
      for (t in 1:15) {
        summe = summe + (Epsilon[i,t]  * Epsilon[j,t])
      }
      E_hat[i,j]= summe/15
    }
  }
  mylist[[tt]] =  E_hat
}

omega = kronecker(E_hat, diag(1, 2)) 
omega

# balanced
# balanced_ehat = (t(as.matrix(Epsilon)) %*% as.matrix(Epsilon))/20
# rhd =  diag(1, 20)
# balanced_sigma = kronecker(balanced_ehat, rhd)

epart = solve(t(model.matrix(BeckTest)) %*% model.matrix(BeckTest))

PCSE = epart %*% (t(model.matrix(BeckTest)) %*% omega %*% model.matrix(BeckTest)) %*% epart
PCSE
sqrt(diag(PCSE))

