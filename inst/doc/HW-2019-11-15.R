## ----include=TRUE-------------------------------------------------------------
library(bootstrap)
data=scor
n=nrow(data)
sigma=cov(data)*(n-1)/n
lamb=eigen(sigma)$values
theta.hat=lamb[1]/sum(lamb) 
print (theta.hat)
#compute the jackknife replicates, leave-one-out estimates 
theta.jack <- numeric(n)
for (i in 1:n){
  d=data[-i,]
  m=n-1
  sigma=cov(d)*(m-1)/m
lamb=eigen(sigma)$values
theta.jack[i]=lamb[1]/sum(lamb) 
}
bias=(n - 1) * (mean(theta.jack) - theta.hat)
print(mean(theta.jack))
print(bias) #jackknife estimate of bias

se= sqrt((n-1) *mean((theta.jack - mean(theta.jack))^2))
print(se) 

## ----include=TRUE-------------------------------------------------------------
library(DAAG)
attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits
L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16) 
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)

L2 <- lm(magnetic ~ chemical + I(chemical^2)) 
plot(chemical, magnetic, main="Quadratic", pch=16) 
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2 
lines(a, yhat2, lwd=2)

L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16) 
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)

L4 <- lm(magnetic ~ chemical+I(chemical^2)+I(chemical^3)) 
yhat4 <- L4$coef[1] + L4$coef[2] * a+ L4$coef[3] * a^2+ L4$coef[4] * a^3
plot(chemical, magnetic, main="cubic polynomial", pch=16) 
lines(a, yhat4, lwd=2)

n <- length(magnetic) #in DAAG ironslag 
e1 <- e2 <- e3 <- e4 <- numeric(n)
for (k in 1:n) {
y <- magnetic[-k] 
x <- chemical[-k]
J1 <- lm(y ~ x)
yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k] 
e1[k] <- magnetic[k] - yhat1

J2 <- lm(y ~ x + I(x^2))
yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k]+
  J2$coef[3] * chemical[k]^2
e2[k] <- magnetic[k] -yhat2

J3 <- lm(log(y) ~ x) 
logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
yhat3 <- exp(logyhat3) 
e3[k] <- magnetic[k] -yhat3

J4 <- lm(y~ x+I(x^2)+I(x^3)) 
yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k]+
  J4$coef[3] * chemical[k]^2+J4$coef[4] * chemical[k]^3
e4[k] <- magnetic[k] - yhat4
}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
plot(L4$fit, L4$res) 
abline(0, 0) 
qqnorm(L4$res) 
qqline(L4$res) 

summary(L1)
summary(L2)
summary(L3)
summary(L4)


detach(ironslag)

