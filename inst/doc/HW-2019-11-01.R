## ---- include=TRUE------------------------------------------------------------
m=10000
n=100
sk <- function(x) {
#computes the sample skewness coeff. 
xbar <- mean(x)
m3 <- mean((x - xbar)^3)
m2 <- mean((x - xbar)^2)
return( m3 / m2^1.5 )
}

##1-confidence 
alpha=0.1
##beta parameters
shape=seq(1,20,1)
#critical value for the skewness test
cv <- qnorm(1-alpha, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))

N=length(shape)
pwr <- numeric(N)
for (j in 1:N) { #for each alpha1 : 
alpha1 <- shape[j]
sktests <- numeric(m)
for (i in 1:m) { #for each replicate
x <- rbeta(n, shape1=alpha1,shape2=alpha1)
sktests[i] <- as.integer(sk(x) >= cv) }
pwr[j] <- mean(sktests)
}
#plot power vs shape
plot(shape, pwr, type = "b", xlab = bquote(shape), ylim = c(0,1)) 
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors 
lines(shape, pwr+se, lty = 3)
lines(shape, pwr-se, lty = 3)

###vs t distribution 
#critical value for the skewness test
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
dfv=seq(5,80,3)
N=length(dfv)
pwr <- numeric(N)
for (j in 1:N) { #for each alpha1 : 
v <- dfv[j]
sktests <- numeric(m)
for (i in 1:m) { #for each replicate
x1 <- rt(n, df=v)
sktests[i] <- as.integer(abs(sk(x1)) >= cv) }
pwr[j] <- mean(sktests)
}
#plot power vs shape
plot(dfv, pwr, type = "b", xlab = bquote(dfv), ylim = c(0,1)) 
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors 
lines(dfv, pwr+se, lty = 3)
lines(dfv, pwr-se, lty = 3)


## ----include=TRUE-------------------------------------------------------------
n=30
m <- 10000
alpha=0.05
p <- matrix(0,ncol=3,nrow=m) 
for (j in 1:m) {
#number of replicates #storage for p-values
x1 <- rchisq(n, df=1)
ttest <- t.test(x1, alternative = "greater", mu = 1) 
p[j,1] <- ttest$p.value
x2<- runif(n,min=0,max=2)
ttest <- t.test(x2, alternative = "greater", mu = 1) 
p[j,2] <- ttest$p.value
x3 <- rexp(n)
ttest <- t.test(x3, alternative = "greater", mu = 1) 
p[j,3] <- ttest$p.value
}
p1.hat <- mean(p[,1]< alpha)##chisq with df=1
se1.hat <- sqrt(p1.hat * (1 - p1.hat) / m) 
print(c(p1.hat, se1.hat))
p2.hat <- mean(p[,2]< alpha)##unifrom(0,2)
se2.hat <- sqrt(p2.hat * (1 - p2.hat) / m) 
print(c(p2.hat, se2.hat))
p3.hat <- mean(p[,3]< alpha)##exponential(1)
se3.hat <- sqrt(p3.hat * (1 - p3.hat) / m) 
print(c(p3.hat, se3.hat))

## ----include=TRUE-------------------------------------------------------------
1-pnorm(abs((0.651-0.676))/(sqrt(0.651*(1-0.651)+0.676*(1-0.676))))##z-test


## ----include=TRUE-------------------------------------------------------------
prop.test(c(651,676),c(1000,1000))##prop.test


## ----include=TRUE-------------------------------------------------------------
x=sample(c(rep(0,349),rep(1,651)),1000,replace=FALSE)
y=sample(c(rep(0,324),rep(1,676)),1000,replace=FALSE)
xtabs(~x+y,data=cbind(x,y))

