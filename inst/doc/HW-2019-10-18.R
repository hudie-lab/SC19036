## ----include=TRUE-------------------------------------------------------------
set.seed(19036)
n <- 20
m=10000
alpha <- .05
UCL <- replicate(1000, expr = {
x <- rnorm(n, mean = 0, sd = 2)
(n-1) * var(x) / qchisq(alpha, df = n-1) })

y=rchisq(n,df=2)
ybar=mean(y)/qt(df=n-1,1-alpha)
esti_int=replicate(10000,expr={
  y=rchisq(n,df=2)
  (mean(y)-sd(y)*qt(df=n-1,alpha))
  #(mean(y)-2)/sd(y)
})
##the empirical estimate of intervals for variance in example6.4
(mean_x=mean(UCL>4))
(sd_x=sd(UCL>4))
##the empirical estimate of intervals for mean 
(mean_t=mean(esti_int>2))
(sd_t=sd(esti_int>2))


## ----include=TRUE-------------------------------------------------------------
n=300
p=c(0.025,0.05,0.95,0.975)
##true quantile of the skewness distribution
cp=qnorm(p,sd=sqrt(6/n))

m=10000
sb1=replicate(m,expr = {
  x=rnorm(n)
  1/n*sum((x-mean(x))^3)/(((n-1)/n*var(x))^(3/2))
})
esti_b1=rep(0,length(cp))
for(l in 1:length(cp)){
  esti_b1[l]=quantile(sb1,p[l])
}
##estimate of the quantile of the skewness distribution
esti_b1

knitr::kable(rbind(cp,esti_b1),col.names = p)


## ----include=TRUE-------------------------------------------------------------
sd1=rep(0,length(cp))
for(l in 1:length(cp)){
  sd1[l]=p[l]*(1-p[l])/n/(dnorm(cp[l],sd=sqrt(6/n)))^2
}
sd1

