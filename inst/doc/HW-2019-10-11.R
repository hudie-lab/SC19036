## ----include=TRUE-------------------------------------------------------------
##I set the number of samples as m
m=10000
##the x is distributed on U(0,pi/3)
x=runif(m,max=pi/3)
##the MC integrate
esti=mean(sin(x))*pi/3
##the exact value of the integrate
exac=1-cos(pi/3)

c(esti,exac)

## ----include=TRUE-------------------------------------------------------------
x=runif(m)
##without variance reduction, using MC integrate
s1=exp(-x)/(1+x^2)
m1=mean(s1)
v1=var(s1)/m
##antithetic method to reduce variance
s2=rep(0,m/2)
for(i in 1:(m/2)){
  s2[i]=(exp(-x[i])/(1+x[i]^2)+exp(-(1-x[i]))/(1+(1-x[i])^2))/2
}
m2=mean(s2)
v2=var(s2)/m
##percentage of variance reduction
v2/v1
##the estimate theta
c(m1,m2)
##the estimate variance
c(v1,v2)

## ----include=TRUE-------------------------------------------------------------
g=function(x) exp(-x)/(1+x^2)
##importance sampling
u <- runif(m) #f3, inverse transform method
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
##the estimate theta
(im1=mean(fg))
##the estimate variance
(iv1=var(fg)/m)


##stratified importance sampling
k=5
##I repeat the sampling for N times
N=50
im1=im2=iv1=iv2=rep(0,N)
for(n in 1:N){
  ##estimates from importance sampling
  u <- runif(m) #f3, inverse transform method
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
  im1[n]=mean(fg)
  iv1[n]=var(fg)/m
  estim=rep(0,k)
  estiv=rep(0,k)
  
  ##here, I calculate the intervals by dividing the whole intervals into 5 subintervals through the quantiles.
    interval=rep(0,k+1)
  for(j in 1:(k-1)){
    a=j/k
    interval[j+1]=-log(1-a*(1-exp(-1)))
  }
    interval[k+1]=1
  ##estimates from stratified importance sampling
  for(j in 1:k){
  b=interval[j+1]
  a=interval[j]
  ##importance sampling on every stratification
  u=runif(m,a,b)
  x=-log(1-u*(1-exp(-1)))
  #here, the fj should be as 1/(b-a)*f
  fg=g(x) / (1/(b-a)*exp(-x) / (1 - exp(-1)))
  estim[j]=mean(fg)
  estiv[j]=var(fg)
  }
  ##estimate of theta by stratified importance sampling
  im2[n]=sum(estim)
  ##estimate of variance
  iv2[n]=sum(estiv)/m
}
##comparison between 5.10(importance) and 5.15(stratified importance)
c(mean(im1),mean(im2))
c(mean(iv1),mean(iv2))
##the perentage of variance
mean(iv2)/mean(iv1)


