## ----include=TRUE-------------------------------------------------------------

##sample generator
raylsamp=function(N,sigma){
    s2=sigma^2
    k=1
  x=0
  while(k<=N){
    ##N : the number of samples
  u=runif(1)
  ##y : the random variable from g(x)
  y=rgamma(1,shape=2,scale=1)
  ##accept-reject step
  if(u<=exp(-y^2/(2*s2)+y-s2/2)) {
    k=k+1
    x[k]=y
  }
  }
  ##print the samples
  x[-1]
  }

##the samples from the acceptance-rejection method above
N=50000
##set the parameter sigma
sigma=2

AR_x=raylsamp(N,sigma)
hist(AR_x,freq = FALSE,main="the acceptance-rejection samples from rayleigh distribution")
y=seq(0,max(AR_x),0.1)
lines(cbind(y,y/sigma^2*exp(-y^2/2/sigma^2)),col="red")
abline(v=sigma,col="blue")
text(2.25,0.25,"theoretical sigma",col="blue")
text(7,0.2,"red lines:theoretical f(x)",col="red")


## ----include=TRUE-------------------------------------------------------------
##the samples from the acceptance-rejection method above
N=1000
##set the parameter sigma
sigma=2:5

#par(mfrow=c(2,2))
for(i in 1:length(sigma)){
  s=sigma[i]
  AR_x=raylsamp(N,s)
hist(AR_x,breaks=50,freq = FALSE,main=paste0("the acceptance-rejection samples from rayleigh distribution with sigma=",s))
y=seq(0,max(AR_x),0.1)
lines(cbind(y,y/s^2*exp(-y^2/2/s^2)),col="red")
abline(v=s,col="blue")
text(s,0.1,"theoretical sigma",col="blue")
text(max(AR_x)-5,0.2,"red lines:theoretical f(x)",col="red")
}


## ----include=TRUE-------------------------------------------------------------
N=1000
####random numbers from the first normal distribution 
x1=rnorm(N)
####random numbers from the second normal distribution 
x2=rnorm(N,mean=3)

p=0.75
####the random variable which stands for the mixing probability
r=sample(c(0,1),N,replace=TRUE,prob=c(p,1-p))
####the mixture random samples
x=(1-r)*x1+r*x2

hist(x,breaks=50,freq = FALSE,ylim=c(0,0.5),main=paste0("mixture of normal distribution with p=","p"))
####generate the density imposed
y=seq(min(x1,x2),max(x1,x2),0.1)
lines(cbind(y,p*dnorm(y)+(1-p)*dnorm(y,mean=3)),col="red")
text(4,0.4,"red lines for the density",col="red")



## ----include=TRUE-------------------------------------------------------------
N=1000
x1=rnorm(N)
x2=rnorm(N,mean=3)

#par(mfrow=c(4,3))
####set the different probabilities of p
pi=seq(0.2,0.8,0.05)
for(i in 1:(length(pi)-1)){
  p=pi[i]
  r=sample(c(0,1),N,replace=TRUE,prob=c(p,1-p))
x=(1-r)*x1+r*x2

hist(x,breaks=50,freq = FALSE,ylim=c(0,0.5),main=paste0("mixture of normal distribution with p=",p))
y=seq(min(x1,x2),max(x1,x2),0.1)
lines(cbind(y,p*dnorm(y)+(1-p)*dnorm(y,mean=3)),col="red")
text(max(x)-2,0.4,"red lines for the density",col="red")
}


## ----eval=FALSE---------------------------------------------------------------
#  
#  ####function for Wishart distribution
#  rwishart=function(n,sigma){
#    ##d is determined by the dimension of sigma
#    d=nrow(sigma)
#    M=list()
#      ##the Bartlett's decomposition
#      T=matrix(rep(0,d*d),d,d)
#    for(i in 1:d){
#      for(j in 1:i){
#          if(i>j) T[i,j]=rnorm(1)
#          if(i==j) T[i,j]=sqrt(rchisq(1,df=(n-i+1)))
#      }
#    }
#      ##the choleski factorization
#    L=t(chol(sigma))
#    Mt=(L%*%T)%*%t(L%*%T)
#  
#    ##the random matrix on the wishart distribution
#    Mt
#  }
#  

## ----include=TRUE-------------------------------------------------------------
library(SC19036)
n=5
d=2
####the sigma needs to be symetric and positive limited.
sigma=matrix(c(5,1,1,3),d,d)
(M=rwishart(n,sigma))

## ----include=TRUE-------------------------------------------------------------
##the density function of wishart distribution

dwishart=function(A,n,digma){
  d=nrow(sigma)
  det(A)^((n-d-1)/2)*exp(-0.5*sum(diag(solve(sigma)%*%A)))/2^(d*n/2)/gamma(n/2)/det(sigma)^(d/2)
}

n=5
N=500

##the random generator in stats Rpackage
Ms=rWishart(N,df=nrow(sigma),sigma)
dw=matrix(0,nrow=N,ncol=2)
for(i in 1:N){
  dw[i,1]=dwishart(rwishart(n,sigma),n,sigma)
  dw[i,2]=dwishart(as.matrix(Ms[1:2,1:2,i]),n,sigma)
}

##the random generator in stats Rpackage
Ms=rWishart(n,df=nrow(sigma),sigma)


## ----include=TRUE-------------------------------------------------------------
library(rgl)
ar=matrix(0,ncol=6,nrow=N)
for(i in 1:N){
  m=rwishart(n,sigma)
  mt=rWishart(1,nrow(sigma),sigma)
  ar[i,1:3]=unique(c(m))
  ar[i,4:6]=unique(c((mt[1:2,1:2,1])))
} 
#plot3d(cbind(ar[,1:3],ar[,4:6]),col=c("blue","red"))


