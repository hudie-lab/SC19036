## ---- TRUE--------------------------------------------------------------------
##randomly generate 100 numbers from the normal distribution(with mean=0,sd=1)
rs=rnorm(100,mean=0,sd=1)
##plot the random numbers
plot(rs)

## ----include=TRUE-------------------------------------------------------------
##generate 100 random numbers from the discrete distribution with prob=c(0.2,0.3,0.5)
x=sample(1:3,100,replace=TRUE,prob=c(0.2,0.3,0.5))
##table the 100 numbers
xt=table(x)
xt

