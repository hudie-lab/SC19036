## ----include=FALSE------------------------------------------------------------
dl <- function (x) 0.5*exp(-abs(x))
rw.Metropolis <- function( sigma, x0, N) { 
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dl(y) / dl(x[i-1]))) x[i]<-y 
    else{
    x[i] <- x[i-1]
    k <- k + 1 }
  }
  return(list(x=x, k=k)) 
}

## ----include=TRUE-------------------------------------------------------------
library(SC19036)
N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis( sigma[1], x0, N) 
rw2 <- rw.Metropolis( sigma[2], x0, N) 
rw3 <- rw.Metropolis( sigma[3], x0, N) 
rw4 <- rw.Metropolis( sigma[4], x0, N)

#refline <- qt(c(.025, .975), df=n)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
for (j in 1:4) {
plot(rw[,j], type="l",
xlab=bquote(sigma == .(round(sigma[j],3))), ylab="X", ylim=range(rw[,j]))
#abline(h=refline)
  }

#the acceptance rate of each chain
print(c(N-c(rw1$k, rw2$k, rw3$k, rw4$k))/N)


