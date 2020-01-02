## ----include=TRUE-------------------------------------------------------------

n1 <- 20
n2 <- 50
mu1 <- mu2 <- 0 
m <- 1000

count5test <- function(z) {
  n <- length(z)
x <- z[1:(n/2)]
y <- z[(n/2+1):n]
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y)) 
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0) 
return(as.integer(max(c(outx, outy)) > 5))
}

permu <- function(z , R=9999) {
  n <- length(z)
  out <- numeric(R)
  for (r in 1: R){
      p <- sample(1:n ,n ,replace = FALSE)
      out[r] <- count5test(z[p])
  }
  sum(out) / R ### the p-value of the permutation
}              ###  test fot the count five test

### under H0 , equal variance
sigma1 <- sigma2 <- 1 
alphahat <- replicate(m, expr={
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
x <- x - mean(x) #centered by sample mean 
y <- y - mean(y)
z <- c(x,y)
permu(z,R=999) 
})

mean(alphahat<0.05)  ## the type-I-error of the null hypothesis

### under H1 , not equal variance
sigma1 <- 1
sigma2 <- 5
alphahat2 <- replicate(m, expr={
x <- rnorm(n1, mu1, sigma1)
y <- rnorm(n2, mu2, sigma2)
x <- x - mean(x) #centered by sample mean 
y <- y - mean(y)
z <- c(x,y)
permu(z,R=999) 
})

1-mean(alphahat2<0.05)  ### the power for the alternative hypothesis

## ----eval=FALSE---------------------------------------------------------------
#  library(MASS)
#  library(Ball)
#  library(boot)
#  
#  dCov <- function(x, y) {
#    x <- as.matrix(x);  y <- as.matrix(y)
#    n <- nrow(x); m <- nrow(y)
#    if (n != m || n < 2) stop("Sample sizes must agree")
#    if (! (all(is.finite(c(x, y)))))
#      stop("Data contains missing or infinite values")
#    Akl <- function(x) {
#      d <- as.matrix(dist(x))
#      m <- rowMeans(d); M <- mean(d)
#      a <- sweep(d, 1, m); b <- sweep(a, 2, m)
#      b + M
#    }
#    A <- Akl(x);  B <- Akl(y)
#    sqrt(mean(A * B))
#  }
#  
#  ndCov2 <- function(z, ix, dims) {
#    #dims contains dimensions of x and y
#    p <- dims[1]
#    q <- dims[2]
#    d <- p + q
#    x <- z[ , 1:p] #leave x as is
#    y <- z[ix, -(1:p)] #permute rows of y
#    return(nrow(z) * dCov(x, y)^2)
#  }
#  set.seed(12345)
#  n <- c( 10, 20 ,50, 100, 150,200 )
#  m <- 20   ###the number of simulation times
#  p.cor <- matrix(0,nrow=length(n),ncol=2)
#  p.ball <- matrix(0,nrow=length(n),ncol=2)
#  
#  for (s in 1:length(n)){
#    sim_p.cor <- matrix(0, nrow=m, ncol=2)
#    sim_p.ball <- matrix(0, nrow=m, ncol=2)
#  
#    for (l in 1:m){
#      set.seed(12345*(l+10*s))
#  
#      x <- mvrnorm(n[s],mu=rep(0,2),Sigma=diag(rep(1,2)))
#    e <- mvrnorm(n[s],mu=rep(0,2),Sigma=diag(rep(1,2)))
#    y1 <- x/4 + e
#    y2 <- x/4*e
#    z <- cbind (y1, x , y2)
#  boot.obj1 <- boot(data = cbind(x,y1), statistic = ndCov2, R = 999,
#                   sim = "permutation", dims = c(2, 2))
#  # permutatin: resampling without replacement
#  tb1 <- c(boot.obj1$t0, boot.obj1$t)
#  sim_p.cor[l,1] <- mean(tb1>=tb1[1])
#  sim_p.ball[l,1] <- bcov.test(x,y1,num.permutations=999,seed=l)$p.value
#  
#  boot.obj2 <- boot(data = cbind(x,y2), statistic = ndCov2, R = 999,
#                   sim = "permutation", dims = c(2, 2))
#  # permutatin: resampling without replacement
#  tb2 <- c(boot.obj2$t0, boot.obj2$t)
#  sim_p.cor[l,2] <- mean(tb2>=tb2[1])
#  sim_p.ball[l,2] <- bcov.test(x,y2,num.permutations =999,seed=l)$p.value
#    }
#    p.cor[s,] <- colMeans(sim_p.cor < 0.05)
#    p.ball[s,] <- colMeans(sim_p.ball < 0.05 )
#  }
#  plot(x=n,y=p.cor[,1],type="l",lty=1,main="Y = X/4 + e",ylab="power")
#  legend(x=120,y=0.4,legend = c("distance covariance","ball test"),lty=c(1,2))
#  lines(x = n, y=p.ball[,1] ,lty=2)
#  
#  plot(x=n,y=p.cor[,2],type="l",lty=1,main="Y = X/4 * e",ylab="power")
#  legend(x=120,y=0.4,legend = c("distance covariance","ball test"),lty=c(1,2))
#  lines(x = n, y=p.ball[,2] ,lty=2)

