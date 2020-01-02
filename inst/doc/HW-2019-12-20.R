## ---- include=TRUE------------------------------------------------------------
dl <- function (x) 0.5*exp(-abs(x))
rw.Metropolis <- function( sigma, x0, N) { 
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i]*dl(x[i-1]) <= dl(y)) {x[i]<-y} 
    else{
    x[i] <- x[i-1]
    k <- k + 1 }
  }
  return(list(x=x, k=k)) 
}


## ----eval=TRUE----------------------------------------------------------------
library(Rcpp)
cppFunction('NumericVector laprn (double sigma,double x0,int N) {
    std::unordered_set<int> seen;
    NumericVector x(N);
    NumericVector y(N);
    x[0] = x0;
    NumericVector u = runif(N);
    int k = 0;
    for (int i =1; i < N;i++) {
        NumericVector y = rnorm(1, x[i-1], sigma);
        if (u[i-1] <= (0.5*exp(-abs(y[0]))) / (0.5*exp(-abs(x[i-1])))){
            x[i] = y[0];
        }
        else{
          x[i] = x[i-1];
          k = k + 1 ;
          }
    }
    return(x);
}')

## -----------------------------------------------------------------------------
    # Can create source file in Rstudio
    library(microbenchmark)
    x0 <- 25
    N <- 2000
    sigma <- 0.05
    (ts <- microbenchmark(rnR=rw.Metropolis(sigma,x0,N),
                         rnC=laprn(sigma,x0,N)))

## ----include=TRUE-------------------------------------------------------------
rnR=rw.Metropolis(sigma,x0,N)[[1]]
rnC=laprn(sigma,x0,N)
qqplot(rnR,rnC)

