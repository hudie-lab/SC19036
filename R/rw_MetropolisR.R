#' @title Random walk Metropolis sampler for generating the standard Laplace distribution
#' @name rw.Metropolis
#' @description R function to implement a random walk Metropolis sampler for generating the standard Laplace distribution.
#' @param sigma the standard deviance of state \code{X_t} of the random walk
#' @param x0 the initial state of the random walk
#' @param N the number of samples
#' @return random samples of size \code{N}
#' @examples
#' \dontrun{
#' rw.Metropolis(4,1,100)
#' }
#' @export
rw.Metropolis <- function( sigma, x0, N) { 
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i]*0.5*exp(-abs(x[i-1])) <= 0.5*exp(-abs(dl(y)))) {x[i]<-y} 
    else{
      x[i] <- x[i-1]
      k <- k + 1 }
  }
  return(list(x=x, k=k)) 
}