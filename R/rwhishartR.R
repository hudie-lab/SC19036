
#' @title sampler for Wishart distribution
#' @name rwishart
#' @description R function to implement a sampler for Wishart distribution.
#' @param sigma the mean matrix of Wishart distribution
#' @param n the scale parameter of Wishart distribution
#' @return a random sample from Wishart distribution
#' @examples
#' \dontrun{
#' sigma=matrix(c(5,1,1,3),2,2)
#' rwishart(5,sigma)
#' }
#' @export
rwishart <- function(n,sigma){
  ##d is determined by the dimension of sigma
  d <- nrow(sigma)
  M <- list()
  ##the Bartlett's decomposition
  T <- matrix(rep(0,d*d),d,d) 
  for(i in 1:d){
    for(j in 1:i){
      if(i>j) T[i,j] <- rnorm(1)
      if(i==j) T[i,j] <- sqrt(rchisq(1,df=(n-i+1)))
    }
  }
  ##the choleski factorization
  L <- t(chol(sigma))
  Mt <- (L%*%T)%*%t(L%*%T)
  
  ##the random matrix on the wishart distribution
  Mt
}