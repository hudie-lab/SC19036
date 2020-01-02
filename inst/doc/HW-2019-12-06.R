## ---- include=TRUE------------------------------------------------------------
x <- seq(1:10)
y1 <- exp(log(x))
y2 <- log(exp(x))
identical(y1,y2)
all.equal(y1,y2)

## ----include=TRUE-------------------------------------------------------------
k <- c(4:25,100, 500, 1000)
sk <- function(k,a) 1 - pt(sqrt(a^2*k/(k+1-a^2)), df = k)
intersec_sk <- function(k,a) {
  sk(k,a) - sk(k-1,a)
}
solution <- numeric(length(k))
for( i in 1:length(k)){
  m <- k[i] 
  n <- sqrt(m-1)
  solution[i] <- uniroot(intersec_sk, k = m, c(0.00001,n + 0.00001))$root
}
knitr::kable(cbind(k,solution))

## ----include=TRUE-------------------------------------------------------------
ck <- function(k,a) sqrt(a^2*k/(k+1-a^2))
f1 <- function(k,a) {
  cka <- ck(k-1,a)
  co <- exp(lgamma(k/2)-lgamma((k-1)/2))/sqrt(k-1)
  f <- function(k,u) (1+(u^2)/(k-1))^(-k/2)
  int <- integrate(f, lower = 0, upper = cka ,k=k)$value
  return(co*int)
}
f2 <- function(k,a){
  cka <- ck(k,a)
  co <- exp(lgamma((k+1)/2)-lgamma(k/2))/sqrt(k)
  f <- function(k,u) (1+(u^2)/(k))^(-k/2-0.5)
  int <- integrate(f, lower = 0, upper = cka ,k=k)$value
  return(co*int)
}

solv <- function(k,a){
  f1(k,a) - f2(k,a)
}
k <- c(4:25,100, 500, 1000)
solution1 <- numeric(length(k))
for( i in 1:length(k)){
  m <- k[i] 
  n <- sqrt(m-1)
  upp <- n
  tol <- 10^(-5)
  if(abs(solv(m,upp)) <= tol) solution1[i] <- upp
  else solution1[i] <- uniroot(solv, k = m,c(0.00001,upp), tol = 10^(-5) )$root
}
knitr::kable(cbind(k,solution,solution1))

## ----include=TRUE-------------------------------------------------------------
na <- 28
nb <- 24
no <- 41
nab <- 70
L <- c(0.3,0.3)
N <- 10000
tol <- .Machine$double.eps^0.5
L.old <- L 

logml <- numeric()

for(j in 1:N){
  naa <- na*L.old[1]/(2-L.old[1]-2*L.old[2])
  nbb <- nb*L.old[2]/(2-L.old[2]-2*L.old[1])
  a <- naa + na + nab
  c <- nbb + nb + nab
  b <- na + nb + no - naa - nbb
  L <- c(a/(a+b+c), c/(a+b+c))
  logml[j] <- a*log(L[1])+c*log(L[2])+b*log(1-L[1]-L[2])#+log(2)*(nab+na+nb-naa-nbb)
  
  if (sum((L - L.old)^2) < tol) break
  L.old <- L
}

result <- list()
result$interate <- j ##the times for interation
result$p <- L[1]  ##the estimate of p
result$q <- L[2]  ##the estimate of q
result$logml <- logml ## the maximum of the log-likelihood of each interation
result
plot(logml,type="l",main = "the log-maximum likelihood values")

