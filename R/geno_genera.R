#' @title the function generate genotype file 
#' @name geno_genera
#' @description the function to generate the genotype file of families containing two parents and two children.
#' @param text the fam file, it is a data.frame here.
#' @param m the number of SNPs, it is an integer.
#' @return the genotype file, the output is a data.frame.
#' @examples
#' \dontrun{
#' text <- fam_genera(50)
#' m <- 100
#' gene_match(text,m)
#' }
#' @export

geno_genera <- function (text , m){
  n <- length(unique(text[,1]))
  text[,1] <- as.numeric(as.character(text[,1]))
  text[,2] <- as.numeric(as.character(text[,2]))
  text[,3] <- as.numeric(as.character(text[,3]))
  text[,4] <- as.numeric(as.character(text[,4]))
  text[,5] <- as.numeric(as.character(text[,5]))
  outbed <- integer()
  pm <- runif(m,0.1,1)
  for(j in 1:n){
    no.fam <- unique(text[,1])[j]
    l.fam <- text[which(text[,1]==no.fam),2]
    iid <- l.fam[order(l.fam)]
    fid <- rep(no.fam,length(l.fam))
    bed <- cbind(fid, iid)
    for(i in 1:m){
      p <- pm[i]
      ## p : the MAF of one SNP
      
      AA <- (1-p)^2
      Aa <- 2*p*(1-p)
      aa <- p^2
      prob <- c(AA^2, AA*Aa,AA*aa,Aa*AA,Aa*Aa,Aa*aa,aa*aa)
      
      pairs <- 1:7
      parent <- sample (pairs, 1,prob=prob)
      if(parent==1) father <- mother <- kid1 <- kid2 <- 0
      if(parent==2) {
        father <- 0
        mother <- 1
        kid1 <- sample(0:1,1)
        kid2 <- sample(0:1,1)
      }
      if(parent==3){
        father <- 0
        mother <- 2
        kid1 <- kid2 <-1
      }
      if(parent==4){
        father <- 1
        mother <- 0
        kid1 <- sample(0:1,1)
        kid2 <- sample(0:1,1)
      }
      if(parent==5){
        father <- mother <- 1
        kid1 <- sample(0:2, 1, prob=c(0.25,0.5,0.25))
        kid2 <- sample(0:2, 1, prob=c(0.25,0.5,0.25))
      }
      if(parent==6){
        father <- 1
        mother <- 2
        kid1 <- sample (1:2 , 1)
        kid2 <- sample (1:2 , 1)
      }
      if(parent==7) father <- mother <- kid1 <- kid2 <-2
      snp <- c(father, mother ,kid1,kid2)
      bed <- cbind(bed,snp)
    }
    outbed <- rbind(outbed,bed)
  }
  colnames(outbed) <- c("fid","iid",paste0("snp",1:m))
  phenotype <- rep(-9,nrow(outbed))
  outbed0 <- data.frame(text[,1:5],phenotype,outbed[,3:ncol(outbed)])
  l <- list()
  l$bed <-outbed0
  l$maf <- pm
  return(l)
}
