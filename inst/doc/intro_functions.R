## ----eval=FALSE---------------------------------------------------------------
#  fam_genera <- function(n){
#  
#    fid <- rep (1:n ,4 )
#    iid <- c(rep(1,n),rep(2,n),rep(3,n),rep(4,n))
#    iid <- paste0(fid,paste0("000",iid))
#    pid <- rep(1,2*n)
#    mid <- rep(2,2*n)
#    pid <- c(rep(0,2*n),paste0(fid[1:(2*n)],paste0("000",pid)))
#    mid <- c(rep(0,2*n),paste0(fid[1:(2*n)],paste0("000",mid)))
#    sex <- c(rep(1,n),rep(2,n),sample(1:2,2*n,replace=TRUE))
#    fam <- data.frame(fid,iid,pid,mid,sex)
#    fam <- fam[order(fam$fid),]
#    return(fam)
#  }

## ----eval=TRUE----------------------------------------------------------------
library(SC19036)
n <- 50
result <- fam_genera(n)
head(result)

## ----eval=FALSE---------------------------------------------------------------
#  fam_relation <- function (fam){
#    n <- nrow(fam)
#    fam[,1] <- as.numeric(as.character(fam[,1]))
#    fam[,2] <- as.numeric(as.character(fam[,2]))
#    fid <- rep(unique(fam[,1]),each=6)[1:(6*n/4)]
#    iid1 <- iid2 <- relationship <- NULL
#    for( i in 1:length(unique(fid))){
#  
#      a <- unique(fid)[i]
#      l <- unique(fam[which(fam[,1]==a),2])
#      iid1[(i-1)*6+1] <- l[1]
#      iid2[(i-1)*6+1] <- l[2]
#      iid1[(i-1)*6+2] <- l[1]
#      iid2[(i-1)*6+2] <- l[3]
#      iid1[(i-1)*6+3] <- l[1]
#      iid2[(i-1)*6+3] <- l[4]
#      iid1[(i-1)*6+4] <- l[2]
#      iid2[(i-1)*6+4] <- l[3]
#      iid1[(i-1)*6+5] <- l[2]
#      iid2[(i-1)*6+5] <- l[4]
#      iid1[(i-1)*6+6] <- l[3]
#      iid2[(i-1)*6+6] <- l[4]
#    }
#    relationship <- rep(c("couple","parentOffspring","parentOffspring",
#                          "parentOffspring", "parentOffspring" , "fullSib" ),length(unique(fid)))
#    re <- data.frame(fid,iid1,iid2,relationship)
#  
#    return(re)
#  
#  }

## ----eval=TRUE----------------------------------------------------------------
library(SC19036)
relation <- fam_relation(result)
head(relation)

## ----eval=FALSE---------------------------------------------------------------
#  geno_genera <- function (text , m){
#    n <- length(unique(text[,1]))
#    text[,1] <- as.numeric(as.character(text[,1]))
#    text[,2] <- as.numeric(as.character(text[,2]))
#    text[,3] <- as.numeric(as.character(text[,3]))
#    text[,4] <- as.numeric(as.character(text[,4]))
#    text[,5] <- as.numeric(as.character(text[,5]))
#    outbed <- integer()
#    pm <- runif(m,0.1,1)
#    for(j in 1:n){
#      no.fam <- unique(text[,1])[j]
#      l.fam <- text[which(text[,1]==no.fam),2]
#      iid <- l.fam[order(l.fam)]
#      fid <- rep(no.fam,length(l.fam))
#      bed <- cbind(fid, iid)
#      for(i in 1:m){
#        p <- pm[i]
#        ## p : the MAF of one SNP
#  
#        AA <- (1-p)^2
#        Aa <- 2*p*(1-p)
#        aa <- p^2
#        prob <- c(AA^2, AA*Aa,AA*aa,Aa*AA,Aa*Aa,Aa*aa,aa*aa)
#  
#        pairs <- 1:7
#        parent <- sample (pairs, 1,prob=prob)
#        if(parent==1) father <- mother <- kid1 <- kid2 <- 0
#        if(parent==2) {
#          father <- 0
#          mother <- 1
#          kid1 <- sample(0:1,1)
#          kid2 <- sample(0:1,1)
#        }
#        if(parent==3){
#          father <- 0
#          mother <- 2
#          kid1 <- kid2 <-1
#        }
#        if(parent==4){
#          father <- 1
#          mother <- 0
#          kid1 <- sample(0:1,1)
#          kid2 <- sample(0:1,1)
#        }
#        if(parent==5){
#          father <- mother <- 1
#          kid1 <- sample(0:2, 1, prob=c(0.25,0.5,0.25))
#          kid2 <- sample(0:2, 1, prob=c(0.25,0.5,0.25))
#        }
#        if(parent==6){
#          father <- 1
#          mother <- 2
#          kid1 <- sample (1:2 , 1)
#          kid2 <- sample (1:2 , 1)
#        }
#        if(parent==7) father <- mother <- kid1 <- kid2 <-2
#        snp <- c(father, mother ,kid1,kid2)
#        bed <- cbind(bed,snp)
#      }
#      outbed <- rbind(outbed,bed)
#    }
#    colnames(outbed) <- c("fid","iid",paste0("snp",1:m))
#    phenotype <- rep(-9,nrow(outbed))
#    outbed0 <- data.frame(text[,1:5],phenotype,outbed[,3:ncol(outbed)])
#    l <- list()
#    l$bed <-outbed0
#    l$maf <- pm
#    return(l)
#  }
#  

## ----eval=TRUE----------------------------------------------------------------
library(SC19036)
m <- 100
geno <- geno_genera(result,m)
head(geno[[1]],5)[,1:12]
head(geno[[2]],10)

## ----eval=FALSE---------------------------------------------------------------
#  ERM_genera <- function(text){
#    fam_file <- text
#    fam_file <- fam_file[, 1:2]
#    names(fam_file) <- c("FID", "IID")
#    fam_file$order <- 1:nrow(fam_file)
#    rel <- re[, c( 2,3, ncol(re))]
#    names(rel) <- c("IID_1", "IID_2", "relationship")
#    rel2 <- rel[,c(2,1,3)]
#    names(rel2) <- c("IID_1", "IID_2", "relationship")
#    rel <- rbind(rel, rel2)
#  
#    rel <- merge(rel, fam_file[,c("IID", "order")], by.x="IID_1", by.y="IID")
#    rel <- merge(rel, fam_file[,c("IID", "order")], by.x="IID_2", by.y="IID")
#    rel <- rel[, c("IID_1", "IID_2", "relationship", "order.x", "order.y")]
#    rel <- rel[order(rel$order.x, rel$order.y), ]
#    sum(rel$order.x >= rel$order.y)
#    rel <- rel[rel$order.x > rel$order.y, ]
#  
#    n <- nrow(fam_file)
#    ### environment family
#    rel$coef <- 0
#    rel[rel$relationship == "parentOffspring", "coef"] <- 1 ## hudie set  ##original 0.5
#    rel[rel$relationship == "couple", "coef"] <- 1 ## hudie set  ##original 0.5
#    rel[rel$relationship == "fullSib", "coef"] <- 1 ## hudie set  ##original 0.5
#    x <- 1:n
#    y <-1:n
#    xy1 <- xy2 <- numeric()
#    for(i in 1:(n-1)){
#      for(j in (i+1):n) {
#        xy1 <- c(xy1,x[i])
#        xy2 <- c(xy2,y[j])
#      }
#    }
#    xy <- cbind(xy1,xy2)
#    for( i in 1:length(unique(rel$order.x))){
#      a <- unique(rel$order.x)[i]
#      b <- rel[which(rel$order.x==a),"order.y"]
#      for(j in 1:length(b)){
#        xy <- xy[-which(xy[,2]==a & xy[,1]==b[j]),]
#      }
#    }
#  
#    order.x <- c(rel$order.x,xy[,2])
#    order.y <- c(rel$order.y,xy[,1])
#    relationship <- c(rel$coef,rep(0,length(order.x)-length(rel$coef)))
#    order.x <- c(order.x,1:n)##add the diagnal entry
#    order.y <- c(order.y,1:n)
#  
#    relationship <- c(relationship,rep(1,n))
#    snp <- rep(500,length(order.x))
#    matrix_family <- data.frame(order.x, order.y, snp,relationship)
#    matrix_family_id <- data.frame(order.x, order.y, snp,relationship)
#  
#    ###environment couple
#    rel$coef <- 0
#    rel[rel$relationship == "couple", "coef"] <- 1 ##hudie-set
#    x <- 1:n
#    y <- 1:n
#    xy1 <- xy2 <- numeric()
#    for(i in 1:(n-1)){
#      for(j in (i+1):n) {
#        xy1=c(xy1,x[i])
#        xy2=c(xy2,y[j])
#      }
#    }
#    xy <- cbind(xy1,xy2)
#    for( i in 1:length(unique(rel$order.x))){
#      a <- unique(rel$order.x)[i]
#      b <- rel[which(rel$order.x==a),"order.y"]
#      for(j in 1:length(b)){
#        xy <- xy[-which(xy[,2]==a & xy[,1]==b[j]),]
#      }
#    }
#  
#    order.x <- c(rel$order.x,xy[,2])
#    order.y <- c(rel$order.y,xy[,2])
#    relationship <- c(rel$coef,rep(0,length(order.x)-length(rel$coef)))
#    order.x <- c(order.x,1:n)##add the diagnal entry
#    order.y <- c(order.y,1:n)
#  
#    relationship <- c(relationship,rep(1,n))
#    snp <- rep(500,length(order.x))
#    matrix_couple <- data.frame(order.x, order.y, snp,relationship)
#    matrix_couple_id <- data.frame(order.x, order.y, snp,relationship)
#  
#  
#    ##environment full-sibling
#    rel$coef <- 0
#    rel[rel$relationship == "fullSib", "coef"] <- 1## hudie set  ##original 0.5
#    x <- 1:n
#    y <- 1:n
#    xy1 <- xy2 <- numeric()
#    for(i in 1:(n-1)){
#      for(j in (i+1):n) {
#        xy1 <- c(xy1,x[i])
#        xy2 <- c(xy2,y[j])
#      }
#    }
#    xy <- cbind(xy1,xy2)
#    for( i in 1:length(unique(rel$order.x))){
#      a <- unique(rel$order.x)[i]
#      b <- rel[which(rel$order.x==a),"order.y"]
#      for(j in 1:length(b)){
#        xy <- xy[-which(xy[,2]==a & xy[,1]==b[j]),]
#      }
#    }
#  
#    order.x <- c(rel$order.x,xy[,2])
#    order.y <- c(rel$order.y,xy[,2])
#    relationship <- c(rel$coef,rep(0,length(order.x)-length(rel$coef)))
#    order.x <- c(order.x,1:n)##add the diagnal entry
#    order.y <- c(order.y,1:n)
#  
#    relationship <- c(relationship,rep(1,n))
#    snp <- rep(500,length(order.x))
#    matrix_fullsilb <- data.frame(order.x, order.y, snp,relationship)
#    matrix_fullsilb_id <- data.frame(order.x, order.y, snp,relationship)
#  
#    l <- list(matrix_family,matrix_family_id,matrix_couple,matrix_couple_id,matrix_fullsilb,matrix_fullsilb_id)
#    return(l)
#    }
#  

