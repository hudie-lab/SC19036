#' @title the function to generate the Environment Relationship Matrix
#' @name ERM_genera
#' @description generate the environment relationship matrix from three aspects: family, couple, fullsilbings.
#' @param text the fam file to input, can get from the fam_genera()
#' @return matrix_family the ERM of family, for the file with ".grm.gz"
#' @return matrix_family_id the file with ".grm.id
#' @return matrix_couple the ERM of couple, for the file with ".grm.gz"
#' @return matrix_couple_id the file with ".grm.id
#' @return matrix_fullsilb the ERM of fullsilbing, for the file with ".grm.gz"
#' @return matrix_fullsilb the file with ".grm.id
#' @references Yang J, Lee SH, Goddard ME, Visscher PM (2011) GCTA: a tool for genome-wide complex trait analy- sis. Am J Hum Genet 88: 76–82. doi: 10.1016/j.ajhg.2010.11.011 PMID: 21167468
#' @references Xia, C. et al. Pedigree- and SNP-associated genetics and recent environment are the major contributors to anthropometric and cardiometabolic trait variation. PLoS. Genet. 12, e1005804 (2016).
#' @examples
#' \dontrun{
#' text=fam_genera(100)
#' l=ERM_genera(text)
#' }
#' @export
ERM_genera <- function(text){
  fam_file <- text
  fam_file <- fam_file[, 1:2]
  names(fam_file) <- c("FID", "IID")
  fam_file$order <- 1:nrow(fam_file)
  rel <- re[, c( 2,3, ncol(re))]
  names(rel) <- c("IID_1", "IID_2", "relationship")
  rel2 <- rel[,c(2,1,3)]
  names(rel2) <- c("IID_1", "IID_2", "relationship")
  rel <- rbind(rel, rel2)
  
  rel <- merge(rel, fam_file[,c("IID", "order")], by.x="IID_1", by.y="IID")
  rel <- merge(rel, fam_file[,c("IID", "order")], by.x="IID_2", by.y="IID")
  rel <- rel[, c("IID_1", "IID_2", "relationship", "order.x", "order.y")]
  rel <- rel[order(rel$order.x, rel$order.y), ]
  sum(rel$order.x >= rel$order.y)
  rel <- rel[rel$order.x > rel$order.y, ]
  
  n <- nrow(fam_file)
  ### environment family
  rel$coef <- 0 
  rel[rel$relationship == "parentOffspring", "coef"] <- 1 ## hudie set  ##original 0.5
  rel[rel$relationship == "couple", "coef"] <- 1 ## hudie set  ##original 0.5
  rel[rel$relationship == "fullSib", "coef"] <- 1 ## hudie set  ##original 0.5
  x <- 1:n
  y <-1:n
  xy1 <- xy2 <- numeric()
  for(i in 1:(n-1)){
    for(j in (i+1):n) {
      xy1 <- c(xy1,x[i])
      xy2 <- c(xy2,y[j])
    }
  }
  xy <- cbind(xy1,xy2)
  for( i in 1:length(unique(rel$order.x))){
    a <- unique(rel$order.x)[i]
    b <- rel[which(rel$order.x==a),"order.y"]
    for(j in 1:length(b)){
      xy <- xy[-which(xy[,2]==a & xy[,1]==b[j]),]
    }
  }
  
  order.x <- c(rel$order.x,xy[,2])
  order.y <- c(rel$order.y,xy[,1])
  relationship <- c(rel$coef,rep(0,length(order.x)-length(rel$coef)))
  order.x <- c(order.x,1:n)##add the diagnal entry
  order.y <- c(order.y,1:n)
  
  relationship <- c(relationship,rep(1,n))
  snp <- rep(500,length(order.x))
  matrix_family <- data.frame(order.x, order.y, snp,relationship)
  matrix_family_id <- data.frame(order.x, order.y, snp,relationship)
  
  ###environment couple
  rel$coef <- 0 
  rel[rel$relationship == "couple", "coef"] <- 1 ##hudie-set 
  x <- 1:n
  y <- 1:n
  xy1 <- xy2 <- numeric()
  for(i in 1:(n-1)){
    for(j in (i+1):n) {
      xy1=c(xy1,x[i])
      xy2=c(xy2,y[j])
    }
  }
  xy <- cbind(xy1,xy2)
  for( i in 1:length(unique(rel$order.x))){
    a <- unique(rel$order.x)[i]
    b <- rel[which(rel$order.x==a),"order.y"]
    for(j in 1:length(b)){
      xy <- xy[-which(xy[,2]==a & xy[,1]==b[j]),]
    }
  }
  
  order.x <- c(rel$order.x,xy[,2])
  order.y <- c(rel$order.y,xy[,2])
  relationship <- c(rel$coef,rep(0,length(order.x)-length(rel$coef)))
  order.x <- c(order.x,1:n)##add the diagnal entry
  order.y <- c(order.y,1:n)
  
  relationship <- c(relationship,rep(1,n))
  snp <- rep(500,length(order.x))
  matrix_couple <- data.frame(order.x, order.y, snp,relationship)
  matrix_couple_id <- data.frame(order.x, order.y, snp,relationship)
  
  
  ##environment full-sibling
  rel$coef <- 0 
  rel[rel$relationship == "fullSib", "coef"] <- 1## hudie set  ##original 0.5
  x <- 1:n
  y <- 1:n
  xy1 <- xy2 <- numeric()
  for(i in 1:(n-1)){
    for(j in (i+1):n) {
      xy1 <- c(xy1,x[i])
      xy2 <- c(xy2,y[j])
    }
  }
  xy <- cbind(xy1,xy2)
  for( i in 1:length(unique(rel$order.x))){
    a <- unique(rel$order.x)[i]
    b <- rel[which(rel$order.x==a),"order.y"]
    for(j in 1:length(b)){
      xy <- xy[-which(xy[,2]==a & xy[,1]==b[j]),]
    }
  }
  
  order.x <- c(rel$order.x,xy[,2])
  order.y <- c(rel$order.y,xy[,2])
  relationship <- c(rel$coef,rep(0,length(order.x)-length(rel$coef)))
  order.x <- c(order.x,1:n)##add the diagnal entry
  order.y <- c(order.y,1:n)
  
  relationship <- c(relationship,rep(1,n))
  snp <- rep(500,length(order.x))
  matrix_fullsilb <- data.frame(order.x, order.y, snp,relationship)
  matrix_fullsilb_id <- data.frame(order.x, order.y, snp,relationship)
  
  l <- list(matrix_family,matrix_family_id,matrix_couple,matrix_couple_id,matrix_fullsilb,matrix_fullsilb_id)
  return(l)
  }
