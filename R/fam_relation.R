
#' @title the function generate fam relationship file for families
#' @name fam_relation
#' @description the function to generate the fam relationship file of families containing two parents and two children.
#' @param fam the family file, it is a data.frame here.
#' @return the fam_relationship, this is a data.frame output.
#' @details Here, the fam relationship is created by the fam file from fam_genera(). And the relationship would be "couple","parentOffspring" or "fullSib". 
#' @examples
#' \dontrun{
#' fam <- fam_genera(100)
#' fam_relation(fam)
#' }
#' @export
fam_relation <- function (fam){
  n <- nrow(fam)
  fam[,1] <- as.numeric(as.character(fam[,1]))
  fam[,2] <- as.numeric(as.character(fam[,2]))
  fid <- rep(unique(fam[,1]),each=6)[1:(6*n/4)]
  iid1 <- iid2 <- relationship <- NULL
  for( i in 1:length(unique(fid))){
    
    a <- unique(fid)[i]
    l <- unique(fam[which(fam[,1]==a),2])
    iid1[(i-1)*6+1] <- l[1]
    iid2[(i-1)*6+1] <- l[2]
    iid1[(i-1)*6+2] <- l[1]
    iid2[(i-1)*6+2] <- l[3]
    iid1[(i-1)*6+3] <- l[1]
    iid2[(i-1)*6+3] <- l[4]
    iid1[(i-1)*6+4] <- l[2]
    iid2[(i-1)*6+4] <- l[3]
    iid1[(i-1)*6+5] <- l[2]
    iid2[(i-1)*6+5] <- l[4]
    iid1[(i-1)*6+6] <- l[3]
    iid2[(i-1)*6+6] <- l[4]
  }
  relationship <- rep(c("couple","parentOffspring","parentOffspring",
                        "parentOffspring", "parentOffspring" , "fullSib" ),length(unique(fid)))
  re <- data.frame(fid,iid1,iid2,relationship)
  
  return(re)
  
}
