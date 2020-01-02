#' @title the function generate fam file for families
#' @name fam_genera
#' @description the function to generate the fam file of families containing two parents and two children.
#' @param n the number of families, this is an integer.
#' @return the fam file, it is a data.frame output. 
#' @details This function is to generate the .fam file for PLINK use only after adding the information of genotypes and phenotypes. And here it sets the structures of the families already. For each family, there are two parents, and two siblings.
#' @examples
#' \dontrun{
#' fam_genera(100)
#' }
#' @export
fam_genera <- function(n){
  ## fid : family ID
  ## iid : individual ID
  ## pid : parternal ID
  ## mid : maternal ID

  fid <- rep (1:n ,4 )
  iid <- c(rep(1,n),rep(2,n),rep(3,n),rep(4,n))
  iid <- paste0(fid,paste0("000",iid))
  pid <- rep(1,2*n)
  mid <- rep(2,2*n)
  pid <- c(rep(0,2*n),paste0(fid[1:(2*n)],paste0("000",pid)))
  mid <- c(rep(0,2*n),paste0(fid[1:(2*n)],paste0("000",mid)))
  sex <- c(rep(1,n),rep(2,n),sample(1:2,2*n,replace=TRUE))
  fam <- data.frame(fid,iid,pid,mid,sex)
  fam <- fam[order(fam$fid),]
  return(fam)
}