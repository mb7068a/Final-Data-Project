#'YPVI_Finder
#'
#'returns the change in national performance
#'
#'Details
#'
#'@author Matthew Brown
#'
#'@param x year of election
#'
#'@return The movement in national congressional popular vote
#'
#'@examples
#'
#'@export
#'

YPVI_Finder <- function(x){
  yp<-read.csv("data/yp.csv", sep="\t")
  a<-yp%>%dplyr::filter(YPVI,year==x)
  return(a)
}
