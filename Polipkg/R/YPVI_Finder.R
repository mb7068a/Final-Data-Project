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
#'
#'@export
#'

yp<-

YPVI_Finder <- function(x){
  a<-yp%>%dplyr::filter(YPVI,year==x)
  return(a)
}
