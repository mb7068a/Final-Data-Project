#'Natl_mvmt
#'
#'returns the change in national performance
#'
#'Details
#'
#'@author Matthew Brown
#'
#'@param x year of last election
#'
#'@param y year of current election
#'
#'@param z Is the candidate a Republican
#'
#'@return The movement in national congressional popular vote
#'
#'@examples
#'
#'
#'@export
#'

Natl_mvmt <- function(x,y,z){
  a<-YPVI_Finder(x)
  b<-YPVI_Finder(y)
  return(ifelse(z==TRUE,a$YPVI-b$YPVI,b$YPVI-a$YPVI))
}
