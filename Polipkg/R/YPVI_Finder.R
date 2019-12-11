#'YPVI_Finder
#'
#'returns the change in national performance
#'
#'@import dplyr
#'
#'@author Matthew Brown
#'
#'@param x year of election
#'
#'@return The movement in national congressional popular vote
#'
#'@export
#'

YPVI_Finder <- function(x){
  yp<<-yp()
  a<-yp%>%dplyr::filter(.data$year==x)
  a<-a[-c(1,3,4)]
  return(a)
}
