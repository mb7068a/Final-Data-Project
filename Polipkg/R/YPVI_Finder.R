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
  yp<-read.csv("https://raw.github.com/mb7068a/Final-Data-Project/master/Tables/ypv2.csv", sep="\t")
  a<-yp%>%dplyr::filter(year==x)
  a<-a[-c(1,3,4)]
  return(a)
}
