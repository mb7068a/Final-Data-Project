#'yp
#'
#'returns the yearly performance table
#'
#'@import dplyr
#'@import utils
#'@author Matthew Brown
#'
#'
#'@return The movement in national congressional popular vote
#'
#'@export


yp<-function(){
  return(read.csv("https://raw.github.com/mb7068a/Final-Data-Project/master/Tables/ypv2.csv", sep="\t"))
}
