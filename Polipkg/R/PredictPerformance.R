#'Natl_mvmt
#'
#'returns the adjusted performance of a candidate
#'
#'Details
#'
#'@author Matthew Brown
#'
#'@param x share of the vote
#'
#'@param y year of last election
#'
#'@param z congressional popular vote this cycle
#'
#'@return The movement in for
#'
#'@examples
#'un_col(matrix(c(3,2,4,6,5,1,8,6,1), ncol=3))
#'
#'@export


PredictPerformance <- function(x,y,z,w,v){
  x/(x+y)-1.3027542*z-.5-0.5188205 
  
}
  



f6
Share of fundraising = .2063 * adjusted performance +.2569 * Incumbent -.2601 * Running against incumbent