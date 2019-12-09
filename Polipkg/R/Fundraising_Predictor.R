#'Fundraising_Predictor
#'
#'Calculates the amount of money a candidate must raise in order to be competative.
#'
#'Details
#'@import dplyr
#'@import utils
#'@author Matthew Brown
#'
#'@param x year of last election
#'@param y The Political Party of candidate a (1 = Republican, 2 = Democrat)
#'@param z Previous Margin
#'@param b Predicted National Margin at time of election
#'@param c The amount of money candidate b is expected to raise
#'
#'@return The amount of money a candidate should raise in order to be competative
#'
#'
#'@export

Fundraising_Predictor<-function(x,y,z,b,c){
  y<-ifelse(y="Republican",1,y)
  y<-ifelse(y="Democrat",2,y)
  RPVI<-2*(.01*(z)-.5)
  NPVI<-(2*((b*.01)-.5))
  a<-YPVI_Finder(x)
  YPVI<-NPVI-a*ifelse(y==1,-1,1)
  CRPVI<-0.2382058*RPVI
  CYPVI<-0.3457135*(YPVI)
  CSS<-0.5815470
  INT<--0.3182581
  d<-(((-INT*c-c*CYPVI-c*CRPVI)/(CSS+INT+CYPVI+CRPVI)))
  d<-format(b,big.mark = ",",nsmall=2)
  d<-paste('$',b,sep = "")
  paste("Candidate A should spend", d, "to be competative")
}
