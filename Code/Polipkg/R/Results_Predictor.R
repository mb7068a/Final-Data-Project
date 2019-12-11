#'Results_Predictor
#'
#'Calculates the predicted margin of loss or victory for a candidate.
#'
#'@import dplyr
#'@import utils
#'
#'@author Matthew Brown
#'
#'@param x year of last election
#'@param w The Political Party of candidate a (1 = Republican, 2 = Democrat)
#'@param z Previous Margin
#'@param b Predicted National Margin at time of election
#'@param c The amount of money candidate a is expected to raise
#'@param d The amount of money candidate b is expected to raise
#'
#'@return The predicted margin of loss or victory for candidate a.
#'
#'
#'@export

Results_Predictor<-function(x,w,z,b,c,d){
  w<-ifelse(w=="Republican",1,w)
  w<-ifelse(w=="Democrat",2,w)
  RPVI<-2*(.01*(z)-.5)
  NPVI<-(2*((b*.01)-.5))
  a<-YPVI_Finder(x)
  YPVI<-NPVI-a*ifelse(w==1,-1,1)
  CRPVI<-0.2382058*RPVI
  CYPVI<-0.3457135*(YPVI)
  CSS<-0.5815470
  INT<--0.3182581
  e<-CRPVI+CYPVI+CSS*(c/(c+d))
  e<-100*e
  f<-1*ifelse(e>=0,1,2)
  e<-abs(e)
  e<-paste(b,'%',sep = "")
  g<-paste(ifelse(f==1,"Candidate A will win by","Candidate A will loose by"))
  paste(g,b)
}
