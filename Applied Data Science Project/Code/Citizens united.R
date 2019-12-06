library("tidyverse")
Senate5<- read.csv("~/Documents/GitHub/Final-Data-Project/Tables/Senate.csv", sep="\t")
Senate5<-Senate5%>%
  filter(year<=2008)
#Export Data
write.table(Senate5, "~/Documents/GitHub/Final-Data-Project/Tables/Senate.csv", sep="\t")

