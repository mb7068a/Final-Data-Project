yp18 <- read.csv("Data/export copy 4.csv", skip=55)
yp16 <- read.csv("Data/export-2 copy 3.csv", skip=55)
yp14 <- read.csv("Data/export-3 copy 3.csv", skip=54)
yp12 <- read.csv("Data/export-4 copy.csv", skip=55)
yp10 <- read.csv("Data/export.csv", skip=54)
yp08 <- read.csv("Data/export-2.csv", skip=54)
yp06 <- read.csv("Data/export-3.csv", skip=56)
yp04 <- read.csv("Data/export copy.csv", skip=55)
yp02 <- read.csv("Data/export-2 copy.csv", skip=55)
yp00 <- read.csv("Data/export-3 copy.csv", skip=54)
yp98 <- read.csv("Data/export-4.csv", skip=54)
yp96 <- read.csv("Data/export-5.csv", skip=55)
yp94 <- read.csv("Data/export-6.csv", skip=54)
yp92 <- read.csv("Data/export-7.csv", skip=55)
yp90 <- read.csv("Data/export-8.csv", skip=55)
yp88 <- read.csv("Data/export copy 3.csv", skip=55)
yp86 <- read.csv("Data/export-2 copy 2.csv", skip=55)
yp84 <- read.csv("Data/export-3 copy 2.csv", skip=54)

yp18<-yp18[ -c(1:14)]
yp16<-yp16[ -c(1:14)]
yp14<-yp14[ -c(1:14)]
yp12<-yp12[ -c(1:14)]
yp10<-yp10[ -c(1:14)]
yp08<-yp08[ -c(1:14)]
yp06<-yp06[ -c(1:14)]
yp04<-yp04[ -c(1:14)]
yp02<-yp02[ -c(1:14)]
yp00<-yp00[ -c(1:14)]
yp98<-yp98[ -c(1:14)]
yp96<-yp96[ -c(1:14)]
yp94<-yp94[ -c(1:14)]
yp92<-yp92[ -c(1:14)]
yp90<-yp90[ -c(1:14)]
yp88<-yp88[ -c(1:14)]
yp86<-yp86[ -c(1:14)]
yp84<-yp84[ -c(1:14)]

yp18<-yp18%>%mutate(year=2018)
yp16<-yp16%>%mutate(year=2016)
yp14<-yp14%>%mutate(year=2014)
yp12<-yp12%>%mutate(year=2012)
yp10<-yp10%>%mutate(year=2010)
yp08<-yp08%>%
  mutate(year=2008)
yp06<-yp06%>%
  mutate(year=2006)
yp04<-yp04%>%
  mutate(year=2004)
yp02<-yp02%>%
  mutate(year=2002)
yp00<-yp00%>%
  mutate(year=2000)
yp98<-yp98%>%
  mutate(year=1998)
yp96<-yp96%>%
  mutate(year=1996)
yp94<-yp94%>%
  mutate(year=1994)
yp92<-yp92%>%
  mutate(year=1992)
yp90<-yp90%>%
  mutate(year=1990)
yp88<-yp88%>%
  mutate(year=1988)
yp86<-yp86%>%
  mutate(year=1986)
yp84<-yp84%>%
  mutate(year=1984)


yp1<-full_join(yp90, yp92)
yp2<-full_join(yp1, yp94)
yp3<-full_join(yp2, yp96)
yp4<-full_join(yp3, yp98)
yp5<-full_join(yp4, yp00)
yp6<-full_join(yp5, yp02)
yp7<-full_join(yp6, yp04)
yp8<-full_join(yp7, yp06)
yp9<-full_join(yp8, yp08)
yp11<-full_join(yp9, yp10)
yp13<-full_join(yp11, yp12)
yp15<-full_join(yp13, yp14)
yp17<-full_join(yp15, yp16)
yp19<-full_join(yp17, yp18)
yp20<-full_join(yp88, yp19)
yp21<-full_join(yp86, yp20)
yp<-full_join(yp84, yp21)
ypp<-yp%>%
  mutate(year=year+6)%>%
  mutate(YPPVI=0.01*(DemVotesMajorPercentAll-RepVotesMajorPercentAll))
ypp<-ypp[ -c(1:2)]
yp<-yp%>%
  mutate(YPVI=0.01*(DemVotesMajorPercentAll-RepVotesMajorPercentAll))
  
yp<-yp[ -c(1:2)]
yp<-full_join(yp, ypp)
yp<-yp%>%
mutate(YPM=(YPVI-YPPVI))

write.table(yp, "~/Documents/GitHub/Final-Data-Project/Tables/ypv2.csv", sep="\t")

