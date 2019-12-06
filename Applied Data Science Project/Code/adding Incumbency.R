names(Senate7)[9]<-"ICO"
Senate8<-Senate7%>%
  mutate(Inc=ifelse(ICO == "Incumbent",TRUE,FALSE))%>%
  mutate(Open=ifelse(ICO == "Open",TRUE,FALSE))
Senate8<-Senate8%>%
  mutate(AInc=ifelse(Inc == FALSE & Open ==FALSE,TRUE,FALSE))




#Create new models to account for incumbency and those running against incumbents
f5<-lm(Senate8$RPVI ~ Senate8$Previous_RPVI + Senate8$share_of_spent + Senate7$YPVI2 + Senate8$Inc)
f6<-lm(Senate8$RPVI ~ Senate8$Previous_RPVI + Senate8$share_of_spent + Senate7$YPVI2 + Senate8$Inc + Senate8$AInc)
f7 <- lm(Senate8$RPVI ~ Senate8$Previous_RPVI + Senate8$share_of_spent + Senate7$YPVI2 + Senate8$AInc)


summary(f4)
summary(f5)
summary(f6)
summary(f7)
#Incmbency has no statistical significance 
