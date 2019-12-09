Senate7<-full_join(Senate4,yp)
Senate7<-Senate7%>%
  mutate(YPVI2=ifelse(party=="republican",YPM*-1,YPM*1))



ggplot(Senate7, aes(share_of_spent, adjusted_performance)) +
  geom_point(aes(color=party)) +
  stat_smooth(method = "lm")

Senate7%>%
  filter(year==2008)%>%
  ggplot(aes(share_of_spent, adjusted_performance)) +
  geom_point(aes(color=party)) +
  stat_smooth(method = "lm") +
  ggtitle("Spending vs. Adjusted Electoral Performance for 2008")+
  xlab("Share of Money Spent") +
  ylab("Adjusted Performance")


f4<-lm(Senate7$RPVI ~ Senate7$Previous_RPVI + Senate7$share_of_spent + Senate7$YPVI2)
summary(f4)
coef(f4)
#no statistical significance 
f1
sd(resid(f4))
sd(resid(f1))

write.table(yp, "c:/mydata.txt", sep="\t")