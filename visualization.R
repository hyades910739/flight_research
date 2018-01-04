####  Data Visualization


attach(flight)
library(ggplot2)
library(data.table)
library(magrittr)

ggplot(flight, aes(Distance, ArrDelay)) +
  geom_point(shape = 16, size = 3,alpha=0.8,color="#99e6ff", show.legend = FALSE) +
  theme_minimal()+
  ggtitle("Distance vs ArrDelay") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))
ggsave("123.png")






ggplot(flight, aes(ArrTime, ArrDelay)) +
  geom_point(shape = 16, size = 3,alpha=0.8,color="#99e6ff", show.legend = FALSE) +
  theme_minimal()+
  ggtitle("ArrTime vs ArrDelay") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))
ggsave("123.png")

ggplot(flight, aes(x=ArrTime, y=ArrDelay,col=Distance)) +
  geom_point(shape = 16, size = 3,alpha=0.6) +
  theme_minimal()+
  scale_color_gradient(low="#99e6ff", high="red")
  ggtitle("ArrTime vs ArrDelay") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))
ggsave("1234.png")

ggplot(flight, aes(x=CRSArrTime, y=ArrDelay,col=Distance)) +
  geom_point(shape = 16, size = 3,alpha=0.6) +
  theme_minimal()+
  scale_color_gradient(low="#99e6ff", high="red")
ggtitle("CRSArrTime vs ArrDelay") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))
ggsave("12345.png")



###########
flight %>% group_by(Month) %>% summarise(meanDelay=mean(ArrDelay)) %>% 
  ggplot(., aes(x=Month, y=meanDelay)) + 
  geom_bar(stat="identity", fill="#99e6ff", colour="black", position=position_dodge())+
  ggtitle("Month vs ArrDelay") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))


flight %>% group_by(Month) %>% summarise(Delay60=mean(DelayOver60 %>% as.character() %>% as.numeric())) %>% 
  ggplot(., aes(x=Month, y=Delay60)) + 
  geom_bar(stat="identity", fill="#99e6ff", colour="black", position=position_dodge())+
  ggtitle("Month vs Delay60") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))

flight %>% group_by(Month) %>% 
  summarise(meanDelay=mean(ArrDelay),Delay60=mean(DelayOver60 %>% as.character() %>% as.numeric())) %>% 
  write.csv(.,"flight.temp.csv",row.names=F)

########### Week

temp = flight %>% group_by(DayOfWeek) %>% 
  summarise(meanDelay=mean(ArrDelay) %>% round(.,2),Delay60=(100*mean(DelayOver60 %>% as.character() %>% as.numeric())) %>% round(.,2)) %T>% 
  write.csv(.,"flight.temp.csv",row.names=F)  

temp %>% ggplot(., aes(x=DayOfWeek, y=meanDelay)) + 
  geom_bar(stat="identity", fill="#99e6ff", colour="black", position=position_dodge())+
  ggtitle("Week vs ArrDelay") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5)) 

temp %>%  ggplot(., aes(x=DayOfWeek, y=Delay60)) + 
  geom_bar(stat="identity", fill="#99e6ff", colour="black", position=position_dodge())+
  ggtitle("week vs Delay60") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))

rm(temp)

########### month

temp = flight %>% group_by(Month) %>% 
  summarise(meanDelay=mean(ArrDelay) %>% round(.,2),Delay60=(100*mean(DelayOver60 %>% as.character() %>% as.numeric())) %>% round(.,2)) %T>% 
  write.csv(.,"flight.temp.csv",row.names=F)  

temp %>% ggplot(., aes(x=Month, y=meanDelay)) + 
  geom_bar(stat="identity", fill="#99e6ff", colour="black", position=position_dodge())+
  ggtitle("Month vs ArrDelay") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5)) 

temp %>%  ggplot(., aes(x=Month, y=Delay60)) + 
  geom_bar(stat="identity", fill="#99e6ff", colour="black", position=position_dodge())+
  ggtitle("Month vs Delay60") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))

rm(temp)



bp2 = function(x,y,title,round=0,cut=10){
  y = y %>%  as.character() %>% as.numeric()
  dis = round((max(x)-min(x))/cut,round)
  str=min(x) 
  if(!round) str = floor(str)
  px = c()
  py =c()
  for(i in 1:cut){
    sel = which(x>=str,x<(str+dis))
    px = c(px,(str+str+dis)/2)
    py = c(py,mean(y[sel]))
    str = str+dis
  }
  data.frame(px=px,py=py) %>% 
    ggplot(., aes(x=px, y=py)) + geom_bar(stat="identity", fill="#99e6ff", colour="black")+
    ggtitle(title) +
    theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))
  
}


bp2(flight$Distance,flight$DelayOver60,"Distance vs DelayOver60",0,15)
bp2(flight$Distance,flight$ArrDelay,"Distance vs MeanDelay",0,15)


ggplot(flight,aes(ArrDelay))+geom_histogram(binwidth = 10)

bp2(flight$ArrTime,flight$ArrDelay,"ArrTime vs ArrDelay",0,24)
bp2(flight$ArrTime,flight$DelayOver60,"ArrTime vs Delay60",0,24)

#


temp = flight %>% group_by(UniqueCarrier) %>% 
  summarise(meanDelay = mean(ArrDelay),Delay60 = mean((DelayOver60 %>% as.character() %>% as.numeric())))

ggplot(temp, aes(x=UniqueCarrier, y=meanDelay)) + geom_bar(stat="identity", fill="#99e6ff", colour="black")+
      ggtitle("Carrier vs MeanDelay") +
      theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))

ggplot(temp, aes(x=UniqueCarrier, y=Delay60)) + geom_bar(stat="identity", fill="#99e6ff", colour="black")+
  ggtitle("Carrier vs Delay60") +
  theme(plot.title = element_text(size=18,face="bold",hjust = 0.5))



