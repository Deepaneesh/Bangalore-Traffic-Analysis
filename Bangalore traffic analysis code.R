# package loading 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(tseries)
library(forcats)
library(forecast)
library(DT)
library(readr)
# data loading 
dir()
data=read.csv("Banglore_traffic_Dataset.csv")
colnames(data)
data$Date=as.Date(data$Date,formate="%Y-%m-%d")
data$Days=weekdays(data$Date) %>%  as.factor()
levels(data$Days)
View(data)

data %>% group_by(Days) %>% summarise(Average_Traffic=mean(Traffic.Volume)) %>%
  ggplot(aes(x=factor(Days,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),y=Average_Traffic,fill=Days))+
  geom_bar(stat="identity")+geom_text(aes(label=round(Average_Traffic,0)),vjust=1.5)+labs(
    title="WEEKDAYS Vs TRAFFIC",
    y="TRAFFIC VOLUME(AVERAGE)",
    x="WEEKDAYS",
    caption = "It is the bar-diagram of Average traffic volume in Bangalure based on weekdays ")+
  theme(plot.title = element_text(face = "bold",hjust=0.5),
        legend.position = "None",
        plot.caption = element_text(face="bold"))
data %>% ggplot(aes(x=Average.Speed,y=Congestion.Level))+geom_point(shape=1)  
cor(data$Average.Speed,data$Congestion.Level)
data$month=month(data$Date)|>as.factor()
month.abb
levels(data$month)=month.abb
data$month
data$year=year(data$Date)
k=data %>% group_by(year,month) %>% summarise(Speed=mean(Average.Speed),Congestion=mean(Congestion.Level)) 
graph2=k %>% ggplot(aes(x=Speed,y=Congestion))+geom_smooth(se=F)+geom_point()+labs(title = "Speed Vs Congestion")+theme(plot.title = element_text(hjust = 0.5,face="bold"))
k
graph2
cor(k$Speed,k$Congestion)
data$Weather.Conditions=as.factor(data$Weather.Conditions)
graph=data %>% ggplot(aes(x=Average.Speed,y=Traffic.Volume,colour =Weather.Conditions))+
  facet_wrap(~Weather.Conditions)+geom_point()+
  labs(title ="Average Speed Vs Traffic Volume",
       subtitle = "Based on Weather Condtion ",
       x="Average Speed",
       y=" Traffic Volume")+
  theme(legend.position = "none",
        plot.title = element_text(face="bold",hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)
        )
graph

plotgraph=ggplotly(graph)
plotlygraph2=ggplotly(graph2)




data1=data %>%
  subset(select=c(year,month,Traffic.Volume,Environmental.Impact,Public.Transport.Usage)) %>% 
  group_by(year,month) %>%summarise(Traffic_volume=sum(Traffic.Volume),
                                    Environmental_Impact=sum(Environmental.Impact),
                                    Public_Transport_usage=sum(Public.Transport.Usage)) 
data1$sno=1:32
data1 %>% View()



ts_data=data1[1:31,] %>% subset(select=c(year,month,Traffic_volume))
typeof(ts_data)
ts_data$sn01=1:31
class(ts_data)
datatime=ts(ts_data$Traffic_volume,start=min(ts_data$sn01),end=max(ts_data$sn01),frequency = 12)
class(datatime)
plot(datatime,xlab="Month (starting form 2022 jan)",ylab="Traffic volume",main="Traffic Volume depends on time")
#data is in stationary 
acf(datatime)
pacf(datatime)
# adf test to test stationary p is less than 0.05 then it is stationary 
adf.test(datatime)

model=auto.arima(datatime,ic="bic",trace = T)
model
acf(ts(model$residuals))

pacf(ts(model$residuals))
myf=forecast(model,level=95,h=15) 
myf
plot(myf,xlab="Month (starting form 2022 jan)",ylab="Traffic volume",main="Predicted Traffic Volume(upto 2025 june)")


