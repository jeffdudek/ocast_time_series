

library(ggplot2)
henry.theme<-theme(axis.text.x = element_text(size=30),
                   axis.text.y = element_text(size = 30),
                   axis.title.x = element_text(size=35),
                   axis.title.y = element_text(size=35),
                   axis.line = element_line (size=2))


soil_y<-read.csv("new_soil_water_potential.csv", header=T)
soil_y$vpd_kpa<-soil_y$vpd*.1


##lfm ~ vpd
ggplot(data=soil_y)+
  geom_point(position=position_jitter(w=2, h=0), aes(x=vpd, y=lfm), color="black", size=4)+
  geom_smooth(aes(x=vpd, y=lfm), color="black", method=lm, size=2, formula= (y ~ x))+
  xlab("Vapor Pressure Deficit (mb)")+
  ylab("Live Fuel Moisture (%)")+
  henry.theme+
  xlim(0,20)+
  scale_x_continuous(breaks = seq(0, 20, by=5))+ 
  xlim(0,18)+
  scale_y_continuous(breaks = seq(0, 160, by=20))


### lfm ~ vpd_kpa
ggplot(data=soil_y)+
  geom_point(position=position_jitter(w=.2, h=0),aes(x=vpd_kpa, y=lfm), color="black", size=4)+
  geom_smooth(aes(x=vpd_kpa, y=lfm), color="black", method=lm, size=2, formula= (y ~ x))+
  xlab("Vapor Pressure Deficit (kpa)")+
  ylab("Live Fuel Moisture (%)")+
  henry.theme+
  xlim(0,20)+
  scale_x_continuous(breaks = seq(0, 2, by=.5))+ 
  xlim(0,2)+
  scale_y_continuous(breaks = seq(0, 160, by=20))


#### lfm ~ soil_y jittered
ggplot(data=soil_y)+
  theme_bw()+
  geom_point(position=position_jitter(w=.7, h=0), aes(x=avg_soil_y, y=lfm), color="black", size=4)+
  geom_smooth(aes(x=avg_soil_y, y=lfm), color="black", method=lm, formula= (y ~ x), size=2)+
  xlab("Soil Water Potential (-mpa)")+
  ylab("Live Fuel Moisture (%)")+
  henry.theme+
  scale_x_continuous(breaks = seq(0, 2, by=.5))+ 
  xlim(0,2)+
  scale_y_continuous(breaks = seq(0, 160, by=20))+
  theme(axis.text = element_text(color="black"))

#### lfm ~ soil_y jittered
ggplot(data=soil_y)+
  theme_bw()+
  geom_point(position=position_jitter(w=.7, h=0), aes(x=avg_soil_y, y=lfm), color="black", size=4)+
  geom_smooth(aes(x=avg_soil_y, y=lfm), color="black", method=lm, formula= (y ~ x), size=2)+
  xlab("Soil Water Potential (-mpa)")+
  ylab("Live Fuel Moisture (%)")+
  henry.theme+
  scale_x_continuous(breaks = seq(0, 2, by=.5))+ 
  xlim(0,2)+
  scale_y_continuous(breaks = seq(0, 160, by=20))+
  theme(axis.text = element_text(color="black"))

fit.1<-lm(mean_lfm ~ vpd + avg_soil_y, data=soil_y)
summary(fit.1)

fit.2<-lm(mean_lfm ~ vpd, data=soil_y)
summary(fit.2)

fit.3<-lm(mean_lfm ~ avg_soil_y, data=soil_y)
summary(fit.3)

### Multiple regression model
#### y = 113.5667 - 1.8759(vpd) - 9.5155(avg_soil_y) 

predictions.data<-read.csv("wood_mesonet_data.csv", header=T)
predictions.data$jeffdate <- paste(predictions.data$MONTH, "-", predictions.data$DAY, "-", predictions.data$YEAR, sep ="")
predictions.data$new.date<-as.Date(predictions.data$jeffdate, format = "%m-%e-%Y")
predictions.data$daily.lfm <- 113.5667 - (1.8759*predictions.data$VDEF) - (9.5155*predictions.data$avg_soil_y)
head(predictions.data)

ggplot(data=subset(predictions.data, monthly_predicted_lfm != "NA"), aes(x=new.date, y=monthly_predicted_lfm)) +
  #geom_rect(aes(ymin=60, ymax=110, xmin = as.Date("04-12-2018",format = "%m-%d-%Y"), xmax = as.Date("04-26-2018",format = "%m-%d-%Y")),alpha =  0.1, fill = "pink", inherit.aes = FALSE) +
  geom_point(size=4) +
  geom_line(size=2) +
  ylab("Live Fuel Moisture") +
  xlab("Date") +
  scale_x_date(breaks = "1 month", date_labels="%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        axis.line = element_line (size=2))+ 
  scale_y_continuous(expand=c(0,0)) +
  annotate("text", x=as.Date("10-10-2017", format="%m-%d-%Y"), y=105, label="Rhea and Complex 34 Fires", size= 8, col="red")+
  annotate("rect", xmin=as.Date("04-11-2018",format = "%m-%d-%Y"), xmax = as.Date("04-26-2018",format = "%m-%d-%Y"),
           ymin=40, ymax=110,
           alpha=.5,
           fill="red")+
  geom_point(aes(x=as.Date("04-15-2018", format="%m-%d-%Y"), y=77.32), shape=17, size=4, color="purple")+
  annotate("rect", xmin=as.Date("01-01-2017",format = "%m-%d-%Y"), xmax = as.Date("09-01-2017",format = "%m-%d-%Y"),
           ymin=52, ymax=62,
           size=2,
           alpha=.5,
           fill="white",
           color="black")+
  annotate('text', x=as.Date("03-09-2017", format = "%m-%d-%Y"), y=65, label="Legend", size=10, color="black")+
  geom_point(aes(x=as.Date("01-10-2017", format="%m-%d-%Y"), y=60), shape=17, size=4, color="purple")+
  annotate("text", x=as.Date("04-10-2017", format="%m-%d-%Y"), y=60, label= "Observed Points", color="purple", size=8)+
  geom_point(aes(x=as.Date("01-10-2017", format="%m-%d-%Y"), y=55), size=4, color="black")+
  annotate("text", x=as.Date("04-10-2017", format="%m-%d-%Y"), y=55, label= "Predicted Points", color="black", size=8)+
  geom_point(aes(x=as.Date("03-04-2018", format="%m-%d-%Y"), y=86.54), shape=17, size=4, color="purple")+
  geom_point(aes(x=as.Date("05-25-2018", format="%m-%d-%Y"), y=68.99), shape=17, size=4, color="purple")+
  geom_point(aes(x=as.Date("06-27-2018", format="%m-%d-%Y"), y=70.27), shape=17, size=4, color="purple")+
  geom_point(aes(x=as.Date("08-16-2018", format="%m-%d-%Y"), y=101.24), shape=17, size=4, color="purple")+
  geom_point(aes(x=as.Date("04-15-2018", format="%m-%d-%Y"), y=77.32), shape=17, size=4, color="purple")+
  #geom_point(aes(x=as.Date("10-22-2017", format="%m-%d-%Y"), y=109.52), shape=17, size=4, color="purple")+
  geom_point(aes(x=as.Date("12-10-2017", format="%m-%d-%Y"), y=86.22), shape=17, size=4, color="purple")+
  geom_point(aes(x=as.Date("02-03-2018", format="%m-%d-%Y"), y=91.56), shape=17, size=4, color="purple")+
  geom_point(aes(x=as.Date("06-27-2018", format="%m-%d-%Y"), y=79.39), shape=17, size=4, color="purple")+
  annotate("segment", x = as.Date("10-10-2017", format="%m-%d-%Y"), xend = as.Date("04-11-2018", format="%m-%d%Y"), y = 104, yend = 90, colour = "red", size=5, alpha=0.6, arrow=arrow())
