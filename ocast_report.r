
library(ClimInd)
library(ggplot2)
spec.data<-read.csv("new.indices.csv", header=T)
head(spec.data)

henry.theme<-theme(axis.text.x = element_text(size=30),
                     axis.text.y = element_text(size = 30),
                     axis.title.x = element_text(size=35),
                     axis.title.y = element_text(size=35),
                     axis.line = element_line (size=2))



### Spec Done
ggplot(data=spec.data)+
  geom_point(aes(x=lfm, y=ndvi), color="black", size=4)+
  geom_smooth(aes(x=lfm, y=ndvi), color="black", method=lm, size=2)+
  xlab("Live Fuel Moisture")+
  ylab("Spectral Index")+
  annotate("text",  x= 55, y=.95, label="NDVI", size=10, color="black")+
  henry.theme+
  annotate("text", x= 55, y= .87, label=ndvi.r2, size=8)+
  scale_y_continuous(breaks=c(0.00, 0.25, .50, 0.75, 1.00), limits= c(0.00, 1.00))

### Alex spec

ggplot(data=spec.data, aes(x=ndvi, y=ndwi))+
  geom_point(size=4)+
  geom_smooth(method=lm, size=2)+
  scale_y_continuous(breaks=c(0.00, 0.25, .50, 0.75, 1.00), limits=c(0.00, 1.00))+
  scale_x_continuous(breaks=c(0.00, 0.25, .70 ,0.75, .80, .85), limit=c(0.7, .85))+
  henry.theme
  
fit.alex<-lm(ndwi ~ ndvi, data=spec.data)
summary(fit.alex)

#### both ndvi and ndwi
ggplot(data=spec.data)+
  geom_point(aes(x=lfm, y=ndvi), color="black", size=4)+
  geom_smooth(aes(x=lfm, y=ndvi), color="black", method=lm, size=2)+
  geom_point(aes(x=lfm, y=ndwi), color="red", size=4)+
  geom_smooth(aes(x=lfm, y=ndwi), color="red", method=lm, size=2)+
  xlab("Live Fuel Moisture")+
  ylab("Spectral Index")+
  annotate("text",  x= 77, y=.29, label="NDWI", size=10, color="red")+
  annotate("text",  x= 55, y=.95, label="NDVI", size=10, color="black")+
  henry.theme+
  annotate("text", x= 55, y= .87, label=ndvi.r2, size=8)+
  annotate("text", x= 77, y= .2, label=ndwi.r2, size=8, color="red")+
  scale_y_continuous(breaks=c(0.00, 0.25, .50, 0.75, 1.00), limits= c(0.00, 1.00))



  
ndvi.r2<-expression(paste("", R^2, "=.859"))
ndwi.r2<-expression(paste("", R^2, "=.813"))

soil_y<-read.csv("new_soil_water_potential.csv", header=T)

                                
ggplot(data=soil_y)+
  geom_point(aes(x=soil_25_y, y=leaf_y), color="black")+
  geom_smooth(aes(x=soil_25_y, y=leaf_y), color="black")


soil_y<-read.csv("new_soil_water_potential.csv", header=T)
ggplot(data=soil_y)+
  geom_point(aes(x=soil_60_y, y=leaf_y), color="black")+
  geom_smooth(aes(x=soil_60_y, y=leaf_y), color="black")

soil_y<-read.csv("new_soil_water_potential.csv", header=T)
ggplot(data=soil_y)+
  geom_point(aes(x=avg_soil_y, y=leaf_y), color="black")+
  geom_smooth(aes(x=avg_soil_y, y=leaf_y), color="black", method=lm, formula= y~ x)


##avg soil y
soil_y<-read.csv("new_soil_water_potential.csv", header=T)
ggplot(data=soil_y)+
  geom_point(aes(x=avg_soil_y, y=mean_leaf_y), color="black")+
  geom_smooth(aes(x=avg_soil_y, y=mean_leaf_y), color="black", method=lm, formula= (y ~ x))

##5cm soil y
soil_y<-read.csv("new_soil_water_potential.csv", header=T)
ggplot(data=soil_y)+
  geom_point(aes(x=soil_5_y, y=mean_leaf_y), color="black")+
  geom_smooth(aes(x=soil_5_y, y=mean_leaf_y), color="black", method=lm, formula= (y ~ x))

##25 cm
ggplot(data=soil_y)+
  geom_point(aes(x=soil_25_y, y=mean_leaf_y), color="black")+
  geom_smooth(aes(x=soil_25_y, y=mean_leaf_y), color="black", method=lm, formula= (y ~ x))



##### Multiple Regression and Plots

soil_y<-read.csv("new_soil_water_potential.csv", header=T)
soil_y$vpd_kpa<-soil_y$vpd*.1


#### lfm ~ vpd no jitter
ggplot(data=soil_y)+
  geom_point(aes(x=vpd, y=lfm), color="black", size=4)+
  geom_smooth(aes(x=vpd, y=lfm), color="black", method=lm, size=2, formula= (y ~ x))+
  xlab("Vapor Pressure Deficit (mb)")+
  ylab("Live Fuel Moisture (%)")+
  henry.theme+
  xlim(0,20)+
  scale_x_continuous(breaks = seq(0, 20, by=5))+ 
  xlim(0,18)+
  scale_y_continuous(breaks = seq(0, 160, by=20))
  
### lfm ~ vpd jittered
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


### lfm ~ soil y no jitt
ggplot(data=soil_y)+
  geom_point(aes(x=avg_soil_y, y=lfm), color="black", size=4)+
  geom_smooth(aes(x=avg_soil_y, y=lfm), color="black", method=lm, formula= (y ~ x), size=2)+
  xlab("Soil Water Potential (-mpa)")+
  ylab("Live Fuel Moisture (%)")+
  henry.theme+
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
 

help(xla)
fit.1<-lm(mean_lfm ~ vpd + avg_soil_y, data=soil_y)
summary(fit.1)


fit.2<-lm(mean_lfm ~ vpd, data=soil_y)
summary(fit.2)

fit.3<-lm(mean_lfm ~ avg_soil_y, data=soil_y)
summary(fit.3)

wood.data<-read.csv("all_wood_data.csv", header=T)
wood.data.sub<-subset(wood.data, TMAX != "na" & RAIN !="na")
na.omit(wood.data)
length(wood.data$TMAX)
length(wood.data$RAIN)

install.packages("ClimInd")
library(ClimInd)
install.packages("weathermetrics")

?kbdi
??kbdindex
help(kbdindex)
help(strptime)
wood.data$date<-as.Date(paste(wood.data$YEAR, wood.data$MONTH, wood.data$DAY, sep="/"))
head(wood.data)
wood.data$temp.c<-fahrenheit.to.celsius(wood.data$TMAX, round=2)
head(wood.data)
wood.data$rainfall.mm<-wood.data$RAIN * 25.4

soil.y<-read.csv("new_soil_water_potential.csv", header=T)
ggplot(data=soil.y, aes(x=kbdi, y=lfm))+
  geom_point(position=position_jitter(w=100, h=0), size=4)+
  henry.theme+
  xlim(5, 500)+
  scale_y_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140), limits = c(20, 140))+
  xlab("KBDI")+ ylab("Live Fuel Moisture (%)")

## no jitter
ggplot(data=soil.y, aes(x=kbdi, y=lfm))+
  geom_point(size=4)+
  geom_smooth(method=lm, fullrange=T, size=2)+
  henry.theme+
  xlim(5, 500)+
  scale_y_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140), limits = c(20, 140))+
  xlab("KBDI")+ ylab("Live Fuel Moisture (%)")


help("scale_y_continuous")

fit.kbdi<-lm(mean_lfm ~ kbdi, data=soil.y)
summary(fit.kbdi)

### Multiple regression model
#### y = 113.5667 - 1.8759(vpd) - 9.5155(avg_soil_y) 


##### All Sites Model
### Done



ggplot(data=soil_y)+
  geom_point(aes(x=leaf_y, y=lfm), color= "black", alpha=.3, size=4)+
  geom_smooth(aes(x=leaf_y, y=lfm), color="black", method=lm, formula = y~ log(x), size=2)+
  geom_hline(yintercept = 80, color= "red", linetype = "dashed", size=2)+
  geom_hline(yintercept = 60, color= "red", linetype = "dashed", size=2)+
  xlab("Leaf Water Potential (-MPa)")+
  ylab("Live Fuel Moisture (%)")+
  xlim(0.8,5)+
  ylim(49,140)+
  henry.theme+
  scale_y_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160))


leaf.y.data<-subset(soil_y, leaf_y != NA)
length(leaf.y.data$leaf_y)


#### Predictions


library(scales)

install.packages("dplyr")
library(dplyr)


predictions.data<-read.csv("wood_mesonet_data.csv", header=T)
predictions.data$jeffdate <- paste(predictions.data$MONTH, "-", predictions.data$DAY, "-", predictions.data$YEAR, sep ="")
predictions.data$new.date<-as.Date(predictions.data$jeffdate, format = "%m-%e-%Y")
### super jeff equation == y = 113.5667 - 1.8759(vpd) - 9.5155(avg_soil_y) 
predictions.data$daily.lfm <- 113.5667 - (1.8759*predictions.data$VDEF) - (9.5155*predictions.data$avg_soil_y)
head(predictions.data)

# 
# ?sapply
# sapply(predictions.data$Date,1,function(predictions.data) datefun(predictions.data))
# sapply(predictions.data, class)
# 
# 
# newer.date<-seq(as.Date("2017-01-01"), as.Date("2019-08-31"), by="days")
# predictions.data["newer.date"]<-newer.date
# head(predictions.data)
# 
# ggplot(predictions.data, aes(x=new.date, y=predicted_lfm))+ geom_point() +
#   geom_line(aes(x=new.date, y=predicted_lfm))+
#   scale_x_date(labels= date_format("%d-%m-%y"))+
#   ylim(0, 150)

#### BILL STARTED MESSING WITH CODE HERE
library(cowplot)
theme_set(theme_cowplot())
#  predictions.data$new.date <- as.POSIXct(predictions.data$Date, format = "m-%d-%Y")
# # predictions.data$new.date.xlt <- as.POSIXlt(predictions.data$new.date, format = "%Y-%m-%d")
# #monthly plot
ggplot(data=subset(predictions.data, monthly_predicted_lfm != "NA"), aes(x=new.date, y=monthly_predicted_lfm))+
  geom_point() +
  geom_line() +
  ylab("Monthly Predicted LFM") +
  xlab("Date") +
  # scale_x_datetime(breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

predictions.data.subset <- subset(predictions.data,
                                                predicted_lfm != "NA",
                                                predicted_lfm != "!")

predictions.data.subset$predicted_lfm <- as.numeric(predictions.data.subset$predicted_lfm)

###shitty date plot
ggplot(data=predictions.data.subset, aes(x=new.date, y=predicted_lfm))+
  geom_point() +
  geom_line() +
  ylab("Monthly Predicted LFM") +
  xlab("Date") +
  # scale_x_datetime(breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))




###Time Series Done
### chart annotation example
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

  
pdf("one month time series.pdf", width = 21, height =17)
#now run plot
dev.off()
##3Rhea Fire Done
### RHEA FIRE version of shitty date plot
ggplot(data=predictions.data, aes(x=new.date, y=daily.lfm))+
  geom_point(size=4) +
  geom_line(size=2) +
  ylab("Live Fuel Moisture") +
  xlab("Date") +
  # scale_x_datetime(breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size= 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        axis.line = element_line (size=1))+
  xlim(c(as.Date("02-12-2018",format = "%m-%d-%Y"),as.Date("04-26-2018",format = "%m-%d-%Y"))) +
  geom_hline(yintercept=80, col= "red", linetype = "dashed", size=2)+
  geom_vline(xintercept=as.Date("04-12-2018", format="%m-%d-%Y"), col="red", linetype="dashed",size=2)+
  geom_vline(xintercept=as.Date("04-26-2018", format="%m-%d-%Y"), col="red", linetype="dashed", size=2)+
  geom_vline(xintercept=as.Date("04-11-2018", format="%m-%d-%Y"), col="blue", linetype="dashed",size=2)+
  geom_vline(xintercept=as.Date("04-23-2018", format="%m-%d-%Y"), col="blue", linetype="dashed", size=2)+
  annotate("text", x=as.Date("04-07-2018", format="%m-%d-%Y"), y=55, label="Rhea Fire", size=10, color="red")+
  annotate("text", x=as.Date("04-03-2018", format="%m-%d-%Y"), y=40, label="Complex 34 Fire", size=10, color="blue")+
  geom_point(aes(x=as.Date("04-15-2018", format="%m-%d-%Y"), y=77.32), shape=17, size=4, color="purple")+
  annotate("rect", xmin=as.Date("02-16-2018",format = "%m-%d-%Y"), xmax = as.Date("03-12-2018",format = "%m-%d-%Y"),
           ymin=45, ymax=65,
           size=2,
           alpha=.5,
           fill="white",
           color="black")+
  annotate('text', x=as.Date("02-22-2018", format = "%m-%d-%Y"), y=68, label="Legend", size=10, color="black")+
  geom_point(aes(x=as.Date("02-18-2018", format="%m-%d-%Y"), y=60), shape=17, size=4, color="purple")+
  annotate("text", x=as.Date("02-28-2018", format="%m-%d-%Y"), y=60, label= "Observed Points", color="purple", size=8)+
  geom_point(aes(x=as.Date("02-18-2018", format="%m-%d-%Y"), y=50), size=4, color="black")+
  annotate("text", x=as.Date("02-28-2018", format="%m-%d-%Y"), y=50, label= "Predicted Points", color="black", size=8)
  
  
##4/15, lfm=77.83

 #annotate("rect", xmin=as.Date("04-12-2018",format = "%m-%d-%Y"), xmax = as.Date("04-26-2018",format = "%m-%d-%Y"),
           #ymin=35, ymax=80,
           #alpha=.5,
           #fill="red")
 
  
 

  
  ggplot(data=predictions.data, aes(x=new.date, y=daily.lfm))+
  geom_point() +
  geom_line() +
  ylab("Monthly Predicted LFM") +
  xlab("Date") +
  # scale_x_datetime(breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlim(c(as.Date("03-06-2017",format = "%m-%d-%Y"), as.Date("03-26-2017",format = "%m-%d-%Y"))) +
  geom_hline(yintercept=80, col= "red", linetype = "dashed")+
  geom_vline(xintercept = as.Date("04-12-2017",format = "%m-%d-%Y"), col="red", linetype= "solid")
  
  
  

#rhea.fire<-read.csv("rhea_fire.csv", header=T)
#rhea.fire$new.date<-as.Date(rhea.fire$Date)


#ggplot(data=rhea.fire)+
 # geom_point(aes(x=new.date, y=predicted_lfm))+
  #scale_x_date(labels= date_format("%d-%m-%y"))
                        

#qplot(new.date, monthly_predicted_lfm, data = predictions.data, geom="line", ylab = "Predicted LFM (%)") +
#  scale_x_datetime(date_breaks = "2 day", labels = date_format("%d-%m--%y"))

head(predictions.data)
ggplot(data=predictions.data, aes(x=daily.lfm, y=mean_lfm))+
  geom_point()+
  geom_smooth(method="lm")
  
#validation

all.data<-read.csv("new_soil_water_potential.csv", header=T)
head(all.data)

library(dplyr)
help(sample_n)
val.sub<-sample_n(all.data, 20)

all.data$daily.pred.lfm<-113.5667 - (1.8759*all.data$vpd) - (9.5155*all.data$avg_soil_y)

validation.fit.1<-lm(lfm ~ daily.pred.lfm, data=val.sub)
summary(validation.fit.1)

ggplot(data=val.sub, aes(x=daily.pred.lfm, y=lfm))+
  geom_point()+
  geom_smooth(method="lm")
