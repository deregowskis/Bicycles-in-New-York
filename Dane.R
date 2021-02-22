library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggrepel)

styczen2018 <- read.csv("201801-citibike-tripdata.csv")
luty2018 <- read.csv("201802-citibike-tripdata.csv")
marzec2018 <- read.csv("201803-citibike-tripdata.csv")
kwiecien2018 <- read.csv("201804-citibike-tripdata.csv")
maj2018 <- read.csv("201805-citibike-tripdata.csv")
czerwiec2018 <- read.csv("201806-citibike-tripdata.csv")
lipiec2018 <- read.csv("201807-citibike-tripdata.csv")
sierpien2018 <- read.csv("201808-citibike-tripdata.csv")
wrzesien2018 <- read.csv("201809-citibike-tripdata.csv")
pazdziernik2018 <- read.csv("201810-citibike-tripdata.csv")
listopad2018 <- read.csv("201811-citibike-tripdata.csv")
grudzien2018 <- read.csv("201812-citibike-tripdata.csv")

dane <- kwiecien2018

#rozk³ad spo³eczeñstwa
pon1 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-09",birth.year>=1997))
wto1 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-10",birth.year>=1997))
sro1 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-11",birth.year>=1997))
czw1 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-12",birth.year>=1997))
pia1 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-13",birth.year>=1997))
sob1 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-14",birth.year>=1997))
nie1 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-15",birth.year>=1997))

pon2 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-09",birth.year<1997,birth.year>=1952))
wto2 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-10",birth.year<1997,birth.year>=1952))
sro2 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-11",birth.year<1997,birth.year>=1952))
czw2 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-12",birth.year<1997,birth.year>=1952))
pia2 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-13",birth.year<1997,birth.year>=1952))
sob2 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-14",birth.year<1997,birth.year>=1952))
nie2 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-15",birth.year<1997,birth.year>=1952))

pon3 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-09",birth.year<1952))
wto3 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-10",birth.year<1952))
sro3 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-11",birth.year<1952))
czw3 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-12",birth.year<1952))
pia3 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-13",birth.year<1952))
sob3 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-14",birth.year<1952))
nie3 <- nrow(filter(dane,as.Date(dane$starttime)=="2018-04-15",birth.year<1952))

#wiek przedprodukcyjny
plot.new()
plot.window(c(1,7), c(400, 1600))
points(1:7, c(pon1,wto1,sro1,czw1,pia1,sob1,nie1),col='blue',lwd=5,pch=19)
box()
axis(1, at = 1:7)
axis(2, seq(from=400,to=1600,by=400), las = 1)
title("Spo³eczeñstwo w wieku przedprodukcyjnym")
#wiek produkcyjny
plot.new()
plot.window(c(1,7), c(200, 80000))
points(1:7, c(pon2,wto2,sro2,czw2,pia2,sob2,nie2),col='red',lwd=5,pch=19)
box()
axis(1, at = 1:7)
axis(2, seq(from=0,to=80000,by=10000), las = 1)
title("Spo³eczeñstwo w wieku produkcyjnym")
#wiek poprodukcyjny
plot.new()
plot.window(c(1,7), c(200, 1000))
points(1:7, c(pon3,wto3,sro3,czw3,pia3,sob3,nie3),col='green',lwd=5,pch=19)
box()
axis(1, at = 1:7)
axis(2, seq(from=200,to=1000,by=200), las = 1)
title("Spo³eczeñstwo w wieku poprodukcyjnym")


#dane dotycz¹ce rozk³adu wieku
dane <- dane %>% group_by(birth.year) %>% summarize(n=n())
plot.new()
plot.window(c(1885,2018),c(0,60000))
points(dane$birth.year,dane$n,col='black',lwd=5,pch=19)
box()
axis(1,)


plot(dane$birth.year,dane$n)

#dane dotycz¹ce pocz¹tku/koñca podró¿y w poszczególnych stacjach
dane_start <- select(dane,start.station.latitude,start.station.longitude)
dane_start <- rename(dane_start,Latitude=start.station.latitude,Longitude=start.station.longitude)
dane_end <- select(dane,end.station.latitude,end.station.longitude)
dane_end <- rename(dane_end,Latitude=end.station.latitude,Longitude=end.station.longitude)
dane <- rbind(dane_start,dane_end)
dane <- dane %>% group_by(Latitude,Longitude) %>% summarize(n=n()) %>% arrange(-n)
dane <- filter(dane,Latitude<44) #pomijamy stacjê wysuniêt¹ najdalej na pó³nocny wschód, bo psuje wykres 
rysunek <- ggplot(dane, aes(x= Latitude, y= Longitude, label=n))+ geom_point(aes(colour = -n),size=4)
rysunek_etykiety <- (rysunek + geom_label_repel(aes(label = n), box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50') + theme_classic())

# wykresy:
#
#
library(data.table)
citybike<-data.table::fread("201808-citibike-tripdata.csv")

citybike[,starttime_posix:=(as.POSIXct(starttime))][  #dodaje kolumne z godzina wyporzyczenia w formacie POSIX
  ,stoptime_posix:=(as.POSIXct(stoptime))            #dodaje kolumne z godzina oddania w formacie POSIX
  ]

citybike[,starthour:=as.numeric(format(starttime_posix,"%H"))][  #dodaje kolumne z godzina wyporzyczenia
  ,stophour:=as.numeric(format(stoptime_posix,"%H"))]         #dodaje kolumne z godzina oddania ]

citybike[,.(count = .N, sum = sum(tripduration),mean = mean(tripduration)), by = starthour]->hour_summary_number_hire
godzina_wyporzyczenia<-hour_summary_number_hire$starthour
srednia_czasu_wyporzyczenia<-hour_summary_number_hire$mean


barplot(srednia_czasu_wyporzyczenia,xlab = "godzina wypoÅ¼yczenia",names.arg = godzina_wyporzyczenia,ylab="srednia czasu wypoÅ¼yczenia")
hist(citybike$starthour,xlab="hour",main="suma czasÃ³w wypoÅ¼yczeÅ„ rozpoczÄ™tych o danej godzinie")


# zapis do plikuku 
png("hisgram.png")
hist(citybike$starthour,xlab="hour",main="suma czasÃ³w wypoÅ¼yczeÅ„ rozpoczÄ™tych o danej godzinie")
dev.off()
png("barplot.png")
barplot(srednia_czasu_wyporzyczenia,xlab = "godzina wypoÅ¼yczenia",names.arg = godzina_wyporzyczenia,ylab="srednia czasu wypoÅ¼yczenia")
dev.off()




# mapka roweru ktÃ³ry odwiedziÅ‚ najwiecej stacji
library(ggplot2)
citybike<-data.table::fread("201808-citibike-tripdata.csv")

unique(citybike,by=c("bikeid","start station id")) ->ct   # zlicza unikatowe kombinacje roweru i stacji 
ct[,.(count = .N), by = bikeid]->x
setorderv(x,cols ="count",order=-1L)
bike<-x[[1,1]]         # id roweru ktÃ³ry odwiedziÅ‚ najwiecej stacji

citybike[bikeid==bike]->bike  # histotia podruzy roweru

#staje    
station<-unique(citybike,by="start station id")

dane<-station<-unique(citybike,by="start station id")
dane<-dane[,.(Latitude=`start station latitude`,Longitude=`start station longitude`,id=`start station id`)]

setorderv(dane,cols=c("Latitude","Longitude"),order=c(-1L,-1L)) #usuwanie odstajÄ…cej stacji(powoduje znaczne zmniejszenie wykresu)

dane<-dane[-1,]

ggplot()+
  geom_point(aes(x= Latitude, y= Longitude),data=dane,size=0.25) +
  geom_path(aes(x=`start station latitude`, y=`start station longitude`),data=bike,colour="blue",size=0.3)



#wypoÅ¼yczenia wiek przedprodukcyjny
#wypoÅ¼yczenia wiek produkcyjny
#wypoÅ¼yczenia wiek poprodukcyjny
citybike<-data.table::fread("201805-citibike-tripdata.csv")


data<-citybike
data[,starttime_posix:=(as.POSIXct(starttime))] #dodaje kolumne z czasem wyporzyczenia w formacie POSIX
data[,starthour:=as.numeric(format(starttime_posix,"%H"))]  #dodaje kolumne z godzina wyporzyczenia
data1<-data[`birth year`>(2018-21)]
data2<-data[`birth year`<(2018-21),][`birth year`>(2018-66),]
data3<-data[`birth year`<(2018-66)]
data1[,.(count = .N, sum = sum(tripduration),mean = mean(tripduration)), by = starthour]->summary1 #podsumowanie
data2[,.(count = .N, sum = sum(tripduration),mean = mean(tripduration)), by = starthour]->summary2
data3[,.(count = .N, sum = sum(tripduration),mean = mean(tripduration)), by = starthour]->summary3
setorderv(summary1,cols ="starthour",order=1L) #sortowanie
setorderv(summary2,cols ="starthour",order=1L)
setorderv(summary3,cols ="starthour",order=1L)

png("przedprodukcyjny.png")
barplot(summary1$count,xlab = "godzina wyporzyczenia",names.arg =summary1$starthour,ylab="iloÅ›Ä‡ wyporzyczeÅ„", main="wypoÅ¼yczenia wiek przedprodukcyjny")
dev.off()
png("produkcyjny.png")
barplot(summary2$count,xlab = "godzina wyporzyczenia",names.arg =summary2$starthour,ylab="iloÅ›Ä‡ wyporzyczeÅ„",main="wypoÅ¼yczenia wiek produkcyjny")
dev.off()
png("poprodukcyjny.png")
barplot(summary3$count,xlab = "godzina wyporzyczenia",names.arg =summary3$starthour,ylab="iloÅ›Ä‡ wyporzyczeÅ„",main="wypoÅ¼yczenia wiek poprodukcyjny")
dev.off()
