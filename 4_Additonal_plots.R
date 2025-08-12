#more plots

library(readr)      #readr v.2.1.5      
library(dplyr)      #dplyr v.1.1.4 
library(ggplot2)    #ggplot2 v.3.5.1
library(maps)

setwd("/Users/Marshall_Lab/Desktop/Chapter_3")

#plot pliocene fossil occurrences
plio <- read_csv("https://paleobiodb.org/data1.2/occs/list.csv?datainfo&rowcount&base_name=Mammalia&interval=Pliocene,Pliocene&cc=US&envtype=terrestrial&pgm=gplates&show=full", skip = 21)

usa <- map_data("usa")

ggplot(data=plio, aes(x = lng, y = lat)) + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='lightgrey') + 
  geom_point(color="#111D4A") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim = c(-130, -65), ylim = c(20, 50))

#38369A, #EAC435

#overestimate
basinData1<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data/modern_climate_contiguous_US_BASIN_OVERLAP.csv")
basinAnal1<-na.omit(basinData1)
dataBasinsAnal1<-data.frame(Species=basinAnal1$Species,tmean=basinAnal1$tmean, pmean=basinAnal1$pmean, Age=basinAnal1$AgeQual, site=basinAnal1$siteid)
dataBasinsAnal1<-separate_wider_delim(dataBasinsAnal1, cols = site, delim = "_", names = c("Lat", "Long"))
dataBasinsAnal1<-droplevels(dataBasinsAnal1[dataBasinsAnal1$Age==as.character("5_Now"),])
dataBasinsAnal1<-dataBasinsAnal1[!duplicated(dataBasinsAnal1[!names(dataBasinsAnal1) %in% c("Lat","Long")]),]
dataBasinsAnal_over = dataBasinsAnal1 %>%
  filter(Species %in% c("Puma concolor","Dipodomys spectabilis"))

modernData2<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv")
AllAnal2<-na.omit(modernData2)
dataAllAnal2<-data.frame(Species=AllAnal2$Species,tmean=AllAnal2$tmean, pmean=AllAnal2$pmean, Age=AllAnal2$AgeQual, Lat=AllAnal2$LAT, Long=AllAnal2$LONG)
dataAllAnal2<-droplevels(dataAllAnal2[dataAllAnal2$Age==as.character("5_Now"),])
dataAllAnal2<-dataAllAnal2[!duplicated(dataAllAnal2[!names(dataAllAnal2) %in% c("Lat","Long")]),]
dataAllAnal_1 = dataAllAnal2 %>%
  filter(Species %in% c("Puma concolor","Dipodomys spectabilis"))

data_puma_est <- dataBasinsAnal1 %>% filter(Species == "Puma concolor")
data_puma_all <- dataAllAnal_1 %>% filter(Species == "Puma concolor")

ggplot() + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='lightgrey') + 
  geom_point(data=data_puma_all, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", shape=21) +
  geom_point(data=data_puma_est, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", fill="yellow", shape=21, size=2) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


data_dipodomys_est <- dataBasinsAnal1 %>% filter(Species == "Dipodomys spectabilis")
data_dipodomys_all <- dataAllAnal_1 %>% filter(Species == "Dipodomys spectabilis")

ggplot() + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='lightgrey') + 
  geom_point(data=data_dipodomys_all, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", shape=21) +
  geom_point(data=data_dipodomys_est, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", fill="yellow", shape=21, size=2) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


#underestimate
basinData2<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data/modern_climate_contiguous_US_BASIN_OVERLAP_underestimate.csv")
basinAnal2<-na.omit(basinData2)
dataBasinsAnal2<-data.frame(Species=basinAnal2$Species,tmean=basinAnal2$tmean, pmean=basinAnal2$pmean, Age=basinAnal2$AgeQual, site=basinAnal2$siteid)
dataBasinsAnal2<-separate_wider_delim(dataBasinsAnal2, cols = site, delim = "_", names = c("Lat", "Long"))
dataBasinsAnal2<-droplevels(dataBasinsAnal2[dataBasinsAnal2$Age==as.character("5_Now"),])
dataBasinsAnal2<-dataBasinsAnal2[!duplicated(dataBasinsAnal2[!names(dataBasinsAnal2) %in% c("Lat","Long")]),]
dataBasinsAnal_under = dataBasinsAnal2 %>%
  filter(Species %in% c("Sorex vagrans","Geomys personatus"))

dataAllAnal_2 = dataAllAnal2 %>%
  filter(Species %in% c("Sorex vagrans","Geomys personatus"))


data_sorex_est <- dataBasinsAnal2 %>% filter(Species == "Sorex vagrans")
data_sorex_all <- dataAllAnal_2 %>% filter(Species == "Sorex vagrans")

ggplot() + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='lightgrey') + 
  geom_point(data=data_sorex_all, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", shape=21) +
  geom_point(data=data_sorex_est, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", fill="yellow", shape=21, size=2) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


data_geomys_est <- dataBasinsAnal2 %>% filter(Species == "Geomys personatus")
data_geomys_all <- dataAllAnal_2 %>% filter(Species == "Geomys personatus")

ggplot() + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='lightgrey') + 
  geom_point(data=data_geomys_all, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", shape=21) +
  geom_point(data=data_geomys_est, aes(x = as.numeric(Long), y = as.numeric(Lat)), color="black", fill="yellow", shape=21, size=2) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
