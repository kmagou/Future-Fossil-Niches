# Code by K. Magoulick to create convex hulls August 2025
# R version 4.4.1 (Race for Your Life)

library(sf)     #sf v.1.0-16 
library(tidyr)  #tidyr v.1.3.1
library(dplyr)  #dplyr v.1.1.4 
library(readr)  #readr v.2.1.5  
library(ggplot2)  #ggplot2 v.3.5.1
library(gridExtra)#gridExtra v.2.3

##### Load data 
#Modern data and cleaning
modernData<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv")
AllAnal<-na.omit(modernData)
dataAllAnal<-data.frame(Species=AllAnal$Species,tmean=AllAnal$tmean, pmean=AllAnal$pmean, Age=AllAnal$AgeQual, Lat=AllAnal$LAT, Long=AllAnal$LONG)
dataAllAnal<-droplevels(dataAllAnal[dataAllAnal$Age==as.character("5_Now"),])
dataAllAnal<-dataAllAnal[!duplicated(dataAllAnal[!names(dataAllAnal) %in% c("Lat","Long")]),]

#overestimate and cleaning
basinData1<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data/modern_climate_contiguous_US_BASIN_OVERLAP.csv")
basinAnal1<-na.omit(basinData1)
dataBasinsAnal1<-data.frame(Species=basinAnal1$Species,tmean=basinAnal1$tmean, pmean=basinAnal1$pmean, Age=basinAnal1$AgeQual, site=basinAnal1$siteid)
dataBasinsAnal1<-separate_wider_delim(dataBasinsAnal1, cols = site, delim = "_", names = c("Lat", "Long"))
dataBasinsAnal1<-droplevels(dataBasinsAnal1[dataBasinsAnal1$Age==as.character("5_Now"),])
dataBasinsAnal1<-dataBasinsAnal1[!duplicated(dataBasinsAnal1[!names(dataBasinsAnal1) %in% c("Lat","Long")]),]

#underestimate and cleaning
basinData2<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data/modern_climate_contiguous_US_BASIN_OVERLAP_underestimate.csv")
basinAnal2<-na.omit(basinData2)
dataBasinsAnal2<-data.frame(Species=basinAnal2$Species,tmean=basinAnal2$tmean, pmean=basinAnal2$pmean, Age=basinAnal2$AgeQual, site=basinAnal2$siteid)
dataBasinsAnal2<-separate_wider_delim(dataBasinsAnal2, cols = site, delim = "_", names = c("Lat", "Long"))
dataBasinsAnal2<-droplevels(dataBasinsAnal2[dataBasinsAnal2$Age==as.character("5_Now"),])
dataBasinsAnal2<-dataBasinsAnal2[!duplicated(dataBasinsAnal2[!names(dataBasinsAnal2) %in% c("Lat","Long")]),]

# get counts data
setwd("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data")
counts <- read_csv("species_counts_underestimate.csv")

test<-counts[counts$basins>=20,]
test<-na.omit(test)

filtered_dataAllAnal = dataAllAnal %>%
  filter(Species %in% test$species)

filtered_dataBasinsAnal_over = as.data.frame(dataBasinsAnal1) %>%
  filter(Species %in% test$species)

filtered_dataBasinsAnal_under = as.data.frame(dataBasinsAnal2) %>%
  filter(Species %in% test$species)

#group and summarise by species, and get hulls
hulls_all <- filtered_dataAllAnal %>%
  st_as_sf(coords = c("Long", "Lat"), crs = "WGS84") %>%
  group_by(Species) %>%
  summarize() %>% 
  st_convex_hull()

hulls_all$area_m2 <- st_area(hulls_all$geometry)

hulls_all$area_km2 <- units::set_units(hulls_all$area_m2, km^2)

area_all <- hulls_all %>%
  select(Species, area_km2) %>%
  st_drop_geometry() %>%
  rename(species = Species) %>%
  units::drop_units()

#hulls_all <- hulls_all[, -which(names(hulls_all) == "geometry")]

#hulls_all$area <- as.numeric(as.character(hulls_all$area))

#get overlap data
setwd("/Users/Marshall_Lab/Desktop/Chapter_3")
final_data_over<-read_csv("Similarity_Basins-NoBasins_overestimate.csv")
final_data_under<-read_csv("Similarity_Basins-NoBasins_underestimate.csv")

final_data_over <- final_data_over %>%
  left_join(area_all, by = c("species"))

final_data_under <- final_data_under %>%
  left_join(area_all, by = c("species"))

#linear model
model_over <- lm(Overlap ~ area_km2, data = final_data_over)
summary(model_over)

model_under <- lm(Overlap ~ area_km2, data = final_data_under)
summary(model_under)

#plot
plot(model_over, which = 1)
plot(model_over, which = 2)

(lm_plot_over <- ggplot(final_data_over, aes(x = area_km2, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "springgreen3") +
    labs(x = bquote(Size~of~present~covex~hull~(km^2)), y = "Overlap (Schoener's D)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)) +
    lims(x = c(0, 10050000), y = c(0, 1)))


(lm_plot_under <- ggplot(final_data_under, aes(x = area_km2, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "skyblue3") +
    labs(x = bquote(Size~of~present~covex~hull~(km^2)), y = "Overlap (Schoener's D)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)) +
    lims(x = c(0, 10050000), y = c(0, 1)))

grid.arrange(lm_plot_over, lm_plot_under, ncol = 1)
