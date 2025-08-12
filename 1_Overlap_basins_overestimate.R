# Code by K. Magoulick 6/24/25
# R version 4.4.1 (Race for Your Life)

library(sf)       #sf v.1.0-16 
library(spdep)    #spdep v.1.0-16
library(readr)    #readr v.2.1.5      
library(dplyr)    #dplyr v.1.1.4 

# turn off spherical geometry
sf_use_s2(FALSE)

setwd("/Users/Marshall_Lab/Desktop/Chapter_3/Boerker_et_al_GUM_v1.0/GUM_v1.0")

GUM = st_read("GUM_v1.0.shp") #read in unconsolidated sediments shapefile

# Filter out ‘Ice and Glaciers’ and ‘Glacial’ classes (including proglacial, till, and undifferentiated) as per Galván et al. 2025
GUM_filt <- GUM %>% 
  filter(!XX %in% c("Du", "Gu", "Gp", "Gt"))

#fix any invalid geometries
GUM_filt_valid <- st_make_valid(GUM_filt)

#crop to western hemisphere so it hopefully doesn't crash
GUM_crop <- st_crop(GUM_filt_valid, c(xmin = -180, xmax = 0, ymin = -90, ymax = 90))

#get occurrence data
occurrences<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv") #get occurrences

occurrences$id <- 1:nrow(occurrences) 

occurrences_new = st_as_sf(occurrences, 
                           coords = c("LONG", "LAT"), 
                           crs = st_crs(4326))                               #convert to SF

occurrences_new2 = st_transform(occurrences_new, crs = st_crs(GUM_crop)) #convert to correct CRS

overlap = st_intersection(occurrences_new2, GUM_crop) #intersect unconsolidated sediments and points

plot(overlap)

setwd("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data")

#Save data
write_csv(overlap, "modern_climate_contiguous_US_BASIN_OVERLAP.csv")