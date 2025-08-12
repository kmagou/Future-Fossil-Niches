library(sf)
library(dplyr)
library(readr)

setwd("/Users/Marshall_Lab/Desktop/Chapter 3")

sed_basins = st_read("Sed_BasinsV1.0/Basins.shp") #read in basins shapefile

occurrences<-read.csv("/Users/Marshall_Lab/Desktop/Chapter 3/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv") #get occurrences

occurrences$id <- 1:nrow(occurrences) 

occurrences_new = st_as_sf(occurrences, 
                           coords = c("LONG", "LAT"), 
                           crs = st_crs(4326))                               #convert to SF

occurrences_new2 = st_transform(occurrences_new, crs = st_crs(sed_basins)) #convert to correct CRS

overlap = st_intersection(occurrences_new2, sed_basins) #intersect basins and points

plot(overlap)

setwd("/Users/Marshall_Lab/Desktop/Chapter 3/Kat_data")

write_csv(overlap, "modern_climate_contiguous_US_BASIN_OVERLAP.csv")

overlap = read_csv("modern_climate_contiguous_US_BASIN_OVERLAP.csv")

occ_list = as.list(overlap$id)

df_new <- filter(occurrences, id %in% occ_list)

occ_counts_new = df_new %>%
  group_by(Species) %>%
  summarise(n = n())
