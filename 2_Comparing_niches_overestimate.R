# Adapted by K. Magoulick 4/8/25 from "4_Comparing_niches_Pre_Post"
# Original code by Silvia Pineda-Munoz; 19 August 2019
# R version 4.4.1 (Race for Your Life)

#  packages
library(ecospat)    #ecospat v.4.1.1
library(readr)      #readr v.2.1.5      
library(dplyr)      #dplyr v.1.1.4 
library(ggplot2)    #ggplot2 v.3.5.1
library(ggnewscale) #ggnewscale v.0.5.0
library(maps)
library(taxize)     #taxize v.0.10.0
library(tidyr)      #tidyr v.1.3.1


# working directory
setwd("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data")

##### Load data 
#Basin overlap data
basinData<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data/modern_climate_contiguous_US_BASIN_OVERLAP.csv")
basinAnal<-na.omit(basinData)

#Modern data
modernData<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/modern_climate_contiguous_US.csv")
AllAnal<-na.omit(modernData)

#Background climate data
backData<-read.csv("/Users/Marshall_Lab/Desktop/Chapter_3/2019_03_31_GIS_Time_corrected analysis/datasets/Data_Analysis_files/Contiguous_US/background_modern_contiguous_US.csv")

backData<-data.frame(backData,
                    AgeQual=rep("5_Now",times=nrow(backData)))

##### Final data cleaning
BG<-unique(data.frame(tmean=backData$tmean, pmean=backData$pmean,Age=backData$AgeQual))
BG<-na.omit(backData)
BG<-unique(data.frame(tmean=round(BG$tmean),pmean=round(BG$pmean),
                      Age=BG$Age))

dataAllAnal<-data.frame(Species=AllAnal$Species,tmean=AllAnal$tmean, pmean=AllAnal$pmean, Age=AllAnal$AgeQual)
dataAllAnal<-droplevels(dataAllAnal[dataAllAnal$Age==as.character("5_Now"),])
dataAllAnal<-(unique(dataAllAnal))

dataBasinsAnal<-data.frame(Species=basinAnal$Species,tmean=basinAnal$tmean, pmean=basinAnal$pmean, Age=basinAnal$AgeQual)
dataBasinsAnal<-droplevels(dataBasinsAnal[dataBasinsAnal$Age==as.character("5_Now"),])
dataBasinsAnal<-(unique(dataBasinsAnal))

# This does a final clean and leaves only species with min 20 specimens in Basins
BasinsSpecies<-as.data.frame.matrix(table(data.frame(dataBasinsAnal$Species,dataBasinsAnal$Age)))
AllSpecies<-as.data.frame.matrix(table(data.frame(dataAllAnal$Species,dataAllAnal$Age)))

subSpecies <- merge(BasinsSpecies, AllSpecies, by=0, all=TRUE)
colnames(subSpecies)<-c("species","basins","all")

#write_csv(subSpecies, "species_counts_overestimate.csv")
under_subSpecies <- read_csv("species_counts_underestimate.csv")

counts_combine <- subSpecies %>% 
  left_join(under_subSpecies, by = "species")

test<-counts_combine[counts_combine$basins.x>=20 & counts_combine$basins.y>=20,]
test<-na.omit(test)
test
nrow(test)
speciesAnal<-test$species
speciesAnal.df <- data.frame(speciesAnal)

dataBasinsAnal = dataBasinsAnal %>%
  filter(Species %in% speciesAnal.df$speciesAnal)

dataAllAnal = dataAllAnal %>%
  filter(Species %in% speciesAnal.df$speciesAnal)

##### Calculate changes with niche overlap, similarity, and equivalency

setwd("/Users/Marshall_Lab/Desktop/Chapter_3/Kat_data/Overlap_results_D_overestimate")

#data for the analysis
overallData<-(rbind(BG[,1:2],dataAllAnal[,2:3]))
dataAll<-(rbind(BG[,1:2],dataAllAnal[,2:3]))
dataBasins<-rbind(BG[,1:2],dataBasinsAnal[,2:3])

#Arrays necessary
Overlap<-array(dim=length(speciesAnal))
similSp<-array(dim=length(speciesAnal))

equivSp<-array(dim=length(speciesAnal))

persInSp<-array(dim=length(speciesAnal))

Overlap<-array(dim=length(speciesAnal))
Expansion<-array(dim=length(speciesAnal))
Stability<-array(dim=length(speciesAnal))
Unfilling<-array(dim=length(speciesAnal))
ExpansionSp<-array(dim=length(speciesAnal))
StabilitySp<-array(dim=length(speciesAnal))
UnfillingSp<-array(dim=length(speciesAnal))

max_MAT_mod<-array(dim=length(speciesAnal))
min_MAT_mod<-array(dim=length(speciesAnal))
max_MAT_fos<-array(dim=length(speciesAnal))
min_MAT_fos<-array(dim=length(speciesAnal))

max_MAP_mod<-array(dim=length(speciesAnal))
min_MAP_mod<-array(dim=length(speciesAnal))
max_MAP_fos<-array(dim=length(speciesAnal))
min_MAP_fos<-array(dim=length(speciesAnal))

for (i in 1:length(speciesAnal)){
  
  subsetAll1<-droplevels(dataAllAnal[dataAllAnal$Species==as.character(speciesAnal[i]),])
  subsetBasins1<-droplevels(dataBasinsAnal[dataBasinsAnal$Species==as.character(speciesAnal[i]),])
  
  gridAll<-ecospat.grid.clim.dyn (glob=overallData, 
                                  glob1=dataAll,
                                  sp=subsetAll1[,2:3], R=50)
  
  gridBasins<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=dataBasins,
                                     sp=subsetBasins1[,2:3], R=50)
  nulData<-data.frame(pmean=seq(0,5,0.2),tmean=seq(10,15,0.2))
  
  baseData<-ecospat.grid.clim.dyn (glob=overallData, 
                                   glob1=nulData,
                                   sp=nulData, R=50)
  
  Sim<-ecospat.niche.similarity.test(gridAll,gridBasins,100,rand.type = 2)
  similSp[i]<-Sim$p.D
  
  EqL<-ecospat.niche.equivalency.test(gridAll,gridBasins,100)
  equivSp[i]<-EqL$p.D
  
  Overlap[i]<-ecospat.niche.overlap(gridAll,gridBasins,cor=T)$D
  mod<-ecospat.niche.dyn.index(gridAll,gridBasins)
  Expansion[i]<-mod$dynamic.index.w[1]
  Stability[i]<-mod$dynamic.index.w[2]
  Unfilling[i]<-mod$dynamic.index.w[3]
  
  SOverlap<-array(dim=100)
  SsOverlap<-array(dim=100)
  SExpansion<-array(dim=100)
  SStability<-array(dim=100)
  SUnfilling<-array(dim=100)
  
  for(j in 1:100){
    subsetAll<-droplevels(dataAllAnal[dataAllAnal$Species==as.character(speciesAnal[i]),])
    subsetBasins<-dataBasinsAnal[dataBasinsAnal$Species==as.character(speciesAnal[i]),]
    
    subsetAll$Type <- 'All'
    subsetBasins$Type <- 'Basins'
    
    #sizeSub<-min(nrow(subsetAll),nrow(subsetBasins))
    #subsetAll<-subsetAll[sample(nrow(subsetAll),sizeSub),]
    #subsetBasins<-subsetBasins[sample(nrow(subsetBasins),sizeSub),]
    
    subsetAll<-rbind(subsetAll,subsetBasins)
    subsetAll<-data.frame(subsetAll[,1:4],Type=sample(subsetAll$Type))
    SsubsetAll<-droplevels(subsetAll[subsetAll$Type=="All",])
    SsubsetBasins<-droplevels(subsetAll[subsetAll$Type=="Basins",])
    
    All<-(rbind(dataAll,SsubsetAll[,2:3]))
    Basins<-(rbind(dataBasins,SsubsetBasins[,2:3]))
    
    SgridAll<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=All,
                                     sp=SsubsetAll[,2:3], R=50)
    SgridBasins<-ecospat.grid.clim.dyn (glob=overallData, 
                                        glob1=Basins,
                                        sp=SsubsetBasins[,2:3], R=50)
    
    
    SsOverlap[j]<-ecospat.niche.overlap(SgridAll,SgridBasins,cor=T)$D
    mod<-ecospat.niche.dyn.index(gridAll,gridBasins)
    SExpansion[j]<-mod$dynamic.index.w[1]
    SStability[j]<-mod$dynamic.index.w[2]
    SUnfilling[j]<-mod$dynamic.index.w[3]
  }
  
  #Overlap[i]<-mean(SOverlap)
  persInSp[i]<-length(SsOverlap[SsOverlap<=Overlap[i]])/100
  
  ExpansionSp[i]<-length(SExpansion[SExpansion<=Expansion[i]])/100
  
  StabilitySp[i]<-length(SStability[SStability<=Stability[i]])/100
  
  UnfillingSp[i]<-length(SUnfilling[SUnfilling<=Unfilling[i]])/100
  
  ##Plotting
  #This section of code was adapted from code by Marc Riera in [CITATION]
  subsetAll2<-droplevels(dataAllAnal[dataAllAnal$Species==as.character(speciesAnal[i]),])
  subsetBasins2<-dataBasinsAnal[dataBasinsAnal$Species==as.character(speciesAnal[i]),]
  
  gridAll<-ecospat.grid.clim.dyn (glob=overallData, 
                                  glob1=All,
                                  sp=subsetAll2[,2:3], R=50)
  
  gridBasins<-ecospat.grid.clim.dyn (glob=overallData, 
                                     glob1=Basins,
                                     sp=subsetBasins2[,2:3], R=50)
  nulData<-data.frame(pmean=seq(0,5,0.2),tmean=seq(10,15,0.2))
  
  baseData<-ecospat.grid.clim.dyn (glob=overallData, 
                                   glob1=nulData,
                                   sp=nulData, R=50)
  
  # Create data frame
  cat <- ecospat.niche.dyn.index(gridAll,gridBasins)$dyn
  
  # Assign colors to different portions of the niche (abandonment-unfilling-stability-expansion-pioneering (ausep for short), which means we also looked at non-analogue climatic space)
  cat.df <- terra::as.data.frame(cat, xy = TRUE) %>% 
    mutate(fill.ausep = case_when(lyr.1 == 0 ~ "white", lyr.1 == 1 ~ "#CCA7D1", lyr.1 == 2 ~ "#9BBFDC", lyr.1 == 3 ~ "#4DAF4A", lyr.1 == 4 ~ "#FF7F00", lyr.1 == 5 ~ "#E41A1C", lyr.1 == 6 ~ "grey"))
  
  max_MAT_mod[i]<-cat.df %>% filter(lyr.1 %in% c(1,2,3)) %>%  summarise(max_val = max(x, na.rm = TRUE))
  min_MAT_mod[i]<-cat.df %>% filter(lyr.1 %in% c(1,2,3)) %>%  summarise(min_val = min(x, na.rm = TRUE))
  max_MAT_fos[i]<-cat.df %>% filter(lyr.1 %in% c(3,4,5)) %>%  summarise(max_val = max(x, na.rm = TRUE))
  min_MAT_fos[i]<-cat.df %>% filter(lyr.1 %in% c(3,4,5)) %>%  summarise(min_val = min(x, na.rm = TRUE))
  
  max_MAP_mod[i]<-cat.df %>% filter(lyr.1 %in% c(1,2,3)) %>%  summarise(max_val = max(y, na.rm = TRUE))
  min_MAP_mod[i]<-cat.df %>% filter(lyr.1 %in% c(1,2,3)) %>%  summarise(min_val = min(y, na.rm = TRUE))
  max_MAP_fos[i]<-cat.df %>% filter(lyr.1 %in% c(3,4,5)) %>%  summarise(max_val = max(y, na.rm = TRUE))
  min_MAP_fos[i]<-cat.df %>% filter(lyr.1 %in% c(3,4,5)) %>%  summarise(min_val = min(y, na.rm = TRUE))
  
  # Occurrences in the invaded range
  gridBasins$z.uncor[gridBasins$z.uncor[] == 0] <- NA
  grid.basins.z.uncor.df <- terra::as.data.frame(gridBasins$z.uncor, xy = TRUE)
  
  # Niches in the native and invaded ranges
  gridAll.Z.df <- terra::as.data.frame(gridAll$Z, xy = TRUE)
  gridBasins.Z.df <- terra::as.data.frame(gridBasins$Z, xy = TRUE)
  
  #Create plot
  plot.spp <-
    ggplot() +
    geom_raster(data = cat.df, aes(x = x, y = y, fill = fill.ausep), show.legend = FALSE) +
    scale_fill_identity() +
    ggnewscale::new_scale_fill() + # this came in handy to add more than one scale
    geom_raster(data = grid.basins.z.uncor.df, aes(x = x, y = y, fill = lyr.1), alpha = 0.7, show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "black") +
    # these geom_segment() draw the arrows one typically sees in ecospat plots, joining centroids
    #geom_segment(aes(x = median(scores.occ.nat$Axis1), y = median(scores.occ.nat$Axis2), xend = median(scores.occ.inv$Axis1), yend = median(scores.occ.inv$Axis2)), arrow = arrow(length = unit(0.1, "cm")), linewidth = 1) +
    #geom_segment(aes(x = median(scores.clim.nat$Axis1), y = median(scores.clim.nat$Axis2), xend = median(scores.clim.inv$Axis1), yend = median(scores.clim.inv$Axis2)), arrow = arrow(length = unit(0.1, "cm")), linewidth = 1, color = "red") +
    theme_bw() +
    # remove background lines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "italic", size = 11)) +
    # remove padding
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    # add contours of the niche in the native range – note that I was plotting the whole niche and the 75% quantile
    geom_contour(data = gridAll.Z.df, aes(x = x, y = y, z = lyr.1), breaks = quantile(gridAll$Z[gridAll$Z > 0], probs = 0), color = "green3", linetype = "solid") +
    geom_contour(data = gridAll.Z.df, aes(x = x, y = y, z = lyr.1), breaks = quantile(gridAll$Z[gridAll$Z > 0], probs = 0.25), color = "green3", linetype = "dashed") +
    # add contours of the niche in the invaded range
    geom_contour(data = gridBasins.Z.df, aes(x = x, y = y, z = lyr.1), breaks = quantile(gridBasins$Z[gridBasins$Z > 0], probs = 0), color = "red3", linetype = "solid") +
    geom_contour(data = gridBasins.Z.df, aes(x = x, y = y, z = lyr.1), breaks = quantile(gridBasins$Z[gridBasins$Z > 0], probs = 0.25), color = "red3", linetype = "dashed") +
    theme_bw() +
    # remove background lines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, face = "italic", size = 11)) +
    # remove padding
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    #add labels
    ggtitle(speciesAnal[i]) +
    xlab("Mean Annual Temperature (\u00B0C)") + 
    ylab("Mean Annual Precipitation (mm)")
  
  pdf(paste0("./Overlap_",as.character(speciesAnal[i]),".pdf"),height = 3,width = 5)
  print(plot.spp)
  par(mfrow=c(1,2))
  ecospat.plot.overlap.test(EqL, "D","Equivalency lower")
  ecospat.plot.overlap.test(Sim, "D", "Similarity")
  
  dev.off()
  
  ggsave(paste0("./Overlap_",as.character(speciesAnal[i]),".eps"), plot = plot.spp, width = 4, height = 4)
}

#Create dataframe with results
resultsAnaly<-data.frame(speciesAnal,
                         Overlap,persInSp,similSp,
                         equivSp,
                         Expansion,ExpansionSp,
                         Stability,StabilitySp,
                         Unfilling,UnfillingSp, 
                         unlist(max_MAT_mod), unlist(min_MAT_mod),
                         unlist(max_MAT_fos), unlist(min_MAT_fos),
                         unlist(max_MAP_mod), unlist(min_MAP_mod),
                         unlist(max_MAP_fos), unlist(min_MAP_fos))

#rename columns
names(resultsAnaly)[12] <- "max_MAT_mod"
names(resultsAnaly)[13] <- "min_MAT_mod"
names(resultsAnaly)[14] <- "max_MAT_fos"
names(resultsAnaly)[15] <- "min_MAT_fos"

names(resultsAnaly)[16] <- "max_MAP_mod"
names(resultsAnaly)[17] <- "min_MAP_mod"
names(resultsAnaly)[18] <- "max_MAP_fos"
names(resultsAnaly)[19] <- "min_MAP_fos"

#Plot maps
dataAllAnal2<-data.frame(Species=AllAnal$Species,tmean=AllAnal$tmean, pmean=AllAnal$pmean, Age=AllAnal$AgeQual, Lat=AllAnal$LAT, Long=AllAnal$LONG)
dataAllAnal2<-droplevels(dataAllAnal2[dataAllAnal2$Age==as.character("5_Now"),])
dataAllAnal2<-dataAllAnal2[!duplicated(dataAllAnal2[!names(dataAllAnal2) %in% c("Lat","Long")]),]

dataBasinsAnal2<-data.frame(Species=basinAnal$Species,tmean=basinAnal$tmean, pmean=basinAnal$pmean, Age=basinAnal$AgeQual, site=basinAnal$siteid)
dataBasinsAnal2<-separate_wider_delim(dataBasinsAnal2, cols = site, delim = "_", names = c("Lat", "Long"))
dataBasinsAnal2<-droplevels(dataBasinsAnal2[dataBasinsAnal2$Age==as.character("5_Now"),])
dataBasinsAnal2<-dataBasinsAnal2[!duplicated(dataBasinsAnal2[!names(dataBasinsAnal2) %in% c("Lat","Long")]),]

dataBasinsAnal2 = dataBasinsAnal2 %>%
  filter(Species %in% speciesAnal.df$speciesAnal)

dataAllAnal2 = dataAllAnal2 %>%
  filter(Species %in% speciesAnal.df$speciesAnal)

usa <- map_data("usa")

ggplot(data=dataAllAnal2, aes(x = Long, y = Lat)) + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='lightgrey') + 
  geom_point(color="#FF7377") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

ggplot(data=dataBasinsAnal2, aes(x = as.numeric(Long), y = as.numeric(Lat))) + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill='lightgrey') + 
  geom_point(color="springgreen3") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

#Plot overall niche space

#get the density of points
#function from https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/
get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

densityAll <- dataAll %>% 
  select(x = tmean, y = pmean) %>% 
  mutate(density = get_density(x, y, n = 100))

#map the density of points
(densityAll_plot <- densityAll %>% 
    ggplot(aes(x = x, y = y, color = density)) +
    geom_point(alpha = 0.2) +
    scale_color_gradient(low = "#FF7377", high = "black") +
    theme_bw() +
    theme(legend.position = "none") +  
    xlab("Mean Annual Temperature (\u00B0C)") + 
    ylab("Mean Annual Precipitation (mm)") +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 16)))

densityBasins <- dataBasins %>% 
  select(x = tmean, y = pmean) %>% 
  mutate(density = get_density(x, y, n = 100))

#map the density of points
(densityBasins_plot <- densityBasins %>% 
    ggplot(aes(x = x, y = y, color = density)) +
    geom_point(alpha = 0.2) +
    scale_color_gradient(low = "springgreen3", high = "black") +
    xlim(min(dataAll$tmean), max(dataAll$tmean)) +
    ylim(min(dataAll$pmean), max(dataAll$pmean)) +
    theme_bw() +
    theme(legend.position = "none") +  
    xlab("Mean Annual Temperature (\u00B0C)") + 
    ylab("Mean Annual Precipitation (mm)") +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 16)))


# get order data
# set your api key as an environmental variable so you do not upload
Sys.setenv(ENTREZ_KEY = "e3e45b379994bffa4f88a23e682b9f6f2708") #delete key before uploading to github

# now call the variable that you set
Sys.getenv("ENTREZ_KEY")
api_key <- Sys.getenv("ENTREZ_KEY")
resultsAnaly_tax <- tax_name(resultsAnaly$speciesAnal, get = 'order', db = 'ncbi')

resultsAnaly$order <- resultsAnaly_tax$order

# Add number of occurrences total and inside basins
test1 <- subset(test, select=-c(basins.y,all.y))
names(test1)[2] <- "basins"
names(test1)[3] <- "all"
resultsAnaly <- rename(resultsAnaly,c("species"="speciesAnal"))
final_data <- merge(resultsAnaly,test1,by="species")

# Add percent in basins
final_data = final_data %>% mutate(percent = basins/all)

# Add change in MAT and MAP
final_data = final_data %>% mutate(delta_max_MAT = max_MAT_fos-max_MAT_mod)
final_data = final_data %>% mutate(delta_min_MAT= min_MAT_fos-min_MAT_mod)
final_data = final_data %>% mutate(delta_max_MAP = max_MAP_fos-max_MAP_mod)
final_data = final_data %>% mutate(delta_min_MAP= min_MAP_fos-min_MAP_mod)

# Get body size data
setwd("/Users/Marshall_Lab/Desktop/Chapter_3")

antraits = read.csv("Complete_dataset_published.csv")
antraits$binomial_Hedges_final<-gsub("_"," ",as.character(antraits$binomial_Hedges_final))
antraits <- rename(antraits,c("species"="binomial_Hedges_final"))
antraits <- subset(antraits, select = c(species, Adult_body_mass_g))

# Filter body size data to those in our species list
filtered_antraits = antraits %>%
  filter(species %in% final_data$species)

final_data <- final_data %>% 
  left_join(filtered_antraits, by = "species")

# Save results
write.csv(final_data,"Similarity_Basins-NoBasins_overestimate.csv")

