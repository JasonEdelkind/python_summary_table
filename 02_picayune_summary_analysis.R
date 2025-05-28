#load libraries
library(sf)
library(dplyr)
library(geodata)
library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(raster)
library(ggmap)
library(smoothr)
library(lubridate)
library(ggplot2)

#read in and plot picayune boundary
df<-read.csv("./data/telem.total.csv")
summary.final<-read.csv("./data/summary_table.csv")
removal<-read.csv(".\\data\\removal.scout.csv")
#kml_boundary <- sf::read_sf(".\\shapefiles\\Parks_Boundary.shp") $for PSSF
kml_boundary <- sf::read_sf(".\\shapefiles\\PSSRP_Boundary.shp") #for PSRP

#fixing blank scout removal names
removal$Sentinel<-ifelse(removal$Additional_ID == "PYBI-114", "KJ", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-06MAR24", "Vlad", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP02-06MAR24", "Vlad", removal$Sentinel)
removal$ID<-ifelse(removal$ID == "BP01- 08MAR24", "BP01-08MAR24", removal$ID)
removal$Sentinel<-ifelse(removal$ID == "BP01-08MAR24", "Eddie", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-11MAR24", "Abe", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-12MAR24", "Ronin", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-02JAN25", "Pacino", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Bobby Rubino", "Bobby Jo", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Joey", "Bobby Jo", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Harriett", "Harriet", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Darryl McLovin", "McLovin", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Timmy", "Timmy T", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Cash", "Ca$h", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Jaegar", "Jaeger", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Dos Equis", "XX", removal$Sentinel)


#change crs
kml_boundary<-st_transform(kml_boundary, crs = 4269)

#create df of coord data
#xy <-data.frame(df$Long,df$Lat)

#creating a copy of df to turn into an spdf dataframe
spdf<-df

#converting point data to spatial points dataframe
coordinates(spdf) <- ~Long+Lat
proj4string(spdf) <- CRS("+proj=longlat +datum=NAD83")
spdf <- st_as_sf(spdf,coords = 40:41)
spdf <- spdf %>% 
  st_transform(crs = 4269)


#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create basemap
base<-get_map(c(left=min(df$Long),bottom=min(df$Lat),right=max(df$Long),top=max(df$Lat)), source="google",maptype="satellite",zoom=12)

#Add the US and Mexico border layers
base<-ggmap(base)
#geom_path(data=country_map,aes(x=long,y=lat,group=group),size=0.4)

base

#assign name to geometry
kml_boundary$LABEL<-"PSRP"

#plot data
base+
  geom_sf(data = kml_boundary, aes(fill = LABEL),inherit.aes=FALSE,show.legend = FALSE)+
  geom_sf(data = spdf, aes(fill=Snake_ID), show.legend = FALSE, size=2,  inherit.aes = FALSE)

#select only points that occur within the PSSF boundary
PSSF_p<-spdf[which(st_intersects(kml_boundary, spdf, sparse = FALSE)), ]


#creating a copy of df to turn into an spdf dataframe
spdf.removals<-removal

#converting point data to spatial points dataframe
coordinates(spdf.removals) <- ~Long+Lat
proj4string(spdf.removals) <- CRS("+proj=longlat +datum=NAD83")
spdf.removals <- st_as_sf(spdf.removals,coords = 40:41)
spdf.removals <- spdf.removals %>% 
  st_transform(crs = 4269)
PSSRP_removals<-spdf.removals[which(st_intersects(kml_boundary, spdf.removals, sparse = FALSE)), ]

#merging id data to the removal dataframe
names.ids<-df %>% dplyr::select(c(4,5))
names.ids<-unique(names.ids)
colnames(names.ids)[1]<-"Snake_ID"
colnames(PSSRP_removals)[22]<-"Snake_Name"

PSSRP_removals<-merge(PSSRP_removals, names.ids, by=intersect(names(PSSRP_removals), names(names.ids)), all.x=TRUE)

















#identify snakes that occur within PSSF
ids<-unique(PSSF_p$Snake_ID)
names<-unique(PSSF_p$Snake_Name)

#filter the summary dataframe to only include PSSF snakes
summary.PSRP<-summary.final %>% filter(summary.final$Snake_ID %in% ids)
removal<-removal %>% filter(removal$Sentinel %in% names)

write.csv(summary.PSSF,file="./data/summary_PSSF_table.csv",row.names = FALSE)

#summarize the data for individual snakes
total.avg<-removal %>% group_by(Sentinel) %>% summarise(avg.lbs=mean(Weight_lbs))
total.avg.male<-removal %>% group_by(Sentinel) %>% filter(Sex=="Male") %>% summarise(avg.lbs=mean(Weight_lbs))
total.avg.female<-removal %>% group_by(Sentinel) %>% filter(Sex=="Male") %>% summarise(avg.lbs=mean(Weight_lbs))

summary.PSRP$search<-as.integer(summary.PSRP$search)
summary.PSRP$search<-ifelse(is.na(summary.PSRP$search),0,summary.PSRP$search)
summary.PSRP$Snake_ID<-as.factor(summary.PSRP$Snake_ID)
summary.PSRP$search<-ifelse(summary.PSRP$search=="",0,summary.PSRP$search)

PSSF.ind.summ<-summary.PSRP %>% group_by(Snake_ID) %>%  
                                summarise(seasons.tracked=n(),
                                          total.removal=sum(pythons.removed,na.rm=T),
                                          total.female=sum(females,na.rm=T),
                                          total.male=sum(males,na.rm=T),
                                          total.ground=sum(ground,na.rm=T),
                                          total.peak=sum(peak,na.rm=T),
                                          total.search = sum(search,na.rm=T),
                                          total.sample.size=sum(sample.size,na.rm=T),
                                          total.activity.season.trapdays=sum(trapdays.corrected.breeding.season,na.rm=T)
                                         )
test<-summary.PSRP %>%  group_by(Snake_ID) %>% summarise(sum= sum(search))

                                 