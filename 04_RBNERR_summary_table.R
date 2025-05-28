library(dplyr)
library(lubridate)
library(sf)
library(raster)
library(tibble)
library(adehabitatHR)
library(amt)
library(strex)
library(ggmap)

#disable scientific notation
options(scipen=999)

removal<-read.csv(".\\data\\removal.scout.csv")
df<-read.csv("./data/telem.total.csv")
kml_boundary <- sf::read_sf(".\\shapefiles\\removal_boundaryKML.kml") #for PSRP

#remove rogue 1, rogue 2, and caesar
df<-df %>% filter(df$Snake_Name !="Rogue 1" & df$Snake_Name != "Rogue 2" & df$Snake_Name != "Caesar")

#filter removals to just scout method
removal<-removal %>% filter(Method=="Scout" & Entity == "COSWFL")

#fix missing year value
df$Date<-as.Date(df$Date, format = "%Y-%m-%d")
removal$Date<-as.Date(removal$Date, format = "%Y-%m-%d")
df$Year<-year(df$Date)


#fixing blank scout names in removal database
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
removal<-removal %>% filter(removal$Designation!="Six L's Agricultural Buffer")
removal<-removal %>% filter(removal$ID!="BP01-20DEC22")
removal<-removal %>% filter(removal$ID!="BP03-14FEB23")
removal<-removal %>% filter(Sentinel!="Nice")

#change crs
kml_boundary<-st_transform(kml_boundary, crs = 4269)

#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create basemap
base<-get_map(c(left=min(df$Long),bottom=min(df$Lat),right=max(df$Long),top=max(df$Lat)), source="google",maptype="satellite",zoom=11)
base<-ggmap(base)
base

#creating a copy of df to turn into an spdf dataframe
spdf.removals<-removal

#converting point data to spatial points dataframe
coordinates(spdf.removals) <- ~Long+Lat
proj4string(spdf.removals) <- CRS("+proj=longlat +datum=NAD83")
spdf.removals <- st_as_sf(spdf.removals,coords = 40:41)
spdf.removals <- spdf.removals %>% 
  st_transform(crs = 4269)

#select only removal points within the polygon boundary
RBNERR_removals<-spdf.removals[which(st_intersects(kml_boundary, spdf.removals, sparse = FALSE)), ]
RBNERR_removals<-RBNERR_removals %>% arrange(Sentinel)
names<-unique(RBNERR_removals$Sentinel)

#plot data
base+
  geom_sf(data = kml_boundary, aes(fill = Name),inherit.aes=FALSE,show.legend = FALSE)+
  geom_sf(data = RBNERR_removals, aes(fill=Sentinel), show.legend = FALSE, size=2,  inherit.aes = FALSE)






RBNERR_removals<-RBNERR_removals %>% arrange(Sentinel)







#add custom ID-Year identification tag to telemetry and removal databases
names.ids<-df %>% dplyr::select(4,5) %>% unique()

colnames(names.ids)[2] <- 'Sentinel'
#df<-df %>% mutate(id_year = paste(df$Snake_ID, "_", df$Year, sep=""))
removal<-merge(removal, names.ids, by=intersect(names(removal), names(names.ids)), all.x=TRUE)
removal<-removal %>% dplyr::select(-2)
df<-df %>% dplyr::select(-1)
#removal<-removal %>% mutate(id_year = paste(removal$Snake_ID, "_", removal$Year, sep=""))

#fixing removal rows with incorrect years
removal$Year<-ifelse(removal$ID == "BP01-19JAN24" | removal$ID == "BP03-21FEB24",2024,removal$Year)

#add a season column for both the telemetry and removal dataframes
#define a function to identify the breeding season
season<-function(x){ifelse(
  x[['Date']] >= ymd(paste0(x[['Year']],"-01-01")) & 
    x[['Date']] <= ymd(paste0(x[['Year']],"-10-14")),
  paste(as.numeric(x[['Year']])-1,"-",x[['Year']],sep=""), 
  paste(x[['Year']],"-",as.numeric(x[['Year']])+1,sep=""))
}

#apply the function
df$season<-apply(df, 1, season)
removal$season<-apply(removal, 1, season)

# df$season<-NA
# df$season<-ifelse(df$Date>=as.Date("2012-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2013-10-14",format="%Y-%m-%d"),"2012-2013",df$season)
# df$season<-ifelse(df$Date>=as.Date("2013-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2014-10-14",format="%Y-%m-%d"),"2013-2014",df$season)
# df$season<-ifelse(df$Date>=as.Date("2014-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2015-10-14",format="%Y-%m-%d"),"2014-2015",df$season)
# df$season<-ifelse(df$Date>=as.Date("2015-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2016-10-14",format="%Y-%m-%d"),"2015-2016",df$season)
# df$season<-ifelse(df$Date>=as.Date("2016-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2017-10-14",format="%Y-%m-%d"),"2016-2017",df$season)
# df$season<-ifelse(df$Date>=as.Date("2017-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2018-10-14",format="%Y-%m-%d"),"2017-2018",df$season)
# df$season<-ifelse(df$Date>=as.Date("2018-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2019-10-14",format="%Y-%m-%d"),"2018-2019",df$season)
# df$season<-ifelse(df$Date>=as.Date("2019-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2020-10-14",format="%Y-%m-%d"),"2019-2020",df$season)
# df$season<-ifelse(df$Date>=as.Date("2020-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2021-10-14",format="%Y-%m-%d"),"2020-2021",df$season)
# df$season<-ifelse(df$Date>=as.Date("2021-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2022-10-14",format="%Y-%m-%d"),"2021-2022",df$season)
# df$season<-ifelse(df$Date>=as.Date("2022-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2023-10-14",format="%Y-%m-%d"),"2022-2023",df$season)
# df$season<-ifelse(df$Date>=as.Date("2023-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2024-10-14",format="%Y-%m-%d"),"2023-2024",df$season)
# df$season<-ifelse(df$Date>=as.Date("2024-10-15",format="%Y-%m-%d") & df$Date<=as.Date("2025-10-14",format="%Y-%m-%d"),"2024-2025",df$season)
# 
# removal$season<-NA
# removal$season<-ifelse(removal$Date>=as.Date("2012-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2013-10-14",format="%Y-%m-%d"),"2012-2013",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2013-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2014-10-14",format="%Y-%m-%d"),"2013-2014",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2014-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2015-10-14",format="%Y-%m-%d"),"2014-2015",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2015-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2016-10-14",format="%Y-%m-%d"),"2015-2016",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2016-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2017-10-14",format="%Y-%m-%d"),"2016-2017",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2017-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2018-10-14",format="%Y-%m-%d"),"2017-2018",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2018-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2019-10-14",format="%Y-%m-%d"),"2018-2019",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2019-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2020-10-14",format="%Y-%m-%d"),"2019-2020",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2020-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2021-10-14",format="%Y-%m-%d"),"2020-2021",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2021-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2022-10-14",format="%Y-%m-%d"),"2021-2022",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2022-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2023-10-14",format="%Y-%m-%d"),"2022-2023",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2023-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2024-10-14",format="%Y-%m-%d"),"2023-2024",removal$season)
# removal$season<-ifelse(removal$Date>=as.Date("2024-10-15",format="%Y-%m-%d") & removal$Date<=as.Date("2025-10-14",format="%Y-%m-%d"),"2024-2025",removal$season)

#create a function to isolate unique id-years and calculate minimum distance traveled for that season
df<-df %>% mutate(id_season = paste(df$Snake_ID, "_",df$season, sep=""))
removal<-removal %>% mutate(id_season = paste(removal$Snake_ID, "_",removal$season,sep=""))
id.season<-unique(df$id_season)

#create a summary table of sample size
df.summ<-df %>% group_by(id_season) %>% summarise(n=n())


#remove duplicate record
df<-df %>% filter(!(df$id_season=="PYBI_96_2023-2024" & df$Date == as.Date("2024-01-26", format = "%Y-%m-%d")))
df<-df %>% filter(!(df$id_season=="PYBI_98_2024-2025" & df$UTM_N == 1))
df<-df %>% filter(!(df$id_season=="PYBI_23_2015-2016" & df$Date == as.Date("2016-03-11", format = "%Y-%m-%d")))

#fix incorrect dates
df$Date<-if_else(df$id_season=="PYBI_89_2023-2024" & df$Date == as.Date("2024-01-25",format = "%Y-%m-%d"), as.Date("2024-02-22",format = "%Y-%m-%d"),df$Date)
df$Date<-if_else(df$id_season=="PYBI_89_2023-2024" & df$Date == as.Date("2024-02-23",format = "%Y-%m-%d"), as.Date("2024-02-22",format = "%Y-%m-%d"),df$Date)

min_dist<-list()
for(i in 1:length(id.season)){
  telem.ind<-df %>% filter(df$id_season == id.season[i]) #first filter to isolate a single individual in a single season
  sf_object <- st_as_sf(telem.ind, coords = c("Long", "Lat"),crs=4269) #then create an sf object from the individual's telemetry data
  sf_object<-st_transform(sf_object, 26917)   #convert sf object to utms
  L1 <- st_combine(sf_object) %>% st_cast("LINESTRING")   #create a linestring object of the telemetry data
  min_dist[i]<-(st_length(L1)/1609.34) #calculate minimum distance in mi
}

#assign names to min dist 
names(min_dist)<-id.season

#converting minimum distance list to df
min_dist_df<-as.data.frame(min_dist) #convert list to df
min_dist_df<-t(min_dist_df) #transpose df
min_dist_df<-as.data.frame(min_dist_df) #reconvert back to df
min_dist_df<-tibble::rownames_to_column(min_dist_df,"id_season") #turn row names into column
colnames(min_dist_df)[2]<-"min.dist.mi" #assign name to min dist column

## calculating core min distance traveled method A (Dec15 - Mar 15)
df.core.A<-df
df.core.A<-df.core.A %>% arrange(id_season,timestamp)
df.core.A$Mo<-format(df.core.A$Date, format ="%m")
df.core.A$month.day<-as.Date(df.core.A$Date, format="%Y-%m-%d")
df.core.A$month.day<-format(df.core.A$month.day,format="%m-%d")
df.core.A$month.day<-as.Date(df.core.A$month.day, format="%m-%d")
df.core.A<-df.core.A %>% filter(df.core.A$month.day<=as.Date("2025-03-15",format="%Y-%m-%d") | df.core.A$month.day >=as.Date("2025-12-15",format="%Y-%m-%d")) #filtering to select points within the core removal area
id.season.core<-unique(df.core.A$id_season) #isolate unique id.season combos

#create a summary table of sample size
df.core.A.summ<-df.core.A %>% group_by(id_season) %>% summarise(n=n())

min_dist.core<-list()
for(i in 1:length(id.season.core)){
  telem.ind<-df.core.A %>% filter(df.core.A$id_season == id.season.core[i]) #first filter to isolate a single individual in a single season
  sf_object <- st_as_sf(telem.ind, coords = c("Long", "Lat"),crs=4269) #then create an sf object from the individual's telemetry data
  sf_object<-st_transform(sf_object, 26917)   #convert sf object to utms
  L1 <- st_combine(sf_object) %>% st_cast("LINESTRING")   #create a linestring object of the telemetry data
  min_dist.core[i]<-(st_length(L1)/1609.34) #calculate minimum distance in mi
}

#assign names to min dist 
names(min_dist.core)<-id.season.core

#converting minimum distance list to df
min_dist.core_df<-as.data.frame(min_dist.core) #convert list to df
min_dist.core_df<-t(min_dist.core_df) #transpose df
min_dist.core_df<-as.data.frame(min_dist.core_df) #reconvert back to df
min_dist.core_df<-tibble::rownames_to_column(min_dist.core_df,"id_season") #turn row names into column
colnames(min_dist.core_df)[2]<-"min.dist.core.A.mi" #assign name to min dist column

## calculating core min distance traveled method B (Jan - Mar)
df$Mo<-month(df$Date)
df.core.B<-df
df.core.B<-df.core.B %>% arrange(id_season,timestamp)
df.core.B$Mo<-format(df.core.B$Date, format ="%m")
df.core.B$Mo<-as.numeric(df.core.B$Mo)
df.core.B<-df.core.B %>% filter(df.core.B$Mo<=3) #filtering to select points within the core removal area
id.season.core<-unique(df.core.B$id_season) #isolate unique id.season combos

#create a summary table of sample size
df.core.B.summ<-df.core.B %>% group_by(id_season) %>% summarise(n=n())

min_dist.core.B<-list()
for(i in 1:length(id.season.core)){
  telem.ind<-df.core.B %>% filter(df.core.B$id_season == id.season.core[i]) #first filter to isolate a single individual in a single season
  sf_object <- st_as_sf(telem.ind, coords = c("Long", "Lat"),crs=4269) #then create an sf object from the individual's telemetry data
  sf_object<-st_transform(sf_object, 26917)   #convert sf object to utms
  L1 <- st_combine(sf_object) %>% st_cast("LINESTRING")   #create a linestring object of the telemetry data
  min_dist.core.B[i]<-(st_length(L1)*0.000621371) #convert meters to miles) 
}

#assign names to min dist 
names(min_dist.core.B)<-id.season.core

#converting minimum distance list to df
min_dist.core.B_df<-as.data.frame(min_dist.core.B) #convert list to df
min_dist.core.B_df<-t(min_dist.core.B_df) #transpose df
min_dist.core.B_df<-as.data.frame(min_dist.core.B_df) #reconvert back to df
min_dist.core.B_df<-tibble::rownames_to_column(min_dist.core.B_df,"id_season") #turn row names into column
colnames(min_dist.core.B_df)[2]<-"min.dist.core.B.mi" #assign name to min dist column

#finding total numbers of males and females removed per scout
nrow(removal[removal$Sex=="Male",])

removal.summ<-removal %>% 
  group_by(id_season) %>% 
  summarise(n=n(),
            males = sum(Sex == "Male"),
            females = sum(Sex == "Female"))
           
  



#finding average weight female and male removed per scout
weight.summ<-removal %>% 
  group_by(id_season) %>% 
  summarise(avg.total=round(mean(Weight_lbs),digits=2),
            avg.m = round(mean(Weight_lbs[Sex == "Male"]),digits=2),
            avg.f = round(mean(Weight_lbs[Sex == "Female"]),digits=2))



#reading in human effort data
effort<-read.csv("./data/human.effort.csv")
colnames(effort)<-c("name","id","ground","peak","search","season")
effort[is.na(effort)] = 0 

#add leading zeros to id column
effort$id<-as.integer(effort$id)
effort<-effort %>% 
  mutate(id=sprintf("%02d", effort$id))
effort$id<-ifelse(effort$name == "VICTOR", "V5",effort$id)
effort$id<-ifelse(effort$name == "ORION", "D7",effort$id)

#adding the PYBI prefix
effort$id<-sprintf("PYBI_%02s",effort$id)

#creating the id-season column
effort<-effort %>% mutate(id_season = paste(effort$id, "_",effort$season, sep=""))

## calculating seasonal MCPs

# #isolate relevant rows
# telemetry.mcp<-df
# telemetry.mcp <- telemetry.mcp[, c("Snake_ID","id_season", "Long", "Lat","Date","Time")] 
# 
# # Define long/lat columns
# coordinates(telemetry.mcp) <- c("Long", "Lat")
# 
# # Set the coordinate reference system (CRS)
# proj4string(telemetry.mcp) <- CRS("+init=epsg:4326")
# 
# # Project data to utms
# telemetry.mcp <- spTransform(telemetry.mcp, 
#                              CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))
# 
# #convert to df
# telemetry.mcp<-data.frame(telemetry.mcp)
# 
# #create track object
# id.season<-unique(telemetry.mcp$id_season)
# track_obj<-list()
# for(i in 1:length(id.season)){
#   telem.ind<-telemetry.mcp %>% filter(telemetry.mcp$id_season == id.season[i]) #first filter to isolate a single individual in a single season
#   track_obj[[i]]<-track(telem.ind,x=telem.ind$coords.x1,y=telem.ind$coords.x2,t=telem.ind$timestamp,crs=NA_crs_,
#                         order_by_ts=TRUE,check_duplicates=FALSE,all_cols=FALSE,verbose=FALSE)
# }
# 
# MCP_100<-list()
# MCP<-list()
# for(i in 1:length(track_obj)){
#  MCP[[i]]<-hr_mcp(track_obj[[i]],levels=1)
#  area<-hr_area(MCP[[i]])
#  MCP_100[[i]]<-as.numeric((area[[3]])* 0.0000003861) #area in sq mi
# }
# 
# #assign names
# names(MCP)<-id.season
# names(MCP_100)<-id.season
# names(track_obj)<-id.season
# 
# #converting MCP list to df 
# MCP_100<-as.data.frame(MCP_100) #convert list to df
# MCP_100<-t(MCP_100) #transpose df
# MCP_100<-as.data.frame(MCP_100) #reconvert back to df
# MCP_100<-tibble::rownames_to_column(MCP_100,"id_season") #turn row names into column
# colnames(MCP_100)[1]<-"id.season" #assign name to columns
# colnames(MCP_100)[2]<-"MCP_100_sqmi" #assign name to columns
# 
# 
# ## calculating core MCPs using method A
# #isolate relevant rows
# telemetry.mcp<-df.core.A
# telemetry.mcp <- telemetry.mcp[, c("id_season", "Long", "Lat","Date","Time")] 
# 
# # Define long/lat columns
# coordinates(telemetry.mcp) <- c("Long", "Lat")
# 
# # Set the coordinate reference system (CRS)
# proj4string(telemetry.mcp) <- CRS("+init=epsg:4326")
# 
# # Project data to utms
# telemetry.mcp <- spTransform(telemetry.mcp, 
#                              CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))
# 
# #convert to df
# telemetry.mcp<-data.frame(telemetry.mcp)
# 
# #create track object
# id.season<-unique(telemetry.mcp$id_season)
# track_obj_core.A<-list()
# for(i in 1:length(id.season)){
#   telem.ind<-telemetry.mcp %>% filter(telemetry.mcp$id_season == id.season[i]) #first filter to isolate a single individual in a single season
#   track_obj_core.A[[i]]<-track(telem.ind,x=telem.ind$coords.x1,y=telem.ind$coords.x2,t=telem.ind$timestamp,crs=NA_crs_,
#                         order_by_ts=TRUE,check_duplicates=FALSE,all_cols=FALSE,verbose=FALSE)
# }
# 
# MCP_100_core.A<-list()
# MCP_core.A<-list()
# for(i in 1:length(track_obj_core.A)){
#   MCP_core.A[[i]]<-hr_mcp(track_obj_core.A[[i]],levels=1)
#   area<-hr_area(MCP_core.A[[i]])
#   MCP_100_core.A[[i]]<-as.numeric((area[[3]])* 0.0000003861) #area in sq mi
# }
# 
# #assign names
# names(MCP_core.A)<-id.season
# names(MCP_100_core.A)<-id.season
# names(track_obj_core.A)<-id.season
# 
# #converting MCP list to df
# MCP_100_core.A<-as.data.frame(MCP_100_core.A) #convert list to df
# MCP_100_core.A<-t(MCP_100_core.A) #transpose df
# MCP_100_core.A<-as.data.frame(MCP_100_core.A) #reconvert back to df
# MCP_100_core.A<-tibble::rownames_to_column(MCP_100_core.A,"id_season") #turn row names into column
# colnames(MCP_100_core.A)[1]<-"id.season" #assign name to columns
# colnames(MCP_100_core.A)[2]<-"MCP_100_core.A_sqmi" #assign name to columns
# 
# ## calculating core MCPs using method B
# #isolate relevant rows
# telemetry.mcp<-df.core.B
# telemetry.mcp <- telemetry.mcp[, c("id_season", "Long", "Lat","Date","Time")] 
# 
# # Define long/lat columns
# coordinates(telemetry.mcp) <- c("Long", "Lat")
# 
# # Set the coordinate reference system (CRS)
# proj4string(telemetry.mcp) <- CRS("+init=epsg:4326")
# 
# # Project data to utms
# telemetry.mcp <- spTransform(telemetry.mcp, 
#                              CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))
# 
# #convert to df
# telemetry.mcp<-data.frame(telemetry.mcp)
# 
# #create track object
# id.season<-unique(telemetry.mcp$id_season)
# track_obj_core.B<-list()
# for(i in 1:length(id.season)){
#   telem.ind<-telemetry.mcp %>% filter(telemetry.mcp$id_season == id.season[i]) #first filter to isolate a single individual in a single season
#   track_obj_core.B[[i]]<-track(telem.ind,x=telem.ind$coords.x1,y=telem.ind$coords.x2,t=telem.ind$timestamp,crs=NA_crs_,
#                         order_by_ts=TRUE,check_duplicates=FALSE,all_cols=FALSE,verbose=FALSE)
# }
# 
# MCP_100_core.B<-list()
# MCP_core.B<-list()
# for(i in 1:length(track_obj_core.B)){
#   MCP_core.B[[i]]<-hr_mcp(track_obj_core.B[[i]],levels=1)
#   area<-hr_area(MCP_core.B[[i]])
#   MCP_100_core.B[[i]]<-as.numeric((area[[3]])* 0.0000003861) #area in sq mi
# }
# 
# #assign names
# names(MCP_core.B)<-id.season
# names(MCP_100_core.B)<-id.season
# names(track_obj_core.B)<-id.season
# 
# #converting MCP list to df 
# MCP_100_core.B<-as.data.frame(MCP_100_core.B) #convert list to df
# MCP_100_core.B<-t(MCP_100_core.B) #transpose df
# MCP_100_core.B<-as.data.frame(MCP_100_core.B) #reconvert back to df
# MCP_100_core.B<-tibble::rownames_to_column(MCP_100_core.B,"id_season") #turn row names into column
# colnames(MCP_100_core.B)[1]<-"id.season" #assign name to columns
# colnames(MCP_100_core.B)[2]<-"MCP_100_core.B_sqmi" #assign name to columns


##calculate home range overlap and MCPs

# Make an amt `track` object with our sample data set

#telemetry.mcp<-telemetry.mcp %>% group_by(id_season) %>% mutate (n=n()) %>% filter(n>=3) #Maybe Not Necessary????
#isolate relevant rows
telemetry.mcp<-df
telemetry.mcp <- telemetry.mcp[, c("Snake_ID","id_season", "Long", "Lat","Date","Time")] 

# Define long/lat columns
coordinates(telemetry.mcp) <- c("Long", "Lat")

# Set the coordinate reference system (CRS)
proj4string(telemetry.mcp) <- CRS("+init=epsg:4326")

# Project data to utms
telemetry.mcp <- spTransform(telemetry.mcp, 
                             CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

#convert to df
telemetry.mcp<-data.frame(telemetry.mcp)

telemetry.mcp<-ungroup(telemetry.mcp)
track <- telemetry.mcp %>%
  
  # Add formatted date-time
  # This stage can be tricky, so double-check those date values in your own data.
  mutate(ts = as.POSIXct(paste(Date, Time, sep = " "))) %>%
  # Make track with coordinates, date-time, id
  make_track(coords.x1, coords.x2, ts, id = id_season,
             # Make sure to specify coordinate reference system (CRS)
             crs = "+init=epsg:26917") %>%
  # Use nest() to allow us to deal with multiple animals (5 in sample set)
  # Each animal's track is stored in a tibble (table) nested within the data column
  nest(data = -"id") %>%
  arrange(id)

track <- track %>% 
  mutate(mcp = map(data, function(x) 
    x %>% hr_mcp(levels = c(1.0)))) 


track$area<-NA
for(i in 1:nrow(track)){
  area<-hr_area(track$mcp[[i]])
  track$area[i]<-area[[3]]
}


track$area<-track$area* 0.0000003861 #convert to sq mi


#filter to remove any HRs with a value of zero
track<-track %>% filter(track$area !=0)

# track$Snake_ID <- gsub("^([^_]*_[^_]*)_.*$", "\\1", track$id) #create id column
# track$season<-str_after_nth(track$id, "_", 2) #create season column
# track$year.2<-str_after_nth(track$season, "-", 1) #create year 2 column
# track$year.1<-str_before_nth(track$season, "-", 1) #create year 1 column

hr_overlap <- hr_overlap(track$mcp,
                                labels = track$id, 
                                which = "all", 
                                # alternative which = "consecutive",
                                # "one_to_all"
                                conditional = FALSE)

#removing zero values
hr_overlap$id1 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", hr_overlap$from) #create id1 column
hr_overlap$id2 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", hr_overlap$to) #create id2 column
hr_overlap$season1<-str_after_nth(hr_overlap$from, "_", 2) #create season column
hr_overlap$season2<-str_after_nth(hr_overlap$to, "_", 2) #create season column

hr_overlap$year1<-str_before_nth(hr_overlap$season1, "-", 1) #create year 1 column
hr_overlap$year2<-str_before_nth(hr_overlap$season2, "-", 1) #create year 2 column

hr_overlap$year1<-as.numeric(hr_overlap$year1)
hr_overlap$year2<-as.numeric(hr_overlap$year2)

hr_overlap_cleaned<-hr_overlap %>% filter(hr_overlap$year2-hr_overlap$year1 == 1 & hr_overlap$id1 == hr_overlap$id2)
hr_overlap_cleaned<-hr_overlap_cleaned[, c("to","overlap")] 

######MCPs and overlap for core method A###############################################################
# Make an amt `track` object with our sample data set

#telemetry.mcp_core.A<-telemetry.mcp_core.A %>% group_by(id_season) %>% mutate (n=n()) %>% filter(n>=3) #Maybe Not Necessary????
#isolate relevant rows
telemetry.mcp_core.A<-df.core.A
telemetry.mcp_core.A <- telemetry.mcp_core.A[, c("Snake_ID","id_season", "Long", "Lat","Date","Time")] 

# Define long/lat columns
coordinates(telemetry.mcp_core.A) <- c("Long", "Lat")

# Set the coordinate reference system (CRS)
proj4string(telemetry.mcp_core.A) <- CRS("+init=epsg:4326")

# Project data to utms
telemetry.mcp_core.A <- spTransform(telemetry.mcp_core.A, 
                             CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

#convert to df.core.A
telemetry.mcp_core.A<-data.frame(telemetry.mcp_core.A)

telemetry.mcp_core.A<-ungroup(telemetry.mcp_core.A)
track.core.A <- telemetry.mcp_core.A %>%
  
  # Add formatted date-time
  # This stage can be tricky, so double-check those date values in your own data.
  mutate(ts = as.POSIXct(paste(Date, Time, sep = " "))) %>%
  # Make track.core.A with coordinates, date-time, id
  make_track(coords.x1, coords.x2, ts, id = id_season,
             # Make sure to specify coordinate reference system (CRS)
             crs = "+init=epsg:26917") %>%
  # Use nest() to allow us to deal with multiple animals (5 in sample set)
  # Each animal's track.core.A is stored in a tibble (table) nested within the data column
  nest(data = -"id") %>%
  arrange(id)

track.core.A <- track.core.A %>% 
  mutate(mcp = map(data, function(x) 
    x %>% hr_mcp(levels = c(1.0)))) 


track.core.A$area<-NA
for(i in 1:nrow(track.core.A)){
  area<-hr_area(track.core.A$mcp[[i]])
  track.core.A$area[i]<-area[[3]]
}


track.core.A$area<-track.core.A$area* 0.0000003861 #convert to sq mi


#filter to remove any HRs with a value of zero
track.core.A<-track.core.A %>% filter(track.core.A$area !=0)

# track.core.A$Snake_ID <- gsub("^([^_]*_[^_]*)_.*$", "\\1", track.core.A$id) #create id column
# track.core.A$season<-str_after_nth(track.core.A$id, "_", 2) #create season column
# track.core.A$year.2<-str_after_nth(track.core.A$season, "-", 1) #create year 2 column
# track.core.A$year.1<-str_before_nth(track.core.A$season, "-", 1) #create year 1 column

hr_overlap.core.A <- hr_overlap(track.core.A$mcp,
                         labels = track.core.A$id, 
                         which = "all", 
                         # alternative which = "consecutive",
                         # "one_to_all"
                         conditional = FALSE)

#removing zero values
hr_overlap.core.A$id1 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", hr_overlap.core.A$from) #create id1 column
hr_overlap.core.A$id2 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", hr_overlap.core.A$to) #create id2 column
hr_overlap.core.A$season1<-str_after_nth(hr_overlap.core.A$from, "_", 2) #create season column
hr_overlap.core.A$season2<-str_after_nth(hr_overlap.core.A$to, "_", 2) #create season column

hr_overlap.core.A$year1<-str_before_nth(hr_overlap.core.A$season1, "-", 1) #create year 1 column
hr_overlap.core.A$year2<-str_before_nth(hr_overlap.core.A$season2, "-", 1) #create year 2 column

hr_overlap.core.A$year1<-as.numeric(hr_overlap.core.A$year1)
hr_overlap.core.A$year2<-as.numeric(hr_overlap.core.A$year2)

hr_overlap_cleaned.core.A<-hr_overlap.core.A %>% filter(hr_overlap.core.A$year2-hr_overlap.core.A$year1 == 1 & hr_overlap.core.A$id1 == hr_overlap.core.A$id2)
hr_overlap_cleaned.core.A<-hr_overlap_cleaned.core.A[, c("to","overlap")] 


######MCPs and overlap for core method B###############################################################

#telemetry.mcp_core.B<-telemetry.mcp_core.B %>% group_by(id_season) %>% mutate (n=n()) %>% filter(n>=3) #Maybe Not Necessary????
#isolate relevant rows
telemetry.mcp_core.B<-df.core.B
telemetry.mcp_core.B <- telemetry.mcp_core.B[, c("Snake_ID","id_season", "Long", "Lat","Date","Time")] 

# Define long/lat columns
coordinates(telemetry.mcp_core.B) <- c("Long", "Lat")

# Set the coordinate reference system (CRS)
proj4string(telemetry.mcp_core.B) <- CRS("+init=epsg:4326")

# Project data to utms
telemetry.mcp_core.B <- spTransform(telemetry.mcp_core.B, 
                                    CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

#convert to df.core.B
telemetry.mcp_core.B<-data.frame(telemetry.mcp_core.B)

telemetry.mcp_core.B<-ungroup(telemetry.mcp_core.B)
track.core.B <- telemetry.mcp_core.B %>%
  
  # Add formatted date-time
  # This stage can be tricky, so double-check those date values in your own data.
  mutate(ts = as.POSIXct(paste(Date, Time, sep = " "))) %>%
  # Make track.core.B with coordinates, date-time, id
  make_track(coords.x1, coords.x2, ts, id = id_season,
                    # Make sure to specify coordinate reference system (CRS)
                    crs = "+init=epsg:26917") %>%
  # Use nest() to allow us to deal with multiple animals (5 in sample set)
  # Each animal's track.core.B is stored in a tibble (table) nested within the data column
  nest(data = -"id") %>%
  arrange(id)

track.core.B <- track.core.B %>% 
  mutate(mcp = map(data, function(x) 
    x %>% hr_mcp(levels = c(1.0)))) 


track.core.B$area<-NA
for(i in 1:nrow(track.core.B)){
  area<-hr_area(track.core.B$mcp[[i]])
  track.core.B$area[i]<-area[[3]]
}


track.core.B$area<-track.core.B$area* 0.0000003861 #convert to sq mi


#filter to remove any HRs with a value of zero
track.core.B<-track.core.B %>% filter(track.core.B$area !=0)

# track.core.B$Snake_ID <- gsub("^([^_]*_[^_]*)_.*$", "\\1", track.core.B$id) #create id column
# track.core.B$season<-str_after_nth(track.core.B$id, "_", 2) #create season column
# track.core.B$year.2<-str_after_nth(track.core.B$season, "-", 1) #create year 2 column
# track.core.B$year.1<-str_before_nth(track.core.B$season, "-", 1) #create year 1 column

hr_overlap.core.B <- hr_overlap(track.core.B$mcp,
                                       labels = track.core.B$id, 
                                       which = "all", 
                                       # alternative which = "consecutive",
                                       # "one_to_all"
                                       conditional = FALSE)

#removing zero values
hr_overlap.core.B$id1 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", hr_overlap.core.B$from) #create id1 column
hr_overlap.core.B$id2 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", hr_overlap.core.B$to) #create id2 column
hr_overlap.core.B$season1<-str_after_nth(hr_overlap.core.B$from, "_", 2) #create season column
hr_overlap.core.B$season2<-str_after_nth(hr_overlap.core.B$to, "_", 2) #create season column

hr_overlap.core.B$year1<-str_before_nth(hr_overlap.core.B$season1, "-", 1) #create year 1 column
hr_overlap.core.B$year2<-str_before_nth(hr_overlap.core.B$season2, "-", 1) #create year 2 column

hr_overlap.core.B$year1<-as.numeric(hr_overlap.core.B$year1)
hr_overlap.core.B$year2<-as.numeric(hr_overlap.core.B$year2)

hr_overlap_cleaned.core.B<-hr_overlap.core.B %>% filter(hr_overlap.core.B$year2-hr_overlap.core.B$year1 == 1 & hr_overlap.core.B$id1 == hr_overlap.core.B$id2)
hr_overlap_cleaned.core.B<-hr_overlap_cleaned.core.B[, c("to","overlap")] 

#calculating track days
library(camtrapR)




df<-ungroup(df)


#changing first releases to V
df$Location<-ifelse(df$id_season=="PYBI_23_2015-2016" & df$Location == "R", "V",df$Location)
df$Location<-ifelse(df$id_season=="PYBI_28_2024-2025" & df$UTM_E == 427983, "V",df$Location)
df$Location<-ifelse(df$id_season=="PYBI_80_2022-2023" & df$Date == as.Date("2023-01-23", format = "%Y-%m-%d"), "V",df$Location)


df<-df %>% filter(!(df$id_season=="PYBI_96_2023-2024" & df$Date == as.Date("2024-01-26", format = "%Y-%m-%d")))
df<-df %>% filter(!(df$id_season=="PYBI_23_2015-2016" & df$Date == as.Date("2016-03-11", format = "%Y-%m-%d")))

df<-df %>% filter(!(df$id_season=="PYBI_98_2024-2025" & df$UTM_N == 1))
df$Location<-ifelse(df$id_season=="PYBI_03_2012-2013" & df$Date == as.Date("2013-02-04", format = "%Y-%m-%d"), "V",df$Location)
df$Location<-ifelse(df$id_season=="PYBI_38_2016-2017" & df$Date == as.Date("2017-04-19", format = "%Y-%m-%d"), "V",df$Location)
df$Location<-ifelse(df$id_season=="PYBI_55_2018-2019" & df$Date == as.Date("2019-01-17", format = "%Y-%m-%d"), "V",df$Location)
df$Location<-ifelse(df$id_season=="PYBI_89_2023-2024" & df$Date == as.Date("2024-04-16", format = "%Y-%m-%d"), "E",df$Location)
df$Date<-if_else(df$id_season=="PYBI_89_2023-2024" & df$Date == as.Date("2024-01-25",format = "%Y-%m-%d"), as.Date("2024-02-22",format = "%Y-%m-%d"),df$Date)
df$Date<-if_else(df$id_season=="PYBI_89_2023-2024" & df$Date == as.Date("2024-02-23",format = "%Y-%m-%d"), as.Date("2024-02-22",format = "%Y-%m-%d"),df$Date)


df.CT<-df %>% filter(df$Location != "C") %>% 
  filter(Date>=as.Date(paste(Year,"-10-15",sep=""),format="%Y-%m-%d") | Date <= as.Date(paste(Year,"-04-15",sep=""),format="%Y-%m-%d")) %>% 
group_by(id_season) %>% summarise(
                                                setup = first(Date),
                                                retrieval = last(Date))

#remove snakes where setup and retrieval dates are identical
df.CT<-df.CT %>% filter(df.CT$retrieval !=df.CT$setup)

trapdays<-cameraOperation(df.CT,
                          stationCol="id_season",
                          setupCol="setup",
                          retrievalCol="retrieval")



trapdays.df<-as.data.frame(rowSums(trapdays, na.rm=TRUE))
trapdays.df<-tibble::rownames_to_column(trapdays.df,"id_season") #turn row names into column
colnames(trapdays.df)[2]<-"trapdays"

#create a column calculating days between observations of the same python
df$Date2<-as.vector(df$Date)
df<-df %>% arrange(Date) %>%  group_by(id_season) %>% 
  mutate(days.diff = Date2-lag(Date2))

#create a column to only include the days between observations if the location is a release
df.trapdays<-df %>% filter(Location != "C") %>%  filter(Date>=as.Date(paste(Year,"-10-15",sep=""),format="%Y-%m-%d") | Date <= as.Date(paste(Year,"-04-15",sep=""),format="%Y-%m-%d"))


#create a column to only include days.diff if the location is R (isolates only day differences between extractions and releases)
df.trapdays<-df.trapdays %>% group_by(id_season) %>% 
  mutate(inactive.days = if_else(Location == "R",days.diff, NA))
df.trapdays<-df.trapdays %>% select(c("timestamp","id_season","inactive.days"))

#merge inactive days with master data frame
df<-ungroup(df)
df.trapdays<-ungroup(df.trapdays)
df<-as.data.frame(df)
df.trapdays<-as.data.frame(df.trapdays)

df<-merge(df, df.trapdays, by=intersect(names(df), names(df.trapdays)), all.x=TRUE)



#calculating distances moved between extractions and releases
library(geosphere)

df<-df %>% arrange(id_season,timestamp)

transloc<-df #%>% filter(Location == "E" | Location == "R")


transloc<-transloc %>% arrange(id_season,Date,timestamp) #arrange by id-season and timestamp to calculate the correct distances (except for first row distances to be removed next)
transloc["distance"] <- c(NA,
                    sapply(seq.int(2,nrow(transloc)), function(i){
                      distm(c(transloc$Long[i-1],transloc$Lat[i-1]),
                            c(transloc$Long[i], transloc$Lat[i]),
                            fun = distHaversine)   #this function will calculate distance between coordinates and the previous row's coordinates
                    })
)

transloc.firstrows<-transloc %>% group_by(id_season) %>%  filter(row_number() == 1) # create a column to identify the first record of each id-season
transloc.firstrows<-transloc.firstrows %>% select(c("id_season","Date","timestamp","distance"))
colnames(transloc.firstrows)[4]<-"distance2"
transloc<-merge(transloc, transloc.firstrows,by=intersect(names(transloc), names(transloc.firstrows)), all.x=TRUE) #merge that column back to the main df
transloc<-transloc %>% arrange(id_season,Date,timestamp)
transloc$distance<-ifelse(!(is.na(transloc$distance2)),NA,transloc$distance) #make all first distance values NA (no previous value from the same id-season)
transloc.R<-transloc %>% filter(Location == "R")
transloc.R$distance<-transloc.R$distance*0.000621371 #convert meters to miles

################calculating core translocation distance method A#############################################
df.core.A<-df.core.A %>% arrange(id_season,timestamp)

transloc.A<-df.core.A #%>% filter(Location == "E" | Location == "R")


transloc.A<-transloc.A %>% arrange(id_season,Date,timestamp) #arrange by id-season and timestamp to calculate the correct distances (except for first row distances to be removed next)
transloc.A["distance"] <- c(NA,
                          sapply(seq.int(2,nrow(transloc.A)), function(i){
                            distm(c(transloc.A$Long[i-1],transloc.A$Lat[i-1]),
                                  c(transloc.A$Long[i], transloc.A$Lat[i]),
                                  fun = distHaversine)   #this function will calculate distance between coordinates and the previous row's coordinates
                          })
)

transloc.A.firstrows<-transloc.A %>% group_by(id_season) %>%  filter(row_number() == 1) # create a column to identify the first record of each id-season
transloc.A.firstrows<-transloc.A.firstrows %>% select(c("id_season","Date","timestamp","distance"))
colnames(transloc.A.firstrows)[4]<-"distance2"
transloc.A<-merge(transloc.A, transloc.A.firstrows,by=intersect(names(transloc.A), names(transloc.A.firstrows)), all.x=TRUE) #merge that column back to the main df.core.A
transloc.A<-transloc.A %>% arrange(id_season,Date,timestamp)
transloc.A$distance<-ifelse(!(is.na(transloc.A$distance2)),NA,transloc.A$distance) #make all first distance values NA (no previous value from the same id-season)
transloc.A.R<-transloc.A %>% filter(Location == "R")
transloc.A.R$distance<-transloc.A.R$distance*0.000621371 #convert meters to miles

################calculating core translocation distance method B#############################################
df.core.B<-df.core.B %>% arrange(id_season,timestamp)

transloc.B<-df.core.B #%>% filter(Location == "E" | Location == "R")


transloc.B<-transloc.B %>% arrange(id_season,Date,timestamp) #arrange by id-season and timestamp to calculate the correct distances (except for first row distances to be removed next)
transloc.B["distance"] <- c(NA,
                            sapply(seq.int(2,nrow(transloc.B)), function(i){
                              distm(c(transloc.B$Long[i-1],transloc.B$Lat[i-1]),
                                    c(transloc.B$Long[i], transloc.B$Lat[i]),
                                    fun = distHaversine)   #this function will calculate distance between coordinates and the previous row's coordinates
                            })
)

transloc.B.firstrows<-transloc.B %>% group_by(id_season) %>%  filter(row_number() == 1) # create a column to identify the first record of each id-season
transloc.B.firstrows<-transloc.B.firstrows %>% select(c("id_season","Date","timestamp","distance"))
colnames(transloc.B.firstrows)[4]<-"distance2"
transloc.B<-merge(transloc.B, transloc.B.firstrows,by=intersect(names(transloc.B), names(transloc.B.firstrows)), all.x=TRUE) #merge that column back to the main df.core.B
transloc.B<-transloc.B %>% arrange(id_season,Date,timestamp)
transloc.B$distance<-ifelse(!(is.na(transloc.B$distance2)),NA,transloc.B$distance) #make all first distance values NA (no previous value from the same id-season)
transloc.B.R<-transloc.B %>% filter(Location == "R")
transloc.B.R$distance<-transloc.B.R$distance*0.000621371 #convert meters to miles



test<-df%>% filter(id_season == "PYBI_27_2015-2016")



#create the final summary df
summary<-data.frame(df$Snake_ID,df$season,df$id_season)
summary<-unique(summary)
summary<-summary %>% arrange(df.id_season)
min_dist_df<-min_dist_df %>% arrange(id_season)
summary<-cbind(summary,min_dist_df)

colnames(summary)[1]<-"Snake_ID"
colnames(summary)[2]<-"season"
summary<-merge(summary, min_dist_df, by=intersect(names(summary), names(min_dist_df)), all.x=TRUE)
summary<-merge(summary, min_dist.core_df, by=intersect(names(summary), names(min_dist.core_df)), all.x=TRUE)
summary<-merge(summary, min_dist.core.B_df, by=intersect(names(summary), names(min_dist.core.B_df)), all.x=TRUE)
colnames(summary)[1]<-"id.season"
colnames(summary)[5]<-"id_season"
summary<-merge(summary, removal.summ, by=intersect(names(summary), names(removal.summ)), all.x=TRUE)
summary<-merge(summary, weight.summ, by=intersect(names(summary), names(weight.summ)), all.x=TRUE)
summary<-merge(summary, effort, by=intersect(names(summary), names(effort)), all.x=TRUE)
summary<-summary %>% dplyr::select(-c(14,15))

#replacing NAs and NaNs with zero
summary[is.na(summary)] = 0 


#rename columns for adding to summary table
track<-track[, c("id","area")] 
colnames(track)[1]<-"id_season"
colnames(track)[2]<-"MCP_sqmi"

track.core.A<-track.core.A[, c("id","area")] 
colnames(track.core.A)[1]<-"id_season"
colnames(track.core.A)[2]<-"MCP.core.A_sqmi"

track.core.B<-track.core.B[, c("id","area")] 
colnames(track.core.B)[1]<-"id_season"
colnames(track.core.B)[2]<-"MCP.core.B_sqmi"

colnames(hr_overlap_cleaned)[1]<-"id_season"

colnames(hr_overlap_cleaned.core.A)[1]<-"id_season"
colnames(hr_overlap_cleaned.core.A)[2]<-"overlap_core.A"

colnames(hr_overlap_cleaned.core.B)[1]<-"id_season"
colnames(hr_overlap_cleaned.core.B)[2]<-"overlap_core.B"

#add mcp and consecutive overlap values to summary table
summary<-merge(summary, track, by=intersect(names(summary), names(track)), all.x=TRUE) #add the mcp values
summary<-merge(summary, track.core.A, by=intersect(names(summary), names(track.core.A)), all.x=TRUE) #add the mcp values
summary<-merge(summary, track.core.B, by=intersect(names(summary), names(track.core.B)), all.x=TRUE) #add the mcp values
summary<-merge(summary, hr_overlap_cleaned, by=intersect(names(summary), names(hr_overlap_cleaned)), all.x=TRUE) #add the overlap values
summary<-merge(summary, hr_overlap_cleaned.core.A, by=intersect(names(summary), names(hr_overlap_cleaned.core.A)), all.x=TRUE) #add the overlap values
summary<-merge(summary, hr_overlap_cleaned.core.B, by=intersect(names(summary), names(hr_overlap_cleaned.core.B)), all.x=TRUE) #add the overlap values

#add the trap days and inactivity days
summary<-merge(summary, trapdays.df, by=intersect(names(summary), names(trapdays.df)), all.x=TRUE) #add the overlap values

inactivity.summ<-df.trapdays %>% group_by(id_season) %>% summarise(total.inactive.days=sum(inactive.days,na.rm=T)) #sum inactivity days be id-season

summary<-merge(summary, inactivity.summ, by=intersect(names(summary), names(inactivity.summ)), all.x=TRUE) #add the inactivity values

summary<-summary %>% mutate(trapdays.corrected = trapdays - total.inactive.days) #subtract the inactivity values from the trap days to get the final trapday values

#merge the translocated distance values to the summary dataframe
transloc.R<-transloc.R %>% select(c("id_season","Date","timestamp","distance"))
colnames(transloc.R)[4]<-"transloc.dist"
transloc.summ<-transloc.R %>% group_by(id_season) %>% summarise(total.transloc.dist = sum(transloc.dist,na.rm=T)) #sum the translocation values fofr each individual-season
summary<-merge(summary,transloc.summ,by=intersect(names(summary), names(transloc.summ)), all.x=TRUE)
summary$min.dist.mi.corrected<-ifelse(is.na(summary$total.transloc.dist) | summary$total.transloc.dist == 0, summary$min.dist.mi,(summary$min.dist.mi-summary$total.transloc.dist))

#merge the core translocated distance values to the summary dataframe
transloc.A.R<-transloc.A.R %>% select(c("id_season","Date","timestamp","distance"))
colnames(transloc.A.R)[4]<-"transloc.A.dist"
transloc.A.summ<-transloc.A.R %>% group_by(id_season) %>% summarise(total.transloc.A.dist = sum(transloc.A.dist,na.rm=T)) #sum the transloc.Aation values fofr each individual-season
summary<-merge(summary,transloc.A.summ,by=intersect(names(summary), names(transloc.A.summ)), all.x=TRUE)
summary$min.dist.core.A.mi.corrected<-ifelse(is.na(summary$total.transloc.A.dist) | summary$total.transloc.A.dist == 0, summary$min.dist.core.A.mi,(summary$min.dist.core.A.mi-summary$total.transloc.A.dist))

#merge the core translocated distance values to the summary dataframe
transloc.B.R<-transloc.B.R %>% select(c("id_season","Date","timestamp","distance"))
colnames(transloc.B.R)[4]<-"transloc.B.dist"
transloc.B.summ<-transloc.B.R %>% group_by(id_season) %>% summarise(total.transloc.B.dist = sum(transloc.B.dist,na.rm=T)) #sum the transloc.Bation values fofr each individual-season
summary<-merge(summary,transloc.B.summ,by=intersect(names(summary), names(transloc.B.summ)), all.x=TRUE)
summary$min.dist.core.B.mi.corrected<-ifelse(is.na(summary$total.transloc.B.dist) | summary$total.transloc.B.dist == 0, summary$min.dist.core.B.mi,(summary$min.dist.core.B.mi-summary$total.transloc.B.dist))

#adding sample size to summary table
colnames(df.summ)[2]<-"sample.size"
colnames(df.core.A.summ)[2]<-"sample.size.core.A"
colnames(df.core.B.summ)[2]<-"sample.size.core.B"

summary<-merge(summary,df.summ,by=intersect(names(summary), names(df.summ)), all.x=TRUE)
summary<-merge(summary,df.core.A.summ,by=intersect(names(summary), names(df.core.A.summ)), all.x=TRUE)
summary<-merge(summary,df.core.B.summ,by=intersect(names(summary), names(df.core.B.summ)), all.x=TRUE)

#selecting relevant columns
colnames(summary)[8]<-"pythons.removed"
colnames(summary)[25]<-"trapdays.corrected.breeding.season"

summary.final<-summary %>% select(!(c("min.dist.mi","min.dist.core.A.mi","min.dist.core.B.mi","id.season","trapdays","total.inactive.days","total.transloc.dist","total.transloc.A.dist","total.transloc.B.dist")))
summary.final<-summary.final %>% select(3,2,1,4,6,5,7,8,9,10,11,12,23,24,25,13,14,15,16,17,18,19,20,21,22)

#function to round all numeric columns to x decimal places
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

summary.final<-round_df(summary.final, 2)

#replacing specific columns with zero
summary.final <- summary.final %>% 
  mutate(sample.size = replace_na(sample.size, 0),
         sample.size.core.A = replace_na(sample.size.core.A, 0),
         sample.size.core.B = replace_na(sample.size.core.B, 0)
         )

write.csv(summary.final,file="./data/summary_table.csv",row.names = FALSE)








