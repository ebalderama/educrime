library(readxl)

library(sp)
library(spdep)
library(maptools)
library(rgdal)

library(rgeos)
library(ggmap)
library(grid)
library(fields)
library(RColorBrewer)
library(dplyr)
library(broom)
library(geosphere)
#Contain crimes to one year, for each school find the distance to each crime only in that
#district


#Elementary Schools
account.elem<-read_excel("Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=2,skip=1)
account.elem$SchoolID<-account.elem$`School ID`

#High Schools
account.high<-read_excel("Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=3,skip=1)
account.high$SchoolID<-account.high$`School ID`

#Combination Schools
account.combo<-read_excel("Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=4,skip=2)
account.combo$SchoolID<-account.combo$`School ID`

#Option Schools
account.option<-read_excel("Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=3,skip=1)
account.option$SchoolID<-account.option$`School ID`

#ZIP code shape file
zips<-readOGR("Boundaries - ZIP Codes/geo_export_f32b5a2b-4f79-41c5-8654-e2587746d820.shp","geo_export_f32b5a2b-4f79-41c5-8654-e2587746d820")
poly.df<-tidy(zips,region="zip")

#School District Shapefiles
dists<-readOGR("CPS Geographic Networks/geo_export_99bc1f97-7a71-4da1-829c-48549be68f20.shp","geo_export_99bc1f97-7a71-4da1-829c-48549be68f20")
poly.dist<-tidy(dists,region="network_nu")

#School locations
schools<-read.csv("CPS_Schools_2013-2014_Academic_Year.csv")

#Crime data
crimes2015<-read.csv("C:/Users/Nick/Dropbox/CPS Spatial/Crimes_-_2015.csv")

#Final elementary school data
elem.df<-merge(schools, account.elem, by="SchoolID")
elem.df$Total.Points<-elem.df$`SQRP Total Points Earned`



#Final high school data
high.df<-merge(schools, account.high, by="SchoolID")
high.df$Total.Points<-high.df$`SQRP Total Points Earned`

#Final combination school data
combo.df<-merge(schools, account.combo, by="SchoolID")
combo.df$Total.Points<-combo.df$`SQRP Total Points Earned`

#Final option school data
option.df<-merge(schools, account.option, by="SchoolID")
option.df$Total.Points<-option.df$`SQRP Total Points Earned`

#Chicago Map
chimap<-get_map(location="Chicago",zoom=10,scale="auto",maptype="terrain")

#Elementary School Map using ZIP codes
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.df) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=elem.df) + 
  scale_color_gradient2(limits=c(1,5), low="white" , mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 

#High School Map using ZIP codes
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.df) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=high.df) + 
  scale_color_gradient2(limits=c(1,5), mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 

#Combination School Map using ZIP codes
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.df) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=combo.df) + 
  scale_color_gradient2(limits=c(1,5), mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 

#Option School Map using ZIP codes
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.df) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=option.df) + 
  scale_color_gradient2(limits=c(1,5), mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 

#Elementary School Map using school districts
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.dist) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=elem.df) + 
  scale_color_gradient2(limits=c(1,5), mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 

#High School Map using school districts
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.dist) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=high.df) + 
  scale_color_gradient2(limits=c(1,5), mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 

#Combination School Map using school districts
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.dist) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=combo.df) + 
  scale_color_gradient2(limits=c(1,5), mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 

#Option School Map using school districts
ggmap(chimap) + 
  geom_path(aes(x=long, y=lat, group=group), data=poly.dist) +
  geom_point(aes(x=Longitude,y=Latitude, color=Total.Points), alpha=.5,data=option.df) + 
  scale_color_gradient2(limits=c(1,5), mid="yellow",high="purple4")+
  scale_x_continuous(limits = c(-87.96,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.64,42.05), expand = c(0, 0)) 


#Finds the distances from a set of crimes to a couple schools
#Goal is to apply this more widely across crimes and schools
homicide_data<-crimes2015[crimes2015$Primary.Type=="HOMICIDE",c(21,20)]
school_coords<-cbind(schools$Longitude,schools$Latitude)

crime_distances<-distm(school_coords,homicide_data,fun=distVincentyEllipsoid)

dist_conv<-function(x){
  x_km<-x/1000
  x_mile<-x_km*0.621371
}
crimes_distances_miles<-apply(crime_distances,1,dist_conv)

homicide_distances<-apply(crime_distances,1,dist_conv)

#Attempts to use over to find aggregates
xy<-cbind(elem.df$Longitude,elem.df$Latitude)
elem.points<-data.frame(Points=elem.df$Total.Points)
spdf <- SpatialPointsDataFrame(coords = xy, data = elem.points,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

p1<-Polygon(zips@polygons[[1]]@Polygons[[1]]@coords)
p2<-Polygons(list(p1),"s1")
l<-list(p2)
sps<-SpatialPolygons(l,proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

over(spdf,sps,fn=mean)
help("SpatialPolygons")

new.df<-as.data.frame(cbind(poly.df$long,poly.df$lat,poly.df$id))
new.list<-split(new.df,new.df$V3)
final.list<-lapply(new.list,function(x){x["id"]<-NULL;x})
ps<-sapply(final.list,Polygon)
help(sapply)
str(new.list)
help(over)