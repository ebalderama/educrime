library(readxl)
library(gdata)

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

#Creates a perl executable for the read.xls function
perl<-"C:/Strawberry/perl/bin/perl5.24.1.exe"

#Elementary Schools
account.elem<-read.xls("D:/Documents/Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=2,skip=2,perl=perl)
account.elem$SchoolID<-account.elem$School.ID

#High Schools
account.high<-read.xls("D:/Documents/Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=3,skip=2,perl=perl)
account.high$SchoolID<-account.high$School.ID
str(account.high)

#Combination Schools
account.combo<-read.xls("D:/Documents/Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=4,skip=3,perl=perl)
account.combo$SchoolID<-account.combo$School.ID

#Option Schools
account.option<-read.xls("D:/Documents/Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=3,skip=2,perl=perl)
account.option$SchoolID<-account.option$School.ID

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

#Beta regression
install.packages("betareg")
library(betareg)

#Start by only working with continuous variables
#Elementary schools
account.elem$modPoints<-(account.elem$SQRP.Total.Points.Earned-1)/4
account.elem$modPoints[account.elem$modPoints==1]<-.999

elem_reg<-betareg(modPoints~Score+Score.1+Score.2+Score.3+Score.4+Score.5+Score.6+Score.7+Score.8+Score.9+Score.10+Score.11+Score.12+Score.13+Score.14+Score.15+Score.,data=account.elem)
summary(elem_reg)

#High Schools
account.high$modPoints<-(account.high$SQRP.Total.Points.Earned-1)/4
account.high$modPoints[account.high$modPoints==1]<-.999

#Including Score.4 created an error
high_reg<-betareg(modPoints~Score+Score.1+Score.2+Score.5+Score.+Score..1+Score..2+Score..3+Score..4+Score..5+Score..6+Score..8,data=account.high)
summary(elem_reg)

#Combo schools
account.combo$modPoints<-(account.combo$SQRP.Total.Points.Earned-1)/4
account.combo$modPoints[account.combo$modPoints==1]<-.999
str(account.combo[,100:137])

#Not including score.5, score.6, Score.9, Score.10, Score.11, score.13, score.14, score., score., score..1-11, score.16-21 
combo_reg<-betareg(modPoints~Score+Score.1+Score.2+Score.3+Score.4+Score.7+Score.8+Score+Score.12,data=account.combo)
summary(combo_reg)

#Option 
account.option$modPoints<-(account.option$SQRP.Total.Points.Earned-1)/4
account.option$modPoints[account.option$modPoints==1]<-.999

#Cannot include Score.4, also an issue with Score..7
option_reg<-betareg(modPoints~Score+Score.1+Score.2+Score.5+Score..1+Score..2+Score..3+Score..4+Score..5+Score..6+Score..8,data=account.option)
summary(option_reg)
Contact GitHub API Training Shop Blog About
© 2017 GitHub, Inc. Terms Privacy Security Status Help

