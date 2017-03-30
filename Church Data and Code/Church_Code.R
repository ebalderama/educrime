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
#ZIP codes for a shapefile
comm_areas<-readOGR("C:/Users/Nick Fox/Downloads/Boundaries - Community Areas/geo_export_4fbea0cb-8eca-47d2-8dd9-ba6ccb5ed7cd.shp")
comm.nb<-poly2nb(comm_areas)
w<-nb2mat(comm.nb,style="W")
rownames(w)
names(comm_areas)
comm_areas$area_numbe<-as.character(comm_areas$area_numbe)

#Homicide data
homicides<-read.csv("~/educrime/Chicago Data/homicides.csv")
homicides$id<-as.character(homicides$Community.Area-1)
summary(homicides$id)

#Final data sets
poly.df1<-tidy(comm_areas,region="area_numbe")

hom.poly.df<-left_join(poly.df1,homicides)

crime_freq<-as.data.frame(table(homicides$id))
crime_freq$id<-as.character(crime_freq$Var1)
final.df<-left_join(hom.poly.df,crime_freq)

#Church data
churches<-read.csv("D:/Downloads/Parish Address List.csv")
churches$full_address<-as.character(churches$full_address)

#School data
church_schools<-read.csv("D:/Downloads/school_address_list.csv")
church_schools$full_address<-as.character(church_schools$full_address)

#Gets the coordinates of the churches using the addresses
church_locations<-list()
for (i in 1:dim(churches)[1]) {
  church_locations[[i]]<-geocode(churches$full_address[i])
}

churches_final<-cbind(churches,do.call(rbind,church_locations))

#Gets the coordinates of the schools using the 
school_locations<-list()
for (i in 1:dim(church_schools)[1]){
  school_locations[[i]]<-geocode(church_schools$full_address[i])
}

church_schools_final<-cbind(church_schools,do.call(rbind,school_locations))

#Chicago Map
chimap<-get_map(location="Chicago",zoom=10,scale="auto",maptype="terrain")

#Maps the churches
ggmap(chimap) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=Freq), data=final.df) +
  scale_fill_gradientn(colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),alpha=.7,data=churches_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))

#Maps the schools
ggmap(chimap) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Freq), data=final.df) +
  scale_fill_gradientn(colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),alpha=.7,data=church_schools_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))

save(churches_final,church_schools_final,file="~/educrime/Church Data and Code/churchdat.Rdata")
