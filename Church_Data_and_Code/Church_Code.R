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



#===========================================
#Population data
#===========================================
pop.dat<-read.csv("~/educrime/Church_Data_and_Code/2010_Census.csv", skip=1)
pop.dat <- read.csv("Church_Data_and_Code/2010_Census.csv", skip=1)

#pop.final <- data.frame(id=as.character(pop.dat$GeogKey), population=pop.dat$Total.Population)
#pop.final$community<-toupper(as.character(pop.final$community))
#pop.final$community[32]<-c("LOOP")
#pop.final$community[76]<-c("OHARE")
#pop.final$id <- as.character(1:77)



#===========================================
#Homicide crimes
#===========================================
homicides <- read.csv("Chicago Data/homicides.csv")

total.homicides <- rep(0,77)
temp1 <- table(homicides$Community.Area)
total.homicides[as.numeric(names(temp1))] <- temp1


#Total crime data
crime <- read.csv("D:/Downloads/Chicago Data/Crimes_-_2015.csv")
crime$Primary.Type<-as.character(crime$Primary.Type)
vio.crime<-crime[crime$Primary.Type==c("ASSAULT","BATTERY","ROBBERY","CRIM SEXUAL ASSAULT","HOMICIDE"),]

total.viocrime <- rep(0,77)
temp2 <- table(vio.crime$Community.Area)
total.viocrime[as.numeric(names(temp2))] <- temp2

#===========================================
#ZIP codes for a shapefile
#===========================================
comm_areas<-readOGR("C:/Users/Nick Fox/Downloads/Boundaries - Community Areas/geo_export_4fbea0cb-8eca-47d2-8dd9-ba6ccb5ed7cd.shp")
comm_areas<-readOGR("Church_Data_and_Code/Boundaries - Community Areas/geo_export_4fbea0cb-8eca-47d2-8dd9-ba6ccb5ed7cd.shp","geo_export_4fbea0cb-8eca-47d2-8dd9-ba6ccb5ed7cd")

#polygon data frame
poly.df1 <- tidy(comm_areas,region="area_numbe")




#===========================================
# Final data frame for mapping
# (add any new variables for mapping here
# then re-run this block)
#===========================================
comm_data <- data.frame(id=as.character(1:77),
                        Population=pop.dat$Total.Population,
                        Homicide=total.homicides
                        )

comm_data1<- data.frame(id=as.character(1:77),
                        Population=pop.dat$Total.Population,
                        Violent_Crime=total.viocrime)

#Final data sets
final.df <- left_join(poly.df1, comm_data)

final.df1 <- left_join(poly.df1, comm_data1)
#===========================================





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




#===========================================
#Chicago Map
#===========================================
chimap<-get_map(location="Chicago",zoom=10,scale="auto",maptype="terrain")


#===========================================
#Maps the churches using raw numbers and rates
#===========================================
mytheme <- theme(plot.title=element_text(size=rel(1.5),face="bold"),
                 axis.title=element_text(size=rel(1.2)),
                 axis.text=element_text(size=rel(1.2)),
                 #legend.title=element_blank(),
                 legend.title=element_text(size=14,colour="black",face="bold"),
                 legend.text=element_text(size=12,colour="black",face="bold"),
                 # legend.background=element_rect(colour=NULL,fill="white"),
                 legend.background=element_blank(),
                 legend.justification=c(1,1),
                 #legend.position=c(0.05,0.07),
                 legend.position=c(.98,.98),
                 legend.box="horizontal",
                 #legend.margin=unit(.75,"cm"), 
                 # legend.key=element_rect(colour="white"),
                 legend.key=element_blank(),
                 legend.key.size=unit(2,"lines"),
                 legend.key.width=unit(.75,"cm"),
                 legend.key.height=unit(.75,"cm"),
                 legend.title.align=0,
                 legend.text.align=0
                 )


ggmap(chimap) + 
#ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=Violent_Crime), alpha=.8, col=1, size=.1, data=final.df1) +
  scale_fill_gradientn("", colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),data=churches_final) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Total Violent Crime") +
  mytheme +
  scale_x_continuous(limits = c(-88,-87.4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.6,42.1), expand = c(0, 0))+ 
  geom_point(aes(lon,lat), data = church3, col=2, size=2) 
  
ggmap(chimap) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=Violent_Crime/Population), alpha=.8, col=1, size=.1, data=final.df1) +
  scale_fill_gradientn("per capita rate", colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),data=churches_final) +
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Total Violent Crime per Capita") +
  mytheme +
  scale_x_continuous(limits = c(-88,-87.4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.6,42.1), expand = c(0, 0))




ggmap(chimap) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=Homicide), col="black", size=.1, data=final.df) +
  scale_fill_gradientn(colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),alpha=.7,data=churches_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))



ggmap(chimap) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=rate), data=final.df) +
  scale_fill_gradientn(colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),alpha=.7,data=churches_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))

#Maps the schools using raw numbers and rates
ggmap(chimap) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Freq), data=final.df) +
  scale_fill_gradientn(colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),alpha=.7,data=church_schools_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))

ggmap(chimap) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=rate), data=final.df) +
  scale_fill_gradientn(colors=tim.colors()) +
  geom_point(aes(x=lon,y=lat),alpha=.7,data=church_schools_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))

save(churches_final,church_schools_final,file="~/educrime/Church Data and Code/churchdat.Rdata")
