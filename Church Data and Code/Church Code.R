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
  geom_point(aes(x=lon,y=lat),alpha=.7,data=churches_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))

#Maps the schools
ggmap(chimap) + 
  geom_point(aes(x=lon,y=lat),alpha=.7,data=church_schools_final) +
  scale_x_continuous(limits = c(-87.86,-87.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(41.62,42.04), expand = c(0, 0))

save(churches_final,church_schools_final,file="~/educrime/Church Data and Code/churchdat.Rdata")

