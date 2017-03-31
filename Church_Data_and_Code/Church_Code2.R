library(gdata)
library(betareg)
#Elementary Schools
account.elem<-read.xls("Accountability_SQRPratings_2016-2017_SchoolLevel.xls",sheet=2,skip=2)

SQRP <- account.elem[,4]
summary(SQRP)

Y <- (SQRP-1)/4
Y[Y==1] <- 0.999
summary(Y)

X <- account.elem[]

g <- betareg(Y ~ 1)


account.elem$SchoolID<-account.elem$School.ID

#Creates a data set using only the continuous variables to start out with
account_elem_final<-account.elem[,-c(1:6,74,80:83)]

#Creates the modified points variable then adjusts for cases equal to 1
account_elem_final$modPoints <- (account.elem[,4]-1)/4
account_elem_final$modPoints[account_elem_final$modPoints==1]<-.999

#This model does not work
betareg(modPoints~.,data=account_elem_final)

#This model works
betareg(modPoints~Score+Score.1,data=account_elem_final)



comm_areas<-readOGR("Church_Data_and_Code/Boundaries - Community Areas/geo_export_4fbea0cb-8eca-47d2-8dd9-ba6ccb5ed7cd.shp")
homicides<-read.csv("Chicago Data/homicides.csv")
pop.dat<-read.csv("Church_Data_and_Code/2010_Census.csv", skip=1)







#===========================================================
#===========================================================
#===========================================================
#===========================================================
#===========================================================

library(foreign)
vc <- read.spss("Church_Data_and_Code/2016violentcrimes.sav", to.data.frame = TRUE)


xc <- vc[,3]
yc <- vc[,4]
good.data <- !(xc<100000 | yc<1000000 | is.na(xc) | is.na(yc))

vcxy <- data.frame(x=xc[good.data], y=yc[good.data])
vcll <- data.frame(lon=as.numeric(as.character(vc$Longitude))[good.data], lat=vc$Latitude[good.data])




#===========================================================
# St. Sabina – 1210 W 78th Place (Auburn Gresham)
# St. Benedict the African – 340 W 66th Street (Englewood) 
# Immaculate Conception – 2745 W 44th Street (Brighton Park)
# St. Michael the Archangel – 4821 S Damen Ave (New City)
# St. Agatha – 3147 W Douglas Blvd (North Lawndale)
# St. Martin de Porres – 5112 W Washington Blvd (Austin)
#===========================================================

church1 <- geocode("1210 W 78th Place, Chicago, IL", output = "more")
church2 <- geocode("340 W 66th Street, Chicago, IL", output = "more")
church3 <- geocode("2745 W 44th Street, Chicago, IL", output = "more")
church4 <- geocode("4821 S Damen Ave, Chicago, IL", output = "more")
church5 <- geocode("3147 W Douglas Blvd, Chicago, IL", output = "more")
church6 <- geocode("5112 W Washington Blvd, Chicago, IL", output = "more")

Sabina <- get_map(church1$address, zoom=14)
Benedict <- get_map(church2$address, zoom=14)
Immaculate <- get_map(church3$address, zoom=14)
Michael <- get_map(church4$address, zoom=14)
Agatha <- get_map(church5$address, zoom=14)
Martin <- get_map(church6$address, zoom=14)




#===========================================================
# Distances to violent crimes
#1 meter is 3.28084 feet
#1 meter is 0.00062137119223733386 miles
#===========================================================
dist1 <- distHaversine(church1[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church2[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church3[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church4[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church5[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)

dist1 <- distHaversine(church6[,1:2], vcll) #in meters
dist1 <- dist1*0.00062137119223733386 #in miles
sum(dist1 < 1)




#===========================================================
# Kernel Density Estimation
#===========================================================
disc.area <- dist1 < 1  #disc radius
pixels <- 50  #resolution for kde2d



getKde <- function(in_df, n=100, lims=c(range(in_df[,1]),range(in_df[,2]))){
  require(MASS)
  #pts <- as.matrix(in_df[,c('lon','lat')])
  pts <- as.matrix(in_df[,1:2])
  dens <- kde2d(pts[,1],pts[,2],n=n,lims=lims)
  #dens$z <- dens$z#/(xyarea/degarea)
  dens_df <- data.frame(expand.grid(dens$x,dens$y),z=c(dens$z))#*length(pts[,1]))
  colnames(dens_df) <- c('x','y','z')
  return(dens_df)
}

library(splancs)
xrange = c(sbox(as.matrix(vcxy[disc.area,]))[1,1], sbox(as.matrix(vcxy[disc.area,]))[2,1])
yrange = c(sbox(as.matrix(vcxy[disc.area,]))[1,2], sbox(as.matrix(vcxy[disc.area,]))[4,2])

londpm <- diff(range(vcll$lon))/diff(range(vcxy$x))
latdpm <- diff(range(vcll$lat))/diff(range(vcxy$y)) #degrees per meter
lonrange <- c(min(vcll$lon[disc.area]) - (min(vcxy$x[disc.area])-xrange[1])*londpm,
              max(vcll$lon[disc.area]) + (xrange[2]-max(vcxy$x[disc.area]))*londpm)
latrange <- c(min(vcll$lat[disc.area]) - (min(vcxy$y[disc.area])-yrange[1])*latdpm,
              max(vcll$lat[disc.area]) + (yrange[2]-max(vcxy$y[disc.area]))*latdpm)





#===========================================================
# data frame for mapping
#===========================================================
densxy.df <- getKde(vcxy[disc.area,], n=pixels, lims=c(xrange,yrange))
densll.df <- getKde(vcll[disc.area,], n=pixels, lims=c(lonrange,latrange))

dens.df <- data.frame(lon=densll.df$x, lat=densll.df$y)
dens.df$density <- densxy.df$z
dens.df$density[densxy.df$z < 1e-9] <- NA
dens.df$rate <- densxy.df$z*sum(disc.area)
dens.df$rate[densxy.df$z < 1e-9] <- NA
#===========================================================



degarea <- diff(lonrange)*diff(latrange)
xyarea <- diff(xrange)*diff(yrange)

sum(disc.area) #total crimes in disc

mean(dens.df$density*sum(disc.area)*xyarea*pi/4, na.rm=T) #should be about n
mean(dens.df$density, na.rm=T)*xyarea*pi/4 #should be about 1
sum(dens.df$density, na.rm=T)*xyarea/pixels^2 #should be about 1









#===========================================================
# Maps
#===========================================================


## Church 1
ggmap(Sabina) + 
  geom_point(aes(lon, lat), data = church1, col=2, size=2) +
  geom_point(aes(lon, lat), data = church1, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","high","")) +
  ggtitle("St. Sabina", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Sabina_kern.png")

## Church 2
ggmap(Benedict) + 
  geom_point(aes(lon, lat), data = church2, col=2, size=2) +
  geom_point(aes(lon, lat), data = church2, alpha=.4, col=2, size=7) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","","high")) +
  ggtitle("St. Benedict the African", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Benedict_kern.png")

## Church 3
ggmap(Immaculate) + 
  geom_point(aes(lon, lat), data = church3, col=2, size=2) +
  geom_point(aes(lon, lat), data = church3, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","","low","","high","")) +
  ggtitle("Immaculate Conception", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Immaculate_kern.png")

## Church 4
ggmap(Michael) + 
  geom_point(aes(lon, lat), data = church4, col=2, size=2) +
  geom_point(aes(lon, lat), data = church4, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","high","")) +
  ggtitle("St. Michael the Archangel", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Michael_kern.png")


## Church 5
ggmap(Agatha) + 
  geom_point(aes(lon, lat), data = church5, col=2, size=2) +
  geom_point(aes(lon, lat), data = church5, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","high","")) +
  ggtitle("St. Agatha", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Agatha_kern.png")

## Church 6
ggmap(Martin) + 
  geom_point(aes(lon, lat), data = church6, col=2, size=2) +
  geom_point(aes(lon, lat), data = church6, alpha=.4, col=2, size=8) +
  geom_point(aes(lon, lat), data = subset(vc1, dist1 < 1),  size=.5) +
  geom_tile(aes(lon, lat, fill=density), alpha=.3, data=dens.df) +
  scale_fill_gradientn(colours=tim.colors(), na.value=NA, labels=c("","low","","","high","")) +
  ggtitle("St. Martin de Porres", "Density of violent crimes within 1 mile") + 
  labs(x="Longitude", y="Latitude")
ggsave("Martin_kern.png")





