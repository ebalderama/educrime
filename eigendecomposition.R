
#===============================================
# Adjacency Matrix
#===============================================
library(rgdal)
library(spdep)
library(maptools)

#ZIP code boundaries
bounds <- readOGR("Chicago Data/Boundaries - ZIP Codes/geo_export_f32b5a2b-4f79-41c5-8654-e2587746d820.shp","geo_export_f32b5a2b-4f79-41c5-8654-e2587746d820")

bounds_ES <- readOGR("Chicago Data/Chicago Public Schools - Elementary School Attendance Boundaries SY1617/geo_export_81861cd7-922f-48b5-a361-c09561bc92f3.shp","geo_export_81861cd7-922f-48b5-a361-c09561bc92f3")

bounds_HS <- readOGR("Chicago Data/Chicago Public Schools - High School Attendance Boundaries SY1617/geo_export_7f3862fd-0e72-4813-aed0-f14cc71e4fa4.shp","geo_export_7f3862fd-0e72-4813-aed0-f14cc71e4fa4")

#Adjacency matrix:
ADJ <- nb2mat(poly2nb(bounds), style="B")

ADJ_ES <- nb2mat(poly2nb(bounds_ES))

ADJ_HS <- nb2mat(poly2nb(bounds_HS))
#===============================================
# Eigen decomposition
#===============================================
library(rARPACK)

#specifies Intrinsic CAR prior
rho = 1

#CAR covariance
Q <- diag(colSums(ADJ))-rho*ADJ

Q_ES <- diag(colSums(ADJ_ES))-rho*ADJ_ES

Q_HS <- diag(colSums(ADJ_HS))-rho*ADJ_HS

#Eigen-decomposition
evd <- eigs_sym(Q, nrow(bounds), "SM")

evd_ES <- eigs_sym(Q_ES, nrow(bounds_ES), "SM")

evd_HS <- eigs_sym(Q_HS, nrow(bounds_HS), "SM")

#consider only eigenvectors corresponding to nonzero eigenvalues
#ZIPS
v <- rev(evd$values)
ev0 <- which(v < 0.0000001)
if(length(ev0)>0) v <- v[-ev0]

V <- evd$vectors[,ncol(evd$vectors):1]
if(length(ev0)>0) V <- V[,-ev0]

#Elementary schools
v_ES <- rev(evd_ES$values)
ev0_ES <- which(v_ES < 0.0000001)
if(length(ev0_ES)>0) v_ES <- v_ES[-ev0_ES]

V_ES <- evd_ES$vectors[,ncol(evd_ES$vectors):1]
if(length(ev0_ES)>0) V_ES <- V_ES[,-ev0_ES]

#High Schools
v_HS <- rev(evd_HS$values)
ev0_HS <- which(v_HS < 0.0000001)
if(length(ev0_HS)>0) v_HS <- v_HS[-ev0_HS]

V_HS <- evd_HS$vectors[,ncol(evd_HS$vectors):1]
if(length(ev0_HS)>0) V_HS <- V_HS[,-ev0_HS]


#look into total variance explained
varex <- cumsum(1/v)/sum(1/v)
which(varex > .5)[1]

plot(1:length(varex), varex, type = 's', ylab = "% variance explained", xlab = "eigenvector")
abline(h=0.5)
abline(h=0.67)

#use only the first few eigenvectors that capture "enough" spatial variability
v <- v[1:15]
V <- V[,1:15]

v_ES <- v_ES[1:15]
V_ES <- V_ES[,1:15]

v_HS <- v_HS[1:15]
V_HS <- V_HS[,1:15]


#===============================================
#Visualize the eigenvectors
#===============================================
library(broom)
library(dplyr)
library(ggplot2)
library(fields)

bounds.df <- tidy(bounds,region="zip")
ev.df <- data.frame(id = as.character(bounds$zip), V = V)
df <- left_join(bounds.df, ev.df, by="id")

ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = V.1), data = df) +
  scale_fill_gradientn(name = "", colours = tim.colors(), na.value = NA) +
  ggtitle("Eigenvector 1")

ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = V.2), data = df) +
  scale_fill_gradientn(name = "", colours = tim.colors(), na.value = NA) +
  ggtitle("Eigenvector 2")

