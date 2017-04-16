
#===============================================
# Adjacency Matrix
#===============================================
library(rgdal)
library(spdep)
library(maptools)

#ZIP code boundaries
bounds <- readOGR("Chicago Data/Boundaries - ZIP Codes/geo_export_f32b5a2b-4f79-41c5-8654-e2587746d820.shp","geo_export_f32b5a2b-4f79-41c5-8654-e2587746d820")

#Adjacency matrix:
ADJ <- nb2mat(poly2nb(bounds), style="B")


#===============================================
# Eigen decomposition
#===============================================
library(rARPACK)

#specifies Intrinsic CAR prior
rho = 1

#CAR covariance
Q <- diag(colSums(ADJ))-rho*ADJ

#Eigen-decomposition
evd <- eigs_sym(Q, nrow(bounds), "SM")

#consider only eigenvectors corresponding to nonzero eigenvalues
v <- rev(evd$values)
ev0 <- which(v < 0.0000001)
if(length(ev0)>0) v <- v[-ev0]

V <- evd$vectors[,ncol(evd$vectors):1]
if(length(ev0)>0) V <- V[,-ev0]


#look into total variance explained
varex <- cumsum(1/v)/sum(1/v)
which(varex > .5)[1]

plot(1:length(varex), varex, type = 's', ylab = "% variance explained", xlab = "eigenvector")
abline(h=0.5)
abline(h=0.67)

#use only the first few eigenvectors that capture "enough" spatial variability
v <- v[1:15]
V <- V[,1:15]



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

