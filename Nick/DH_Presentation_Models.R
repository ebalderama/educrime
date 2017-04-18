################################# Elementary Schools #################################
#Modifies the data to be in numeric form, as well as fills in some missing values between columns
CPS_2015_ES_SB[,101] <- as.numeric(as.character(CPS_2015_ES_SB[,101]))
CPS_2015_ES_SB$SQRP_ELEM_Points_2015.y[44:47] <- c(3.8,3.8,3.8,3.8)
CPS_2015_ES_SB$SQRP_ELEM_Points_2015.y[50:53] <- c(2.8,2.8,2.8,2.8)

#Crime data for elementary schools
library(dplyr)
crime.temp1 <- CC_2015_ES_SB[CC_2015_ES_SB$Primary.Type==c("ASSAULT","BATTERY","ROBBERY","CRIM SEXUAL ASSAULT","HOMICIDE"),]

crime.count1 <- as.data.frame(table(crime.temp1$School_Boundary))
names(crime.count1) <- c("School_Boundary","Crime_Freq")
crime.count1$School_Boundary <- as.integer(crime.count1$School_Boundary)

CPS_2015_ES_SB <-left_join(CPS_2015_ES_SB,crime.count1)
names(CPS_2015_ES_SB)
#Creates X and Y variables for inputs
y <- CPS_2015_ES_SB$Expelled_N_2015

x <- CPS_2015_ES_SB[,c(80,82,88,101,347,455,476,499)]

#y0 <- as.numeric(CPS_2015_ES_SB$Suspensions_ISS.OSS_N_2015)

#x0 <- CPS_2015_ES_SB[,c(80,82,86,88,90,92,101,347,455,476,499)]
#Performs mean imputation for missing values in the X's
for (i in 1:dim(x)[2]){
  x[is.na(x[,i]),i] <- mean(x[,i], na.rm=TRUE)
  }

#for (i in 1:dim(x0)[2]){
#  x0[is.na(x0[,i]),i] <- mean(x0[,i], na.rm=TRUE)
#}

##Converts X to a matrix then runs the double hurdle model
x <- as.matrix(scale(x))

#x0 <- as.matrix(x0)
school_pop <- CPS_2015_ES_SB$Totals_20th_2015
school_pop[is.na(school_pop)] <- mean(school_pop)

ind_ES <- which(is.na(cbind(CPS_2015_ES_SB$Expelled_N_2015,CPS_2015_ES_SB$School_Boundary)), arr.ind=TRUE)
#Eigenvectors
es.evecs <- CPS_2015_ES_SB[,500:514]

es.evecs <- as.matrix(es.evecs)
#Final inputs
y <- y[-unique(ind_ES)]

x <- x[-unique(ind_ES),]

school_pop <- school_pop[-unique(ind_ES)]

es.evecs <- es.evecs[-unique(ind_ES),]                
#Elementary school expulsion models
dh.model.ES_exp<-dhurdle(Y=y,Effort=school_pop,X=x,
                  lowEx = 1, V=es.evecs,v=v_ES,spatial=c(T,T,F),
                  iters=2000, burn=1000, nthin=10, plot=TRUE)

#Elementary school suspension models
dh.model.ES_sus<-dhurdle(Y=y0[!is.na(y0)],Effort=school_pop[!is.na(y0)],X=x0[!is.na(y0),],
                         lowEx = 4, V=V_ES,v=v_ES,spatial=c(T,T,F),
                         iters=3000, burn=500, nthin=10, plot=TRUE)

for (i in 1:15){
  hist(dh.model.ES_exp$alpha[,i,"Zero"])
}

z.means <- vector()
t.means <- vector()
for (i in 1:9){
  z.means[i] <- mean(dh.model.ES_exp$beta[,i,"Zero"])
  t.means[i] <- mean(dh.model.ES_exp$beta[,i,"Typical"])
}
colnames
##################################### HIGH SCHOOLS ###############################
#Uses the 2015 data
library(dplyr)
CPS_2015_HS_SB[,101] <- as.numeric(as.character(CPS_2015_HS_SB[,101]))

crime.temp <- CC_2015_HS_SB[CC_2015_HS_SB$Primary.Type==c("ASSAULT","BATTERY","ROBBERY","CRIM SEXUAL ASSAULT","HOMICIDE"),]

crime.count <- as.data.frame(table(crime.temp$School_Boundary))
names(crime.count) <- c("School_Boundary","Crime_Freq")
crime.count$School_Boundary <- as.integer(crime.count$School_Boundary)
names(CPS_2015_HS_SB)
CPS_2015_HS_SB <-left_join(CPS_2015_HS_SB,crime.count)

y1 <- CPS_2015_HS_SB$Expelled_N_2015

#y01 <- as.numeric(CPS_2015_HS_SB$Suspensions_ISS.OSS_N_2015)

x1 <- CPS_2015_HS_SB[,c(80,82,88,101,130,499)]

x1$SQRP.Total <- apply(cbind(CPS_2015_HS_SB$SQRP_HS_Points_2015.x,CPS_2015_HS_SB$SQRP_HS_Points_2015.y), 1, sum, na.rm=T)

x1$SQRP.Total[x1$SQRP.Total==0] <- NA

#x01 <- CPS_2015_HS_SB[,c(80,82,86,88,90,92,101,130,499)]

#x01$SQRP.Total <- apply(cbind(CPS_2015_HS_SB$SQRP_HS_Points_2015.x,CPS_2015_HS_SB$SQRP_HS_Points_2015.y), 1, sum, na.rm=T)

#x01$SQRP.Total[x01$SQRP.Total==0] <- NA
#Performs mean imputation
for (i in 1:dim(x1)[2]){
  x1[is.na(x1[,i]),i] <- mean(x1[,i], na.rm=TRUE)
}

#for (i in 1:dim(x01)[2]){
 # x01[is.na(x01[,i]),i] <- mean(x01[,i], na.rm=TRUE)
#}

school_pop.1 <- CPS_2015_HS_SB$Totals_20th_2015
school_pop.1[is.na(school_pop.1)] <- mean(school_pop.1, na.rm=T)

#x1 <- x1[!is.na(y1),]
x1 <- scale(as.matrix(x1))

#x01 <- x01[!is.na(y01),]

#x01 <- as.matrix(x01)

#x01 <- cbind(rep(1,nrow(x1)),x1)

#Eigenvectors
hs.evecs <- CPS_2015_HS_SB[,500:514]

hs.evecs <- as.matrix(hs.evecs)

ind <- which(is.na(cbind(CPS_2015_HS_SB$Expelled_N_2015,CPS_2015_HS_SB$School_Boundary)), arr.ind = TRUE)

#Final inputs
y1 <- y1[-unique(ind)]

x1 <- x1[-unique(ind),]

school_pop.1 <- school_pop.1[-unique(ind)]

hs.evecs <- hs.evecs[-unique(ind),]

#High School Expulsions
dh.model.HS_exp <- dhurdle(Y=y1, Effort=school_pop.1, X=x1,
                           lowEx = 1, V=hs.evecs, v=v_HS, spatial=c(T,T,F),
                           iters=2000, burn=1000, nthin=10, plot=TRUE)


#High School Suspensions
dh.model.HS_sus <- dhurdle(Y=y01[!is.na(y01)],Effort=school_pop.1[!is.na(y01)],X=x01,
                           lowEx = 83, V=V_HS, v=v_HS, spatial=c(T,T,F),
                           iters=3000, burn=500, nthin=10, plot=TRUE)


z.means1 <- vector()
t.means1 <- vector()
for (i in 1:8){
  z.means1[i] <- mean(dh.model.HS_exp$beta[,i,"Zero"])
  t.means1[i] <- mean(dh.model.HS_exp$beta[,i,"Typical"])
}
