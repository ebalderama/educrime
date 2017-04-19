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

y0 <- as.numeric(CPS_2015_ES_SB$Suspensions_ISS.OSS_N_2015)

x0 <- CPS_2015_ES_SB[,c(80,82,88,101,347,455,476,499)]
#Performs mean imputation for missing values in the X's
for (i in 1:dim(x)[2]){
  x[is.na(x[,i]),i] <- mean(x[,i], na.rm=TRUE)
  }

for (i in 1:dim(x0)[2]){
 x0[is.na(x0[,i]),i] <- mean(x0[,i], na.rm=TRUE)
}

##Converts X to a matrix then runs the double hurdle model
x <- as.matrix(scale(x))

x0 <- as.matrix(x0)

school_pop <- CPS_2015_ES_SB$Totals_20th_2015
school_pop[is.na(school_pop)] <- mean(school_pop, na.rm=TRUE)

school_pop0 <- CPS_2015_ES_SB$Totals_20th_2015
school_pop0[is.na(school_pop0)] <- mean(school_pop0, na.rm=TRUE)

ind_ES <- which(is.na(cbind(CPS_2015_ES_SB$Expelled_N_2015,CPS_2015_ES_SB$School_Boundary)), arr.ind=TRUE)
ind_SUS <- which(is.na(cbind(as.numeric(CPS_2015_ES_SB$Suspensions_ISS.OSS_N_2015),CPS_2015_ES_SB$School_Boundary)), arr.ind =TRUE)

#Eigenvectors
es.evecs <- CPS_2015_ES_SB[,500:514]
es.evecs <- as.matrix(es.evecs)

es.evecs0 <- CPS_2015_ES_SB[,500:514]
es.evecs0 <- as.matrix(es.evecs0)
#Final inputs for expulsions
y <- y[-unique(ind_ES)]

x <- x[-unique(ind_ES),]

school_pop <- school_pop[-unique(ind_ES)]

es.evecs <- es.evecs[-unique(ind_ES),]                
#final inputs for suspensions
y0 <- y0[-unique(ind_SUS)]

x0 <- x0[-unique(ind_SUS),]

school_pop0 <- school_pop0[-unique(ind_SUS)]

es.evecs0 <- es.evecs0[-unique(ind_SUS),]

#Elementary school expulsion models
dh.model.ES_exp1 <- dhurdle(Y=y,Effort=school_pop,X=x,
                  lowEx = 1, V=es.evecs,v=v_ES,spatial=c(T,T,F),
                  iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.ES_exp95 <- dhurdle(Y=y,Effort=school_pop,X=x,
                         lowEx = 3, V=es.evecs,v=v_ES,spatial=c(T,T,F),
                         iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.ES_expINF <- dhurdle(Y=y,Effort=school_pop,X=x,
                         lowEx = Inf, V=es.evecs,v=v_ES,spatial=c(T,T,F),
                         iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.ES_exp1$diags$DIC
mean(dh.model.ES_exp1$diags$cpo)
mean(dh.model.ES_exp1$diags$ppo)

dh.model.ES_exp95$diags$DIC
mean(dh.model.ES_exp95$diags$cpo)
mean(dh.model.ES_exp95$diags$ppo)

dh.model.HS_expINF$diags$DIC
mean(dh.model.ES_expINF$diags$cpo)
mean(dh.model.ES_expINF$diags$ppo)

temp.means1 <- vector()
temp.means2 <- vector()
for(i in 1:9){
  temp.means1[i] <- mean(dh.model.ES_expINF$beta[,i,"Zero"])
  temp.means2[i] <- mean(dh.model.ES_expINF$beta[,i,"Typical"])
}
colnames(x1)
temp.means3 <- vector()
for(i in 1:9){
  temp.means3[i] <- mean(dh.model.ES_exp95$beta[,i,"Extreme"])
}
colnames(x)
#Elementary EXP plots
hist(dh.model.ES_exp95$beta[,9,"Typical"],xlab="",main="Crime Distribution for Typical Counts")
abline(v=-7.57,col="red",lwd="5")

plot(dh.model.ES_exp95$beta[,9,"Typical"],xlab="Iterations",ylab="Crime Frequency",main="Trace Plot for Typical Crime Counts", type="l")
abline(h=-7.57,col="red",lwd="2")

help(abline)

#Elementary school suspension models
dh.model.ES_sus1 <- dhurdle(Y=y0 ,Effort=school_pop0, X=x0,
                         lowEx = 1, V=es.evecs0,v=v_ES,spatial=c(T,T,F),
                         iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.ES_sus95 <- dhurdle(Y=y0 ,Effort=school_pop0, X=x0,
                            lowEx = 104, V=es.evecs0,v=v_ES,spatial=c(T,T,F),
                            iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.ES_susINF <- dhurdle(Y=y0 ,Effort=school_pop0, X=x0,
                            lowEx = 1, V=es.evecs0,v=v_ES,spatial=c(T,T,F),
                            iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.ES_sus1$diags$DIC
mean(dh.model.ES_sus1$diags$cpo)
mean(dh.model.ES_sus1$diags$ppo)

dh.model.ES_sus95$diags$DIC
mean(dh.model.ES_sus95$diags$cpo)
mean(dh.model.ES_sus95$diags$ppo)

dh.model.HS_expINF$diags$DIC
mean(dh.model.ES_susINF$diags$cpo)
mean(dh.model.ES_susINF$diags$ppo)

#Elementary SUS plots
hist(dh.model.ES_sus1$beta[,9,"Extreme"],xlab="",main="Crime Distribution when Extreme Is Greater than 1")
abline(v=83,col="red",lwd="5")

plot(dh.model.ES_sus1$beta[,9,"Extreme"],xlab="Iterations",ylab="Crime Frequency",main="Trace Plot when Extreme is Greater than 1", type="l")
abline(h=83,col="red",lwd=3)

a.means <- vector()
for (i in 1:15){
  a.means[i] <- mean(dh.model.ES_expINF$alpha[,i,"Typical"], na.rm=TRUE)
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

x1 <- CPS_2015_HS_SB[,c(80,82,88,101,130,499)]

x1$SQRP.Total <- apply(cbind(CPS_2015_HS_SB$SQRP_HS_Points_2015.x,CPS_2015_HS_SB$SQRP_HS_Points_2015.y), 1, sum, na.rm=T)

x1$SQRP.Total[x1$SQRP.Total==0] <- NA

y01 <- as.numeric(CPS_2015_HS_SB$Suspensions_ISS.OSS_N_2015)

x01 <- CPS_2015_HS_SB[,c(80,82,88,101,130,499)]

x01$SQRP.Total <- apply(cbind(CPS_2015_HS_SB$SQRP_HS_Points_2015.x,CPS_2015_HS_SB$SQRP_HS_Points_2015.y), 1, sum, na.rm=T)

x01$SQRP.Total[x01$SQRP.Total==0] <- NA

#Performs mean imputation
for (i in 1:dim(x1)[2]){
  x1[is.na(x1[,i]),i] <- mean(x1[,i], na.rm=TRUE)
}

for (i in 1:dim(x01)[2]){
  x01[is.na(x01[,i]),i] <- mean(x01[,i], na.rm=TRUE)
}

school_pop.1 <- CPS_2015_HS_SB$Totals_20th_2015
school_pop.1[is.na(school_pop.1)] <- mean(school_pop.1, na.rm=T)

school_pop.01 <- CPS_2015_HS_SB$Totals_20th_2015
school_pop.01[is.na(school_pop.01)] <- mean(school_pop.01, na.rm=TRUE)

x1 <- scale(as.matrix(x1))

x01 <- scale(as.matrix(x01))

ind <- which(is.na(cbind(CPS_2015_HS_SB$Expelled_N_2015,CPS_2015_HS_SB$School_Boundary)), arr.ind = TRUE)
ind01 <- which(is.na(cbind(as.numeric(CPS_2015_HS_SB$Suspensions_ISS.OSS_N_2015),CPS_2015_HS_SB$School_Boundary)),arr.ind=TRUE)

#Eigenvectors
hs.evecs <- CPS_2015_HS_SB[,500:514]
hs.evecs <- as.matrix(hs.evecs)

hs.evecs01 <- CPS_2015_HS_SB[,500:514]
hs.evecs01 <- as.matrix(hs.evecs01)

#Final inputs for expulsion model
y1 <- y1[-unique(ind)]

x1 <- x1[-unique(ind),]

school_pop.1 <- school_pop.1[-unique(ind)]

hs.evecs <- hs.evecs[-unique(ind),]
#final inputs for suspension model
y01 <- y01[-unique(ind01)]

x01 <- x01[-unique(ind01),]

school_pop.01 <- school_pop.01[-unique(ind01)]

hs.evecs01 <- hs.evecs01[-unique(ind01),]

#High School Expulsions
dh.model.HS_exp1 <- dhurdle(Y=y1, Effort=school_pop.1, X=x1,
                           lowEx = 1, V=hs.evecs, v=v_HS, spatial=c(T,T,F),
                           iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.HS_exp95 <- dhurdle(Y=y1, Effort=school_pop.1, X=x1,
                           lowEx = 8, V=hs.evecs, v=v_HS, spatial=c(T,T,F),
                           iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.HS_expINF <- dhurdle(Y=y1, Effort=school_pop.1, X=x1,
                            lowEx = Inf, V=hs.evecs, v=v_HS, spatial=c(T,T,F),
                            iters=10000, burn=4000, nthin=10, plot=TRUE)
#Summaries of important info
dh.model.HS_exp1$diags$DIC
mean(dh.model.HS_exp1$diags$cpo)
mean(dh.model.HS_exp1$diags$ppo)

dh.model.HS_exp95$diags$DIC
mean(dh.model.HS_exp95$diags$cpo)
mean(dh.model.HS_exp95$diags$ppo)

dh.model.HS_expINF$diags$DIC
mean(dh.model.HS_expINF$diags$cpo)
mean(dh.model.HS_expINF$diags$ppo)

#Plots for HS EXP
plot(dh.model.HS_exp95$beta[,8,"Typical"],type='l',xlab="Iterations",ylab="Crime Frequency",main="Trace Plot for Typical Crime Counts")
abline(h=19,col="red",lwd="3")

hist(dh.model.HS_exp95$beta[,8,"Typical"],xlab="",main="Crime Distribution for Typical Counts")
abline(v=19, col="red",lwd = "5")


#High School Suspensions
dh.model.HS_sus1 <- dhurdle(Y=y01,Effort=school_pop.01,X=x01,
                           lowEx = 1, V=hs.evecs01, v=v_HS, spatial=c(T,T,F),
                           iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.HS_sus95 <- dhurdle(Y=y01,Effort=school_pop.01,X=x01,
                            lowEx = 85, V=hs.evecs01, v=v_HS, spatial=c(T,T,F),
                            iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.HS_susINF <- dhurdle(Y=y01,Effort=school_pop.01,X=x01,
                            lowEx = Inf, V=hs.evecs01, v=v_HS, spatial=c(T,T,F),
                            iters=10000, burn=4000, nthin=10, plot=TRUE)

dh.model.HS_sus1$diags$DIC
mean(dh.model.HS_sus1$diags$cpo)
mean(dh.model.HS_sus1$diags$ppo)

#Plots for HS SUS
hist(dh.model.HS_sus95$beta[,7,"Typical"], xlab="",main="Crime Distribution for Typical Counts")
abline(v=7.25,col="red",lwd="5")

plot(dh.model.HS_sus95$beta[,7,"Typical"],xlab="Iterations",ylab="Crime Frequency",main="Trace Plot for Typical Counts", type='l')
abline(h=7.25,col="red",lwd="3")

z.means1 <- vector()
t.means1 <- vector()

for (i in 1:8){
  z.means1[i] <- mean(dh.model.HS_exp95$beta[,i,"Zero"])
  t.means1[i] <- mean(dh.model.HS_exp95$beta[,i,"Typical"])
}
colnames(x1)
e.means <- vector()
for (i in 1:8) {
  e.means[i] <- mean(dh.model.HS_sus95$beta[,i,"Extreme"])
}
#Elem expul
elem.exp.comps <- data.frame("DIC" = c(dh.model.ES_exp1$diags$DIC,dh.model.ES_exp95$diags$DIC,dh.model.ES_expINF$diags$DIC),
                              "CPO" = c(mean(dh.model.ES_exp1$diags$cpo),mean(dh.model.ES_exp95$diags$cpo),mean(dh.model.ES_expINF$diags$cpo)),
                              "PPO" = c(mean(dh.model.ES_exp1$diags$ppo),mean(dh.model.ES_exp95$diags$ppo),mean(dh.model.ES_expINF$diags$ppo)))
rownames(elem.exp.comps) <- c("1","95","Extreme")

#Elem Sus
elem.sus.comps <- data.frame("DIC" = c(dh.model.ES_sus1$diags$DIC,dh.model.ES_sus95$diags$DIC,dh.model.ES_susINF$diags$DIC),
                             "CPO" = c(mean(dh.model.ES_sus1$diags$cpo),mean(dh.model.ES_sus95$diags$cpo),mean(dh.model.ES_susINF$diags$cpo)),
                             "PPO" = c(mean(dh.model.ES_sus1$diags$ppo),mean(dh.model.ES_sus95$diags$ppo),mean(dh.model.ES_susINF$diags$ppo)))
rownames(elem.sus.comps) <- c("1","95","Extreme")

#HighElem expul
high.exp.comps <- data.frame("DIC" = c(dh.model.HS_exp1$diags$DIC,dh.model.HS_exp95$diags$DIC,dh.model.HS_expINF$diags$DIC),
                             "CPO" = c(mean(dh.model.HS_exp1$diags$cpo),mean(dh.model.HS_exp95$diags$cpo),mean(dh.model.HS_expINF$diags$cpo)),
                             "PPO" = c(mean(dh.model.HS_exp1$diags$ppo),mean(dh.model.HS_exp95$diags$ppo),mean(dh.model.HS_expINF$diags$ppo)))
rownames(high.exp.comps) <- c("1","95","Extreme")

#High Sus
high.sus.comps <- data.frame("DIC" = c(dh.model.HS_sus1$diags$DIC,dh.model.HS_sus95$diags$DIC,dh.model.HS_susINF$diags$DIC),
                             "CPO" = c(mean(dh.model.HS_sus1$diags$cpo),mean(dh.model.HS_sus95$diags$cpo),mean(dh.model.HS_susINF$diags$cpo)),
                             "PPO" = c(mean(dh.model.HS_sus1$diags$ppo),mean(dh.model.HS_sus95$diags$ppo),mean(dh.model.HS_susINF$diags$ppo)))
rownames(high.sus.comps) <- c("1","95","Extreme")

write.csv(elem.exp.comps,file="D:/Documents/Elem_exp_comps.csv")
write.csv(elem.sus.comps,file="D:/Documents/Elem_sus_comps.csv")
write.csv(high.exp.comps,file="D:/Documents/High_exp_comps.csv")
write.csv(high.sus.comps,file="D:/Documents/High_sus_comps.csv")
