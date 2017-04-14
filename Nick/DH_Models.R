#Creates total suspension and expulsion variable
CPS_2011_ES_SB$Total.Suspend.Expul<-
  as.numeric(as.character(CPS_2011_ES_SB$Suspensions_ISS.OSS_N_2011)) +
      as.numeric(as.character(CPS_2011_ES_SB$Expelled_N_2011))

summary(CPS_2011_ES_SB$Total.Suspend.Expul)

#Test data set with no predictors in running the model
test.info<-na.omit(CPS_2011_ES_SB$Total.Suspend.Expul)
test<-hurdle(y=test.info,x=NULL,hurd=Inf,dist="poisson")
names(CPS_2011_ES_SB)

#Initial attempt at building a double hurdle model
CPS_new<-CPS_2011_ES_SB[!is.na(CPS_2011_ES_SB$Total.Suspend.Expul),]
hist(CPS_new$Total.Suspend.Expul)
names(CPS_new)
TSE<-CPS_new$Total.Suspend.Expul
temp<-CPS_new[,c(69,73,77,81)]
xmat<-as.data.frame(apply(temp,2,function(x) as.numeric(as.character(x))))
xmat1<-data.matrix(as.numeric(as.character(CPS_new[1:402,73])))
xmat1[is.na(xmat1),]<-57.27
xmatf<-cbind(rep(1,402),xmat1)

h.model<-hurdle(y=TSE,x=xmat1,dist="poisson",dist.2="poisson",plots=TRUE,iters=10000,burn=2000)

h.model1<-hurdle(y=TSE,x=xmat1,dist="poisson",dist.2="nb",plots=TRUE,iters=10000,burn=2000)

h.model2<-hurdle(y=TSE,x=xmat1,dist="poisson",dist.2="gpd",plots=TRUE,iters=10000,burn=2000)

comp<-data.frame(DIC=c(mean(h.model$DIC),mean(h.model1$DIC),mean(h.model2$DIC)),
                 pD=c(mean(h.model$pD),mean(h.model1$pD),mean(h.model2$pD)),
                 PPO=c(mean(h.model$PPO),mean(h.model1$PPO),mean(h.model2$PPO)),
                 CPO=c(mean(h.model$CPO),mean(h.model1$CPO),mean(h.model2$CPO)))
rownames(comp)<-c("SH","DH-NB","DH-GPD")
comp
#school population first as a covariate, look at posterior distribution of that covariate
#if the mean is around one it makes sense to use as an offset term
#Use log of population because of the link function
#effort is the offset term


dh.model<-dhurdle(Y=CPS_new$Expelled_N_2011,
                  Effort=CPS_new$Totals_20th_2011,
                  X=xmatf,V=NULL,v=NULL,spatial=c(F,F,F),
                  iters=10000, burn=1000)

mean(dh.model$beta[,2,"Zero"])
hist(dh.model$beta[,2,"Zero"])
plot(dh.model$beta[,2,"Zero"],type="l")
names(CPS_new)


#Useful variables: 59-61,perhaps any ISAT composite score, some sort of demographic breakdown,
#Use number enrolling in college as a function of school attendance as a proxy for motivation
#Perhaps include the number of misconducts, maybe incorporate mobility rates 
#may just be better to run the bivariate model with the spatial analysis
#Makes extreme distribution start at Inf for one extreme, or 1 for the other extreme
#Add in crime as covariate based on number per attendance boundary
#Remove Charter, Citywide-Option, Contract, Magnet, Military academy, selective-enrollment

hist(as.numeric(CPS_new$Suspensions_ISS.OSS_N_2011))
hist(as.numeric(CPS_new$Expelled_N_2011))
table(as.numeric(CPS_new$Expelled_N_2011))

summary(as.numeric(CPS_2015_ES_SB$Expelled_N_2015))
summary(as.numeric(CPS_2015_ES_SB$Suspensions_ISS.OSS_N_2015))
a<-CPS_2015_ES_SB[is.na(as.numeric(CPS_2015_ES_SB$Expelled_N_2015)),]
b<-CPS_2015_ES_SB[is.na(as.numeric(CPS_2015_ES_SB$Suspensions_ISS.OSS_N_2015)),]
cbind(a$School_ID,b$School_ID)
