#Modifies the data to be in numeric form, as well as fills in some missing values between columns
CPS_2015_ES_SB[,101] <- as.numeric(as.character(CPS_2015_ES_SB[,101]))
CPS_2015_ES_SB$SQRP_ELEM_Points_2015.y[44:47] <- c(3.8,3.8,3.8,3.8)
CPS_2015_ES_SB$SQRP_ELEM_Points_2015.y[50:53] <- c(2.8,2.8,2.8,2.8)

#This is a data set without missing values in the Y variable
CPS_ES_Final <- CPS_2015_ES_SB[!is.na(CPS_2015_ES_SB$Expelled_N_2015),]

#Creates X and Y variables for inputs
y <- CPS_2015_ES_SB$Expelled_N_2015

x <- CPS_2015_ES_SB[,c(80,82,84,86,88,90,92,94,101,347,460,480)]

#Performs mean imputation for missing values in the X's
for (i in 1:dim(x)[2]){
  x[is.na(x[,i]),i] <- mean(x[,i], na.rm=TRUE)
  }

##Converts X to a matrix then runs the double hurdle model
x <- as.matrix(x)

school_pop <- CPS_2015_ES_SB$Totals_20th_2015
school_pop[is.na(school_pop)] <- 1

x <- x[,-c(3,8)]

dh.model<-dhurdle(Y=y,Effort=school_pop,X=x,
                  lowEx = Inf, V=NULL,v=NULL,spatial=c(F,F,F),
                  iters=2000, burn=50, keepmiss=TRUE, plot=TRUE)

