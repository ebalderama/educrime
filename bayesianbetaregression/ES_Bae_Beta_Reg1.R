
library(Bayesianbetareg)

CPS_All_ES <- read.csv("bayesianbetaregression/CPS_All_ES.csv", header = T)

######################################################################################
#                                SQRP Transfrom
######################################################################################

CPS_All_ES$SQRP_ES_Transform <- ((CPS_All_ES$SQRP_ELEM_Points - 1) / 4)

######################################################################################
#                                Beta RegrESsion for ES
######################################################################################

Y <- CPS_All_ES$SQRP_ES_Transform
Y <- as.matrix(Y)

#temp <- which(is.na(cbind(Y, X)), arr.ind = T)
#temp


X1 <- CPS_All_ES$Crime_Count
X2 <- CPS_All_ES$Totals_20th
X3 <- CPS_All_ES$Bilingual_N
X4 <- CPS_All_ES$SpED_N
X5 <- CPS_All_ES$FreeReducedLunch_N

X <- scale(cbind(X1, X2, X4, X5))
X <- cbind(1, X)

Z <- scale(cbind(X1))
Z <- cbind(1, Z)


burn <- 0.2
jump <- 10
nsim <- 5000
bpri <- rep(0, ncol(X))
Bpri <- diag(10, ncol(X), ncol(X))
gpri <- rep(0, ncol(Z))
Gpri <- diag(10, ncol(Z), ncol(Z))


#This is fast (n1 observations):
n1 <- 100
system.time(
re <- Bayesianbetareg(head(Y, n1), head(X, n1), head(Z, n1), nsim, bpri, Bpri, gpri, Gpri, burn, jump, graph1=F, graph2=F)
)

summary(re)

#trace plots
plot(re$beta.mcmc)
plot(re$gamma.mcmc)




#This is very slow (all n observations):

system.time(
re <- Bayesianbetareg(Y, X, Z, nsim, bpri, Bpri, gpri, Gpri, burn, jump, graph1=F, graph2=F)
)


summary(re)

