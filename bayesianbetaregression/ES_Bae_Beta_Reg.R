library(Bayesianbetareg)

CPS_All_ES <- read.csv("/Users/jcote/Desktop/CPS_CC_Beta/CPS_All_ES.csv", header = T)

######################################################################################
#                                SQRP Transfrom
######################################################################################

CPS_All_ES$SQRP_ES_Transform <- ((CPS_All_ES$SQRP_ELEM_Points - 1) / 4)

######################################################################################
#                                Beta RegrESsion for ES
######################################################################################

Y <- CPS_All_ES$SQRP_ES_Transform

temp <- which(is.na(cbind(Y, X)), arr.ind = T)
temp

X0 <- 1
X1 <- CPS_All_ES$Crime_Count
X2 <- CPS_All_ES$Totals_20th
X3 <- CPS_All_ES$Bilingual_N
X4 <- CPS_All_ES$SpED_N
X5 <- CPS_All_ES$FreeReducedLunch_N

Z0 <- 1
Z1 <- CPS_All_ES$Crime_Count
Z2 <- CPS_All_ES$Totals_20th
Z3 <- CPS_All_ES$Bilingual_N
Z4 <- CPS_All_ES$SpED_N
Z5 <- CPS_All_ES$FreeReducedLunch_N

X <- cbind(X0, X1)
Z <- cbind(Z0, Z1)

burn <- 0.2
jump <- 30
nsim <- 400
bpri <- c(0, 0)
Bpri <- diag(100, nrow = ncol(X), ncol = ncol(X))
gpri <- c(0, 0)
Gpri <- diag(10, nrow = ncol(Z), ncol = ncol(Z))

re <- Bayesianbetareg(Y, X, Z, nsim, bpri, Bpri, gpri, Gpri, burn, jump, graph1=F, graph2=F)

summary(re)

