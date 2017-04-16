
CPS_All_HS <- read.csv("/Users/jcote/Desktop/CPS_CC_Beta/CPS_All_HS.csv", header = T)

######################################################################################
#                                SQRP Transfrom
######################################################################################

CPS_All_HS$SQRP_HS_Transform <- ((CPS_All_HS$SQRP_HS_Points - 1) / 4)


######################################################################################
#                                Beta RegrHSsion for HS
######################################################################################

YH <- CPS_All_HS$SQRP_HS_Transform

temp <- which(is.na(cbind(YH, XH)), arr.ind = T)
temp
X0H <- 1
X1H <- CPS_All_HS$Crime_Count
X2H <- CPS_All_HS$Totals_20th
X3H <- CPS_All_HS$ACT_Overall_Read
X4H <- CPS_All_HS$ACT_Overall_Science
X5H <- CPS_All_HS$ACT_Overall_Math
X6H <- CPS_All_HS$ACT_Overall_English
X7H <- CPS_All_HS$ACT_Overall_Composite
X8H <- CPS_All_HS$Bilingual_N
X9H <- CPS_All_HS$SpED_N
X10H <- CPS_All_HS$FreeReducedLunch_N

Z0H <- 1
Z1H <- CPS_All_HS$Crime_Count
Z2H <- CPS_All_HS$Totals_20th
Z3H <- CPS_All_HS$ACT_Overall_Read
Z4H <- CPS_All_HS$ACT_Overall_Science
Z5H <- CPS_All_HS$ACT_Overall_Math
Z6H <- CPS_All_HS$ACT_Overall_English
Z7H <- CPS_All_HS$ACT_Overall_Composite
Z8H <- CPS_All_HS$Bilingual_N
Z9H <- CPS_All_HS$SpED_N
Z10H <- CPS_All_HS$FreeReducedLunch_N

XH <- cbind(X0H, X1H)
XH <- cbind(X0, X1, X2, X8, X9, X10)
ZH <- cbind(Z0H, Z1H)
ZH <- cbind(Z0, Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10)

burn <- 0.2
jump <- 5
nsim <- 2000
bpriH <- c(0, 0)
bpriH <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
BpriH <- diag(100,nrow=ncol(XH),ncol=ncol(XH))
gpriH <- c(0, 0)
gpriH <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
GpriH <- diag(10,nrow=ncol(ZH),ncol=ncol(ZH))
re <- Bayesianbetareg(YH, XH, ZH, nsim, bpriH, BpriH, gpriH, GpriH, burn, jump, graph1=F, graph2=F)
summary(re)


rHSid <- betarHSiduals(Y,X,re)
diagnostics(re, rHSid)
plotrHSiduals(X, Y, rHSid, type = "Deviance")
print.BayHSianbetareg(re)
breg <- betareg(SQRP_HS_Transform ~ Crime_Count, data = CPS_All_HS)
summary(breg)
