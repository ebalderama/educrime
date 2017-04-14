#======================================================
#======================================================
# Double-hurdle MCMC code
#======================================================
#======================================================






#======================================================
# Pre-defined helper functions
#======================================================


##Likelihood probabilities for each component
PZ <- function(pZ,log=T){
  if(log){log(pZ)}else{pZ}
}
PT <- function(pZ,pE,log=T){
  if(log){log(1-pZ)+log(1-pE)}else{(1-pZ)*(1-pE)}
}
PE <- function(pZ,pE,log=T){
  if(log){log(1-pZ)+log(pE)}else{(1-pZ)*pE}
}


##Generate value from GPD
rgpd <- function(n,mu,sigma,xi){
  mu + sigma*(runif(n)^(-xi) - 1)/xi
}

##pdf of GPD
dgpd <- function(y,mu,sigma,xi,log=T){
  options(warn=-1)
  #logdens <- (1/xi)*log(sigma) - (1+1/xi)*log(sigma + xi*(y-mu))
  #logdens <- log(sigma)/xi - (xi+1)*log(sigma + xi*(y-mu))/xi
  logdens <- (log(sigma) - (xi+1)*log(sigma + xi*(y-mu)))/xi
  logdens[y < mu] <- -Inf
  if(xi < 0){logdens[y > (mu-sigma/xi)] <- -Inf}
  if(log){return(logdens)}else{return(exp(logdens))}
}

##cdf of GPD
pgpd <- function(y,mu,sigma,xi){
  options(warn=-1)
  if(xi != 0){
    cdf <- 1 - (1 + xi*(y-mu)/sigma)^(-1/xi)
  }else{cdf <- 1 - exp(-(y-mu)/sigma)}
  cdf[y < mu] <- 0
  if(xi < 0){cdf[y > (mu-sigma/xi)] <- 1}
  return(cdf)
}

##Probability of y~GPD (discrete version of dgpd)
mgpd <- function(y,mu,sigma,xi,log=T){
  pmf <- pgpd(y+0.5,mu,sigma,xi)-pgpd(y-0.5,mu,sigma,xi)
  if(log){return(log(pmf))}else{return(pmf)}
}


##Probability of y~lnorm (discrete version of dlnorm)
mlnorm <- function(y,lmean,lsd,log=T){
  pmf <- plnorm(y+0.5,lmean,lsd)-plnorm(y-0.5,lmean,lsd)
  if(log){return(log(pmf))}else{return(pmf)}
}

##Likelihood for typical count distribution
dtypical <- function(y,Effort,muT,muGPD,distr=c("nb","poisson","lognormal"),size=1,log=T){
  mu <- Effort*muT
  n <- size
  d <- match.arg(distr)
  lll <- switch(d,
                nb = dnbinom(y,mu=mu,size=n,log=T)-log(pnbinom(muGPD-1,mu=mu,size=n)-pnbinom(0,mu=mu,size=n)),
                poisson = dpois(y,mu,log=T)-log(ppois(muGPD-1,mu)-ppois(0,mu)),
                lognormal = mlnorm(y,mu,sqrt(mu+mu^2/n),log=T)-log(plnorm(muGPD-0.5,mu,sqrt(mu+mu^2/n))-plnorm(0.5,mu,sqrt(mu+mu^2/n)))
  )
  if(log){return(lll)}else{return(exp(lll))}
}





##For filling in missings, sample from likelihood
sample_y <- function(Effort,pZ,pE,muGPD,sigGPD,xiGPD,muT,size){
  Eff <- ifelse(Effort==0,1,Effort)
  samp <- rbinom(length(pZ),1,1-pZ)
  samp[samp>0] <- ifelse(runif(sum(samp)) < pE[samp>0],
                         rgpd(sum(samp),muGPD,sigGPD,xiGPD),
                         rnbinom(sum(samp),mu=Eff[samp>0]*muT[samp>0],size=size))
  return(samp)
}





#======================================================
# Double-hurdle MCMC, main function
#======================================================

#The V's are eigenvectors and eigenvalues
dhurdle <- function(Y,Effort,X,V=NULL,v=NULL,
                    lowEx=5,
                    name=NULL,
                    distr="nb",
                    spatial=c(T,T,F),
                    beta_mn=0,beta_sd=1000, #priors
                    alpha_mn=0,alpha_sd=1000, #priors
                    size_mn=0,size_sd=10, #priors
                    sig_mn=2,sig_sd=5, #priors
                    xi_mn=0,xi_sd=1, #priors
                    tau_a=0.5,tau_b=0.0005, #priors
                    fixsize=1,
                    iters=100,burn=50,update=10,nthin=1,
                    tune=list(z=1,t=1,e=1,s=20,rest=0.3),
                    track.time=T,keepmiss=F,plot=F){
  
  
  # ______________________________________________
  # Bookkeeping:
  #
  start.time <- proc.time()
  cat(name,"Begin timer:",(proc.time()-start.time)[1:3])
  N <- nrow(X)
  p <- ncol(X)
  q <- length(v)
  miss <- is.na(Y)
  Y[miss] <- 0
  distr <- distr
  
  # ______________________________________________
  # initial values:
  #
  beta <- matrix(0,p,3)
  alpha <- matrix(0,q,3)
  muGPD <- lowEx
  sigGPD <- exp(sig_mn)
  xiGPD <- xiGPDinit <- runif(1,0,1)
  tau <- rep(1,3)
  size <- ifelse(is.numeric(fixsize),fixsize,1)
  #est.size <- ifelse(is.numeric(fixsize),F,T)
  
  XB1 <- c(X%*%beta[,1])
  XB2 <- c(X%*%beta[,2])
  XB3 <- c(X%*%beta[,3])
  VA1 <- VA2 <- VA3 <- 0
  if(spatial[1]){VA1 <- c(V%*%alpha[,1])}
  if(spatial[2]){VA2 <- c(V%*%alpha[,2])}
  if(spatial[3]){VA3 <- c(V%*%alpha[,3])}
  pZ <- 1/(1+exp(-XB1-VA1))
  muT <- exp(XB2+VA2)
  pE <- 1/(1+exp(-XB3-VA3))
  
  LT <- dtypical(Y[0<Y&Y<muGPD],Effort[0<Y&Y<muGPD],muT[0<Y&Y<muGPD],muGPD,distr,size,log=T)
  LE <- mgpd(Y[Y>=muGPD],muGPD,sigGPD,xiGPD,log=T)
  p1 <- PZ(pZ[Y==0],log=T)
  p2 <- PT(pZ[0<Y&Y<muGPD],pE[0<Y&Y<muGPD],log=T)
  p3 <- PE(pZ[Y>=muGPD],pE[Y>=muGPD],log=T)
  
  curll <- rep(0,N)
  curll[Y==0] <- p1
  curll[0<Y&Y<muGPD] <- p2 + LT
  curll[Y>=muGPD] <- p3 + LE
  
  
  # ______________________________________________
  # Keep track of stuff:
  #
  keep.pars <- matrix(0,iters,7)
  colnames(keep.pars) <- c("muGPD","sigGPD","xiGPD","size","tauZ","tauT","tauE")
  
  keep.beta <- array(0,c(iters,p,3))
  dimnames(keep.beta)[[1]] <- paste("Iter",1:iters)
  dimnames(keep.beta)[[2]] <- rownames(beta) <- colnames(X)
  dimnames(keep.beta)[[3]] <- colnames(beta) <- c("Zero","Typical","Extreme")
  
  keep.alpha <- array(0,c(iters,q,3))
  dimnames(keep.alpha)[[1]] <- paste("Iter",1:iters)
  dimnames(keep.alpha)[[2]] <- rownames(alpha) <- colnames(V)
  dimnames(keep.alpha)[[3]] <- colnames(alpha) <- c("Zero","Typical","Extreme")
  
  if(keepmiss){
    keep.Ymis <- matrix(0,iters,sum(miss))
    colnames(keep.Ymis) <- as.character(which(miss))
  }
  
  keep.dev <- track.ppo <- track.icpo <- 0
  
  # ______________________________________________
  # Tuning
  #
  accZ <- accT <- accE <- tuneZ <- tuneT <- tuneE <- rep(0,p)
  acc <- tuner <- rep(0,4)
  tuner[1:4] <- tune$rest
  tuneZ[1:p] <- tune$z
  tuneT[1:p] <- tune$t
  tuneE[1:p] <- tune$e
  att <- attS <- 0
  accS <- tuneS <- rep(0,3)
  tuneS[spatial] <- tune$s
  
  
  
  #===================================================================
  #===================================================================
  #=========== Ready, Set, Start the MCMC!
  #=========== 
  cat("\n",name,"Starting MCMC",(proc.time()-start.time)[1:3],"; date:")
  print(Sys.time())
  for(i in 1:iters){
    
    # =======================================================
    # thinning is cool!
    #
    for(thin in 1:nthin){
      
      # ______________________________________________ 
      # Fill in missings:
      #
      if(sum(miss)>0) Y[miss] <- sample_y(Effort[miss],pZ[miss],pE[miss],muGPD,sigGPD,xiGPD,muT[miss],size)
      
      # ______________________________________________ 
      # Keep track of number of metropolis attempts:
      #
      att <- att + 1
      attS <- attS + q
      
      # ______________________________________________
      # Update pZ, beta:
      # 
      for(j in 1:p){
        canbeta <- rnorm(1,beta[j,1],tuneZ[j])
        canXB <- XB1 + X[,j]*(canbeta-beta[j,1])
        canpZ <- 1/(1+exp(-canXB-VA1))
        canp1 <- PZ(canpZ[Y==0],log=T)
        canp2 <- PT(canpZ[0<Y&Y<muGPD],pE[0<Y&Y<muGPD],log=T)
        canp3 <- PE(canpZ[Y>=muGPD],pE[Y>=muGPD],log=T)
        R1 <- sum(canp1,canp2,canp3) + dnorm(canbeta,beta_mn,beta_sd,log=T) -
          sum(p1,p2,p3) - dnorm(beta[j,1],beta_mn,beta_sd,log=T)
        if(is.finite(R1)){if(log(runif(1)) < R1){
          beta[j,1] <- canbeta
          XB1 <- canXB
          pZ <- canpZ
          p1 <- canp1
          p2 <- canp2
          p3 <- canp3
          accZ[j] <- accZ[j] + 1
        }}
      }
      
      # ______________________________________________
      # Update pZ, alpha:
      #
      if(spatial[1]){for(j in 1:q){
        canalpha <- rnorm(1,alpha[j,1],tuneS[1])
        canVA <- VA1 + V[,j]*(canalpha-alpha[j,1])
        canpZ <- 1/(1+exp(-XB1-canVA))
        canp1 <- PZ(canpZ[Y==0],log=T)
        canp2 <- PT(canpZ[0<Y&Y<muGPD],pE[0<Y&Y<muGPD],log=T)
        canp3 <- PE(canpZ[Y>=muGPD],pE[Y>=muGPD],log=T)
        R4 <- sum(canp1,canp2,canp3) + dnorm(canalpha,alpha_mn,1/sqrt(tau[1]*v[j]),log=T) - 
          sum(p1,p2,p3) - dnorm(alpha[j,1],alpha_mn,1/sqrt(tau[1]*v[j]),log=T)
        if(is.finite(R4)){if(log(runif(1)) < R4){
          alpha[j,1] <- canalpha
          VA1 <- canVA 
          pZ <- canpZ
          p1 <- canp1
          p2 <- canp2
          p3 <- canp3
          accS[1] <- accS[1] + 1
        }}
      }}
      
      # ______________________________________________
      # Update muT, beta:
      #
      for(j in 1:p){
        canbeta <- rnorm(1,beta[j,2],tuneT[j])
        canXB <- XB2 + X[,j]*(canbeta-beta[j,2])
        canmuT <- exp(canXB+VA2)
        canLT <- dtypical(Y[0<Y & Y<muGPD],Effort[0<Y & Y<muGPD],canmuT[0<Y & Y<muGPD],muGPD,distr,size,log=T)
        lr <- sum(canLT-LT)
        R2 <- lr + dnorm(canbeta,beta_mn,beta_sd,log=T) -
          dnorm(beta[j,2],beta_mn,beta_sd,log=T)
        if(is.finite(R2)){if(log(runif(1)) < R2){
          beta[j,2] <- canbeta
          XB2 <- canXB 
          muT <- canmuT
          LT <- canLT
          accT[j] <- accT[j] + 1
        }}
      }
      
      # ______________________________________________
      # Update muT, alpha:
      #
      if(spatial[2]){for(j in 1:q){
        canalpha <- rnorm(1,alpha[j,2],tuneS[2])
        canVA <- VA2 + V[,j]*(canalpha-alpha[j,2])
        canmuT <- exp(XB2+canVA)
        canLT <- dtypical(Y[0<Y & Y<muGPD],Effort[0<Y & Y<muGPD],canmuT[0<Y & Y<muGPD],muGPD,distr,size,log=T)
        lr <- sum(canLT-LT)
        R5 <- lr + dnorm(canalpha,alpha_mn,1/sqrt(tau[2]*v[j]),log=T) - 
          dnorm(alpha[j,2],alpha_mn,1/sqrt(tau[2]*v[j]),log=T)
        if(is.finite(R5)){if(log(runif(1)) < R5){
          alpha[j,2] <- canalpha
          VA2 <- canVA 
          muT <- canmuT
          LT <- canLT
          accS[2] <- accS[2] + 1
        }}
      }}
      
      # ______________________________________________
      # Update pE, beta:
      #
      for(j in 1:p){
        canbeta <- rnorm(1,beta[j,3],tuneE[j])
        canXB <- XB3 + X[,j]*(canbeta-beta[j,3])
        canpE <- 1/(1+exp(-canXB-VA3))
        canp2 <- PT(pZ[0<Y&Y<muGPD],canpE[0<Y&Y<muGPD],log=T)
        canp3 <- PE(pZ[Y>=muGPD],canpE[Y>=muGPD],log=T)
        R3 <- sum(canp2,canp3) + dnorm(canbeta,beta_mn,beta_sd,log=T) - 
          sum(p2,p3) - dnorm(beta[j,3],beta_mn,beta_sd,log=T)
        if(is.finite(R3)){if(log(runif(1)) < R3){
          beta[j,3] <- canbeta
          XB3 <- canXB 
          pE <- canpE
          p2 <- canp2
          p3 <- canp3
          accE[j] <- accE[j] + 1
        }}
      }
      
      # ______________________________________________
      # Update pE, alpha:
      #
      if(spatial[3]){for(j in 1:q){
        canalpha <- rnorm(1,alpha[j,3],tuneS[3])
        canVA <- VA3 + V[,j]*(canalpha-alpha[j,3])
        canpE <- 1/(1+exp(-XB3-canVA))
        canp2 <- PT(pZ[0<Y&Y<muGPD],canpE[0<Y&Y<muGPD],log=T)
        canp3 <- PE(pZ[Y>=muGPD],canpE[Y>=muGPD],log=T)
        R6 <- sum(canp2,canp3) + dnorm(canalpha,alpha_mn,1/sqrt(tau[3]*v[j]),log=T) - 
          sum(p2,p3) - dnorm(alpha[j,3],alpha_mn,1/sqrt(tau[3]*v[j]),log=T)
        if(is.finite(R6)){if(log(runif(1)) < R6){
          alpha[j,3] <- canalpha
          VA3 <- canVA 
          pE <- canpE
          p2 <- canp2
          p3 <- canp3
          accS[3] <- accS[3] + 1
        }}
      }}
      
      
      # ______________________________________________
      # Update scale parameter (sigGPD) of the extremes:
      #
      cansigGPD <- exp(rnorm(1,log(sigGPD),tuner[2]))
      canLE <- mgpd(Y[Y>=muGPD],muGPD,cansigGPD,xiGPD,log=T)
      lr <- sum(canLE-LE)
      R8 <- lr + dnorm(log(cansigGPD),sig_mn,sig_sd,log=T) -
        dnorm(log(sigGPD),sig_mn,sig_sd,log=T)
      if(is.finite(R8)){if(log(runif(1)) < R8){
        sigGPD <- cansigGPD
        LE <- canLE
        acc[2] <- acc[2] + 1
      }}
      
      
      # ______________________________________________
      # Update shape parameter (xiGPD) of the extremes:
      #
      canxiGPD <- rnorm(1,xiGPD,tuner[3])
      canLE <- mgpd(Y[Y>=muGPD],muGPD,sigGPD,canxiGPD,log=T)
      lr <- sum(canLE-LE)
      R9 <- lr + dnorm(canxiGPD,xi_mn,xi_sd,log=T) - 
        dnorm(xiGPD,xi_mn,xi_sd,log=T)
      if(is.finite(R9)){if(log(runif(1)) < R9){
        xiGPD <- canxiGPD
        LE <- canLE
        acc[3] <- acc[3] + 1
      }}
      
      
      # ______________________________________________
      # Update CAR precision
      #
      tau <- rgamma(3,q/2+tau_a,colSums(v*alpha^2)/2+tau_b)
      
      
    }
    # End thinning
    # =======================================================
    
    
    
    # ______________________________________________
    # keep track of stuff:
    #
    keep.pars[i,] <- c(muGPD,sigGPD,xiGPD,size,tau)
    keep.beta[i,,] <- beta
    keep.alpha[i,,] <- alpha
    if(keepmiss){keep.Ymis[i,] <- as.vector(Y[miss])}
    
    curll[Y==0] <- p1
    curll[0<Y&Y<muGPD] <- p2 + LT
    curll[Y>=muGPD] <- p3 + LE
    
    if(i>burn){
      track.ppo <- track.ppo + exp(curll)
      track.icpo <- track.icpo + 1/exp(curll)
    }
    keep.dev[i] <- -2*sum(curll)
    
    
    # ______________________________________________
    # tuning
    #
    if(i<0.75*burn & att>50){
      tuneZ <- ifelse(accZ/att < 0.20, 0.8*tuneZ, tuneZ)
      tuneZ <- ifelse(accZ/att > 0.50, 1.2*tuneZ, tuneZ)
      tuneT <- ifelse(accT/att < 0.20, 0.8*tuneT, tuneT)
      tuneT <- ifelse(accT/att > 0.50, 1.2*tuneT, tuneT)
      tuneE <- ifelse(accE/att < 0.20, 0.8*tuneE, tuneE)
      tuneE <- ifelse(accE/att > 0.50, 1.2*tuneE, tuneE)
      tuner <- ifelse(acc/att < 0.20, 0.8*tuner, tuner)
      tuner <- ifelse(acc/att > 0.50, 1.2*tuner, tuner)
      accZ <- accT <- accE <- 0*accZ
      acc <- 0*acc
      tuneS <- ifelse(accS/attS < 0.20, 0.8*tuneS, tuneS)
      tuneS <- ifelse(accS/attS > 0.50, 1.2*tuneS, tuneS)
      accS <- 0*accS
      att <- attS <- 0
    }
    
    
    
    # ______________________________________________
    # Plot current iteration:
    #
    if(plot & i%%update==0){
      par(mfrow=c(3,3))
      #yyy <- c(0,round(seq(muGPD,max(Y),length.out=min(300,max(Y)-muGPD))))
      #lll <- llike(yyy,1,pZ=0,pE=1,muGPD,sigGPD,xiGPD,muT=1,size,log=F)
      #plot(yyy,lll,type="s",xlab="Y",ylab="Density",main=paste(name,"Extreme pmf"))
      plot(keep.beta[1:i,1,1],type="l",ylab="",main=bquote(beta[0]~Zero~prob~intercept))
      plot(keep.beta[1:i,1,2],type="l",ylab="",main=bquote(beta[0]~Typical~mean~intercept))
      if(is.finite(lowEx)){
        plot(keep.beta[1:i,1,3],type="l",ylab="",main=bquote(beta[0]~Extreme~prob))
        plot(keep.pars[1:i,2],type="l",ylab="",main=bquote(sigma[gpd]))
        plot(keep.pars[1:i,3],type="l",ylab="",main=bquote(xi[gpd]))
      }
      
      #if(i <= burn){plot(keep.dev,type="l",ylab="Deviance",main=bquote(D==-2*sum(loglike)))
      #}else{plot(burn:i,keep.dev[burn:i],type="l",ylab="Deviance",main=bquote(D==-2*sum(loglike)))}
      
      plot(keep.beta[1:i,2,1],type="l",ylab="",main=bquote(beta[1]~Zero~prob~SST))
      plot(keep.beta[1:i,2,2],type="l",ylab="",main=bquote(beta[1]~Typical~mean~SST))
      plot(keep.beta[1:i,3,2],type="l",ylab="",main=bquote(beta[1]~Typical~mean~CHL))
      plot(keep.beta[1:i,4,2],type="l",ylab="",main=bquote(beta[1]~Typical~mean~DEP))
      
      #plot(pZ,log(Y/Effort+1),col=ifelse(Y==0,2,1),cex=ifelse(Y==0,0.1,0.2))
      #abline(h=log(muGPD),col="blue")
      #lines(density(pZ[Y==0]),col="red")
    }
    
    if(track.time & i%%update==0){
      cat(name,"iter",i,"; time",(proc.time()-start.time)[1:3],"; date:")
      print(Sys.time())
    }
    
    
  }
  #===========
  #===========  End MCMC chain!
  #===================================================================
  
  
  # ______________________________________________
  # All this to get pD and DIC
  # 
  betabar <- apply(array(keep.beta[(burn+1):iters,,],c(iters-burn,p,3)),2:3,mean)
  XB1 <- c(X%*%betabar[,1])
  XB2 <- c(X%*%betabar[,2])
  XB3 <- c(X%*%betabar[,3])
  VA1 <- VA2 <- VA3 <- 0
  if(spatial[1]){VA1 <- c(V%*%apply(keep.alpha[(burn+1):iters,,1],2,mean))}
  if(spatial[2]){VA2 <- c(V%*%apply(keep.alpha[(burn+1):iters,,2],2,mean))}
  if(spatial[3]){VA3 <- c(V%*%apply(keep.alpha[(burn+1):iters,,3],2,mean))}
  pZbar <- 1/(1+exp(-XB1-VA1))
  muTbar <- exp(XB2+VA2)
  pEbar <- 1/(1+exp(-XB3-VA3))
  
  parsbar <- apply(keep.pars[(burn+1):iters,],2,mean)
  muGPDbar <- parsbar[1]
  sigGPDbar <- parsbar[2]
  xiGPDbar <- parsbar[3]
  sizebar <- parsbar[4]
  
  LTbar <- dtypical(Y[0<Y&Y<muGPDbar],Effort[0<Y&Y<muGPDbar],muTbar[0<Y&Y<muGPDbar],muGPDbar,distr,sizebar,log=T)
  LEbar <- mgpd(Y[Y>=muGPDbar],muGPDbar,sigGPDbar,xiGPDbar,log=T)
  p1bar <- PZ(pZbar[Y==0],log=T)
  p2bar <- PT(pZbar[0<Y&Y<muGPDbar],pEbar[0<Y&Y<muGPDbar],log=T)
  p3bar <- PE(pZbar[Y>=muGPDbar],pEbar[Y>=muGPDbar],log=T)
  
  Dmeans <- -2*sum(p1bar,p2bar,LTbar,p3bar,LEbar)
  Dbar <- mean(keep.dev[(burn+1):iters])
  
  #pd (effective num of params) = mean dev - dev at post means
  pD <- Dbar - Dmeans
  
  
  betabar1 <- apply(array(keep.beta[(burn+1):iters,,],c(iters-burn,p,3)),2:3,median)
  XB1 <- c(X%*%betabar1[,1])
  XB2 <- c(X%*%betabar1[,2])
  XB3 <- c(X%*%betabar1[,3])
  VA1 <- VA2 <- VA3 <- 0
  if(spatial[1]){VA1 <- c(V%*%apply(keep.alpha[(burn+1):iters,,1],2,median))}
  if(spatial[2]){VA2 <- c(V%*%apply(keep.alpha[(burn+1):iters,,2],2,median))}
  if(spatial[3]){VA3 <- c(V%*%apply(keep.alpha[(burn+1):iters,,3],2,median))}
  pZbar <- 1/(1+exp(-XB1-VA1))
  muTbar <- exp(XB2+VA2)
  pEbar <- 1/(1+exp(-XB3-VA3))
  
  parsbar <- apply(keep.pars[(burn+1):iters,],2,median)
  muGPDbar <- parsbar[1]
  sigGPDbar <- parsbar[2]
  xiGPDbar <- parsbar[3]
  sizebar <- parsbar[4]
  
  LTbar <- dtypical(Y[0<Y&Y<muGPDbar],Effort[0<Y&Y<muGPDbar],muTbar[0<Y&Y<muGPDbar],muGPDbar,distr,sizebar,log=T)
  LEbar <- mgpd(Y[Y>=muGPDbar],muGPDbar,sigGPDbar,xiGPDbar,log=T)
  p1bar <- PZ(pZbar[Y==0],log=T)
  p2bar <- PT(pZbar[0<Y&Y<muGPDbar],pEbar[0<Y&Y<muGPDbar],log=T)
  p3bar <- PE(pZbar[Y>=muGPDbar],pEbar[Y>=muGPDbar],log=T)
  
  Dmeds <- -2*sum(p1bar,p2bar+LTbar,p3bar+LEbar)
  Dbarmed <- mean(keep.dev[(burn+1):iters])
  
  #pd (effective num of params) = mean dev - dev at post means
  pDmed <- Dbarmed - Dmeds
  
  
  
  # ______________________________________________
  # Output
  # 
  Ymis <- sum(miss)
  #if(keepmiss) Ymis <- colMeans(keep.Ymis) #keep only the means, otherwise too big.
  if(keepmiss) Ymis <- keep.Ymis[-(1:burn),]
  prior <- rbind(c(beta_mn,alpha_mn,sig_mn,xi_mn,size_mn,tau_a),
                 c(beta_sd,alpha_sd,sig_sd,xi_sd,size_sd,tau_b))
  rownames(prior) <- c("par1","par2")
  colnames(prior) <- c("beta","alpha","sigma","xi","size","tau")
  names(acc) <- c("muGPD","sigGPD","xiGPD","size")
  #if(distr!="NB"){
  #	prior <- prior[,-6]
  #	acc <- acc[-4]
  #	tuner <- tuner[-4]
  #	keep.pars <- keep.pars[,-4]
  #}
  acc <- acc[-1]
  tuner <- tuner[-1]
  keep.pars <- keep.pars[,-1]
  
  
  # ______________________________________________
  # The returned output list
  # 
  list(pars     = keep.pars[-(1:burn),],
       beta     = keep.beta[-(1:burn),,],
       alpha    = keep.alpha[-(1:burn),,spatial],
       Ymis     = Ymis,
       diags    = list(dev=keep.dev,
                       pD=pD,
                       DIC=Dbar+pD,
                       Dbar=Dbar,
                       Dmeans=Dmeans,
                       Dbarmed=Dbarmed,
                       Dmeds=Dmeds,
                       pDmed=pDmed,
                       DICmed=Dbarmed+pDmed,
                       ppo=track.ppo/(iters-burn),
                       cpo=(iters-burn)/(track.icpo)),
       tune     = list(inits=tune,
                       tuner=list(z=tuneZ,t=tuneT,e=tuneE,s=tuneS,rest=tuner),
                       accrate=list(z=accZ/att,t=accT/att,e=accE/att,s=accS/attS,rest=acc/att)),
       info     = list(prior=prior,
                       inits=list(beta=0,alpha=0,muGPD=lowEx,
                                  xiGPD=xiGPDinit,sigGPD=exp(sig_mn),
                                  size=fixsize,tau=1),
                       muGPD=lowEx,
                       spatial=spatial,
                       distr=distr,
                       iters=iters,
                       burn=burn,
                       thin=nthin,
                       name=name),
       proc     = list(version="dhurdle_v6_2",
                       time=proc.time()-start.time,
                       date=Sys.time(),
                       sessionInfo=sessionInfo())
  )
  
}
