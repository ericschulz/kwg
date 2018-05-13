#Models and sampling strategies
#Eric Schulz, March 2018

##############################################################################################################
#KERNELS
##############################################################################################################

#Radial Basis Kernel
rbf <- function(X1,X2,theta){
  #transfer to matrices
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  #check dimensions
  if(ncol(X1) != ncol(X2)){
    stop("X1 and X2 must contain input values of the same dimension.")
  } else if(!all(theta>=0)){
    stop("All parameters must be >= 0.")
  }
  #get dimensions
  N1 <- nrow(X1)
  N2 <- nrow(X2)
  d <- ncol(X1)
  #initialize sigma
  sigma <-  matrix(rep(0, N1*N2),nrow=N1)
  #observational variance
  sf <- theta[d+1]
  #noise variance
  sn <- theta[d+2]
  #loop through
  for(i in 1:d){
    #length scale
    l <- theta[i] #Note: assumes a unique length scale for each dimension
    #x-diff
    xdiff <- (outer(X1[,i],X2[,i],function(x,y) x - y)/l)^2
    sigma <- sigma + xdiff
  }
  #RBF function
  if(identical(X1,X2)){
    id <- diag(rep(1,N1))
    sigma.final <- sf*exp(-0.5*sigma) + sn*id
  } else {
    sigma.final <- sf*exp(-0.5*sigma)
  }
  #return final covariance matrix
  return(sigma.final)
}
class(rbf)<- c(class(rbf), "GP") #identify the rbf kernel as a gp model


##############################################################################################################
#MATRIX INVERSION
##############################################################################################################

#calculate inverse of the cov-function using sigular value decomposition
cov.inverse.svd <- function(X, tol = sqrt(.Machine$double.eps)){
  # Generalized Inverse of a Matrix
  dnx <- dimnames(X)
  if(is.null(dnx)) dnx <- vector("list", 2)
  #singular value decomposition
  s <- svd(X)
  nz <- s$d > tol * s$d[1]
  #inverse
  K.inv <- structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
    dimnames = dnx[2:1])
  #logarithm of determinant.
  log.K.det <- sum(log(s$d))
  #return inverse plus log determinant
  return(list(Inv = K.inv,lDet = log.K.det))
}

#calculate inverse of the cov-function using Cholesky
cov.inverse.chol <- function(X){
  #cholseky decomposition
  R <- chol(X)
  #complex conjugate
  Rt <- Conj(t(R))
  #invert
  R.inv <- solve(R)
  #invert
  Rt.inv <- solve(Rt)
  #multiply matrices
  X.inv <- R.inv %*% Rt.inv
  #log determinant
  log.X.det <- 2*sum(log(diag(R))) 
  #return both
  return(list(Inv = X.inv, lDet = log.X.det))
}

##############################################################################################################
#GAUSSIAN PROCESS
##############################################################################################################

#Gaussian Process function
#X.test: matrix for predcitions
#theta: vector of hyper-parameters (lambda, Sf, Sn)
#X; matrix of observations
#y: vector of observed outcomes
#kernel: used kernel function, can be "rbf", "oru", or "mat"
gpr <- function(X.test, theta, X,Y, k){
  #make it a matrix
  Xstar <- as.matrix(X.test)
  #dimensions
  d <- ncol(as.matrix(X))
  #calculate capital K
  K <- k(X,X,theta) 
  #Check if matrix is positive semi-definite
  if (is.positive.definite(K)){
    KK <- cov.inverse.chol(K) #use Cholesky
  } else {
    KK <- cov.inverse.svd(K) #use SVD
  }
  #times y
  Ky <- KK$Inv %*% Y
  #apply the kernel
  result <- apply(Xstar, 1, function(x){
    XX <- matrix(x,nrow=1) 
    Kstar <- k(X, XX, theta)
    Kstarstar <- k(XX,XX,theta)
    #get mean vector
    mu <- t(Kstar) %*% Ky
    #get covariance
    cv <- Kstarstar - (t(Kstar) %*% KK$Inv %*% Kstar) #BUG: sometimes cv<0, leading to NaN when we return sqrt(cv)
    #DEBUG
    if (cv<0){ cv <- abs(cv)} #TEMPORARY SOLUTION: MANUALLY SET CV TO BE POSITIVE IF NEGATIVE
    #return means and variance
    return(c(mu, cv))
    })
  #as a data frame with names mu and sig
  prediction <- as.data.frame(t(result))
  prediction[is.na(prediction)] <- 0.01 #remove NaN items with the noise variance 
  colnames(prediction) <- c("mu", "sig")
  return(prediction)
}



##############################################################################################################
##Mean Tracker
##############################################################################################################
bayesianMeanTracker <- function(x, y, theta=c(1), prevPost=NULL){ #Updates the previous posterior based on a single observation
  #parameters
  mu0 <- 0 #prior mean
  var0 <- 5 #prior variance
  vare <- theta[1] #error varriance
  if (is.null(prevPost)){#if no posterior prior, assume it is the first observation
    predictions <- data.frame(mu=rep(mu0,64), sig=rep(var0,64))
  }else{#if previous posterior is provided, update
    predictions <- prevPost
  }
  #Which of the 121 options were chosen at time?
  allopts<-expand.grid(0:7, 0:7)
  chosen <- which(allopts$Var1==x[1] & allopts$Var2==x[2])
  #Kalman gain
  kGain <- predictions$sig[chosen] / (predictions$sig[chosen] + vare^2)
  #update mean
  predictions$mu[chosen] <- predictions$mu[chosen] + (kGain * (y-predictions$mu[chosen]))
  #update variance for observed arm
  predictions$sig[chosen] <- predictions$sig[chosen] * (1 - kGain)
  #return output
  return(predictions)
}
class(bayesianMeanTracker)<- c(class(bayesianMeanTracker), "KalmanFilter")


##############################################################################################################
#ACQUISITION FUNCTIONS
##############################################################################################################

#Upper Confidence Bound Sampling
ucb<-function(out, pars, refactor=F){
  if (refactor==TRUE){
    gamma <- pars[1]
    beta_star<-pars[2]
    #calulate all the upper confidence bounds
    outtotal<-(gamma*out$mu)+(beta_star*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    #avoid borderline cases
    outtotal[outtotal<=0]<-0.0001
    outtotal[outtotal>100]<-100
    outtotal<-matrix(outtotal, ncol=nrow(out)/64, byrow=TRUE)
  }else{
    beta <- pars[1]
    #calulate all the upper confidence bounds
    outtotal<-out$mu+(beta*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    #avoid borderline cases
    outtotal[outtotal<=0]<-0.0001
    outtotal[outtotal>50]<-50
    outtotal<-matrix(outtotal, ncol=nrow(out)/64, byrow=TRUE)
  }
  #return them
  return(outtotal)
}
#add "UCB" to the class of the ucb function, so that modelFit can recognize that it has a longer parameter array
class(ucb)<- c(class(ucb), "UCB")


#Greedy Mean
greedyMean <- function(out){
  outtotal<-out$mu #the value of each choice is solely based on the expectation of reward
  #avoid borderline cases
  outtotal[outtotal<0]<-0.0001
  outtotal[outtotal>50]<-50
  outtotal<-matrix(outtotal, nrow=nrow(out)/64, byrow=TRUE)
  return(outtotal)
}
class(greedyMean)<- c(class(greedyMean), "greedyMean")

#Greedy Variance
greedyVar <- function(out){
  outtotal<- sqrt(out$sig) #the value of each choice is solely based on the expected uncertainty
  #avoid borderline cases
  outtotal[outtotal<0]<-0.0001
  outtotal[outtotal>50]<-50
  outtotal<-matrix(outtotal, nrow=nrow(out)/64, byrow=TRUE)
  return(outtotal)
}
class(greedyVar)<- c(class(greedyVar), "greedyVar")