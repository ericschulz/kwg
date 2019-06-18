library(MASS)

myTruncNormSim <- function(lBound = -Inf, uBound = Inf, mu = 0, sd = 1){
  lBoundUni <- pnorm(lBound, mean = mu, sd = sd)
  uBoundUni <- pnorm(uBound, mean = mu, sd = sd)  
  mySample <- qnorm(runif(1, lBoundUni, uBoundUni), mean = mu, sd = sd)
  return(mySample)
}

rankSumGibbsSampler <- function(xVals,yVals, nSamples = 10000, progBar = TRUE, cauchyPriorParameter = 1/sqrt(2),
                                initMeth = "qnorm", nChains = 1, nGibbsIterations = 10){
  if (progBar) {
    myBar <- txtProgressBar(min = 1, max = nSamples, initial = 1, char = "*",style=3,width=50)
  }
  
  n1 <- length(xVals)
  n2 <- length(yVals)
  allRanks <- rank(c(xVals,yVals))
  xRanks <- allRanks[1:n1]
  yRanks <- allRanks[(n1+1):(n1+n2)]
  allVals <- sort(rnorm((n1+n2)))[allRanks] # initial values
  
  deltaSamples <- gSamples <- muSamples <- numeric(nSamples)
  sampledX <- matrix(nrow = nSamples, ncol = n1)
  sampledY <- matrix(nrow = nSamples, ncol = n2)
  
  oldMuProp <- oldDeltaProp <- 0
  
  for (j in 1:nSamples) {
    for (i in sample(1:(n1+n2))) {
      underx <- allVals[allRanks < allRanks[i]][order(allVals[allRanks < allRanks[i]], decreasing = T)][1]
      upperx <- allVals[allRanks > allRanks[i]][order(allVals[allRanks > allRanks[i]], decreasing = F)][1]
      if (is.na(underx)) {underx <- -Inf}
      if (is.na(upperx)) {upperx <- Inf}
      
      if (i <= n1) {
        allVals[i] <- myTruncNormSim(mu = (-0.5*oldMuProp), sd = 1, lBound = underx, uBound = upperx)
      } else if (i > n1) {
        allVals[i] <- myTruncNormSim(mu = (0.5*oldMuProp), sd = 1, lBound = underx, uBound = upperx)
      }
    }
    
    xVals <- allVals[1:n1]
    yVals <- allVals[(n1+1):(n1+n2)]
    
    gibbsResult <- sampleGibbsTwoSample(x = xVals, y = yVals, n1 = n1, n2 = n2, nIter = nGibbsIterations,
                                        rscale = cauchyPriorParameter)
    
    muSamples[j] <- oldMuProp <- gibbsResult[3]
    gSamples[j] <- gibbsResult[4]
    deltaSamples[j] <- oldDeltaProp <- gibbsResult[1]
    
    if (progBar) setTxtProgressBar(myBar,j) 
    sampledX[j,] <- xVals
    sampledY[j,] <- yVals
  }
  resultsList <- list(muSamples = muSamples, deltaSamples = deltaSamples, gSamples = gSamples,
                      sampledX = sampledX, sampledY = sampledY)
  return(resultsList)
}

sampleGibbsTwoSample <- function(x, y, n1, n2, nIter = 10, rscale = 1/sqrt(2)) {
  meanx <- mean(x)
  meany <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigmaSq <- 1 # Arbitrary number for sigma
  g <- 1
  for(i in 1:nIter){   
    #sample mu
    varMu <- (4 * g * sigmaSq) / ( 4 + g * (n1 + n2) )
    meanMu <- (2 * g * (n2 * meany - n1 * meanx)) / ((g * (n1 + n2) + 4))
    mu <- rnorm(1, meanMu, sqrt(varMu)) 
    # sample g
    betaG <- (mu^2 + sigmaSq * rscale^2) / (2*sigmaSq)
    g <- 1/rgamma(1, 1, betaG)
    # convert to delta
    delta <- mu / sqrt(sigmaSq)
  }
  return(c(delta, sigmaSq, mu, g))
}