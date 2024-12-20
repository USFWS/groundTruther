#' drawMultinomProbs
#' 
#' A helper function to draw probabilities of classes using the distribution coefficient estimates from a multinomial model.
#' @param x An object of class \code{multinom}.
#' @param nDraws Number of realizations of coefficient estimates to draw from \code{x}.
#' @param nTries Number of attempts to pull a probability matrix is all positive and has row sums equal to 1. Setting to 1 will prevent these conditions from being checked.
#' @details This function takes the output from \link[nnet]{multinom} and draws new values based on the estimated mean and standard error of each classification. The drawn values are then back-transformed from the logit to the probability scale to provide the probability a classification corresponds to a true class.
#' @return A matrix (if \code{nDraws==1}) or list of matrices (if \code{nDraws>1}) with probabilities remote sensing patch types correspond to ground-truthing patch types. Remote sensing patch classifications are in rows and the probability classifications correspond to ground-truthed patch types are in columns.
#' @export
#' @examples
#' data(substrateLayers)
#' # Determine which classified patch types a ground-truthing sample is most likely to overlap
#' withinProbs <- probWithin(joined=substrateLayers[['gtSSS']], gtSampleID="sampleID", gtPatch="gtPatch", remSensPatch="sssPatch", nDraws=100)
#' # Pull the highest probability matches for each sampleID
#' matchedSamples <- withinProbs[withinProbs[, .I[which.max(prob)], by=sampleID]$V1]
#' gtAccuracy <- multinom(gtPatch~sssPatch, data=matchedSamples)
#' drawMultinomProbs(gtAccuracy)

drawMultinomProbs <- function(x, nDraws=1, nTries=1000){
  stopifnot("multinom" %in% class(x))
  coefs <- coef(x)
  coef_se <- summary(x)$standard.errors
  matDim <- dim(coef(x))
  
  #create list to store matrices
  probMats <- vector("list", nDraws)
  
  for(i in seq_along(probMats)){
    #ensure all probabilities are positive and less than one and that the matrix rows sum to 1. This is only an issue in edge cases (near 0 or 1) where rounding errors may cause values slighlty < 0 or > 1.
    rSums1 <- 0
    allPos <- 0
    allLess1 <- 0
    tries <- 0
    while((!rSums1==1 | !allPos==1 | !allLess1==1) & tries<nTries){
      #draw movement probabilities based on estimated mean and sd
      coef_draw <- matrix(nrow=matDim[1], ncol=matDim[2])
      for(rID in 1:matDim[1]){
        for(cID in 1:matDim[2]){
          coef_draw[rID, cID] <- ifelse(is.na(coef_se[rID, cID]),
                                        coefs[rID, cID], #for instances where standard error is not estimated, use the mean coef estimate
                                        rnorm(n=1, mean=coefs[rID, cID], sd=coef_se[rID, cID]))
        }
      }
      #create model.matrix for calculating estimated means for each class, borrowed from predict.multinom
      m <- model.frame(delete.response(x$terms), data=data.frame(x$xlevels), xlev = x$xlevels)
      mMat <- model.matrix(delete.response(x$terms), m, contrasts=x$contrasts)
      lprobs <- mMat %*% t(coef_draw)
      
      #convert from logit scale to probability
      eprob <- exp(lprobs)
      eprob_rowSums <- apply(eprob, 1, sum, na.rm=TRUE)
      prob <- eprob/(1+eprob_rowSums)
      #calculate probability of reference class
      refProb <- 1/(1+eprob_rowSums)
      
      prob <- cbind(refProb, prob)
      dimnames(prob)[[1]] <- paste0("classified_as_", x$xlevels[[1]])
      dimnames(prob)[[2]] <- paste0("prob_", x$lev)
      
      rSums1 <- mean(apply(prob, 1, sum, na.rm=TRUE)==1)
      allPos <- mean(unlist(prob>=0))
      allLess1 <- mean(unlist(prob<=1))
      tries <- tries + 1
    }
    
    if(!rSums1==1 | !allPos==1 | !allLess1==1){
      warning("Unable to draw matrix of probabilities between 0 and 1 with row sums of 1 for draw ", i, " after ", tries, "attempts.")
    } else {
    }
    probMats[[i]] <- prob
  }
  
  if(nDraws==1){
    return(prob)
  } else {
    return(probMats)
  }
}