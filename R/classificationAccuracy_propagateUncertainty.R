#' classificationAccuracy_propagateUncertainty
#' 
#' Join ground-truthing data to patch classification from remote sensing data (e.g., side-scan sonar), calculate classification accuracy, and propagate uncertainty in classifications.
#' @param gtShp A point shapefile of class \code{sf} with ground-truthing data, should be in projected coordinate system (e.g., UTM, state plane).
#' @param remSensShp A polygon shapefile of class \code{sf} with remote sensing classifications, should be in projected coordinate system (e.g., UTM, state plane) and must match the \code{gtShp} coordinate system if \code{crs} is \code{NULL}.
#' @param crs Optional. CRS to be used to reproject \code{gtShp} and \code{remSensShp}.
#' @param matchMethod Character vector of length 1, determining method to match and subset ground-truthing data with remote sensing polygons. If "randomPoints", then ground-truthing data point locations are drawn from a GPS error distribution (see \link[findSD]{findSD}) \code{nDraws} times, classification accuracy is calculated for each set of draws, and patch type is reclassified \code{nReclass} times to propagate uncertainty. If "bufferedSubset", ground-truthing data points are supplied a buffer with a radius of gpsAccuracy and only buffered points completely within a single patch type are used to assess accuracy and propagate uncertainty.
#' @param gtSampleID The name of the \code{gtShp} field containing the ground-truthing sampleID.
#' @param gtPatch Factor, the name of the \code{gtShp} field containing the ground-truthing classifications.
#' @param remSensSampleID The name of the \code{remSensShp} field containing the sampleID for each classified patch.
#' @param remSensPatch Factor, the name of the \code{remSensShp} field containing the remote sensing classifications.
#' @param nDraws Number of random draws from an uncorrelated bivariate normal distribution used to propagate positional uncertainty.
#' @param nReclass Number of draws from a multinomial distribution used to reclassify patches and propagate classification uncertainty.
#' @param nTries Maximum number of attempts used to draw a set of ground-truthing location realizations that are within all patch types classified through remote sensing.
#' @param gpsAccuracy Magnitude of the horizontal radius that encompasses 95% of the data, this is also referred to as distance root mean square error. If \code{crs} is supplied, units must match the crs units, otherwise the units must match the coordinate system units of the supplied shapefiles (e.g., meters for UTM coordinate systems).
#' @param gpsAccuracyField The name of the \code{gtShp} field containing the ground-truthing GPS accuracy. If provided, gpsAccuracy will be ignored.
#' @param accuracyLevel The proportion of the true locations contained within a distance equal to \code{gpsAccuracy} from a measured GPS point.
#' @param subsetByProb Logical, if \code{TRUE} and \code{matchMethod=="randomPoints"}, each ground-truthing realization is designated as being within or outside of the corresponding remote sensing polygon using a Bernoulli trial with a probability of success equal to the probability the ground-truthing point is within the patch type identified through remote sensing. Only points designated as being within a patch are retained in \code{gtRemSens_dt}. This can lead to incomplete representation of ground-truthing data for some realizations, patch types that are infrequently observed are more likely to be omitted from representations of joined ground-truthing and remote sensing data.
#' @param weightedModel Logical, if \code{TRUE} and \code{matchMethod=="randomPoints"}, a point cloud of possible ground-truthing locations is used to determine the probability each point is within adjacent patch types, pseudo-observations ground-truthing points are then produced by replicating points to represent each possible patch type match, and the probability a ground-truthing point is within each patch type is used to weight a multinomal model estimating classification accuracy.
#' @param proportionOverlap The proportion of a buffered area that must fall within a single patch type in order for the corresponding ground-truthing data to be retained. Proportions must be greater than 0.5 to avoid potential conflicts with multiple patch types being matched to individual ground-truthing points.
#' @details Uncertainty in patch type classifications is assessed and propagated in several steps, If \code{matchMethod=="bufferedSubset"} a buffer with 
#' a radius equal to \code{gpsAccuracy} is drawn around each ground-truthing point. Buffered ground-truthing points are joined with the polygons of the 
#' remote sensed patches and only ground-truthing points with buffered areas contained within a single substrate type are retained for propagation of 
#' classification uncertainty, representing a data set where the degree of confidence the two data types (ground-truthing points and remote sensing polygons) 
#' overlap is high. A multinomial model is then used to estimate the probability of remote sensed data being classified (correctly or incorrectly) as 
#' each patch type. Lastly, probable classification accuracies are drawn from the multinomial model (see \link{drawMultinomProbs}) and the remote sensing classifications
#' and classification probabilities are supplied to \link{drawClass} to reclassify the patch type of each remote sensing polygon \code{nReclass} times. 
#' 
#' If \code{matchMethod=="randomPoints"} \code{gpsAccuracy} is passed to \link{findSD} through \link{locationPointCloud} to determine the corresponding standard 
#' deviation of the distance root mean square error supplied to \code{gpsAccuracy}, the standard deviation of the GPS error distribution is then passed to 
#' \link{propagateGPSUncertainty} to draw multiple realizations of ground-truthing locations, with the number of realizations defined by \code{nDraws}. The 
#' multiple realizations of ground-truthing points are spatially joined to the remote sensing data and supplied to \link{probWithin} to determine the probability 
#' a ground-truthing location is within each patch type identified by remote sensing. The probability of being within a remote sensed patch is calculated as, the 
#' proportion of realizations within the point cloud that are within each patch identified through remote sensing. If \code{subsetByProb==TRUE}, each 
#' ground-truthing realization is then designated as being within or outside of the corresponding remote sensing polygon using a Bernoulli process with a 
#' probability of success equal to the probability the ground-truthing is within the patch type identified through remote sensing. Only points designated as being 
#' within a patch are retained for estimation of classification accuracy. If \code{weighedModel==FALSE}, \code{nReclass} multinomial models are developed to estimate 
#' the probability of remote sensed data being classified as a for each set of ground-truthing location realizations. If \code{nReclass > nDraws} realization sets 
#' are drawn from the propagated ground-truthing point clouds \code{nReclass} times, with replacement. If \code{weightedModel==TRUE}, then psuedo-observations of 
#' ground-truthing locations are created to match ground-truthing observations with each classified patch type. Classification accuracy is then estimated with a 
#' multinomial model where pseudo-observations are weighted by the probability the corresponding ground-truthing point is within the patch type matched to the
#' pseudo-observation, thus pseudo-observations with lower probabilities of being within a particular patch type contribute less to the estimated classification 
#' accuracy than psuedo-observations with higher probabilities of being within a particular patch type. Probable classification accuracies are then drawn from the 
#' weighted multinomial model (see \link{drawMultinomProbs}) and the remote sensing classifications and classification probabilities are supplied to \link{drawClass} 
#' to reclassify the patch type of each remote sensing polygon \code{nReclass} times.
#' @return A list with: \itemize{
#' \item{\code{gtRemSens}} {A shapefile of class \code{sf} with spatially joined ground-truthing and remote sensing classifications, where ground-truthing patch types are in a column named \code{gtPatch} and \code{remSensPatch} contains classified patch types. If \code{matchMethod=="bufferedSubset"}, the shapefile is subset to only include ground-truthing data points at least \code{proportionOverlap} proportion of their buffered area contained within a single patch type identified through remote sensing (e.g., side-scan sonar).}
#' \item{\code{remSensPropUncert}} {A \link[data.table]{data.table} with reclassified patch types, where \code{drawID} is the set of ground-truthing sample location realizations a row belongs to, \code{reClassID} is the set of patch type reclassifications a row belongs to, \code{pullRows} is the row of a confusion matrix (appended from \code{confusionMats} if \code{matchMethod=="randomPoints"}, \code{drawnClassID} is the reclassified patch type as a numeric value, and \code{drawnClass} is the reclassified patch type as a character.}
#' \item{\code{probMats}} {A list with matrices of classification accuracy (probabilities remote sensing patch types correspond to ground-truthing patch types, drawn from a multinomial model) for each realization of a set of ground-truthing samples. Remote sensing patch classifications are in rows and the probability classifications correspond to ground-truthed patch types are in columns.}
#' \item{\code{withinProbs} (if \code{matchMethod=="randomPoints"})} {A \link[data.table]{data.table} with the probability (\code{prop}) a patch type at a ground-truth location (\code{gtPatch}) is within a patch type classified by remote sensing (\code{remSensPatch}) for each ground-truth \code{sampleID}. The number of ground-truthing location draws within a polygon (\code{N}) is also provided.}
#' \item{\code{fullMat}} {A vector documenting if probability matrices are complete (\code{TRUE} if complete). Vector elements correspond to slots in \code{probMats}. Probability matrices are flagged as incomplete if the patch types in column names do not match ground-truthing patch types in \code{gtShp}, the patch types in row names do not match the patch types in \code{remSensShp}, or negative probabilities are present in the matrix.}
#' \item{\code{weightedProbs} (if \code{matchMethod=="randomPoints"} & \code{weightedModel==TRUE})} {A matrix of class \link[data.table]{data.table}, with mean probabilities remote sensing patch types correspond to ground-truthing patch types. Remote sensing patch types are rows in the column \code{remSensPatch} and ground-truthing designations are in columns. Probabilities are estimated from a multinomial model using the probability a ground-truthing point is within a polygon type to weight the data points.}
#' \item{\code{meanProbs} (if \code{matchMethod=="bufferedSubset"})} {A matrix of class \link[data.table]{data.table}, with mean probabilities remote sensing patch types correspond to ground-truthing patch types. Remote sensing patch types are rows in the column \code{remSensPatch} and ground-truthing designations are in columns. Probabilities are estimated from a multinomial model.}
#' } 
#' @export
#' @examples
#' data(substrateLayers)
#' # Using random points method and weighted model describing classification accuracy
#' propUncert_weighted <- classificationAccuracy_propagateUncertainty(gtShp=substrateLayers[['gtDom_sf']], remSensShp=substrateLayers[['sonarClass']], matchMethod="randomPoints",
#'                                                                           gtSampleID="samplID", gtPatch="domSub", remSensSampleID="id",
#'                                                                           remSensPatch="substrate", 
#'                                                                           nDraws=100, nReclass=100, gpsAccuracy=5,
#'                                                                           weightedModel=TRUE)
#'                                                                           
#' # Using random points method and models of classification accuracy for each set of ground-truthing realizations
#' propUncert_points <- classificationAccuracy_propagateUncertainty(gtShp=substrateLayers[['gtDom_sf']], remSensShp=substrateLayers[['sonarClass']], matchMethod="randomPoints",
#'                                                                           gtSampleID="samplID", gtPatch="domSub", remSensSampleID="id",
#'                                                                           remSensPatch="substrate", 
#'                                                                           nDraws=100, nReclass=100, gpsAccuracy=5)
#'
#' # Using buffered subset method
#' propUncert_subset <- classificationAccuracy_propagateUncertainty(gtShp=substrateLayers[['gtDom_sf']], remSensShp=substrateLayers[['sonarClass']], matchMethod="bufferedSubset",
#'                                                                  gtSampleID="samplID", gtPatch="domSub", remSensSampleID="id",
#'                                                                  remSensPatch="substrate", 
#'                                                                  nReclass=100, gpsAccuracy=5)


classificationAccuracy_propagateUncertainty <- function(gtShp, remSensShp, crs=NULL, matchMethod="bufferedSubset",
                                                        gtSampleID, gtPatch, 
                                                        remSensSampleID, remSensPatch,
                                                        nDraws=1000,
                                                        nReclass=1000,
                                                        nTries=1000,
                                                        gpsAccuracy=NULL, gpsAccuracyField=NULL, accuracyLevel=0.95,
                                                        subsetByProb=FALSE,
                                                        weightedModel=FALSE,
                                                        proportionOverlap=1){
  #set up env object for running data.table operations with objects defining field names
  dtEnv <- list(gtSampleID=gtSampleID, gtPatch=gtPatch, remSensSampleID=remSensSampleID, remSensPatch=remSensPatch, gpsAccuracyField=gpsAccuracyField)
  
  #ensure levels of gtPatch and remSensPatch match
  stopifnot(class(gtShp[[gtPatch]])=="factor" | class(remSensShp[[remSensPatch]])=="factor")
  
  #other checks
  stopifnot(matchMethod %in% c("randomPoints", "bufferedSubset"))
  
  #define remSensPatches to propagate uncertainty for
  remSensPatchTypes <- levels(remSensShp[[remSensPatch]]) #note levels is used here instead of unique, because when remSensPatch is passed to the multinomial model, estimated classification probabilities are for each level in the data set
  gtPatchTypes <- levels(droplevels(gtShp[[gtPatch]])) #drop unused levels, because multinom will drop unused levels
  
  if(matchMethod=="randomPoints"){
    pointCloud_list <- locationPointCloud(gtShp=gtShp, remSensShp=remSensShp, crs=crs,
                                          gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch, 
                                          nDraws=nDraws, nTries=nTries,
                                          gpsAccuracy=gpsAccuracy, gpsAccuracyField=gpsAccuracyField, accuracyLevel=accuracyLevel,
                                          subsetByProb=subsetByProb)
    
    # setnames(pointCloud_list$gtRemSens_dt, old=c("gtSampleID", "gtPatch", "remSensPatch"), new=c(gtSampleID, gtPatch, remSensPatch))     
    # pointCloud_list$gtRemSens <- st_as_sf(pointCloud_list$gtRemSens_dt)
    
    if(weightedModel==TRUE){
      joined_dt <- data.table(pointCloud_list$gtRemSens)
      uniqueMatches <- joined_dt[!is.na(remSensPatch) & !is.na(gtPatch), mean(prob), by=list(gtSampleID, gtPatch, remSensPatch), env=dtEnv]
      mn_weighted <- quiet(multinom(reformulate(termlabels=remSensPatch, response=gtPatch), weights=V1, uniqueMatches), warning=FALSE)
      
      # mn_weighted <- weightedAccuracy(joined=joined_dt, gtSampleID=gtSampleID, gtPatch=gtPatch,
      #                                 remSensPatch=remSensPatch, prob="prob",
      #                                 gtPatches=gtPatchTypes)
      
      #calculate mean classification accuracy
      weightedProbs <- predict(mn_weighted, mn_weighted$xlevels, type="probs")
      dimnames(weightedProbs)[[1]] <- paste0("classified_as_", mn_weighted$xlevels[[1]])
      dimnames(weightedProbs)[[2]] <- paste0("prob_", dimnames(weightedProbs)[[2]])
      
      probMats <- drawMultinomProbs(mn_weighted, nDraws=nReclass)
      
    } else {
      joined_dt <- data.table(pointCloud_list$gtRemSens)
      #build dataset to pull reclass data from
      if(nReclass > nDraws){
        sampledDraws <- sample(1:nDraws, nReclass, replace=TRUE)
        gtRemSens_dt <- vector("list", length=nReclass)
        for(i in 1:nReclass){
          gtRemSens_dt[[i]] <- joined_dt[drawID==sampledDraws[i], ]
          gtRemSens_dt[[i]][, reClassID := i][]
        }
        gtRemSens_dt <- do.call('rbind', gtRemSens_dt)
      } else {
        sampledDraws <- sample(1:nDraws, nReclass, replace=FALSE)
        gtRemSens_dt <- joined_dt[drawID %in% sampledDraws]
        gtRemSens_dt[, reClassID := which(sampledDraws==drawID), by=1:nrow(gtRemSens_dt)][]
      }
      
      #conduct multinomial analysis for each draw id and estimate prob. of mis-classifying for each patch type
      probMats <- vector("list", length=nReclass)
      for(i in 1:nReclass){
        mn <- gtRemSens_dt[reClassID==i & overlap==1 & !is.na(remSensPatch), quiet(multinom(gtPatch~remSensPatch), warning=FALSE), env=dtEnv]
        probMats[[i]] <- drawMultinomProbs(mn, nDraws=1)
      }
      #convert gtRemSens_dt back to shapefile for output
      gtRemSens <- st_as_sf(gtRemSens_dt)
    }
    #end if matchMethod=="randomPoints"
    
  } else { #matchMethod=="bufferedSubset"
    bufferedSubset <- bufferSubset(gtShp=gtShp, remSensShp=remSensShp, gtSampleID=gtSampleID, remSensSampleID=remSensSampleID, remSensPatch=remSensPatch, 
                                   gpsAccuracy=gpsAccuracy, gpsAccuracyField=gpsAccuracyField, proportionOverlap=proportionOverlap)
    
    #propagate uncertainty
    mn <- quiet(multinom(reformulate(termlabels=remSensPatch, response=gtPatch), bufferedSubset), warning=FALSE)
    meanProbs <- predict(mn, mn$xlevels, type="probs")
    dimnames(meanProbs)[[1]] <- paste0("classified_as_", mn$xlevels[[1]])
    dimnames(meanProbs)[[2]] <- paste0("prob_", dimnames(meanProbs)[[2]])
    
    probMats <- drawMultinomProbs(mn, nDraws=nDraws)
  }
  #end matchMethod=="bufferedSubset"
  
  fullMat <- vector("logical", length=nReclass)
  #check that probability matrix rows match remSens patch levels, columns match gt patch levels observed, all rows sum to 1, and that all probabilities are between 0 and 1
  for(i in 1:nReclass){
    if(isTRUE(all.equal(gsub("prob_", "", colnames(probMats[[i]])), gtPatchTypes)) &  
       isTRUE(all.equal(gsub("classified_as_", "", rownames(probMats[[i]])), remSensPatchTypes)) & 
       isTRUE(mean(apply(probMats[[i]], 1, sum))==1) &
       isTRUE(mean(between(probMats[[i]], 0, 1))==1)){ 
      fullMat[i] <- TRUE
    } else {
      fullMat[i] <- FALSE
    }
  }
  
  remSens_dt <- data.table(remSensShp)
  remSensPropUncert <- remSens_dt[rep(1:nrow(remSens_dt), nReclass)]
  remSensPropUncert[, reClassID := seq_along(remSensPatch), by=remSensSampleID, env=dtEnv][]
  if(matchMethod=="randomPoints" & weightedModel==FALSE){
    remSensPropUncert <- merge(remSensPropUncert, unique(gtRemSens_dt[, .(reClassID, drawID), env=dtEnv]), by="reClassID")
  } else { #matchMethod=="bufferedSubset" or weightedModel==TRUE
    remSensPropUncert[, drawID := 1][]
  }
  
  remSensPropUncert[, rowID := seq_along(drawID)][]
  #randomly assign new class from multinomial distribution based on calculated accuracy
  remSensPropUncert[, drawnClassID := numeric()][]
  remSensPropUncert[!is.na(remSensPatch) & reClassID %in% which(fullMat==TRUE), drawnClassID := drawClass(classProps=probMats, assignedClass=as.numeric(remSensPatch), index=reClassID), by=rowID, env=dtEnv][]
  remSensPropUncert[, drawnClass := gtPatchTypes[drawnClassID]][]
  
  #drop rowID column, since it is no longer needed
  remSensPropUncert[, rowID := NULL][]
  
  if(matchMethod=="randomPoints"){
    if(weightedModel==TRUE){
      return(list(gtRemSens=pointCloud_list$gtRemSens,
                  withinProbs=pointCloud_list$withinProbs,
                  remSensPropUncert=remSensPropUncert,
                  probMats=probMats,
                  fullMat=fullMat,
                  weightedProbs=weightedProbs))
    } else {
      return(list(gtRemSens=gtRemSens,
                  withinProbs=pointCloud_list$withinProbs,
                  remSensPropUncert=remSensPropUncert,
                  probMats=probMats,
                  fullMat=fullMat))
    }
  } else { #matchMethod=="bufferedSubset"
    return(list(gtRemSens=bufferedSubset,
                remSensPropUncert=remSensPropUncert,
                probMats=probMats,
                fullMat=fullMat,
                meanProbs=meanProbs))
  }
}
