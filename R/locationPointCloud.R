#' locationPointCloud
#' 
#' Join ground-truthing data to patch classification from remote sensing data (e.g., side-scan sonar), calculate classification accuracy, and propagate uncertainty in classifications.
#' @param gtShp A point shapefile of class \code{sf} with ground-truthing data, should be in projected coordinate system (e.g., UTM, state plane).
#' @param remSensShp A polygon shapefile of class \code{sf} with remote sensing classifications, should be in projected coordinate system (e.g., UTM, state plane) and must match the \code{gtShp} coordinate system if \code{crs} is \code{NULL}.
#' @param crs Optional. CRS to be used to reproject \code{gtShp} and \code{remSensShp}.
#' @param gtSampleID The name of the \code{gtShp} field containing the ground-truthing sampleID.
#' @param gtPatch Factor, the name of the \code{gtShp} field containing the ground-truthing classifications.
#' @param remSensPatch Factor, the name of the \code{remSensShp} field containing the remote sensing classifications.
#' @param nDraws Number of random draws from an uncorrelated bivariate normal distribution used to propagate positional uncertainty.
#' @param nTries Maximum number of attempts used to draw a set of ground-truthing location realizations that are within all patch types classified through remote sensing.
#' @param gpsAccuracy Magnitude of the horizontal radius that encompasses 95% of the data, this is also referred to as distance root mean square error. If \code{crs} is supplied, units must match the crs units, otherwise the units must match the coordinate system units of the supplied shapefiles (e.g., meters for UTM coordinate systems).
#' @param gpsAccuracyField The name of the \code{gtShp} field containing the ground-truthing GPS accuracy. If provided, gpsAccuracy will be ignored.
#' @param accuracyLevel The proportion of the true locations contained within a distance equal to \code{gpsAccuracy} from a measured GPS point.
#' @param subsetByProb Logical, if \code{TRUE}, each ground-truthing realization is designated as being within or outside of the corresponding remote sensing polygon using a Bernoulli trial with a probability of success equal to the probability the ground-truthing is within the patch type identified through remote sensing. Only points designated as being within a patch are retained in \code{gtRemSens_dt}.
#' @details \code{gpsAccuracy} is passed to \link{findSD} to determine the corresponding standard deviation of the distance root 
#' mean square error supplied to \code{gpsAccuracy}, the standard deviation of the GPS error distribution is then passed to \link{propagateGPSUncertainty} 
#' to draw multiple realizations  of ground-truthing locations, with the number of realizations defined by \code{nDraws}. The multiple realizations of 
#' ground-truthing points are spatially joined to the remote sensing data and supplied to \link{probWithin} to determine the probability a ground-truthing 
#' location is within each patch type identified by remote sensing. The probability of being within  a remote sensed patch is calculated as, the proportion 
#' of realizations within the point cloud that are within each patch identified through remote sensing. Each ground-truthing realization is then designated
#' as being within or outside of the corresponding remote sensing polygon using a Bernoulli process with a probability of success equal to the probability 
#' the ground-truthing is within the patch type identified through remote sensing. Only points designated as being within a patch are retained for calculation 
#' of a confusion matrix and classification uncertainty.
#' @return A list with: \itemize{
#' \item{\code{gtRemSens}} {A shapefile of class \code{sf} of spatially joined ground-truthing and remote sensing classifications.}
#' \item{\code{withinProbs}} {A \link[data.table]{data.table} with the probability (\code{prop}) a patch type at a ground-truth location is within a patch type classified by remote sensing for each ground-truthing sample. The number of ground-truthing location draws within a polygon (\code{N}) is also provided.}
#' } 
#' @export
#' @examples
#' data(substrateLayers)
#' locPointCould <- locationPointCloud(gtShp=substrateLayers[['gtDom_sf']], remSensShp=substrateLayers[['sonarClass']], crs=NULL,
#'                                     gtSampleID="samplID", gtPatch="domSub",
#'                                     remSensPatch="substrate", 
#'                                     nDraws=100, gpsAccuracy=5)

locationPointCloud <- function(gtShp, remSensShp, crs=NULL,
                               gtSampleID, gtPatch,
                               remSensPatch, 
                               nDraws=1000, nTries=1000,
                               gpsAccuracy=NULL, gpsAccuracyField=NULL, accuracyLevel=0.95,
                               subsetByProb=FALSE){
  #set up env object for running data.table operations with objects defining field names
  dtEnv <- list(gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch)
  
  #define remSensPatches to propagate uncertainty for
  remSensPatchTypes <- unique(na.omit(remSensShp[[remSensPatch]]))
  gtPatchTypes <- unique(na.omit(gtShp[[gtPatch]]))
  
  #propagate uncertainty
  gtRemSens <- propagateGPSUncertainty_fullRepresentation(gtShp=gtShp, remSensShp=remSensShp, crs=crs,
                                                          gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch,
                                                          nDraws=nDraws, nTries=nTries,
                                                          gpsAccuracy=gpsAccuracy, gpsAccuracyField=gpsAccuracyField, accuracyLevel=accuracyLevel)
  
  #calculate probability of overlap with a patch
  withinProbs <- probWithin(joined=gtRemSens, gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch, nDraws=nDraws)
  
  gtRemSens_dt <- data.table(gtRemSens)
  gtRemSens_dt <- merge(gtRemSens_dt, withinProbs[, .(gtSampleID, remSensPatch, prob), env=dtEnv], by=c(gtSampleID, remSensPatch), all=TRUE)
  
  if(isTRUE(subsetByProb)){
    #subset by prob. overlap and reclass by draw from gps error distribution
    #use random draw from Bernoulli distribution with prob. equal to prob. of overlap to determine if ground-truthing data should be used for accuracy assessment
    gtRemSens_dt[, overlap := ifelse(is.na(prob), NA, rbinom(1, 1, prob)), by=list(gtSampleID, drawID), env=dtEnv][] # the ifelse statement is to avoid getting NA warnings for instances when the probability of overlap is NA, such as if a ground-truthing location doesn't overlap with any polygons
    fullSets <- gtRemSens_dt[overlap==1, 
                             list(fullRemSens=matchChecker(x=remSensPatch, y=remSensPatchTypes), 
                                  fullGT=matchChecker(x=gtPatch, y=gtPatchTypes)),
                             by=drawID, env=dtEnv][fullRemSens==TRUE & fullGT==TRUE, drawID]
    
    #if a drawID does not have a full set of matches, recategorize ground-truthing points as being within or outside a matched patch
    tries <- 1
    while(length(fullSets)<nDraws & tries<nTries){
      gtRemSens_dt[!drawID %in% fullSets, overlap := ifelse(is.na(prob), NA, rbinom(1, 1, prob)), by=list(gtSampleID, drawID), env=dtEnv][]
      fullSets <- gtRemSens_dt[overlap==1, 
                               list(fullRemSens=matchChecker(x=remSensPatch, y=remSensPatchTypes), 
                                    fullGT=matchChecker(x=gtPatch, y=gtPatchTypes)),
                               by=drawID, env=dtEnv][fullRemSens==TRUE & fullGT==TRUE, drawID]
      tries <- tries + 1
    }
    
    if(length(fullSets)<nDraws & tries>=nTries){
      classPrint <- paste(remSensPatchTypes, sep=", ", collapse=", ")
      classPrint2 <- paste(gtPatchTypes, sep=", ", collapse=", ")
      
      message(paste0("After ", tries, " attempts to categorize ground-truthing points as within or outside classified polygons, a full set of patch types classified from remote sensing data (", classPrint, ") and/or ground-truthing data (", classPrint2, ") was not represented by ", nDraws - length(fullSets), " sets of ground-truthing point realizations (i.e., draws). Errors or an incomplete set of reclassifitions may occur."))
    } else {
      #do nothing
    }
  } else{
    gtRemSens_dt[, overlap := 1]
  }
  
  gtRemSens <- st_as_sf(gtRemSens_dt)
  
  return(list(gtRemSens=gtRemSens,
              withinProbs=withinProbs))
}