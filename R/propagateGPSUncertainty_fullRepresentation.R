#' propagateGPSUncertainty_fullRepresentation
#' 
#' A helper function to redraw ground-truthing point realizations to ensure all sets of draws have at least one ground-truthing point within each required classified patch type.
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
#' @return A point shapefile of class \code{sf} with ground-truthing location realizations and associated data (e.g., patch type) spatially joined to the \code{remSensShp} layer.
#' @export
#' @examples
#' data(substrateLayers)
#' gtShp <- substrateLayers[['gtDom_sf']]
#' remSensShp <- substrateLayers[['sonarClass']]
#' propagateGPSUncertainty_fullRepresentation(gtShp=gtShp,
#'                                            remSensShp=remSensShp,
#'                                            gtSampleID="samplID",
#'                                            gtPatch="domSub",
#'                                            remSensPatch="substrate",
#'                                            gpsAccuracy=5,
#'                                            nDraws=100,
#'                                            nTries=100)
#' @seealso \link{propagateGPSUncertainty} \link{drawPointsAndJoin}, and \link{classificationAccuracy_propagUncertainty}.

propagateGPSUncertainty_fullRepresentation <- function(gtShp, remSensShp, crs=NULL,
                                                       gtSampleID, gtPatch,
                                                       remSensPatch,
                                                       nDraws=1000, nTries=1000,
                                                       gpsAccuracy=NULL, gpsAccuracyField=NULL, accuracyLevel=0.95){
  #check that fields in gtShp and remSensShp are unique
  if(sum(names(gtShp)[!names(remSensShp)=="geometry"] %in% names(remSensShp)[!names(remSensShp)=="geometry"]) > 0){
    message("At least one field in gtShp and remSensShp have the same name, _gt and _remSens will be added to field names to ensure fields are unique")
    names(gtShp)[!names(gtShp)=="geometry"] <- paste0(names(gtShp)[!names(gtShp)=="geometry"], "_gt")
    names(remSensShp)[!names(remSensShp)=="geometry"] <- paste0(names(remSensShp)[!names(remSensShp)=="geometry"], "_remSenShp")
    
    gtSampleID <- paste0(gtSampleID, "_gt")
    gtPatch <- paste0(gtPatch, "_gt")
    
    remSensPatch <- paste0(remSensPatch, "_remSenShp")
    
    if(is.null(gpsAccuracyField)){
      #do nothing
    } else {
      gpsAccuracyField <- paste0(gpsAccuracyField, "_gt") 
    }
  } else {
    #do nothing
  }
  
  #set up env object for running data.table operations with objects defining field names
  dtEnv <- list(gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch)
  
  #first draw
  gtRemSens <- drawPointsAndJoin(gtShp=gtShp, remSensShp=remSensShp, gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch, nDraws=nDraws, 
                                 gpsAccuracy=gpsAccuracy, gpsAccuracyField=gpsAccuracyField, accuracyLevel=accuracyLevel)
  
  #while the number of draws that have ground-truthing points in each classified patch type is less than nDraws, redraw new ground-truthing locations
  nFullSet <- length(unique(gtRemSens$drawID))
  nRedraws <- nDraws - nFullSet
  tries <- 1
  
  while(nFullSet<nDraws & tries<nTries){
    redraw <- drawPointsAndJoin(gtShp=gtShp, remSensShp=remSensShp, gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch, nDraws=nDraws, 
                                gpsAccuracy=gpsAccuracy, gpsAccuracyField=gpsAccuracyField, accuracyLevel=accuracyLevel)
    gtRemSens <- rbind(gtRemSens, redraw)
    
    #update draw ID
    gtRemSens_dt <- data.table(gtRemSens)
    gtRemSens_dt[, drawID := seq_along(gtPatch), by=gtSampleID, env=dtEnv][]
    gtRemSens$drawID <- gtRemSens_dt[, drawID]
    
    nFullSet <- gtRemSens_dt[, max(drawID)]
    nRedraws <- nDraws - nFullSet
    tries <- tries + 1
  }
  if(nFullSet<nDraws){
    classVec <- unique(na.omit(remSensShp[[remSensPatch]]))
    classVec2 <- unique(na.omit(gtShp[[gtPatch]]))
    classPrint <- paste(classVec, sep=", ", collapse=", ")
    classPrint2 <- paste(classVec2, sep=", ", collapse=", ")
    
    message(paste0("Only ", nFullSet, " sets of ground-truthing location realizations were drawn. A full set of patch types classified from remote sensing data (", classPrint, ") and/or ground-truthing data (", classPrint2, ") was not represented by all sets of ground-truthing point realizations after ", tries, " tries."))
  } else {
    #do nothing
  }
  return(gtRemSens)
}