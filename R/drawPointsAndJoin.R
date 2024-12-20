#' drawPointsAndJoin
#' 
#' A helper function to draw ground-truthing point realizations and subset only sets of draws that have at least one ground-truthing point within each required classified patch type.
#' @param gtShp A point shapefile of class \code{sf} with ground-truthing data, should be in projected coordinate system (e.g., UTM, state plane).
#' @param remSensShp A polygon shapefile of class \code{sf} with remote sensing classifications, should be in projected coordinate system (e.g., UTM, state plane) and must match the \code{gtShp} coordinate system if \code{crs} is \code{NULL}.
#' @param crs Optional. CRS to be used to reproject \code{gtShp} and \code{remSensShp}.
#' @param gtSampleID The name of the \code{gtShp} field containing the ground-truthing sampleID.
#' @param gtPatch Factor, the name of the \code{gtShp} field containing the ground-truthing classifications.
#' @param remSensPatch Factor, the name of the \code{remSensShp} field containing the remote sensing classifications.
#' @param nDraws Number of random draws from an uncorrelated bivariate normal distribution used to propagate positional uncertainty.
#' @param gpsAccuracy Magnitude of the horizontal radius that encompasses 95% of the data, this is also referred to as distance root mean square error. If \code{crs} is supplied, units must match the crs units, otherwise the units must match the coordinate system units of the supplied shapefiles (e.g., meters for UTM coordinate systems).
#' @param gpsAccuracyField The name of the \code{gtShp} field containing the ground-truthing GPS accuracy. If provided, gpsAccuracy will be ignored.
#' @param accuracyLevel The proportion of the true locations contained within a distance equal to \code{gpsAccuracy} from a measured GPS point.
#' @return A point shapefile of class \code{sf} with ground-truthing location realizations and associated data (e.g., patch type) spatially joined to the \code{remSensShp} layer.
#' @export
#' @examples
#' data(substrateLayers)
#' gtShp <- substrateLayers[['gtDom_sf']]
#' remSensShp <- substrateLayers[['sonarClass']]
#' drawPointsAndJoin(gtShp=gtShp,
#'                   remSensShp=remSensShp,
#'                   gtSampleID="samplID",
#'                   gtPatch="domSub",
#'                   remSensPatch="substrate",
#'                   gpsAccuracy=5,
#'                   nDraws=100)
#' @seealso \link{propagateGPSUncertainty} and \link{classificationAccuracy_propagUncertainty}.

drawPointsAndJoin <- function(gtShp, remSensShp, crs=NULL,
                              gtSampleID, gtPatch,
                              remSensPatch,
                              nDraws=1000,
                              gpsAccuracy=NULL, gpsAccuracyField=NULL, accuracyLevel=0.95){
  #set up env object for running data.table operations with objects defining field names
  dtEnv <- list(gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch)
  
  #ensure coordinate systems match
  if(is.null(crs)){
    stopifnot(st_crs(gtShp)==st_crs(remSensShp))
  } else {
    gtShp <- st_transform(gtShp, crs=crs)
    remSensShp <- st_transform(remSensShp, crs=crs)
  }
  
  #assign GPS accuracy field
  if(is.null(gpsAccuracyField)){
    stopifnot(is.numeric(gpsAccuracy))
    gtShp$gpsAccuracy <- gpsAccuracy
    #supply field name to gpsAccuracyField for future operations
    gpsAccuracyField <- "gpsAccuracy"
  } else {
    stopifnot(is.numeric(gtShp[[gpsAccuracyField]]))
  }
  #add gpsAccuracyField to dtEnv
  dtEnv$gpsAccuracyField <- gpsAccuracyField
  
  #prep ground-truthing data for propagating uncertainty
  gtCoords <- data.table(st_coordinates(gtShp))
  gtCoords[, eval(gtSampleID) := gtShp[[gtSampleID]]][]
  gtCoords[, eval(gpsAccuracyField) := gtShp[[gpsAccuracyField]]][] #the extra [] is to update and print the data.table, following help page recommendations. If the function ends with a := as the last data.table operation, nothing will be returned. See: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#why-do-i-have-to-type-dt-sometimes-twice-after-using-to-print-the-result-to-console
  
  if(TRUE %in% is.na(gtCoords[[gpsAccuracyField]]) | 0 %in% (gtCoords[[gpsAccuracyField]])){
    warning(paste0(gpsAccuracyField, " contains NA or 0, all instances of NA or 0 are being dropped from processing"))
    gtCoords <-  gtCoords[!is.na(gpsAccuracyField) & gpsAccuracyField>0, env=dtEnv]
  } else {
    #do nothing
  }
  
  gtCoords[, gpsLocSD := findSD(gpsAccuracy=gpsAccuracyField, accuracyLevel=accuracyLevel), by=gtSampleID, env=dtEnv][]
  
  #propagate uncertainty
  gtDom_pointCloud <- propagateGPSUncertainty(sampleID=gtCoords[[gtSampleID]], x=gtCoords$X, y=gtCoords$Y, sd=gtCoords$gpsLocSD, nDraws=nDraws)
  setnames(gtDom_pointCloud, old="sampleID", new=gtSampleID)
  
  #add ID for each draw
  gtDom_pointCloud[, drawID := seq_along(X), by=gtSampleID, env=dtEnv][]
  
  gt_dt <- data.table(gtShp)
  
  #merge point clouds with patch classifications and convert to a shapefile
  gtDom_sf_pointCloud <- merge(gtDom_pointCloud, gt_dt[, .(gtPatch, gtSampleID), env=dtEnv], by=gtSampleID)
  gtDom_sf_pointCloud <- st_as_sf(gtDom_sf_pointCloud, coords=c("X", "Y"), crs=st_crs(gtShp))
  
  #conduct a spatial join to merge the ground-truthing and sonar classifications
  gtRemSens <- st_join(gtDom_sf_pointCloud, remSensShp, st_within, left=TRUE)
  
  gtRemSens_dt <- data.table(gtRemSens)
  
  #identify draws with at least one ground-truthing realization in each classified patch type
  gtPatchTypes <- gt_dt[!is.na(gtPatch), unique(gtPatch), env=dtEnv]
  remSensPatchTypes <- unique(na.omit(remSensShp[[remSensPatch]]))
  keepDraws <- gtRemSens_dt[!is.na(remSensPatch) & !is.na(gtPatch), 
                            list(fullRemSens=matchChecker(x=remSensPatch, y=remSensPatchTypes), 
                                 fullGT=matchChecker(x=gtPatch, y=gtPatchTypes)),
                            by=drawID, env=dtEnv][fullRemSens==TRUE & fullGT==TRUE, drawID]
  #subset to only include draws with at least one ground-truthing realization in each classified patch type
  gtRemSens <- gtRemSens[gtRemSens$drawID %in% keepDraws, ]
  
  return(gtRemSens)
}