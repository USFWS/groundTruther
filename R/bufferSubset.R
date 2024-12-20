#' bufferSubset
#' 
#' Join and subset ground-truthing point shapefile and patch type polygon 
#' shapefile to only include points where a buffered area (defined by 
#' \code{gpsAccuracy}) is fully within a single patch type.
#' @param gtShp A point shapefile of class \code{sf} with ground-truthing data, 
#' should be in projected coordinate system (e.g., UTM, state plane).
#' @param remSensShp A polygon shapefile of class \code{sf} with remote sensing 
#' classifications, should be in projected coordinate system (e.g., UTM, state 
#' plane) and must match the \code{gtShp} coordinate system if \code{crs} is 
#' \code{NULL}.
#' @param crs Optional. CRS to be used to reproject \code{gtShp} and 
#' \code{remSensShp}.
#' @param gtSampleID The name of the \code{gtShp} field containing the 
#' ground-truthing sampleID.
#' @param remSensSampleID The name of the \code{remSensShp} field containing the 
#' sampleID for each classified patch.
#' @param remSensPatch The name of the \code{remSensShp} field containing the 
#' remote sensing classifications.
#' @param gpsAccuracy Magnitude of the horizontal radius that encompasses 95% of 
#' the data, this is also referred to as distance root mean square error. If 
#' \code{crs} is supplied, units must match the crs units, otherwise the units 
#' must match the coordinate system units of the supplied shapefiles (e.g., 
#' meters for UTM coordinate systems).
#' @param gpsAccuracyField The name of the \code{gtShp} field containing the 
#' ground-truthing GPS accuracy. If provided, gpsAccuracy will be ignored.
#' @param proportionOverlap The proportion of a buffered area that must fall 
#' within a single patch type in order for the corresponding ground-truthing 
#' data to be retained. Proportions must be greater than 0.5 to avoid potential 
#' conflicts with multiple patch types being matched to individual 
#' ground-truthing points.
#' @details A radius equal to \code{gpsAccuracy} is drawn around each 
#' ground-truthing point. Buffered  ground-truthing points are joined with the 
#' polygons of the remote sensed patches and only ground-truthing points where 
#' the area of the buffer within a single patch type is greater than the product 
#' of the  \code{proportionOverlap} and the area of the buffer within polygons 
#' of classified patches are retained, representing a data set where the degree 
#' of confidence the two data types (ground-truthing points and remote sensing 
#' polygons) overlap is high. 
#' @return A shapefile of class \code{sf} with spatially joined ground-truthing 
#' and remote sensing classifications, subset to only include data where 
#' ground-truthing points with a buffered area, having a radius equal to 
#' \code{gpsAccuracy}, have at least \code{proportionOverlap} proportion of 
#' their buffered area within a single patch type.
#' @export
#' @examples
#' data(substrateLayers)
#' bufferedSubset <- bufferSubset(gtShp=substrateLayers[['gtDom_sf']], 
#' remSensShp=substrateLayers[['sonarClass']], 
#'                                gtSampleID="samplID", remSensSampleID="id", 
#'                                remSensPatch="substrate", gpsAccuracy=5)
#' @seealso \link{classificationAccuracy_propagUncertainty}.

bufferSubset <- function(gtShp, remSensShp, crs=NULL, gtSampleID, remSensSampleID, remSensPatch, gpsAccuracy=NULL, gpsAccuracyField=NULL, 
                         proportionOverlap=1){
  #set up env object for running data.table operations with objects defining field names
  dtEnv <- list(gtSampleID=gtSampleID, remSensSampleID=remSensSampleID, remSensPatch=remSensPatch)
  
  #ensure proportionOverlap is a proportion > 0.5
  stopifnot(proportionOverlap > 0.5 & proportionOverlap <= 1)
  
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
  
  gtBuffer <- st_buffer(gtShp, dist=gtShp[[gpsAccuracyField]])
  
  #split buffer by remSens polygons
  gtRemSens <- st_intersection(gtBuffer, remSensShp)
  #calculate area of split buffers
  gtRemSens$areaOverlap <- st_area(gtRemSens)
  
  #calculate area of each buffer that is required to be within a single patch type to keep for accuracy estimation
  gtRemSens_dt <- data.table(gtRemSens)
  areaByID <- gtRemSens_dt[!is.na(remSensPatch), .(overlapThreshold=sum(areaOverlap)*proportionOverlap), by=gtSampleID, env=dtEnv]
  
  #subset buffered data
  gtRemSens_dt <- merge(gtRemSens_dt, areaByID, by=gtSampleID)
  gtRemSensKeep <- gtRemSens_dt[areaOverlap >= overlapThreshold]
  gtRemSensSub <- gtRemSens[paste(gtRemSens[[gtSampleID]], gtRemSens[[remSensSampleID]]) %in% gtRemSensKeep[, paste(gtSampleID, remSensSampleID), env=dtEnv],]
  
  return(gtRemSensSub)
}