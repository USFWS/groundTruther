#' surveyPoints
#' 
#' Assists with designing ground-truthing surveys by drawing locations to 
#' conduct ground-truthing data collection. 
#' @param remSensShp A polygon shapefile of class \code{sf} with remote sensing 
#' classifications, should be in projected coordinate system (e.g., UTM, state 
#' plane).
#' @param remSensPatch The name of the \code{remSensShp} field containing the 
#' remote sensing classifications.
#' @param nPatches Number of ground-truthing points to survey. Omit if
#'\code{nProportion} is provided.
#' @param nProportion Proportion of classified patches to receive ground-truthing
#' points to survey. Omit if \code{nPatches} is provided.#' 
#' @param nCluster Number of points to sample within each randomly selected patch.
#' If 1 (default) cluster sampling is not conducted, if greater than 1, cluster
#' sampling is conducted with the number of sub samples in each randomly or 
#' stratified randomly selected polygon equal to nCluster.
#' @param distThreshold Minimum distance which ground-truthing survey locations 
#' must be from a patch edge. This can be set to the accuracy of the GPS used
#' during remote sensing and ground-truthing surveys or some other distance to
#' ensure survey locations are within delineated patches. This helps reduce
#' co-registration errors due to positional inaccuracies, that can occur if 
#' survey points are near patch edges.
#' @param sampleDesign Should survey points be placed randomly 
#' (\code{sampleDesign="random"}) or with a stratified random design 
#' (\code{sampleDesign="stratified random"}). Defaults to stratified random.
#' @param sampAllocation If a stratified random approach is used to determine
#' survey locations, should an optimum or proportionate allocation allocation 
#' be used to distribute survey locations across patch types. If 
#' \code{sampAllocation="optimum"}, a Neyman allocation is used, using the 
#' standard deviation of patch area to minimize variance. If 
#' \code{sampAllocation="proportionate"}, survey points are distributed across 
#' patch types with a proportionate allocation. The Neyman allocation has the 
#' advantage of taking number and size variation of patches into account, but 
#' may under allocate samples to patch types with few observations or low 
#' variability in size. Defaults to optimum sample allocation.
#' @details Survey points are placed with either a random or stratified 
#' random sampling design and limited to areas that are a user defined 
#' distance or more from patch edges, to reduce potential for co-registration 
#' errors in ground-truthing surveys.
#' @return A shapefile of class \code{sf} with x and y coordinates for a user
#' defined number of randomly or stratified randomly drawn survey locations.
#' @export
#' @examples
#' data(substrateLayers)
#' set.seed(11252025)
#' gtSurvey <- surveyPoints(remSensShp = substrateLayers$sonarClass,
#'                          remSensPatch="substrate", sampleDesign="random", 
#'                          nPatches=100, distThreshold=5)
#' 
#' ggplot() +
#'   geom_sf(data=substrateLayers$sonarClass, 
#'           aes(fill=substrate, color=substrate)) +
#'   geom_sf(data=gtSurvey) +
#'   coord_sf(datum=st_crs(substrateLayers$sonarClass),
#'            ylim=c(4580800, 4581200), xlim=c(448500, 448950))
 
surveyPoints <- function(remSensShp, remSensPatch, nPatches=NULL, nProportion=NULL, nCluster=1, distThreshold=NULL, sampleDesign="stratified random", sampleAllocation="optimum"){
  #check that inputs are correct
  stopifnot(sampleDesign %in% c("random", "stratified random")) #allow flexibility for adding more survey designs in future
  stopifnot(!is.null(distThreshold))
  stopifnot(!is.null(nPatches) | !is.null(nProportion))
  if(!is.null(nProportion)){
    stopifnot(nProportion > 0 & nProportion < 1)
  } else {
    #do nothing
  }
  if(sampleDesign=="stratified random"){
    stopifnot(sampleAllocation %in% c("optimum", "proportionate"))
  }
  if(!is.null(nPatches) & !is.null(nProportion)){
    warning("Both nPatches and nProportion were supplied, only nPatches will be used to select survey locations.")
    nProportion <- NULL
  } else {
    #do nothing
  }
  
  #buffer polygons inward by distThreshold
  xBuffer <- st_buffer(remSensShp, dist=(-1* distThreshold))
  #remove degenerate polygons
  xBufferRemaining <- xBuffer[!st_is_empty(xBuffer),]
  xBufferRemaining <- xBufferRemaining[st_is_valid(xBufferRemaining),]
  xBufferRemaining$area <- st_area(xBufferRemaining)
  
  #calculate number of points to retain if nProportion supplied
  if(!is.null(nProportion)){
    nPatches <- nrow(remSensShp) * nProportion
  } else {
    if(nPatches > nrow(xBufferRemaining)){
      warning("nPatches exceeds number of patches with interior points greater than distThreshold from perimeter, nPatches being reduced. Change nPatches or distThreshold to avoid potential issues.")
      nPatches <- nrow(xBufferRemaining)
    } else {
      #do nothing
    }
  }
  
  #pull points
  if(sampleDesign=="random"){
    xPolySamp <- xBufferRemaining[sample(1:nrow(xBufferRemaining), nPatches, replace=FALSE), ]  
  } else { #stratified random
    #start by creating id col in dataset for matching datasets later
    xBufferRemaining$patchID <- 1:nrow(xBufferRemaining)
    
    dtEnv <- list(remSensPatch=remSensPatch)
    xDT <- data.table(xBufferRemaining)
    if(sampleAllocation=="optimum"){
      #use Neyman allocation by area to distribute points
      xDTN <- xDT[, list(N=.N,
                         sd=sd(area)), 
                  by=remSensPatch, env=dtEnv]
      xDTN[, NS := N*sd]
      xDTN[, nSamp := round(nPatches * (NS/sum(NS)))]
    } else { #proportionate sampling
      xDTN <- xDT[, .N, by=remSensPatch, env=dtEnv]
      xDTN[, nSamp := round(nPatches * (N/sum(N)))]
    }
    #check that allocated number of points does not exceed number of patches
    xDTN[, countLow := N<nSamp]
    if(sum(xDTN$countLow)>0){
      warning(paste0("Allocated number of survey points exceeds total number of patches for ", xDTN[countLow>0, remSensPatch, env=dtEnv], " reducing number of survey points to number of patches for ", xDTN[countLow>0, remSensPatch, env=dtEnv]))
      xDTN[countLow>0, nSamp := N]
    } else {
      #do nothing
    }
    #subset data by patch type
    xDTSamps <- xDT[xDTN, on=remSensPatch, {
      .SD[sample(.N, i.nSamp)]
    }, by=.EACHI]
    #pull coords from shapefile
    xPolySamp <- xBufferRemaining[xBufferRemaining$patchID %in% xDTSamps$patchID, ]  
  }
  xSamp <- st_sample(xPolySamp, size=rep(nCluster, nrow(xPolySamp)))
  xSamp <- st_as_sf(xSamp)
  xSamp <- cbind(xSamp, st_coordinates(xSamp))
  xSamp$gtID <- 1:nrow(xSamp)
  return(xSamp[, c("gtID", "X", "Y")])
}