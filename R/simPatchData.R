#'simPatchData
#'
#' Simulate patch classifications and ground-truthing data. Patch polygons are simulated within a rectangular grid and ground-truthing points are randomly placed within the simulated study area.
#' @param xRange Min and max x coordinates that define the horizontal boarder of a rectangular study area.
#' @param yRange Min and max y coordinates that define the vertical boarder of a rectangular study area.
#' @param cellSize X and Y dimensions of rectangular patches in the simulated study area.
#' @param crs The coordinate reference system of the simulated data. 
#' @param nPoints Number of ground-truthing points to simulate.
#' @param patches A vector of character strings identifying the patch classifications to simulate.
#' @param patchProbs A vector that defines the proportion of the study area composed by each patch type.
#' @param classProb A square matrix that defines the probability a patch type is classified as each patch type considered. Rows contain the probability of a patch classification, given the true patch type (columns). Columns must sum to 1. Note this not the same as classification accuracy, which is the probability a classification corresponds to a true patch type. 
#' @return A list with: \itemize{
#' \item{\code{remSensPoly}} {A shapefile of class \code{sf} with simulated remote sensing polygons and classified patch types (remSensPatchType). Other fields include a unique identifier for each polygon (remSensID), the area of each polygon (gridArea), grid geometry (geometry), the patch type numeric identifier (remSensPatchID), true patch type (truePatchType), and true patch identifier (truePatchID).}
#' \item{\code{gtPoints}} {A shapefile of class \code{sf} with simulated ground-truthing point locations (gtPoints) and unique identifiers for each point (gtID).}
#' \item{\code{gtRemSensJoin}} {A shapefile of class \code{sf} with joined ground-truthing and remote sensing data. Ground-truthing patch types (gtPatchType and gtPatchID for the numeric identifier) are the simulated true patch types at the location of each ground-truthing point.}
#' }  
#' @export
#' @examples
#' simPatchData(xRange=c(0, 2500), yRange=c(0, 2500), cellsize=c(16, 16), crs="EPSG:32617", nPoints=2500,
#'              patches <- c("bedrock", "boulder", "cobble", "pebble", "gravel", "sand", "silt", "other"),
#'              patchProbs <- c(0.06, 0.04, 0.14, 0.16, 0.15, 0.25, 0.2),
#'              classProb <- cbind(c(0.6,  0.01, 0.05, 0.02, 0.22, 0.05, 0.05, 0.0), #bedrock
#'                                 c(0.01, 0.48, 0.3,  0.1,  0.09, 0.01, 0.01, 0.0), #boulder
#'                                 c(0.1,  0.2,  0.38, 0.2,  0.1,  0.01, 0.01, 0.0), #cobble
#'                                 c(0.05, 0.05, 0.2,  0.35, 0.2,  0.1,  0.05, 0.0), #pebble
#'                                 c(0.15, 0.05, 0.1,  0.2,  0.25, 0.2,  0.05, 0.0), #gravel
#'                                 c(0.1,  0.05, 0.05, 0.1,  0.25, 0.35, 0.1,  0.0), #sand
#'                                 c(0.15, 0.01, 0.01, 0.01, 0.02, 0.3,  0.5,  0.0), #silt
#'                                 c(1/7,  1/7,  1/7,  1/7,  1/7,  1/7,  1/7,  0.0) #other
#'                                 )
#'              )

simPatchData <- function(xRange=c(0, 1000), yRange=c(0, 1000), cellsize = c(20, 20), crs, nPoints, patches, patchProbs, classAccu){
  stopifnot(length(patches)==length(patchProbs))
  
  stopifnot(mean(apply(classAccu, 2, sum)==1)==1)
  
  #define study area, create grid of patches and draw ground-truthing points within study area
  sfc <- st_sfc(st_polygon(list(rbind(c(xRange[1], yRange[1]), 
                                      c(xRange[2], yRange[1]), 
                                      c(xRange[2], yRange[2]), 
                                      c(xRange[1], yRange[2]), 
                                      c(xRange[1], yRange[1])))), 
                crs=crs)
  geometry <- st_make_grid(sfc, cellsize = cellsize, crs=crs)
  
  #convert locations to sf objects
  remSensPoly <- st_sf(geometry, remSensID=seq_along(geometry), gridArea=prod(cellsize))
  
  patches <- factor(patches, levels=patches)
  
  #draw true patch types
  remSensPoly$gtPatchID <- apply(rmultinom(n=nrow(remSensPoly), size=1, prob=patchProbs), 2, which.max)
  remSensPoly$gtPatchType <- patches[remSensPoly$gtPatchID]
  
  #draw patch classifications
  remSensPoly$remSensPatchID <- NA
  for(i in 1:nrow(remSensPoly)){
    remSensPoly$remSensPatchID[i] <- which.max(rmultinom(n=1, size=1, prob=classProb[, remSensPoly$gtPatchID[i]]))
  }
  remSensPoly$remSensPatchType <- patches[remSensPoly$remSensPatchID]
  
  #pull ground-truthing data
  # gtPoints <- st_sample(sfc, nPoints)
  # gtPoints <- st_sf(gtPoints, gtID=seq_along(gtPoints))
  gtSamp <- sample(1:nrow(remSensPoly), nPoints)
  gtList <- vector("list", length=nPoints)
  for(i in seq_along(gtSamp)){
    gtPts <- st_sample(remSensPoly[gtSamp[i],], 1)
    gtPts <- st_sf(gtPts, gtID=i)
    gtList[[i]] <- gtPts
  }
  gtPoints <- do.call('rbind', gtList)
  # gtPoints <- remSensPoly[gtSamp, c("remSensID", "gtPatchID", "gtPatchType")]
  # gtPoints$gtID <- 1:nPoints
  
  #subset remSensPoly to just co-located ground-truthing and classified polygons
  # gtRemSensJoin <- remSensPoly[gtSamp,]
  # gtRemSensJoin$gtID <- 1:nPoints
  
  # #conduct a spatial join to merge the ground-truthing and remote sensing data
  gtRemSensJoin <- st_join(gtPoints, remSensPoly, st_within, left=TRUE)
  
  return(list(remSensPoly=remSensPoly,
              gtPoints=gtPoints,
              gtRemSensJoin=gtRemSensJoin))
}