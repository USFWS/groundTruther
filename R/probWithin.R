#' probWithin
#' 
#' Estimates the probability a ground-truthing data point is within a mapped polygon.
#' @param joined A sf object connecting the ground-truthing data (with positional uncertainty propagated) and remote sensing classifications through a spatial join.
#' @param gtSampleID The name of the field containing the ground-truthing sampleID.
#' @param gtPatch The name of the field containing the ground-truthing classifications.
#' @param remSensPatch The name of the field containing the remote sensing classifications.
#' @param nDraws The number of draws used to propagate possible sample locations based on GPS accuracy. See \link{propagateGPSUncertainty}
#' @details This is a helper function for \link{classificationAccuracy_propagUncertainty}. The probability a ground-truthing point is within a patch type identified by remote sensing is calculated as the proportion realizations for a given ground-truthing that are within each patch type identified through remote sensing that the point cloud of possible ground-truthing locations overlaps.
#' @return A \link[data.table]{data.table} with the probability (prop) a ground-truth location is within a patch type classified by remote sensing for each ground-truth sample. The number of ground-truthing location draws within a polygon (N) and patch types identified at a ground-truthing location are also provided.
#' @export
#' @examples
#' data(substrateLayers)
#' withinProbs <- probWithin(joined=substrateLayers[['gtSSS']], gtSampleID="sampleID", gtPatch="gtPatch", remSensPatch="sssPatch", nDraws=100)
#' @seealso \link{propagateGPSUncertainty} and \link{classificationAccuracy_propagUncertainty}.

probWithin <- function(joined, gtSampleID, gtPatch, remSensPatch, nDraws){
  joined_dt <- data.table(joined)
  
  classCount <- na.omit(joined_dt[, .N, by=list(gtSampleID, gtPatch, remSensPatch), env=list(gtSampleID=gtSampleID, gtPatch=gtPatch, remSensPatch=remSensPatch)])
  
  #calculate "probability" a ground-truthing point falls within a substrate type
  classCount[, prob := N/nDraws][]
  
  return(classCount)
}