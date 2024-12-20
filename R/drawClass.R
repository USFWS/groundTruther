#' drawClass
#' 
#' Reassigns patch types from a multinomial distribution using classification 
#' accuracy estimates to define the probabilities classified patch types are one 
#' of multiple types.
#' @param classProps A matrix or list of matrices summarizing the classification 
#' accuracy of patch types as proportions or probabilities.
#' @param assignedClass The column id of the confusion matrix or classification 
#' probability matrix that corresponds to class type of a given polygon.
#' @param index Numeric, identifying the list element to pass to 
#' \code{rmultinom}. If provided, \code{classProps} must be a list.
#' @details This is a helper function for 
#' \link{classificationAccuracy_propagUncertainty}.
#' @return  A numeric identifying a reclassified patch type.
#' @import data.table
#' @export
#' @examples 
#' data(substrateLayers)
#' # Determine which classified patch types a ground-truthing sample is most likely to overlap
#' withinProbs <- probWithin(joined=substrateLayers[['gtSSS']], gtSampleID="sampleID", gtPatch="gtPatch", remSensPatch="sssPatch", nDraws=100)
#' # Pull the highest probability matches for each gtSampleID
#' matchedSamples <- withinProbs[withinProbs[, .I[which.max(prob)], by=sampleID]$V1]
#' # Confusion matrix summarizing classification accuracy, called within the data.table syntax
#' confusionMat <- matchedSamples[, confusionMatrix(sssPatch, gtPatch)]
#' # Convert confusion matrix classification counts to proportions, assumes proportions are informative probabilities
#' classProps <- as.matrix(confusionMat$table/apply(confusionMat$table, 1, sum)) 
#' # Convert patch type to numeric, used to identify row in classProps with classification probabilities for a given patch type
#' matchedSamples[, patchNum := as.numeric(sssPatch)]
#' # Create rowID to run drawClass function iteratively
#' matchedSamples[, rowID := seq_along(sampleID)]
#' 
#' # Reclassify single sample
#' drawClass(classProps=classProps, assignedClass=matchedSamples[1, patchNum])
#' 
#' # Reclassify entire data set
#' # Reassign patch type based on assumed classification probabilities
#' matchedSamples[, drawnClassID := drawClass(classProps=classProps, assignedClass=patchNum), by=rowID][]
#' # Convert drawnClassID to patch type
#' matchedSamples[, drawnClass := levels(sssPatch)[drawnClassID]][]
#' @seealso \link{propagateGPSUncertainty} and \link{classificationAccuracy_propagUncertainty}.

drawClass <- function(classProps, assignedClass, index=NULL){
  if(is.null(index)){
    stopifnot(is.matrix(classProps) & is.null(index))
  } else {
    stopifnot(is.list(classProps) & is.numeric(index))
    classProps <- classProps[[index]]
  }
  
  if(is.na(sum(classProps[assignedClass, ]))){
    classDraw <- NA
  } else {
    drawClass <- rmultinom(1, 1, classProps[assignedClass, ])
    classDraw <- which.max(drawClass)
  }
  return(classDraw)
}