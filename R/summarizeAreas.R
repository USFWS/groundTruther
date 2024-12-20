#'summarizeAreas
#'
#' Summarizes the total area of reclassified patch types.
#' @param propUncert_dt A \link[data.table]{data.table} with reclassified patch types output from \link{classificatonAccuracy_propogateUncertainty} (will be named remSensPropUncert in the output from \link{classificatonAccuracy_propogateUncertainty}).
#' @param patchType The field name in \code{propUncert_dt} with the patch type of a reclassified polygon.
#' @param areaField The field name in \code{propUncert_dt} with the area of an individual polygon. Note that area of the polygons should be calculated and included as a field in the remote sensing polygon shapefile (the object supplied to \code{remSensShp]} \link{classificatonAccuracy_propogateUncertainty}) prior to propagating uncertainty.
#' @param reClassID The field name in \code{propUncert_dt} that identifies each set of patch reclassifications.
#' @param sumPlot Logical, should a boxplot summarizing total area of each patch type be produced.
#' @param remSensShp Optional. The the remote sensing polygon shapefile supplied to \code{remSensShp]} in \link{classificatonAccuracy_propogateUncertainty}. If included and \code{sumPlot=TRUE} the total area of the original patch type classifications will be included as yellow squares in the plot. 
#' @param remSensPatchField The field name that identifies the patch type of polygons in the in shapefile supplied to \code{remSensShp}. Only required if an object is supplied to \code{remSensShp}.
#' @param remSensAreaField The field name that identifies the area of polygons in the in shapefile supplied to \code{remSensShp}. Only required if an object is supplied to \code{remSensShp}.
#' @param summarizeBY The field name that identifies the variable to summarize the area of patch types by. If \code{NULL} (the default), a summary is provided for the total study area.
#' @details Note that this function assumes the area units of \code{areaField} and \code{remSensAreaField} are equal. Users must ensure area units are equal for objects supplied to \code{propUncert_dt} and \code{remSensShp}.
#' @return A \link[data.table]{data.table} summarizing the total area of patch types as the mean, median, and 0.025, 0.05, 0.25, 0.75, 0.95, and 0.975 quantiles across sets of patch reclassifications produced by \link{classificatonAccuracy_propogateUncertainty}. If \code{sumPlot=TRUE} a boxplot of summarizing the distribution of total areas of patch types across sets of patch reclassifications is also produced. 
#' @export
#' @examples
#' data(substrateLayers)
#' # Propogate uncertainty in substrate classification
#' propUncert_subset <- classificationAccuracy_propagateUncertainty(gtShp=substrateLayers[['gtDom_sf']], remSensShp=substrateLayers[['sonarClass']], crs=NULL, matchMethod="bufferedSubset",
#'                                                                  gtSampleID="samplID", gtPatch="domSub", remSensSampleID="id",
#'                                                                  remSensPatch="substrate", 
#'                                                                  nReclass=100, gpsAccuracy=5)
#' summarizeAreas(propUncert_dt=propUncert_subset$remSensPropUncert, sumPlot=TRUE, remSensShp=substrateLayers[['sonarClass']], remSensPatchField="substrate", remSensAreaField="area_m2")
#' @seealso \link{classificationAccuracy_propagUncertainty}.

summarizeAreas <- function(propUncert_dt, patchType="drawnClass", areaField="area_m2", reClassID="reClassID",
                           sumPlot=FALSE, remSensShp=NULL, remSensPatchField=NULL, remSensAreaField=NULL,
                           summarizeBy=NULL){
  propUncert_dt <- copy(propUncert_dt) #for some reason changes applied to propUncert_dt are also applied to the data.table supplied to the function, this is a hacky workaround
  
  #set up env object for running data.table operations with objects defining field names
  dtEnv <- list(patchType=patchType, areaField=areaField, reClassID=reClassID, summarizeBy=summarizeBy, remSensAreaField=remSensAreaField, remSensPatchField=remSensPatchField)
  
  if(is.null(summarizeBy)){
    simAreaByIterClass <-  propUncert_dt[!is.na(patchType), sum(areaField, na.rm=TRUE), 
                                         by=list(reClassID, patchType), 
                                         # env=list(patchType=patchType, areaField=areaField, reClassID=reClassID)
                                         env=dtEnv]
    simAreaByClass <- simAreaByIterClass[, list(meanTotalArea=mean(V1, na.rm=TRUE),
                                                medianTotalArea=median(V1, na.rm=TRUE),
                                                x0.025TotalArea=quantile(V1, 0.025, na.rm=TRUE),
                                                x0.05TotalArea=quantile(V1, 0.05, na.rm=TRUE),
                                                x0.25TotalArea=quantile(V1, 0.25, na.rm=TRUE),
                                                x0.75TotalArea=quantile(V1, 0.75, na.rm=TRUE),
                                                x0.95TotalArea=quantile(V1, 0.95, na.rm=TRUE),
                                                x0.975TotalArea=quantile(V1, 0.975, na.rm=TRUE)),
                                         by=list(patchType), 
                                         # env=list(patchType=patchType)
                                         env=dtEnv]
  } else {
    simAreaByIterClass <-  propUncert_dt[!is.na(patchType), sum(areaField, na.rm=TRUE), 
                                         by=list(reClassID, patchType, summarizeBy), 
                                         # env=list(patchType=patchType, areaField=areaField, reClassID=reClassID, summarizeBy=summarizeBy)
                                         env=dtEnv]
    simAreaByClass <- simAreaByIterClass[, list(meanTotalArea=mean(V1, na.rm=TRUE),
                                                medianTotalArea=median(V1, na.rm=TRUE),
                                                x0.025TotalArea=quantile(V1, 0.025, na.rm=TRUE),
                                                x0.05TotalArea=quantile(V1, 0.05, na.rm=TRUE),
                                                x0.25TotalArea=quantile(V1, 0.25, na.rm=TRUE),
                                                x0.75TotalArea=quantile(V1, 0.75, na.rm=TRUE),
                                                x0.95TotalArea=quantile(V1, 0.95, na.rm=TRUE),
                                                x0.975TotalArea=quantile(V1, 0.975, na.rm=TRUE)),
                                         by=list(patchType, summarizeBy), 
                                         # env=list(patchType=patchType, summarizeBy=summarizeBy)
                                         env=dtEnv]
  }
  
  if(sumPlot==TRUE){
    if(is.null(remSensShp) | is.null(remSensPatchField) | is.null(remSensAreaField)){
      p1 <- ggplot(simAreaByIterClass, aes(x=get(patchType), y=V1)) +
        geom_boxplot() +
        labs(y=areaField, x=patchType)
    } else {
      remSensShp <- data.table(remSensShp)
      
      p1 <- ggplot(simAreaByIterClass, aes(x=get(patchType), y=V1)) +
        geom_boxplot() +
        labs(y=areaField, x=patchType) +
        geom_point(data=remSensShp[, sum(remSensAreaField, na.rm=TRUE), 
                                   by=list(remSensPatchField), 
                                   # env=list(remSensAreaField=remSensAreaField, remSensPatchField=remSensPatchField)
                                   env=dtEnv], 
                   aes(x=get(remSensPatchField), y=V1), size=2, fill="yellow", shape=22)
      message("Original Classified Areas Displayed by Yellow Squares")
    }
    if(is.null(summarizeBy)){
      print(p1)
    } else {
      print(p1 + facet_grid(get(summarizeBy) ~ .))
    }
  } else {
    #do nothing
  }
  return(simAreaByClass)
}