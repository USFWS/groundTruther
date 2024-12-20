#' propagateGPSUncertainty
#' 
#' Draws possible locations from a bivariate normal distribution with no correlation, at given sets of x and y coordinates. 
#' @param sampleID A unique ID for each set of x and y coordinates.
#' @param x A vector of x coordinates, x coordinates units must match GPS accuracy units.
#' @param y A vector of y coordinates, y coordinates units must match GPS accuracy units.
#' @param sd Standard deviation describing GPS error distribution.
#' @param nDraws Number of random draws.
#' @return A \link[data.table]{data.table} with X and Y coordinates for each GPS location (sampleID)
#' @export
#' @examples
#' x <- seq(from=10, to=100, by=10)
#' y <- seq(from=10, to=100, by=10)
#' out <- propagateGPSUncertainty(sampleID=1:10, x=x, y=y, 2.551, nDraws=100)
#' plot(X~Y, out)

propagateGPSUncertainty <- function(sampleID, x, y, sd, nDraws=1000){
  coords <- data.table(sampleID=sampleID, X=x, Y=y, sd=sd)
  pointCloud <- coords[, data.table(matrix(rmnorm(n=nDraws, mean=c(X, Y),
                                                  varcov = matrix(c(sd^2, 0, 0, sd^2),
                                                                  nrow=2)),ncol=2)),
                       by=sampleID]
  setnames(pointCloud, c("V1", "V2"), c("X", "Y"))
  return(pointCloud)
}