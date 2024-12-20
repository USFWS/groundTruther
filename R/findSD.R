#' findSD
#' 
#' Finds the scale of a Rayleigh distribution, given the distance root mean square error of GPS accuracy. The distance root mean square error defines the distance from a true location where defined proportion of the data points occur (typically 0.95). The scale of the Rayleigh distribution is equivalent to a bivariate normal distribution with no correlation (analogous to two independent normal distributions). 
#' @param gpsAccuracy Magnitude of the horizontal radius that encompasses a defined proportion of the true locations (typically 0.95), this is also referred to as distance root mean square error.
#' @param accuracyLevel The proportion of the true locations contained within a distance equal to \code{gpsAccuracy} from a measured GPS point.
#' @return The standard deviation of the GPS error assuming a bivariate normal distribution with no correlation, equivalent to the scale of a Rayleigh distribution.
#' @export
#' @examples
#' findSD(gpsAccuracy=2.5, accuracyLevel=0.9)

findSD <-  function(gpsAccuracy=5, accuracyLevel=0.95){
  stopifnot(gpsAccuracy>0)
  stopifnot(between(accuracyLevel, lower=0, upper=1, incbounds = FALSE))
  
  sig <- gpsAccuracy/sqrt(-2 * log(1-accuracyLevel))
  
  return(sig)
}
