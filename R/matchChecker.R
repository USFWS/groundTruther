#' matchChecker
#' 
#' A helper function to check if two vectors share the same unique values.
#' @param x A vector of values.
#' @param y A vector of values.
#' @return A logical, \code{TRUE} if the two vectors have the same set of unique values.
#' @export
#' @examples
#' dt <- data.table(V1=rep(1:10, 12), V2=sample(c("a", "b", "c"), 40, replace=TRUE))
#' dt[, matchChecker(x=V2, y=c("a", "b", "c")), by=V1]

matchChecker <- function(x, y){
  x <- unique(x)
  y <- unique(y)
  return(isTRUE(all.equal(sort(x), sort(y))))
}