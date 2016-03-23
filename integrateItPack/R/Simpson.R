#' A Simpson object
#'
#' X and Y values, and the area under the curve, calculated using the Simpson's rule
#'
#' An object of the class 'Simpson' has the following slots:
#' \itemize{
#' \item \code{xValues} A numeric vector of x-values
#' \item \code{yValues} A numeric vector of y-values
#' \item \code{result} A numeric object of the value of the area under the curve
#' }
#'
#' @author Matt Malis: \email{malis.matt@gmail.com}
#' @aliases 
#' @rdname Simpson
#' @export
#'

setClass(Class = "Simpson", representation = representation(
  xValues = "numeric",
  yValues = "numeric",
  result = "numeric"),
  prototype = prototype(
    xValues = c(),
    yValues = c(),
    result = c()
  ))

#' @export
setMethod("initialize", "Simpson", 
          function(.Object, xValues, yValues, result){
            .Object@xValues<-xValues
            .Object@yValues<-yValues
            .Object@result<-result
            return(.Object)
          })