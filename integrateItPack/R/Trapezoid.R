#' A Trapezoid object
#'
#' X and Y values, and the area under the curve, calculated using the Trapezoidal rule
#'
#' An object of the class 'Trapezoid' has the following slots:
#' \itemize{
#' \item \code{xValues} A numeric vector of x-values
#' \item \code{yValues} A numeric vector of y-values
#' \item \code{result} A numeric object of the value of the area under the curve
#' }
#'
#' @author Matt Malis: \email{malis.matt@gmail.com}
#' @aliases 
#' @rdname Trapezoid
#' @export
#'

setClass(Class = "Trapezoid", representation = representation(
  xValues = "numeric",
  yValues = "numeric",
  result = "numeric"),
  prototype = prototype(
    xValues = c(),
    yValues = c(),
    result = c()
  ))


#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, xValues, yValues, result){
            .Object@xValues<-xValues
            .Object@yValues<-yValues
            .Object@result<-result
            return(.Object)
          })