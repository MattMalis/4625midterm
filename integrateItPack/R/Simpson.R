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


## setting validity for class "Simpson"
setValidity("Simpson", function(object){
  # is xValues a numeric vector
  test1<-is.numeric(object@xValues)
  if(!test1){return ("xValues is not numeric")}
  # is yValues a numeric vector
  test2<-is.numeric(object@yValues)
  if(!test2){return ("yValues is not numeric")}
  # are xValues and yValues the same length
  test3<-(length(object@xValues)==length(object@yValues))
  if(!test3){return("xValues and yValues are not same length")}
  # are there any repeated xValues
  test4<-(length(object@xValues)==length(unique(object@xValues)))
  if(!test4){return("There cannot be repeated xValues")}
  # is result a single numeric value
  test5<-(is.numeric(object@result) & length(object@result)==1)
  if(!test5){return("result needs to be a single numeric value")}
  # is there an even, non-zero number of subintervals? (i.e. is length of xValues odd?)
  # ...because number of subintervals = length(xValues)-1
  test6<-(length(object@xValues)%%2==1 & length(object@xValues)>2)
  if(!test6){return("Simpson's rule requires an even number of subintervals")}
})


#' @export
setMethod("initialize", "Simpson", 
          function(.Object, xValues, yValues, result){
            .Object@xValues<-xValues
            .Object@yValues<-yValues
            .Object@result<-result
            validObject(.Object)
            return(.Object)
          })