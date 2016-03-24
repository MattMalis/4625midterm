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
    
  )
  
)

## setting validity for class "Trapezoid"
setValidity("Trapezoid", function(object){
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
}
)

#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, xValues, yValues, result){
            .Object@xValues<-xValues
            .Object@yValues<-yValues
            .Object@result<-result
            validObject(.Object)
            return(.Object)
          })


#' @export
print.Trapezoid<-function(trap){
  paste("Integrated value, according to Trapezoidal rule, is: ", round(trap@result,3))
}

#' @export
plot.Trapezoid<-function(trap){

    plot(NULL, xlim=c((min(trap@xValues)-(max(trap@xValues)-min(trap@xValues))/10),  
                    (max(trap@xValues)+(max(trap@xValues)-min(trap@xValues))/10)), 
       ## setting xlim and ylim as a function of the min and max x and y values
       ylim=c((min(trap@yValues)-(max(trap@yValues)-min(trap@yValues))/10),  
              (max(trap@yValues)+(max(trap@yValues)-min(trap@yValues))/10)),
       xlab = "x-values", ylab="y-values", main="Integral Approximation with Trapezoidal Rule"
       )
  ## plotting points
  points(trap@xValues, trap@yValues, pch=21)    
  ## plotting outer polygon
  polygon(c(trap@xValues[1],trap@xValues,trap@xValues[length(trap@xValues)]),
          c(0,trap@yValues,0))
  ## separating individual trapezoids with vertical lines
  for(i in 2:(length(trap@xValues)-1)){
    segments(trap@xValues[i],trap@yValues[i],trap@xValues[i],0)
  }
  
}

