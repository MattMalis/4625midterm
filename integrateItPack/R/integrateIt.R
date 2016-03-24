#'Integrating a Trapezoid or a Simpson
#'
#'Creates an object of class Trapezoid or Simpson with user-provided inputs
#'
#'@param xValues A numeric vector of x-values
#'@param yValues A numeric vector of y-values
#'@param rule An instruction of whether to calculate the 'result' value using the Trapezoidal rule or Simpson's rule; either 'Trap' or 'Simp' (default to 'Trap')
#'
#'@return an object of class Trapezoid or Simpson containing
#' \item{xValues}{A numeric vector of x-values}
#' \item{yValues}{A numeric vector of y-values}
#' \item{result}{A numeric object of the value of the area under the curve}
#'@author Matt Malis \email{malis.matt@gmail.com}
#'@note This function calculates the area under a curve, two different ways!
#'@examples 
#'
#'myIntegral<-integrateIt(myXvals, myYvals, 'Simp')
#'@seealso \code{\link{Trapezoid}} \code{\link{Simpson}}  
#'@rdname integrateIt
#'@aliases integrateIt,ANY-method 
#'@export
setGeneric(name = "integrateIt",
           def = function(xValues, yValues, rule){
             standardGeneric("integrateIt")
           })
#'@export
setMethod(f = "integrateIt",
          definition = function(xValues, yValues, rule='Trap'){
            if(rule %in% c('Trap', 'trap', 'TRAP', 'Trapezoid', 'Trapezoidal')){
              ## create Trapezoid with result=0, so we can run validity checks before doing the work of calculating result
              newTrap<-new("Trapezoid", xValues=xValues, yValues=yValues, result=0)
              
              ## sorting xValues, and ensuring that yValues is sorted so as to maintain the original x-y relationship
              xSorted<-sort(xValues)
              ySorted<-NULL
              ySorted<-sapply(xSorted, function(val){ #moving elementwise through xSorted...
                x_i<-which(xValues==val, arr.ind=T) #extracting the xValues index that corresponds to the given element of xSorted...
                return(yValues[x_i]) #extracting the value at that index in yValues, storing in ySorted
              })
              
              ## assign sorted values to xValues and yValues slots of newTrap
              newTrap@xValues<-xSorted
              newTrap@yValues<-ySorted
              
              ## applying Trapezoidal rule to the values now sorted by xValues
              h <- (xSorted[length(xSorted)]-xSorted[1]) /(length(xSorted)-1)
       
              ## coefficients: (1,2,2,...,2,1)
              coeffs<-rep(2, length(xSorted))
              coeffs[1]<-1
              coeffs[length(xSorted)]<-1
              
              
              ## area = (h/2)(sorted y-values * coefficients)
              area<-(h/2)*sum(ySorted*coeffs)
              
              ## assign area to the result slot of the Trapezoid object
              newTrap@result<-area
     
              return(newTrap)
            }
            
            else if(rule %in% c('Simp', 'simp', 'SIMP', 'Simpson', "Simpson's")){
              ## create Simpson with result=0, so we can run validity checks before doing the work of calculating result
              newSimp<-new("Simpson", xValues=xValues, yValues=yValues, result=0)
              ## sorting xValues, and ensuring that yValues is sorted so as to maintain the original x-y relationship
              xSorted<-sort(xValues)
              ySorted<-NULL
              ySorted<-sapply(xSorted, function(val){ #moving elementwise through xSorted...
                x_i<-which(xValues==val, arr.ind=T) #extracting the xValues index that corresponds to the given element of xSorted...
                return(yValues[x_i]) #extracting the value at that index in yValues, storing in ySorted
              })
              
              ## assigning sorted vectors to xValues and yValues slots of newSimp
              newSimp@xValues<-xSorted
              newSimp@yValues<-ySorted
              
              ## applying Simpon's rule to the sorted values
              ## h=(b-a)/n
              h<-(xSorted[length(xSorted)] - xSorted[1]) / (length(xSorted)-1)
              ## coefficients: (1,4,2,4,2,4,...4,2,4,1)
              coeffs<-rep(NA, length(xSorted))
              coeffs[1]<-1
              coeffs[length(xSorted)]<-1
              coeffs[-c(1, length(xSorted))]<-sapply(2:(length(xSorted)-1), function(num){
                return(2*(1+(1+num)%%2))
              })
              ## area = (h/3)(sorted y-values * coefficients)
              area<-(h/3)*sum(ySorted*coeffs)
              ## assign area to the result slot of the Simpson object
              newSimp@result<-area
              return(newSimp)
            }
            else return("Invalid rule. Rule should be either 'Trap' or 'Simp'.")
          })

