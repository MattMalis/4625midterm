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
#'integral1<-integrateIt(myXvals, myYvals, 'Simp')
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
          definition = function(xValues, yValues, rule){
            if(rule %in% c('Trap', 'trap', 'TRAP', 'Trapezoid', 'Trapezoidal')){
              
            }
            else if(rule %in% c('Simp', 'simp', 'SIMP', 'Simpson', 'Simpson's){
              
            }
            else return("Invalid rule. Rule should be either 'Trap' or 'Simp'.")
            
            return(new("Candidate", name=name, delegatesWon=delegatesWon,
                       party=party))#, delegatesNeeded=totalNeeded(party)))
          })

