#'Integrating a Trapezoid or a Simpson
#'
#'Creates an object of class Trapezoid or Simpson with user-provided inputs
#'
#'@param xValues A numeric vector of x-values
#'@param yValues A numeric vector of y-values
#'@param result A numeric object of the value of the area under curve
#'@param rule An instruction of whether to calculate the 'result' value using the Trapezoidal rule or Simpson's rule
#'
#'@return an object of class Trapezoid or Simpson containing
#' \item{name}{The candidate's name}
#' \item{delegatesWon}{Number of delegates the candidate has won}
#' \item{party}{The candidate's party}
#' \item{delegatesNeeded}{Number of delegates needed for the candidate's party nomination}
#'@author Matt Malis \email{malis.matt@gmail.com}
#'@note This function creates a Candidate!
#'@examples 
#'
#'cruzCandidate<-createCandidate("Ted Cruz", 135, "Republican")
#'@seealso \code{\link{totalNeeded}} \code{\link{propNeeded}}  
#'@rdname createCandidate
#'@aliases createCandidate,ANY-method 
#'@export
setGeneric(name = "createCandidate",
           def = function(name, delegatesWon, party){
             standardGeneric("createCandidate")
           })
#'@export
setMethod(f = "createCandidate",
          definition = function(name, delegatesWon, party){
            return(new("Candidate", name=name, delegatesWon=delegatesWon,
                       party=party))#, delegatesNeeded=totalNeeded(party)))
          })

