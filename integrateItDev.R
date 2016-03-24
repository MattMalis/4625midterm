
getwd()
setwd("/Users/iramalis/Desktop/gitstuff/4625midterm")

library(devtools)
library(roxygen2)

devtools::create("integrateItPack")


rm(list=ls())

current.code <- as.package("integrateItPack")
load_all(current.code)
document(current.code)

library(integrateItPack)
?integrateIt
?Trapezoid

## testing
xVals1<-c(180,9,5,5,25)
yVals1<-c(189,4,23,25)
integrateIt(xVals1, yVals1, 'Trap')
yVals2<-c(yVals1, 4)
integrateIt(xVals1, yVals2, 'Trap')
xVals1[4]<-6
integrateIt(xVals1, yVals2, 'Trap')
integrateIt(xVals1, yVals2, 'Simp')
