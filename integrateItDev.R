
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
integrateIt(xVals1, yVals1, 'simp')

yVals2<-c(yVals1, 4)
integrateIt(xVals1, yVals2, 'Trap')
integrateIt(xVals1, yVals2, 'Simp')

xVals1[4]<-6
integrateIt(xVals1, yVals2, 'Trap')
integrateIt(xVals1, yVals2, 'Simp')


xVals3<-c(45,63,4,7,99,23)
yVals3<-c(24,24,6,3,7,89)
integrateIt(xVals3, yVals3, 'TRAP')
integrateIt(xVals3, yVals3, 'Simp')

xVals4<-c(xVals3, 100)
yVals4<-c(yVals3, 150)
integrateIt(xVals4, yVals4, "Simpson's")
