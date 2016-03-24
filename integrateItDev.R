
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

## input vectors of unequal length
xVals1<-c(180,9,5,5,25)
yVals1<-c(189,4,23,25)
integrateIt(xVals1, yVals1, 'Trap')
integrateIt(xVals1, yVals1, 'simp')

## input values of equal length, with repeated x-values
yVals2<-c(yVals1, 4)
integrateIt(xVals1, yVals2, 'Trap')
integrateIt(xVals1, yVals2, 'Simp')

## input values that should work
xVals1[4]<-6
integrateIt(xVals1, yVals2, 'Trap')
integrateIt(xVals1, yVals2, 'Simp')

## input values with odd number of sub-intervals
xVals3<-c(45,63,4,7,99,23)
yVals3<-c(24,24,6,3,7,89)
integrateIt(xVals3, yVals3, 'Simp')
## simp wont work but trap will
myTrap<-integrateIt(xVals3, yVals3, 'TRAP')
print(myTrap)
myTrap
class(myTrap)

##plotting...
plot(myTrap)


## testing plotting with super small scale
mySmallTrap<-integrateIt(xVals3/10000, yVals3/10000, 'TRAP')
print(mySmallTrap)
mySmallTrap
plot(mySmallTrap)

## plotting with negative x-values
myNegativeTrap<-integrateIt(-(xVals3), yVals3, 'TRAP')
print(myNegativeTrap)
myNegativeTrap
plot(myNegativeTrap)


## simp should work now
xVals4<-c(xVals3, 120)
yVals4<-c(yVals3, 150)
print(integrateIt(xVals4, yVals4, "Simpson's"))


