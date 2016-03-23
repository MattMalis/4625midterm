
getwd()
setwd("/Users/iramalis/Desktop/gitstuff/4625midterm")

library(devtools)
library(roxygen2)

devtools::create("integrateItPack")


rm(list=ls())

current.code <- as.package("integrateItPack")
load_all(current.code)
document(current.code)
