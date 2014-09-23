#' Load and/or installs any number of standardpackages
#' 
#' Just a specific function for me that quietly
#' loads all the packages I regularly use. Able to make subsets for specific uses
#' @param which (Optional) Stuff to load. Available: 'Standard' (Default), 'GIS', 'Plot' (Default), 'Analysis', 'Diversity', 'ALL'
#' @keywords package loading
#' @export
#' @usage standardPackages(...)
#' @author Martin Jung

standardPackages <- function(which=c("Standard","Plot")) {
  if(("ALL" %in% which) | ("all" %in% which)){
    which <- c('Standard','GIS', 'Plot', 'Analysis', 'Diversity')
  }
  lF <- function(x){
    cat(paste0("Loading package ",x," \n"))
    library(as.character(x),character.only = T,warn.conflicts = F,quietly = T)
    #if(y==F) { print(paste0(x," not installed. Will install it now"));install.packages(x, dependencies = TRUE)}
  }  
  
  if("Standard" %in% which){
    lF("gdata")
    lF("car")
    lF("MASS")
    lF("plyr")
    lF("reshape2")
    lF("lubridate")
    lF("stringr")
    #lF("Hmisc")
  }
  
  if("Plot" %in% which){
    lF("ggplot2")
    lF("scales")
    lF("GGally")
    lF("lattice")
    lF("latticeExtra")
    lF("grid")
    lF("gridExtra")
    lF("coefplot2")
  }
  
  if('GIS' %in% which){
    lF("rgeos")
    lF("raster")
    lF("sp")
    lF("maptools")
    lF("rgdal")
    lF("MODISTools")
    lF("MODIS")
    lF("spatstat")  
    lF("gstat")
  }
  
  if('Analysis' %in% which){
    lF("lme4")
    lF("MuMIn")
    lF("lmerTest")
    lF("AICcmodavg")
    lF("LMERConvenienceFunctions")
    lF("varComp")
    lF("mgcv")
    lF("gamm4")
    lF("nlme")
    lF("effects")
    lF("multcomp")
  
  }
  
  if('Diversity' %in% which){
    lF("vegan")
    lF("rich")
    lF("BiodiversityR")
    lF("picante")
  }

}