# Install the development package(s) found in this directory.
# Joost Kuckartz, jkuckartz1984@gmail.com
# modified: January 2021
# version: 0.2

#########
# USAGE #
#########
# 1. Source this file

rm(list=ls())                           #clear memory

###################
# INSTALL PACKAGE #
###################

#' Load a library. If it is not yet installed, it will install it.
#' @param x The library to load
loadlibrary <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("devtools")

thisfiledir=getSrcDirectory(function(x) {x})                 #only works if this file is run or sourced!
setwd(thisfiledir)
if (basename(getwd())=="R") {                                #if this directory is "R"
  if (file.exists("../DESCRIPTION")) {                       #and there is a DESCRIPTION file
    setwd("../..")                                           #we're in the package source code
  }
}

subdirs=list.dirs(getwd(),recursive=FALSE)                   #get all subdirectories
ignoredirs=substr(basename(subdirs),1,1)=="."                #certain directories need to be ignored (like .git dir)
subdirs=subdirs[!ignoredirs]
for (i in 1:length(subdirs)) {
  install(subdirs[i], local = TRUE, dependencies = TRUE, upgrade_dependencies = TRUE)  #each subdirectory is a package
}
