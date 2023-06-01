# Make a package automatically.
# Joost Kuckartz, jkuckartz1984@hotmail.com
# modified: January 2021
# version: 0.3

#########
# USAGE #
#########
# 1. Modify variables in this file
# 2. Source this file
# 3. Edit 

rm(list=ls())                           #clear memory

#############
# VARIABLES #
#############

# To edit the package variables, edit the 'makepackagevariables.R' file.
# Keeping one file for each package is the most useful.

################
# MAKE PACKAGE #
################

#' Load a library. If it is not yet installed, it will install it.
#' @param x The library to load
loadlibrary <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dependencies=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

#' Detach all package versions based on specific name. From https://stackoverflow.com/a/6979989/2710064
#' @param pkg The package or its string name to unload
#' @param character.only Set to \code{TRUE} if the provided name is a string. Default=\code{FALSE}.
#' @examples \dontrun{
#' detach_package(vegan)
#' detach_package("vegan", TRUE)
#' }
detach_package <- function(pkg, character.only = FALSE) {
  if(!character.only) {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search()) {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

loadlibrary("devtools")
loadlibrary("roxygen2")
loadlibrary("stringr")
loadlibrary("data.table")

rm(list = c("loadlibrary"))                                  #no need for the function anymore so no conflict warning message at the end

#prepare move files settings
copydelete=TRUE
thisfiledir=getSrcDirectory(function(x) {x})                 #only works if this file is run or sourced!
setwd(thisfiledir)
if (basename(getwd())=="R") {                                #if this directory is "R"
  if (file.exists("../DESCRIPTION")) {                       #and there is a DESCRIPTION file
    setwd("../..")                                           #we already have a package directory structure
	  copydelete=FALSE
  }
}

source("makepackagevariables.R")

#prepare description information
if (length(packageauthors)==1) {
  authorstring=format(packageauthors[[1]], style="R")
} else if (length(packageauthors)> 1) {
  authorstring="c("                                         #start with list item
  for (i in 1:length(packageauthors)) {
    if (i>1) {
      authorstring=paste0(authorstring,",")                 #add comma's to add the next person to list
    }
    rformatauthor=paste0(format(packageauthors[[i]], style="R"),collapse="")  #add the person as R executable text
    authorstring=paste0(authorstring,rformatauthor)
  }
  authorstring=paste0(authorstring,")")                     #close the list
}

existingpackage=as.data.table(installed.packages())         #extract build version from package info (if exists)
existingpackage=existingpackage[Package==packagename,]
packageversion_build = 1
if (nrow(existingpackage)) {
  packageversion = existingpackage[,Version]
  packageversion = str_split(packageversion,"[.]")[[1]]
  curversionmajor=as.numeric(packageversion[1])
  if (is.na(curversionmajor)) {
    curversionmajor=0
  }
  curversionminor=as.numeric(packageversion[2])
  if (is.na(curversionminor)) {
    curversionminor=0
  }
  if (curversionmajor==packageversion_major & curversionminor==packageversion_minor) {
    packageversion_build=as.numeric(packageversion[3])
    if (is.na(packageversion_build)) {
      packageversion_build=0
    }
    packageversion_build = packageversion_build + 1
  }
}

packagedesc=list(                                           #create package description items
  Package = packagename,
  Title = packagetitle,
  Version = paste0(packageversion_major,".",packageversion_minor,".",packageversion_build),
  Date = strftime(Sys.Date(),"%Y-%m-%d"),
  `Authors@R` = authorstring,
  Description = packagedesctext,
  Depends = paste0("R (>= ", as.character(getRversion()), ")"),
  License = packagelicense,
  Encoding = "UTF-8", 
  LazyData = "true"
)

#prepare package directory and settings and create it
packagedir=paste0("./",packagename)
if (!dir.exists(packagedir)) {                              #if directory doesn't exist yet
  message(paste0("Creating package ",packagename,"..."))
  devtools::create(packagename, fields = packagedesc)
} else {
  message(paste0("Updating package ",packagename,"..."))
  chkfile=paste0(packagedir,"/DESCRIPTION")
  if (file.exists(chkfile)) {
    file.remove(chkfile)                                    #remove DESCRIPTION file to update it
  }
  chkfile=paste0(packagedir,"/NAMESPACE")
  if (file.exists(chkfile)) {
    file.remove(chkfile)                                    #remove NAMESPACE file to update it
  }
  chkfile=paste0(packagedir,"/",packagename,".Rproj")
  if (file.exists(chkfile)) {
    file.remove(chkfile)                                    #remove Rproj file to update it
  }
  
  #trick to avoid question for nesting
  challenge_nested_project <- function(path, name) return()
  
  rlang::env_unlock(env = asNamespace('usethis'))
  rlang::env_binding_unlock(env = asNamespace('usethis'))
  assign('challenge_nested_project', challenge_nested_project, envir = asNamespace('usethis'))
  rlang::env_binding_lock(env = asNamespace('usethis'))
  rlang::env_lock(asNamespace('usethis'))
  
  #create package 
  usethis::create_package(packagedir,packagedesc,open=FALSE)
  
  copydelete=FALSE
}

#move source files to right directory
if (copydelete) {
  rfiles=list.files(pattern=".R", include.dirs=FALSE)       #list R files
  locs=is.na(str_locate(rfiles,"makepackagehere")[,1])
  rfiles=rfiles[locs]                                       #avoid makepackagehere file
  locs=is.na(str_locate(rfiles,"makepackagevariables")[,1])
  rfiles=rfiles[locs]                                       #avoid makepackagevariables file
  message(paste0("Moving ",length(rfiles)," source files..."))
  curdir=getwd()
  newdir=paste0(curdir,"/",packagename,"/R")
  file.rename(paste0(curdir,"/",rfiles),paste0(newdir,"/",rfiles))    #move files to "./packagename/R" directory
}

message(paste0("Installing package ",packagename,"..."))
#create documentation and install package
setwd(paste0("./",packagename))                             #set package base folder as working directory
document()                                                  #create documentation for all R files in the folder
setwd("..")                                                 #return to original working directory
detach_package("JuicedGeneral",TRUE)                        #ensure the package is not loaded just in case.
#install(packagename, keep_source=TRUE, dependencies = TRUE) #install this package and its dependencies
install.packages(packagename, repos = NULL, dependencies = TRUE, type = "source") #install this package and its dependencies

#make pdf documentation file
message(paste0("Creating documentation pdf for package ",packagename,"..."))
if (file.exists(paste0(packagename,".pdf"))) {
  file.remove(paste0(packagename,".pdf"))
}
path <- find.package(packagename)
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))
