# NMEA package code
# Joost Kuckartz, Arcadis (joost.kuckartz@arcadis.com)
# Last modified: April 2018


#' Read NMEA files
#'
#' Read NMEA compliant files. If more than one file path is provided, 
#' they are considered continuous and will be appended in the order provided.
#' @param pathtofiles The full paths to the files.
#' @return Data.table with all related NMEA information, with each row containing the GPS
#' details for the single timestamp.
#' @export
nmea_readfiles <- function(pathtofiles) {
  loglinesfirst=NULL                                                              #keeps track of not yet fully matched lines
  logtable=NULL
  for (i in 1:length(pathtofiles)) {
    message("Log ",i," of ",length(pathtofiles),": Reading log...")
    begintime=proc.time()
    if (!file.exists(pathtofiles[i])) {
      stop(paste0("The provided file '",pathtofiles[i],"' does not exist."))
    }
    logtext=suppressWarnings(readLines(pathtofiles[i]))                           #silence the faulty lines warnings
  	if (length(logtext)!=0) {                                                     #empty log file
  	  nmeastepresults=nmea_tablestep1(logtext,loglinesfirst)
  	  loglinesfirst=nmeastepresults$firstlines
  	  logtable=rbind(logtable,nmea_tablestep2(nmeastepresults$logtable),fill=TRUE)
  	  endtime=proc.time()-begintime
  	  message("Log ",i," of ",length(pathtofiles),": Reading log finished after ",round(endtime[[3]],2)," seconds.")
  	} else {
  	  message("Log ",i," of ",length(pathtofiles),": Empty log omitted.")
  	}
  }
  return(logtable)
}

#' Parse NMEA text
#'
#' Parse a vector of strings containing NMEA lines. Each vector element is a
#' new NMEA line.
#' @param nmeatextlines The vector of strings containing NMEA lines.
#' @return Data.table with all related NMEA information, with each row containing the GPS
#' details for the single timestamp.
#' @export
nmea_parsetext <- function(nmeatextlines) {
  logtable=NULL
  if (length(nmeatextlines)) {
    nmeastepresults=nmea_tablestep1(nmeatextlines)
    logtable=nmea_tablestep2(nmeastepresults$logtable)
  }
  return(logtable)
}

#' Parse NMEA text
#'
#' Parse a vector of strings containing NMEA lines. Each vector element is a
#' new NMEA line. This function is used in loops where more than one file needs
#' to be appended.
#' @param nmeatextlines The vector of strings containing NMEA lines.
#' @param previouslines Data.table rows returned from a previous call to this function,
#' to prepend to the current call. Use the \code{$firstlines} named item. See the example 
#' for how this works.
#' @return Named list with the GPS data.table (\code{logtable}) and a data.table with the
#' last rows of the data.table that should be prepended to a next one 
#' (\code{firstlines}), or \code{NULL} if the log did not contain a full set at all.
#' @export
#' @examples
#' \dontrun{
#' prependlines = NULL
#' gpstable = NULL
#' for (file in filelist) {
#'   nmealines = readLines(file)
#'   info = nmea_parsetext_multifile(file,prependlines)
#'   prependlines = info$firstlines
#'   gpstable = rbind(gpstable,info$logtable)
#' }
#' }
nmea_parsetext_multifile <- function(nmeatextlines,previousfirstlines=NULL) {
  retval=NULL
  if (length(nmeatextlines)) {
    nmeastepresults=nmea_tablestep1(nmeatextlines,previousfirstlines)
    nmeatable=nmea_tablestep2(nmeastepresults$logtable)
    retval$logtable=nmeatable
    retval$firstlines=nmeastepresults$firstlines
  }
  return(retval)
}

#' Get possible types
#' 
#' Get a string vector of the possible NMEA line types that can be processed.
#' @return The string vector of the possible NMEA line types that can be processed.
#' @export
nmea_getpossibletypes <- function() {
  return(c("$GNRMC","$GNVTG","$GNGGA","$GNGSA","$GPGSV","$GLGSV"))  
}

#' Execute step 1 in getting to a table.
#' 
#' Internal function, do not call directly.
#' Step 1 transforms a vector string to a data.table, with prepending functionality, and returns
#' a complete set that contains all necessary grouped GPS information, plus a set that can be prepended
#' to the next call.
#' @param logtext Vector string with NMEA lines.
#' @param previouslines Data.table rows (from an earlier executed step 1 on a previous vector string)
#' to prepend to the lines of the current vector string.
#' @return Named list with the first step NMEA data.table (\code{logtable}) and a data.table with the
#' last rows of the data.table that should be prepended to a next one (last set beginning with GNRMC,
#' \code{firstlines}), or \code{NULL} if the log did not contain a full set at all.
nmea_tablestep1 <- function(logtext,previouslines=NULL) {
  retval=NULL
  nmeatable = as.data.table(tstrsplit(x=logtext, split = "[,*]", fill=NA, type.convert=FALSE,  names=TRUE))
  try(setnames(x=nmeatable,old="V1",new="TYPE"), silent = TRUE)
  nmeatable = nmeatable[TYPE %in% nmea_getpossibletypes()]                    #filter the unsupported types
  if (!is.null(previouslines)) {
    nmeatable=rbind(previouslines,nmeatable,fill=TRUE)                        #prepend the first lines coming from a previous file
  }
  nmeatable[,ROW_ID:=.I]  
  if (nrow(nmeatable[TYPE=="$GNRMC",])) {                                     #it's possible there's no RMC in a log
    nmeatable = nmeatable[ROW_ID >= min(nmeatable[TYPE=="$GNRMC",.(ROW_ID)])] #throw away first lines so that first row becomes "GNRMC"
    
    retval$firstlines = nmeatable[ROW_ID >= max(nmeatable[TYPE=="$GNRMC",.(ROW_ID)])]  #last lines, possibly needed for next log reading run
    retval$firstlines[,ROW_ID:=NULL]
    nmeatable = nmeatable[ROW_ID < max(nmeatable[TYPE=="$GNRMC",.(ROW_ID)])]  #throw away last lines so that it consists of a complete set
    
    nmeatable[, GROUP_ID:=0]
    nmeatable[TYPE=="$GNRMC", GROUP_ID:=1]
    nmeatable[, GROUP_ID := cumsum(GROUP_ID)]
    #nmeatable[, GROUPCOUNTER := seq_len(.N), by = cumsum(TYPE == "$GNRMC")]
    
    #in certain circumstances it's possible that the logtable is not of full length (GPGSV or GLGSV is all empty)
    #then we have V2 to V19 instead of V2 to V21
    cnames=paste0("V",2:21)
    add <-cnames[!cnames %in% colnames(nmeatable)]
    if(length(add)!=0) nmeatable[,(add):=NA]                                  #ensure V2 to V21 in nmeatable
    
    retval$logtable=nmeatable
  }
  return(retval)
}

#' Execute step 2 in getting to a table.
#' 
#' Internal function, do not call directly.
#' Step 2 transforms the data.table with \code{TYPE} and the other columns to the final resulting data.table.
#' @param logtable The first step NMEA data.table as returned from \code{\link{nmea_tablestep1}}.
#' @return The final data.table with GPS details.
nmea_tablestep2 <- function(logtable) {
  if (is.null(logtable)) {
    return(NULL)
  }
  if (nrow(logtable)==0) {
    return(NULL)
  }
  logtable[,ROW_ID:=NULL]                 #no need ROW_ID
  
  #split into tables per line
  cn=colnames(logtable)
  cn=cn[!cn %in% "TYPE"]
  dtGNRMC=logtable[TYPE=="$GNRMC",cn,with=FALSE]
  dtGNVTG=logtable[TYPE=="$GNVTG",cn,with=FALSE]
  dtGNGGA=logtable[TYPE=="$GNGGA",cn,with=FALSE]
  dtGNGSA=logtable[TYPE=="$GNGSA",cn,with=FALSE]
  dtGPGSV=logtable[TYPE=="$GPGSV",cn,with=FALSE]
  dtGLGSV=logtable[TYPE=="$GLGSV",cn,with=FALSE]
  
  #ensure right column names for each table
  #dtGNRMC
  # modifycolnames=paste0("V",c(2:21))
  # newcolnames=c("FIXTIME","FIX","LATITUDE","LATITUDE_NS","LONGITUDE","LONGITUDE_NS","SPEED_KTS","TRACKANGLE","FIXDATE","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21")
  # try(setnames(x=dtGNRMC,old=modifycolnames,new=newcolnames), silent = TRUE)      #rename columns
  # changecols=c("SPEED_KTS","LATITUDE","LONGITUDE","TRACKANGLE")
  # dtGNRMC[,(changecols):= lapply(.SD, as.numeric), .SDcols = changecols]          #make numeric columns
  # dtGNRMC[ ,paste0("V",c(11:21)) := NULL]                                         #remove unused columns
  
  modifycolnames=paste0("V",c(2:10))
  newcolnames=c("FIXTIME","FIX","LATITUDE","LATITUDE_NS","LONGITUDE","LONGITUDE_NS","SPEED_KTS","TRACKANGLE","FIXDATE")
  try(setnames(x=dtGNRMC,old=modifycolnames,new=newcolnames), silent = TRUE)      #rename columns
  changecols=c("SPEED_KTS","LATITUDE","LONGITUDE","TRACKANGLE")
  dtGNRMC[,(changecols):= lapply(.SD, as.numeric), .SDcols = changecols]          #make numeric columns
  dtGNRMC[ ,paste0("V",c(11:ncol(dtGNRMC))) := NULL]                              #remove unused columns
  
  #dtGNVTG
  modifycolnames=paste0("V",c(2:8))
  newcolnames=c("TRUETRACK","V3","MAGNETICTRACK","V5","V6","V7","SPEED_KMH")
  try(setnames(x=dtGNVTG,old=modifycolnames,new=newcolnames), silent = TRUE)      #rename columns
  changecols=c("TRUETRACK","MAGNETICTRACK","SPEED_KMH")
  dtGNVTG[,(changecols):= lapply(.SD, as.numeric), .SDcols = changecols]          #make numeric columns
  dtGNVTG[ ,paste0("V",c(3,5:7,9:ncol(dtGNVTG))) := NULL]                         #remove unused columns
  
  #dtGNGGA
  modifycolnames=paste0("V",c(7:12))
  newcolnames=newcolnames=c("FIXQUALITY","TRACKEDSAT","V9","ALTITUDE","V11","HEIGHTGEOID")
  try(setnames(x=dtGNGGA,old=modifycolnames,new=newcolnames), silent = TRUE)      #rename columns
  changecols=c("FIXQUALITY","TRACKEDSAT","ALTITUDE","HEIGHTGEOID")
  dtGNGGA[,(changecols):= lapply(.SD, as.numeric), .SDcols = changecols]          #make numeric columns
  dtGNGGA[ ,paste0("V",c(2:6,9,11,13:ncol(dtGNGGA))) := NULL]                     #remove unused columns
  
  #dtGNGSA
  modifycolnames=paste0("V",c(2:18))
  newcolnames=newcolnames=newcolnames=c("SELECTION","FIX3D",paste0("SAT",1:12),"PDOP","HDOP","VDOP")
  try(setnames(x=dtGNGSA,old=modifycolnames,new=newcolnames), silent = TRUE)      #rename columns
  changecols=newcolnames[2:17]
  dtGNGSA[,(changecols):= lapply(.SD, as.numeric), .SDcols = changecols]          #make numeric columns
  dtGNGSA[ ,paste0("V",c(19:ncol(dtGNGSA))) := NULL]                              #remove unused columns
  
  #extract the DOPs
  dtGNGSAdops=dtGNGSA[,.(GROUP_ID,FIX3D,PDOP,HDOP,VDOP)]
  dtGNGSAdops=dtGNGSAdops[dtGNGSAdops[,.I[1],by=GROUP_ID]$V1]
  #put all dtGNGSA in one row
  measvars=c(paste0("SAT",1:12))
  dtGNGSA=dtGNGSA[,c("GROUP_ID",measvars), with=FALSE]                            #subselection of columns
  dtGNGSA=melt(dtGNGSA, id.vars = c("GROUP_ID"),measure.vars=measvars)            #transform all columns to 1
  dtGNGSA=dtGNGSA[!is.na(value)]
  if (nrow(dtGNGSA)) {
    setorder(dtGNGSA,GROUP_ID,value,na.last = TRUE)
    dtGNGSA[, GROUPCOUNTER := seq_len(.N), by = GROUP_ID]                         #create a groupcounter
    dtGNGSAactives= dtGNGSA[, max(GROUPCOUNTER), by = GROUP_ID]                   #count the amount of active satellites for fix
    try(setnames(x=dtGNGSAactives,old="V1",new="FIXSAT"), silent = TRUE)          #rename columns
    dtGNGSA=dcast(dtGNGSA, GROUP_ID ~ GROUPCOUNTER, value.var="value")            #use groupcounter to get many columns but one row per groupid
    #rename dtGNGSA columns
    colcount=(ncol(dtGNGSA)-1)
    modifycolnames=c("GROUP_ID",1:colcount)
    newcolnames=c("GROUP_ID",paste0("SATFIX_",1:colcount))
    try(setnames(x=dtGNGSA,old=modifycolnames,new=newcolnames), silent = TRUE)    #rename columns
    dtGNGSA=merge(dtGNGSA,dtGNGSAactives,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
    dtGNGSA=merge(dtGNGSA,dtGNGSAdops,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
    rm("dtGNGSAactives","dtGNGSAdops")
  } else {                                                                        #if the complete log has no sat in view
    dtGNGSA=dtGNGSAdops
    dtGNGSA[,FIXSAT:=0]
  }
  
  satidmax=0
  #dtGPGSV
  modifycolnames=paste0("V",c(2:21))
  newcolnames=c("GSVROWS","GSVROWNR","VIEWSAT_GP",as.vector(outer(c("SATPRN_","SATELV_","SATAZI_","SATSNR_"),1:4,paste0,sep="")),"V21")
  try(setnames(x=dtGPGSV,old=modifycolnames,new=newcolnames), silent = TRUE)      #rename columns
  changecols=newcolnames[2:20]
  suppressWarnings( dtGPGSV[,(changecols):= lapply(.SD, as.numeric), .SDcols = changecols] )          #make numeric columns (there could be NA's)
  dtGPGSV[ ,V21 := NULL]                                                          #remove unused columns
  dtGPGSVviews=dtGPGSV[,c("GROUP_ID","VIEWSAT_GP"), with=FALSE]                   #subselection of columns
  dtGPGSVviews=dtGPGSVviews[, .SD[1], by=GROUP_ID]
  #put all dtGPGSV in one row
  measvars=c(paste0("SATPRN_",1:4),paste0("SATELV_",1:4),paste0("SATAZI_",1:4),paste0("SATSNR_",1:4))
  dtGPGSV=dtGPGSV[,c("GROUP_ID",measvars), with=FALSE]                            #subselection of columns
  dtGPGSV = melt(dtGPGSV, id.vars = c("GROUP_ID"),measure.vars = measvars)        #transform all columns to 1
  #ensure that the variables are sorted by PRN
  dtPRNs=dtGPGSV[variable %in% paste0("SATPRN_",1:4),]
  try(setnames(x=dtPRNs,old=c("variable","value"),new=c("varPRN","PRN")), silent = TRUE)
  dtELVs=dtGPGSV[variable %in% paste0("SATELV_",1:4),.(variable,value)]
  try(setnames(x=dtELVs,old=c("variable","value"),new=c("varELV","ELV")), silent = TRUE)
  dtAZIs=dtGPGSV[variable %in% paste0("SATAZI_",1:4),.(variable,value)]
  try(setnames(x=dtAZIs,old=c("variable","value"),new=c("varAZI","AZI")), silent = TRUE)
  dtSNRs=dtGPGSV[variable %in% paste0("SATSNR_",1:4),.(variable,value)]
  try(setnames(x=dtSNRs,old=c("variable","value"),new=c("varSNR","SNR")), silent = TRUE)
  dtvars=cbind(dtPRNs,dtELVs,dtAZIs,dtSNRs)
  setorder(dtvars,GROUP_ID,PRN,na.last = TRUE)                                    #order by PRN
  #dcast goes wrong due to GROUPCOUNTER having different values per group, need to individually dcast
  dtvars[is.na(ELV) & is.na(AZI) & is.na(SNR),PRN:=NA]                            #where the PRN is incorrectly transformed to numeric
  dtvars=dtvars[!is.na(PRN)]
  colcount=0
  if (nrow(dtvars)) {
    dtvars[, GROUPCOUNTER := seq_len(.N), by = GROUP_ID]
    dtPRNs=dtvars[,.(GROUP_ID,GROUPCOUNTER,PRN)]
    dtELVs=dtvars[,.(GROUP_ID,GROUPCOUNTER,ELV)]
    dtAZIs=dtvars[,.(GROUP_ID,GROUPCOUNTER,AZI)]
    dtSNRs=dtvars[,.(GROUP_ID,GROUPCOUNTER,SNR)]
    dtPRNs=dcast(dtPRNs, GROUP_ID ~ GROUPCOUNTER, value.var = "PRN")
    dtELVs=dcast(dtELVs, GROUP_ID ~ GROUPCOUNTER, value.var = "ELV")
    dtAZIs=dcast(dtAZIs, GROUP_ID ~ GROUPCOUNTER, value.var = "AZI")
    dtSNRs=dcast(dtSNRs, GROUP_ID ~ GROUPCOUNTER, value.var = "SNR")
    colcount=ncol(dtPRNs)-1
    try(setnames(x=dtPRNs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATPRN_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    try(setnames(x=dtELVs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATELV_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    try(setnames(x=dtAZIs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATAZI_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    try(setnames(x=dtSNRs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATSNR_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    dtELVs[,GROUP_ID:=NULL]
    dtAZIs[,GROUP_ID:=NULL]
    dtSNRs[,GROUP_ID:=NULL]
    dtGPGSV=cbind(dtPRNs,dtELVs,dtAZIs,dtSNRs)
    dtGPGSV=merge(dtGPGSVviews,dtGPGSV,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
  } else {                                                                        #if the complete log has no sat in view info
    dtGPGSV=dtGPGSVviews
  }
  rm("dtGPGSVviews","dtPRNs","dtELVs","dtAZIs","dtSNRs","dtvars")
  satidmax=colcount
  
  #dtGLGSV
  modifycolnames=paste0("V",c(2:21))
  newcolnames=c("GSVROWS","GSVROWNR","VIEWSAT_GL",as.vector(outer(c("SATPRN_","SATELV_","SATAZI_","SATSNR_"),1:4,paste0,sep="")),"V21")
  try(setnames(x=dtGLGSV,old=modifycolnames,new=newcolnames), silent = TRUE)      #rename columns
  changecols=newcolnames[2:20]
  suppressWarnings( dtGLGSV[,(changecols):= lapply(.SD, as.numeric), .SDcols = changecols] )          #make numeric columns (there could be NA's)
  dtGLGSV[ ,V21 := NULL]                                                          #remove unused columns
  dtGLGSVviews=dtGLGSV[,c("GROUP_ID","VIEWSAT_GL"), with=FALSE]                   #subselection of columns
  dtGLGSVviews=dtGLGSVviews[, .SD[1], by=GROUP_ID]
  #put all dtGLGSV in one row
  measvars=c(paste0("SATPRN_",1:4),paste0("SATELV_",1:4),paste0("SATAZI_",1:4),paste0("SATSNR_",1:4))
  dtGLGSV=dtGLGSV[,c("GROUP_ID",measvars), with=FALSE]                            #subselection of columns
  dtGLGSV = melt(dtGLGSV, id.vars = c("GROUP_ID"),measure.vars = measvars)        #transform all columns to 1
  #ensure that the variables are sorted by PRN
  dtPRNs=dtGLGSV[variable %in% paste0("SATPRN_",1:4),]
  try(setnames(x=dtPRNs,old=c("variable","value"),new=c("varPRN","PRN")), silent = TRUE)
  dtELVs=dtGLGSV[variable %in% paste0("SATELV_",1:4),.(variable,value)]
  try(setnames(x=dtELVs,old=c("variable","value"),new=c("varELV","ELV")), silent = TRUE)
  dtAZIs=dtGLGSV[variable %in% paste0("SATAZI_",1:4),.(variable,value)]
  try(setnames(x=dtAZIs,old=c("variable","value"),new=c("varAZI","AZI")), silent = TRUE)
  dtSNRs=dtGLGSV[variable %in% paste0("SATSNR_",1:4),.(variable,value)]
  try(setnames(x=dtSNRs,old=c("variable","value"),new=c("varSNR","SNR")), silent = TRUE)
  dtvars=cbind(dtPRNs,dtELVs,dtAZIs,dtSNRs)
  setorder(dtvars,GROUP_ID,PRN,na.last = TRUE)                                    #order by PRN
  #dcast goes wrong due to GROUPCOUNTER having different values per group, need to individually dcast
  dtvars[is.na(ELV) & is.na(AZI) & is.na(SNR),PRN:=NA]                            #where the PRN is incorrectly transformed to numeric
  dtvars=dtvars[!is.na(PRN)]
  if (nrow(dtvars)) {
    dtvars[, GROUPCOUNTER := seq_len(.N), by = GROUP_ID]
    dtPRNs=dtvars[,.(GROUP_ID,GROUPCOUNTER,PRN)]
    dtELVs=dtvars[,.(GROUP_ID,GROUPCOUNTER,ELV)]
    dtAZIs=dtvars[,.(GROUP_ID,GROUPCOUNTER,AZI)]
    dtSNRs=dtvars[,.(GROUP_ID,GROUPCOUNTER,SNR)]
    dtPRNs=dcast(dtPRNs, GROUP_ID ~ GROUPCOUNTER, value.var = "PRN")
    dtELVs=dcast(dtELVs, GROUP_ID ~ GROUPCOUNTER, value.var = "ELV")
    dtAZIs=dcast(dtAZIs, GROUP_ID ~ GROUPCOUNTER, value.var = "AZI")
    dtSNRs=dcast(dtSNRs, GROUP_ID ~ GROUPCOUNTER, value.var = "SNR")
    colcount=ncol(dtPRNs)-1
    try(setnames(x=dtPRNs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATPRN_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    try(setnames(x=dtELVs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATELV_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    try(setnames(x=dtAZIs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATAZI_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    try(setnames(x=dtSNRs,old=c("GROUP_ID",1:colcount),new=c("GROUP_ID",paste0("SATSNR_",(1:colcount)+satidmax))), silent = TRUE)      #rename columns
    dtELVs[,GROUP_ID:=NULL]
    dtAZIs[,GROUP_ID:=NULL]
    dtSNRs[,GROUP_ID:=NULL]
    dtGLGSV=cbind(dtPRNs,dtELVs,dtAZIs,dtSNRs)
    dtGLGSV=merge(dtGLGSVviews,dtGLGSV,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
  } else {                                                                        #if the complete log has no sat in view info
    dtGLGSV=dtGLGSVviews
  }
  rm("dtGLGSVviews","dtPRNs","dtELVs","dtAZIs","dtSNRs","dtvars")

  #join into one large table
  mergedtable=merge(dtGNRMC,dtGNVTG,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
  mergedtable=merge(mergedtable,dtGNGGA,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
  mergedtable=merge(mergedtable,dtGNGSA,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
  mergedtable=merge(mergedtable,dtGPGSV,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
  mergedtable=merge(mergedtable,dtGLGSV,by.x="GROUP_ID",by.y="GROUP_ID",all.x=TRUE,all.y=FALSE)
  mergedtable[,GROUP_ID:=NULL]
  
  #data consistency (NA in these columns are the same as zero)
  mergedtable[is.na(FIXSAT), FIXSAT:=0]
  mergedtable[is.na(TRACKEDSAT), TRACKEDSAT:=0]
  mergedtable[is.na(VIEWSAT_GP), VIEWSAT_GP:=0]
  mergedtable[is.na(VIEWSAT_GL), VIEWSAT_GP:=0]
  #transform coordinates from degrees to decimal
  coordvals=mergedtable[,LATITUDE]/100
  mergedtable[,LATITUDE := floor(coordvals) + (coordvals %% 1)*(100/60)]
  coordvals=mergedtable[,LONGITUDE]/100
  mergedtable[,LONGITUDE := floor(coordvals) + (coordvals %% 1)*(100/60)]
  
  #additional extracted information
  mergedtable[,DATETIME := as.POSIXct(paste0(FIXDATE,"T",FIXTIME),format="%d%m%yT%H%M%OS", tz="GMT")]
  mergedtable[,VIEWSAT := VIEWSAT_GP + VIEWSAT_GL]
  
  
  #order columns
  cn=colnames(mergedtable)
  gc=c("DATETIME","FIXTIME","FIXDATE","FIX","FIX3D","FIXQUALITY","FIXSAT","TRACKEDSAT",
       "VIEWSAT","VIEWSAT_GP","VIEWSAT_GL",
       "LATITUDE","LATITUDE_NS","LONGITUDE","LONGITUDE_NS","ALTITUDE",
       "SPEED_KTS","SPEED_KMH","TRACKANGLE","TRUETRACK","MAGNETICTRACK","HEIGHTGEOID",
       "PDOP","HDOP","VDOP")
  gc=c(gc,cn[str_detect(cn,fixed("SATFIX_"))])
  gc=c(gc,cn[str_detect(cn,fixed("SATPRN_"))])
  gc=c(gc,cn[str_detect(cn,fixed("SATAZI_"))])
  gc=c(gc,cn[str_detect(cn,fixed("SATELV_"))])
  gc=c(gc,cn[str_detect(cn,fixed("SATSNR_"))])
  setcolorder(mergedtable,gc)

  #ensure to return only the nmea parts where information is present
  mergedtable=mergedtable[!is.na(DATETIME)]
  return(mergedtable)
}

#' Calculate average SNR
#' 
#' Calculate the average SNR for all or a certain amount of satellites.
#' @param gpstable The data.table containing the GPS details as returned by \code{\link{nmea_readfiles}}
#' or \code{\link{nmea_parsetext}}.
#' @param sortedsatellites If an integer value is provided, it will sort the satellites it 
#' uses for determining the position by SNR descending, and calculate the average SNR of the top provided amount.
#' @return Vector with average SNR. This vector can be appended as a new column using \code{\link[data.table]{cbind}}
#' to the provided \code{gpstable}.
#' @export
#' @family calculations
nmea_calculateaveragesnr <- function(gpstable, sortedsatellites=NULL) {
  gpstable[,FRKJALPT:=.I]
  cn=colnames(gpstable)
  gc1=cn[str_detect(cn,fixed("SATFIX_"))]
  gc2=cn[str_detect(cn,fixed("SATPRN_"))]
  gc3=cn[str_detect(cn,fixed("SATSNR_"))]
  
  #extract the satellite PRNs used for position calculation
  satfix=melt(gpstable[,c("FRKJALPT",gc1),with=FALSE],id.vars = "FRKJALPT",measure.vars = gc1, value.name="PRN")
  satfix[,variable:=NULL]
  #extract the satellite PRNs and related SNR that are in view
  satviewsnr=melt(gpstable[,c("FRKJALPT",gc2,gc3),with=FALSE],measure = list(gc2,gc3), value.name=c("PRN","SNR"))
  satviewsnr[,variable:=NULL]
  #join view PRN with position PRN so we have SNR linked to position PRNs
  satviewsnr=merge(satfix,satviewsnr,by.x=c("FRKJALPT","PRN"),by.y=c("FRKJALPT","PRN"),all.x=TRUE,all.y=FALSE)
  satviewsnr=satviewsnr[!is.na(PRN)]
  #if sorting is needed, sort and only keep the top rows per group
  if (!is.null(sortedsatellites)) {
    setorder(satviewsnr,FRKJALPT,-SNR)
    satviewsnr=satviewsnr[,.SD[1:sortedsatellites], by=FRKJALPT]
  }
  avgsnr=satviewsnr[ , .(AVGSNR = mean(SNR, na.rm = TRUE)), by = FRKJALPT]$AVGSNR
  gpstable[,FRKJALPT:=NULL]               #remove the earlier added column
  return(avgsnr)
}