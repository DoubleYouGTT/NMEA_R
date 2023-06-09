% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMEA.R
\docType{package}
\name{NMEA-package}
\alias{NMEA}
\alias{NMEA-package}
\title{NMEA}
\description{
The NMEA package provides functions for processing (reading) NMEA compliant GPS log files.
It's functions are used to convert one or more than one log file to a data.table containing
all the NMEA information.
}
\section{Compatibility}{

The function only reads the following NMEA lines: GNRMC, GNVTG, GNGGA, GNGSA, GPGSV, and GLGSV.
Any other lines are (currently) omitted. The returned data.table is filtered for double information,
as certain information is present in multiple lines (such as for example the time of fix in
both GNRMC and GNGGA). This information will only be returned once.
}

\section{Conversion}{

As the NMEA lines are textual, the returned columns in the data.table are converted from the original
\code{character} type. Numbers will be converted to \code{numeric}. With many NMEA information containing
numbers, many of the columns are also numeric.
}

\section{NMEA column information}{

The columns of the data.table contain the following information. A lot of the NMEA information can also
be found on \link{http://www.gpsinformation.org/dale/nmea.htm}.
\describe{
\item{DATETIME}{The date and time of the NMEA information in \code{POSIXct} format, in the GMT timezone. Transformed based on the \code{FIXTIME} and \code{FIXDATE} information.}
\item{FIXTIME}{The raw time of the GNRMC line.}
\item{FIXDATE}{The raw date of the GNRMC line.}
\item{FIX}{Status information, can either be A (active) or V (void). From GNRMC line.}
\item{FIX3D}{The 3D fix information, where 1 means no fix, 2 means 2D fix, and 3 means 3D fix. From GNGSA lines.}
\item{FIXQUALITY}{Quality of fix information, ranging from 0 to 8. For general usage, 0 means no fix, 1 means GPS fix, 6 means dead reckoning. More information to be found on the information link. From GNGGA line.}
\item{FIXSAT}{Amount of satellites being used for determining the position (fix). This field is calculated by counting the amount of satellites used for determining the position, as recorded in the GNGSA lines.}
\item{TRACKEDSAT}{Amount of satellites being tracked. Note that this number can be different from \code{FIXSAT}, as for certain receivers it only counts the amount of GPS satellites (and not those from other constellations). Using \code{FIXSAT} is more reliable. From GNGGA line.}
\item{VIEWSAT}{Amount of satellites in view. This field is calculated by counting the amount of satellites in view, as recorded in the GxGSV lines.}
\item{VIEWSAT_GP}{Amount of satellites in view, for GPS. This field is calculated by counting the amount of satellites in view, as recorded in the GPGSV lines.}
\item{VIEWSAT_GL}{Amount of satellites in view, for GLONASS. This field is calculated by counting the amount of satellites in view, as recorded in the GLGSV lines.}
\item{LATITUDE}{The latitude position, in decimal format. The original NMEA format is in degrees and multiplied by 100, but the division and conversion is already performed (for example, the NMEA line shows 4807.038, which means latitude 48 deg 07.038', the returned information shows latitude 48.1173 as this is the decimal verison).}
\item{LATITUDE_NS}{}
\item{LONGITUDE}{}
\item{LONGITUDE_NS}{}
\item{ALTITUDE}{}
\item{SPEED_KTS}{}
\item{SPEED_KMH}{}
\item{TRACKANGLE}{}
\item{TRUETRACK}{}
\item{MAGNETICTRACK}{}
\item{HEIGHTGEOID}{}
\item{PDOP}{}
\item{HDOP}{}
\item{VDOP}{}
\item{SATFIX_x}{}
\item{SATPRN_x}{}
\item{SATAZI_x}{}
\item{SATELV_x}{}
\item{SATSNR_x}{}
}
}

\author{
\strong{Maintainer}: Joost Kuckartz \email{joost.kuckartz@arcadis.com} [copyright holder]

}
