% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nmearead.R
\name{nmea_tablestep1}
\alias{nmea_tablestep1}
\title{Execute step 1 in getting to a table.}
\usage{
nmea_tablestep1(logtext, previouslines = NULL)
}
\arguments{
\item{logtext}{Vector string with NMEA lines.}

\item{previouslines}{Data.table rows (from an earlier executed step 1 on a previous vector string)
to prepend to the lines of the current vector string.}
}
\value{
Named list with the first step NMEA data.table (\code{logtable}) and a data.table with the
last rows of the data.table that should be prepended to a next one (last set beginning with GNRMC,
\code{firstlines}), or \code{NULL} if the log did not contain a full set at all.
}
\description{
Internal function, do not call directly.
Step 1 transforms a vector string to a data.table, with prepending functionality, and returns
a complete set that contains all necessary grouped GPS information, plus a set that can be prepended
to the next call.
}
