% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeTyoelakei.R
\name{makeTyoelakei}
\alias{makeTyoelakei}
\title{Make query for tyoelakeindeksi}
\usage{
makeTyoelakei(span)
}
\arguments{
\item{span}{A numeric vector for years to download c(start,end). With NULL, the default, all available years are downloaded. The end year can be omited, and all available years from start are downloaded.}
}
\description{
Uses \code{\link{getTyoelakei}} to get tyoelakeindeksi data and write to SarjatMuut/Elake.xlxs
}
\seealso{
Other tyoelakei: 
\code{\link{getTyoelakei}()}
}
\concept{tyoelakei}
\keyword{IO}
