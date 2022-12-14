% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_TyoQ_db.R
\encoding{UTF-8}
\name{make_TyoQ_1_rds}
\alias{make_TyoQ_1_rds}
\alias{make_TyoQ_2_rds}
\title{Read Tyovoimatiedustelun maakuntatauluja and write to rds-file}
\usage{
make_TyoQ_1_rds(to_file = NULL, ...)

make_TyoQ_2_rds(to_file = NULL, ...)
}
\arguments{
\item{to_file}{A path to file to write. If \code{NULL} just returns a data.frame (invisibly)}

\item{...}{parametres to \code{link{tyoQfiles}}. For now \code{ddir} for
directory of input data.}
}
\value{
(invisibly) a data.frame and write to rds-file, if given
}
\description{
Read "Maakunta1" and "Maakunta2" px-files and write to rds file.
Add Ita-Uusimaa to Uusimaa if prensent.
writes variables from for Maakunta1.
Ratios are omit because aggregating Uusimaa and Ita-Uusimaa.
}
\examples{
  \dontrun{
  tyoq_dir <- file.path(path.expand('~'), "..", 
  "Pellervon Taloustutkimus PTT ry/Data - Tiedostot/TyÃ¶markkinat/TyÃ¶voimatutkimus MK")
k <- make_TyoQ_1_rds(ddir = tyoq_dir)
k <- make_TyoQ_2_rds(ddir = tyoq_dir)
}
}
\seealso{
Other tyoQ: 
\code{\link{tyoQfiles}()}
}
\concept{tyoQ}
\keyword{IO}
