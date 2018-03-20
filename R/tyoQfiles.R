#' Get files for read_tyoQ
#'
#' Get data files for \code{\link{make_TyoQ_1_rds}} from \code{ddir}
#'
#' @param pattern pattern in files to match
#' @param ddir folder to look for files.
#' @family tyoQ
#' @export
#' @keywords IO
#' @examples \dontrun{
#'  files1 <- tyoQfiles("Maakunta1")
#'  files2 <- tyoQfiles("Maakunta2")
#'  }
tyoQfiles <- function(pattern, ddir = "Y:/Yhteiset datat/Aluetalous/Työ/Työvoimatutkimus MK") {
  files <- dir(path = ddir, pattern = pattern, full.names = TRUE)
  files <- files[order(unlist(lapply(regmatches(files, gregexpr(pattern = "[0-9]", text = files)), paste, collapse = "")))] # sortataan numeroiden perusteella
  return(files)
}
