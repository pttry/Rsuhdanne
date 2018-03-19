#' The directory path for Tietokannat
#'
#' The directory path for Tietokannat and Hakutiedot
#' read from options \code{tietok_dir} and \code{hakut_dir} 
#' and set them if empty with defaults tietok_dir = "Y:/Ennuste/Taulut KT/Tietokannat"
#' and hakut_dir = file.path(tietok_dir, "Hakutiedot").
#' @name constants
#' @aliases tietok_dir hakut_dir
#' @keywords IO
#' @export tietok_dir hakut_dir
tietok_dir <- function(){
  if(is.null(getOption("tietok_dir"))) options(tietok_dir = "Y:/Ennuste/Taulut KT/Tietokannat")
  getOption("tietok_dir")
}
  
hakut_dir <- function(){
  if(is.null(getOption("hakut_dir"))) options(hakut_dir = file.path(tietok_dir(), "Hakutiedot"))
  getOption("hakut_dir")
}

