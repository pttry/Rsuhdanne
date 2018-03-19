#' Write updated haku- file for ETLA
#'
#' Write updated haku-file in \code{\link{hakut_dir}} directory.
#' Read old one and write new one with old and new serie names by name \code{oldname_uusi}. 
#' @param file haut file name to update
#' @param freq a character. frequency of haut, see \code{\link{dload.etla}}
#' @param final a locigal. \code{FALSE} (default) print new ones separetly below old ones with holes in data. 
#' Easier to identify new ones. \code{TRUE} makes usable haku-file.
#' @export
#' @keywords IO
#' @note At the moment \code{rbind} old and new ones, and remove dublicates. 
#' Update have to be finished manually.
#' @examples \dontrun{
#'  updateHaut("KT Y.csv", "Y")}
updateHaut <- function(file, freq, final = FALSE) {
  dpath <- hakut_dir()
  sarjat <- read.csv2(file.path(dpath, file), stringsAsFactors = FALSE)
  sar <- function(x, freq1 = freq){
    list(vars = c(paste(x, ".%", sep = "")), freq = freq1, span = c(2009))  
  }
  haut <- lapply(names(sarjat), sar)
  names(haut) <- names(sarjat)
  
  haku <- dload(haut, datab = "etla")
  
  koodit <- sapply(haku, function(x) attributes(x)$desc$code)
  
  minCode <- function(x, sep = ":"){
    x <- as.character(x)
    y <- strsplit(x, split = sep)
    y <- sapply(y, function(z) z[2])
    y
  }
  
  koodit <- lapply(koodit, minCode)
  names(koodit) <- sapply(strsplit(names(koodit), "\\."), function(z) z[1])
  
  koodit_df <- suppressWarnings(as.data.frame(do.call(cbind, lapply(koodit, as.character))))
  allk <- rbind(sarjat, koodit_df)
  
  allk_clean <- sapply(allk, function(x){
    x[duplicated(x)] <- NA
    if(final){
      n <- seq_along(x)
      n[is.na(x)] <- NA
      x <- x[order(n)]
    }
    x
  })
  
  nfile <- unlist(strsplit(file, "\\."))
  nfile <- paste(nfile[1], "_uusi.", nfile[2], sep = "")
  write.csv2(allk_clean, file = file.path(dpath, nfile), row.names = FALSE, na = "")
  return(TRUE)
}



