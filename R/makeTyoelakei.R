#' Make query for tyoelakeindeksi
#'
#' Uses \code{\link{getTyoelakei}} to get tyoelakeindeksi data and write to SarjatMuut/Elake.xlxs
#' @param span A numeric vector for years to download c(start,end). With NULL, the default, all available years are downloaded. The end year can be omited, and all available years from start are downloaded.
#' @export
#' @family tyoelakei
#' @import XML
#' @keywords IO
makeTyoelakei <- function(span){
  k <- getTyoelakei(span = span)
  write2xlsx(t(as.data.frame(k, row.names = as.integer(time(k)))), 
             file = file.path(tietok_dir(), "SarjatMuut", "Elake.xlsx"), sheetname = "tyoeindeksi", overwrite = TRUE)
  return(TRUE)
} 