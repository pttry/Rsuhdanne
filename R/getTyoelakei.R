#' Get tyoelakeindeksi
#'
#' Get tyoelakeindeksi from \url{http://tyoelakelakipalvelu.etk.fi/fi/indeksi/indeksiluvut.html}
#' @param span A numeric vector for years to download c(start,end). With NULL, the default, all available years are downloaded. The end year can be omited, and all available years from start are downloaded.
#' @export
#' @family tyoelakei
#' @import XML
#' @keywords IO
getTyoelakei <- function(span = NULL){
  u <- "http://www.saadospalvelu.fi/fi/indeksi/indeksiluvut_eri_vuosina"
  # First table
  table = readHTMLTable(u, skip.rows = c(1,2), which = 1)
  
  # Select and name colums
  table <- table[, c(1:4)]
  names(table) <- c("Vuosi", "TEL-indeksi", "Työeläkeindeksi", "Palkkakerroin")
  
  # to numeric and in right order
  rtable <- sapply(table, function(f) as.numeric(levels(f))[f])
  rtable <- rtable[order(rtable[,1]), ]
  ttable <- ts(rtable[,-1], start = rtable[1, 1])
    # selection of years  
  if (!is.null(span)){
    if (is.na(span[2])) end <- NULL else end <-span[2]
    ttable <- window(ttable, span[1], end)
  }
  return(ttable)
}