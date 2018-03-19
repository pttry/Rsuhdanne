#' Read Tyovoimatiedustelun maakuntatauluja
#'
#' read files based on their vars list. Add Itä-Uusimaa to Uusimaa if prensent
#'
#' @param files A vector of files to read
#' @param vars a list of vars lists for \code{\link{dload.tk}}
#' @param year a numeric, a year for time-series to start
#'
#' @family tyoQ
#' @export
#' @keywords IO
read_tyoQ <- function(files, vars, year){
  getData <- function(f, v){
      d <- dload(vars = v, datab = "tk", url = f,
               timev = c("Vuosi", "Ajanjakso"),
               ldrop = list(Ajanjakso = "Vuosikeskiarvo")
            )
      d
  }
  data1 <- lapply(seq_along(files), function(i) getData(files[i], vars[[i]]))

  #Itä-Uusimaan yhdistäminen Uuteenmaahan, jos on
  data1 <- lapply(data1, function(x){
    if (any(grepl("Itä-Uusimaa", colnames(x)))){
      attributes(x)$desc$name <- attributes(x)$desc$name[grep("Itä-Uusimaa", attributes(x)$desc$name[[1]], invert = TRUE), ,drop =FALSE]
      attrib <-  attributes(x)
      x[, grep("^Uusimaa", colnames(x))] <- x[, grep("^Uusimaa", colnames(x))] + x[, grep("Itä-Uusimaa", colnames(x))]
      x <- x[, -grep("Itä-Uusimaa", colnames(x))]
      attributes(x)$class <- attrib$class
      attributes(x)$desc <- attrib$desc
      attributes(x)$meta <- attrib$meta
    }
    x
  })
  data <- ts(data1[[1]], 1998, ceiling(max(unlist(lapply(data1, time)))), 4)
  data[] <- NA
  for (x in data1){
    data[match(time(x),time(data)),] <- x
  }
  data <- window(data, year)
  tn <- time(data)
  datad <- t(as.data.frame(data))
  colnames(datad) <- tn
  rownames(datad) <- attributes(data1[[1]])$desc$name[[1]]

  return(datad)
}
