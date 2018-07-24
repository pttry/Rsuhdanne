#' Read PC-Axis files
#'
#' The funtion to read PC-Axis px-files.
#' 
#' 
#' @param file a file or connection to read. See \code{\link{scan}}
#' @param in.codes A locigal, if \code{TRUE} also CODES are included in
#'   addition to VALUES to identify data.
#' @param org.order A locigal, if \code{TRUE} VALUES and CODES factors are
#'   given in original order. Otherwise ordering based on normal
#'   \code{\link{factor}} manner.
#' @param encoding a encoding for the \code{\link{scan}}.
#' @export
#' @return \code{data.frame}. TODO: In future with metadata attributes.
#' @author Janne Huovari
#' @references \url{http://www.scb.se/Pages/StandardNoLeftMeny____314045.aspx}
#' @keywords IO database
#' @examples
#'   \dontrun{
#'     x <- read_px("http://pxweb2.stat.fi/database/StatFin/tul/tvt/2009/010_tvt_2009_2011-02-18_tau_101_fi.px", org.order = TRUE)
#'   }
read_px <- function(file, in.codes = FALSE, org.order = FALSE, encoding = "unknown"){
  # Funktio px-tiedostojen lukemiseksi  
  px <- scan(file, what="character", sep = "\n", encoding=encoding)
  
  data.n <- match("DATA=", px)
  meta <- px[1:(data.n-1)]
  meta <- gsub(";$",";;", meta, useBytes=TRUE)
  meta.v <- paste(meta, sep=" ", collapse = "")
  meta.v <- unlist(strsplit(meta.v, ";;"))
  
  #rivinumerot arvo ja koodi nimille
  vvalues <- sort(grep("VALUES\\(", meta.v), decreasing = TRUE) #( perässä tiputtaa pois muut kuin oletuskieliset versiot.
  vcodes <- sort(grep("CODES\\(", meta.v), decreasing = TRUE)
  
  values0 <- sub("=", "!!=!!", meta.v[vvalues]) # jotta strsplit ottaa vain ensimmäisen. Voisi korvata jos saa toimimaan suoraan strsplitillä
  values0 <- strsplit(values0,"!!=!!")
  nvalues <- sapply(strsplit(sapply(values0, "[[" ,1),'\"'), "[[" ,2)
  values1 <- strsplit(sapply(values0, "[[" ,2),'\",\"')
  values1 <- sapply(values1, function(x) gsub('\"',"",x))
  names(values1) <- nvalues
  
  
  codes0 <- strsplit(meta.v[vcodes],"=")
  ncodes <- sapply(strsplit(sapply(codes0, "[[" ,1),'\"'), "[[" ,2)
  codes1 <- strsplit(sapply(codes0, "[[" ,2, simplify = "list"),'\",\"')
  codes1 <- lapply(codes1, function(x) gsub('\"',"",x))
  names(codes1) <- ncodes
  
  #sarjojen järjestys määräytyy HEADING ja STUB kenttien perusteella
  vheading <- grep("HEADING", meta.v)
  vstub <- grep("STUB", meta.v)
  s.order <- c(rev(strsplit(gsub('\"',"",strsplit(meta.v[vheading],"=")[[1]][2]),",")[[1]]), rev(strsplit(gsub('\"',"",strsplit(meta.v[vstub],"=")[[1]][2]),",")[[1]]))
  values1 <- values1[s.order]
  codes2 <- values1 # all files doesn't inculde all codes. use values instead values. next line replace with available codes
  codes2[names(codes1)] <- codes1
  
  #Datan parsiminen
  if (org.order) {
    exp.func <- function(from) factor(from, levels = unique(from))
  } else {
    exp.func<-function(from) factor(from)
  }
  values <- expand.grid(lapply(values1, exp.func)) 
  codes <- expand.grid(lapply(codes2, exp.func))
  names(codes) <- paste(names(codes), ".no", sep ="")
  
  data0 <- px[(data.n+1):length(px)]
  data1 <- strsplit(data0, " +")
  data1 <- gsub(";","",unlist(data1))
  data1 <- data1[!(data1 %in% "")]  # loppumerkki voi olla sekä kiinni numerossa että erillään. Sen takia kaksi riviä sen poistamiseksi
  data <- suppressWarnings(as.numeric(data1))
  
  if (!in.codes) {
    pxdata <- data.frame(values, data)
  }else {
    pxdata <- data.frame(codes, values, data)  
  }
  return(pxdata)
  
}
