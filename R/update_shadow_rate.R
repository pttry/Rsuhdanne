#' Update shadow policy rates
#' 
#' download files from http://faculty.chicagobooth.edu/jing.wu/research/data/WX.html
#' and copy to "Y:/Ennuste/Taulut KT/Tietokannat/SarjatKäsin/Wu & Xia (2013) shadow rates"
#' Macrobond in-house links there
#' @export

update_shadow_rate <- function(){
  sfiles <- c(fed = "http://faculty.chicagobooth.edu/jing.wu/research/data/policyrate.xls",
              ecb = "http://faculty.chicagobooth.edu/jing.wu/research/data/shadowrate_ECB.xls",
              boe = "http://faculty.chicagobooth.edu/jing.wu/research/data/shadowrate_UK.xls")
  fpath  <- "Y:/Ennuste/Taulut KT/Tietokannat/SarjatK?sin/Wu & Xia (2013) shadow rates"
  files  <- c(fed = "Fed.xls",
              ecb = "ECB.xls",
              boe = "BoE.xls")
  lapply(names(sfiles), function(x){
    download.file(url = sfiles[x], destfile = file.path(fpath, files[x]), mode = "wb")
  })
  message("Muista paivittaa viela macrobond in-house")
}