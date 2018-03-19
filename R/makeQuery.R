#' Main function to make Rsuhdanne queries
#'
#' Main function to make Rsuhdanne queries.
#'
#' @param datab the name of the database to make query
#' @param span the time interval \code{c(start, end)} for queries if missing, \code{end} can be ommited. 
#' Will no overwrite existing \code{span}s defined in queries.
#' @export
#' @keywords IO
makeQuery <- function(datab, span = NULL){

  # 2. read query groups to update
  qgroups <- dir(file.path(hakut_dir(), datab))
  qgroups <- gsub(".r$", "", qgroups) # .r pois lopusta
  qlist <- lapply(qgroups, readQuery, datab)
  names(qlist) <- qgroups
  
  # 2b make selection
  options("guiToolkit"="tcltk")
  selectlistBox <- function (x, title, text) {
    y <- character()
    wi <- gbasicdialog(title=title, handler = function(h,...){
        assign("y", svalue(chk), envir = parent.env(environment()))
      } 
    )
    lab <- glabel(text, cont=wi)
    chk <- gcheckboxgroup(x, cont=wi)
    visible(wi, set=TRUE)
    return(y)  
  }
  sel <- selectlistBox(x = names(qlist), 
                       title = "Kansantalous - Tietokanta p?ivitys", 
                       text = "Valitse p?ivitett?v?t tietokantataulut:") 
  
  if (length(sel) == 0) stop("Ei valittuja tauluja") else qlist <- qlist[sel]
  
  # 3. make queries. return list
  
    # add spans if missing
  qlist <- lapply(qlist, function(l) lapply(l, function(x) {
    if (is.null(x[["span"]])) x[["span"]] <- span
    return(x)
  }))
    # get and remove trans
  transl <- lapply(qlist, lapply, function(l) l$trans$func)
  qlist <- lapply(qlist, function(l) lapply(l, function(x) {
    x[["trans"]] <- NULL
    return(x)
  }))
  
#   qfun <- function(item, name){
#     dload(item, datab)
#   }

  qret <- lapply(qlist, dload, datab, print.info = FALSE) ## print.info does not work

  # 4. transforms
  # JATKA KIRJOITTAMALLA TRANSFUNKTIO
  for (i in seq_along(qret)){
    for (j in seq_along(qret[[i]])){
      transf <- transl[[i]][[j]]
      for (f in transf) {
        if (!is.null(f)) qret[[i]][[j]] <- suppressWarnings(do.call(f, list(x = qret[[i]][[j]])))
      }
    }
  }

  # 5. write to file
  xl0 <- COMCreate("Excel.Application")
  for (ii in names(qret)){
    write2xlsx(tables = qret[[ii]], 
               file = file.path(tietok_dir(), paste("Sarjat", toupper(datab), sep = ""), paste(ii, "xlsx", sep = ".")),
               xl = xl0, overwrite = TRUE,
               sheetnames = names(qret[[ii]])
               )
  }
  xl0$Quit()
  return(TRUE)
}
