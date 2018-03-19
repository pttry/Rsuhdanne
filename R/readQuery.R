#' @rdname writeQuery
#' @export
readQuery <- function(group, datab){
  qfile <- file.path(hakut_dir(), datab, paste(group, "r", sep = "."))
  lst <- dget(qfile)
  return(lst)
}