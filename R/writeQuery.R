#' Write and read a query list
#'
#' \code{writeQuery} writes a query list to the file in Hakutiedot directory. 
#' \code{readQuery} reads a queries list from the file.
#'
#' Write query lists to the file. Append existing queries. Queries can be deleted with
#' \code{query = NULL}. Uses \code{\link{dput}} and \code{\link{dget}}.
#'
#' @param qname The name of the query to write.
#' @param query a list. The query to write. \code{\link{makeQuery}} passes to \code{\link{dload}}. Could
#'              include item \code{trans} that is list of functions that perform transformation to series 
#'              before saving to file. the \code{trans} is not passed to \code{\link{dload}}.
#' @param group the name of the query group. Also name of the query-file and writen xlsx-file
#' @param datab the name of the database. \code{\link{makeQuery}} passes to \code{\link{dload}}
#' @return \code{readQuery} return list with saved queries.
#' @export
#' @keywords IO
#' @examples
#'   qu <- list(table = "namq_gdp_k",
#'                vars = list(
#'                    s_adj=c("SWDA"),
#'                    unit=c("PCH_PRE"),
#'                    geo=c("DE","EA","FI","FR","SE","UK","US","JP"),
#'                    indic_na=c("B1GM")
#'                )
#'              )
#'   writeQuery("na q", qu, "koe", "eurostat")
#'   writeQuery("na q2", qu, "koe", "eurostat")
#'   writeQuery("na q2", NULL, "koe", "eurostat")
#'   koe <- readQuery("koe", "eurostat")
#'   unlink(file.path(hakut_dir, "eurostat", "koe")) #delete test-file
writeQuery <- function(qname, query, group, datab){
  qfile <- file.path(hakut_dir(), datab, paste(group, "r", sep = "."))
  if (file.exists(qfile)){
    lst <- dget(qfile)
  } else {
    lst <- list()
  }
  lst[[qname]] <- query
  dput(lst, qfile)
}



