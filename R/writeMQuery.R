#' Write multiple a query lists
#'
#' Help to write multiple similar query lists with \code{writeQuery}. See \code{writeQuery} for details.
#'
#' @inheritParams writeQuery 
#' @param qnames a character vector of queries to write.
#' @param multiVars a character. The name of the \code{vars} list item which will be writen to multiple queries.
#'                  The list item value should correspond \code{qnames}.
#' @return Invisibly \code{TRUE}
#' @export
#' @keywords IO
#' @examples
#'   qu <- list(table = "namq_gdp_k",
#'                vars = list(
#'                    s_adj=c("SWDA"),
#'                    unit=c("PCH_PRE"),
#'                    geo=c("DE","EA","FI","FR","SE","UK","US","JP"),
#'                    indic_na = c("b1gm", "p31_s14_s15")
#'                )
#'              )
#'   writeMQuery(qnames = c("bkt", "y kulutus"), query = qu, group = "koe", datab = "eurostat", multiVars = "indic_na")
#'   koe <- readQuery("koe", "eurostat")
#'   unlink(file.path(hakut_dir(), "eurostat", "koe")) #delete test-file
writeMQuery <- function(qnames, query, group, datab, multiVars){
  mq <- query[["vars"]][[multiVars]]
  if (length(qnames) != length(mq)) stop("qnames and ", multiVars, " should have same lenght")
  for (i in seq_along(qnames)){
    query[["vars"]][[multiVars]] <- mq[i]
    writeQuery(qnames[i], query, group, datab)
  }
  invisible(TRUE)
}