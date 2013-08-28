setEurostatTOC <-
function() {
   if (!exists(".eurostatTOC", envir = .SmarterPolandEnv)) {
   .eurostatTOC <-  read.table("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=table_of_contents_en.txt",  sep="\t", header=T,  quote="\"", fill = TRUE, comment.char="")
    assign(".eurostatTOC", .eurostatTOC, envir = .SmarterPolandEnv)
  }
  invisible(0)
}
