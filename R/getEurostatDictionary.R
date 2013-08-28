getEurostatDictionary <-
	function(dictname) {
  read.table(paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?file=dic%2Fen%2F",dictname,".dic",sep=""), sep="\t", header=F, stringsAsFactors=FALSE)
}
