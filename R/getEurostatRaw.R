getEurostatRaw <-
function(kod = "educ_iste") {
  adres <- paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=data%2F",kod,".tsv.gz",sep="")
  tfile <- tempfile()
#  download and read file
  download.file(adres, tfile)
  dat <- read.table(gzfile(tfile), sep="\t", na.strings = ": ", header=F, stringsAsFactors=F)
  unlink(tfile)
  colnames(dat) <- as.character(dat[1,])
  dat <- dat[-1,]
#  remove additional marks
  for (i in 2:ncol(dat)) {
    tmp <- sapply(strsplit(as.character(dat[,i]), split = ' '), `[`, 1)
    tmp[tmp==":"] = NA
    dat[,i] <-as.numeric(tmp)
  }
  dat
}
