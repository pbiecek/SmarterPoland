getEurostatRCV <-
function(kod = "educ_iste") {
  require(reshape)
  dat <- getEurostatRaw(kod)
  dat2 <- t(as.data.frame(strsplit(as.character(dat[,1]), split=",")))
  cnames <- strsplit(colnames(dat)[1], split="[,\\\\]")[[1]]
  colnames(dat2) <- cnames[-length(cnames)]
  rownames(dat2) <- dat[,1]
  rownames(dat) <- dat[,1]
  dat3 <- data.frame(dat2, dat[,-1])
  colnames(dat3) <- c(colnames(dat2), colnames(dat)[-1])
  dat4 <- melt(dat3, id=cnames[-length(cnames)])
  colnames(dat4)[ncol(dat4)-1] = cnames[length(cnames)]
  dat4
}
