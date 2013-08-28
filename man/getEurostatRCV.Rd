\name{getEurostatRCV}
\alias{getEurostatRCV}
\title{
Download a dataset from the eurostat database (ec.europa.eu/eurostat).  
}
\description{
Download a dataset from the eurostat database. The dataset is transformed into the molten / row-column-value format (RCV).
}
\usage{
getEurostatRCV(kod = "educ_iste")
}
\arguments{
  \item{kod}{
   A code name for the data set of interested. See the table of contents of eurostat datasets for more details.
}
}
\value{
A dataset in the molten format with the last column 'value'. See the melt function from reshape package for more details.
}
\references{
Data is downloaded from \code{http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing} website.
}
\author{
Przemyslaw Biecek
}
\seealso{
See Also as \code{\link{getEurostatTOC}}, \code{\link{getEurostatRaw}}, \code{\link{grepEurostatTOC}}.
}
\examples{
\dontrun{
 tmp <- getEurostatRCV(kod = "educ_iste")
 head(tmp)
 
 t1 <- getEurostatRCV("tsdtr420")
 tmp <- cast(t1, geo ~ time , mean, subset=victim=="KIL_MIO_POP")
 tmp2 <- tmp[c(1:10,14:30),1:19]
 tmp3 <- tmp2
 rownames(tmp3) <- tmp2[,1]
 tmp3 <- tmp3[c("UK", "SK", "FR", "PL", "ES", "PT", "LV"),]
 matplot(1991:2008,t(tmp3[,-1]), type="o", pch=19, lty=1, las=1, xlab="", ylab="", yaxt="n")

}
}
\keyword{ database }
