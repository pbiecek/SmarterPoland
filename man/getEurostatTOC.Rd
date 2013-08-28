\name{getEurostatTOC}
\alias{getEurostatTOC}
\title{
Download a table of contents of eurostat datasets (ec.europa.eu/eurostat).
}
\description{
Download a table of contents of eurostat datasets. Note that the values in column 'code' should be used to download a selected dataset.
}
\usage{
getEurostatTOC()
}
\value{
A data.frame with eight columns
\item{title}{The name of dataset of theme}
\item{code}{The codename of dataset of theme, will be used by the getEurostatRCV and getEurostatRaw functions.}
\item{type}{Is it a dataset, folder or table.}
\item{last.update.of.data, last.table.structure.change, data.start, data.end}{Dates.}
}
\references{
The TOC is downloaded from the \code{http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=table_of_contents_en.txt}
}
\author{
Przemyslaw Biecek
}
\seealso{
See Also as \code{\link{getEurostatRCV}}, \code{\link{getEurostatRaw}}, \code{\link{grepEurostatTOC}}.
}
\examples{
\dontrun{
 tmp <- getEurostatTOC()
 head(tmp)
}
}
\keyword{ database }
