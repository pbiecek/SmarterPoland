\name{getEurostatRaw}
\alias{getEurostatRaw}
\title{
Download a dataset from the eurostat database (ec.europa.eu/eurostat).  
}
\description{
Download a dataset from the eurostat database. The dataset is transformed into the tabular format.
}
\usage{
getEurostatRaw(kod = "educ_iste", rowRegExp=NULL, colRegExp=NULL)
}
\arguments{
  \item{kod}{
   A code name for the data set of interested. See the table of contents of eurostat datasets for more details.
}
  \item{rowRegExp}{
   If not NULL this regular expression will be used to filter rows out of downloaded file.
}
  \item{colRegExp}{
   If not NULL this regular expression will be used to filter collumns out of downloaded file.
}
}
\value{
A dataset in data.frame format. First column contains names of cases. Column names usually corresponds to years.
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
 tmp <- getEurostatRaw(kod = "educ_iste")
 head(tmp)
}
}
\keyword{ database }
