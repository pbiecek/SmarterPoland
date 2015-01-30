\name{getEurostatRaw}
\alias{getEurostatRaw}
\title{
Download a Dataset from the Eurostat Database 
}
\description{
Download a dataset from the eurostat database. The dataset is transformed into the tabular format.
}
\usage{
getEurostatRaw(kod = "educ_iste", rowRegExp=NULL, colRegExp=NULL, 
       strip.white = TRUE)
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
  \item{strip.white}{
   Passed to the internal \code{read.table()}. By default it strips white spaces from eurostat values.
}
}
\value{
A dataset in data.frame format. First column contains names of cases. Column names usually corresponds to years.
}
\references{
Data is downloaded from \code{http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing} website.
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
