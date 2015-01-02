\name{Bank Danych Lokalnych}
\alias{getBDLtree}
\alias{getBDLsearch}
\alias{getBDLseries}
\alias{BDLtree}
\title{
Access to the GUS Bank Danych Lokalnych with the use of API developed by MojePanstwo.
}
\description{
Download and parse data from Bank Danych Lokalnych with the use of API developed by MojePanstwo. 
}
\usage{
getBDLtree(debug = 0, raw = FALSE)
getBDLsearch(query = "", debug = 0, raw = FALSE)
getBDLseries(metric_id = "", slice = NULL, time_range = NULL,
            wojewodztwo_id = NULL, powiat_id = NULL, gmina_id = NULL,
            meta = NULL, debug = 0, raw = FALSE)
}
\arguments{
  \item{debug}{
      Level of debug info. 0 for no debug, 1 or 2 for info about processed groups.
}
  \item{raw}{
      If raw = FALSE the the resulting JSON is returned without any transformation. For raw = TRUE results are transformed into a data.frame.
}
  \item{query}{
      A query for DBL search.
}
  \item{metric_id}{
      Metric id, if unknown then look for it in DBL tree or DBL search.
}
  \item{slice}{
      A table with id dimensions, with format [1,34,*]. Use '*' to choose all dimensions (or use empty string).
}
  \item{time_range}{
      Year or range (like 2000:2010), empty means - full range.
}
  \item{wojewodztwo_id}{
      Voievodship id or '*' for all.
}
  \item{powiat_id}{
      County id of '*' for all.
}
  \item{gmina_id}{
      Subcounty id or '*' for all.
}
  \item{meta}{
      Should meta data be returned?
}
}
\value{
The function \code{getBDLtree()} returns a data frame with identifiers of resources in Bank Danych Lokalnych.
}
\references{
The API of Bank Danych Lokalnych developed by MojePanstwo is described as \code{http://mojepanstwo.pl/api/!/bdl/!/bdl/!/bdl/series}
}
\author{
Przemyslaw Biecek
}
\examples{
\dontrun{
 # the data is downloaded and parsed from Internet
 # not that this dataset is precalcualted in the package
 BDLtree <- getBDLtree(2)
 head(BDLtree)

 DBLtransport <- getBDLsearch("transport")
 head(DBLtransport)
 
 BDLseries <- getBDLseries(metric_id = 1)
 head(BDLseries)
}
}
\keyword{ database }
