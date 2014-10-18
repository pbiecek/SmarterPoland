\name{Bank Danych Lokalnych}
\alias{getBDLtree}
\alias{getBDLsearch}
\title{
Access to the GUS Bank Danych Lokalnych with the use of API developed by MojePanstwo.
}
\description{
Download and parse data from Bank Danych Lokalnych with the use of API developed by MojePanstwo. 
}
\usage{
getBDLtree(debug = 0)
getBDLsearch(query = "", debug = 0)
}
\arguments{
  \item{debug}{
      Level of debug info. 0 for no debug, 1 or 2 for info about processed groups.
}
}
\value{
The function \code{getBDLtree()} return a data frame with identifiers of resources in Bank Danych Lokalnych.
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
}
}
\keyword{ database }
