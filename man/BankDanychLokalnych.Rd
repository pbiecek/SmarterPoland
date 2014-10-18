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
      Id wskaźnika, do którego mają należeć serie  query	integer.
}
  \item{slice}{
      Tablica id wymiarów, dla których przecięcia zostaną zwrócone dane. Format: [1,34,*]. Uzyj gwiazdki, aby zwrócić serie dla wszystkich punktów wymiaru. Brak argumentów oznacza wszystkie możliwe przecięcia.	query	string.
}
  \item{time_range}{
      Podaj rok lub zakres (np. 2000:2010), z którego zostaną zwrócone dane. Brak argumentu oznacza pełen dostępny przedział.	query	string.
}
  \item{wojewodztwo_id}{
      Id województwa, dla którego zostaną zwrócone dane lub * dla wszystkich	query	integer.
}
  \item{powiat_id}{
      Id powiatu, dla którego zostaną zwrócone dane lub * dla wszystkich	query	integer.
}
  \item{gmina_id}{
      Id gminy, dla którego zostaną zwrócone dane lub * dla wszystkich	query	integer.
}
  \item{meta}{
      Czy zwrać metadane odpowiedzi? Wartości: 0/1-domyślne.
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
 
 BDLseries <- getBDLseries(metric_id = 1)
 head(BDLseries)
}
}
\keyword{ database }
