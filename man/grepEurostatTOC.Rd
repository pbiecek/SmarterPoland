\name{grepEurostatTOC}
\alias{grepEurostatTOC}
\title{
Names of Eurostat Datasets That Fit Given Pattern 
}
\description{
Lists names of dataset from eurostat with the particular pattern in the description. 

This function downloads list of all datasets available on eurostat and return list of names of datasets that contains particular pattern in the dataset description. 

E.g. all datasets related to education of teaching.
}
\usage{
grepEurostatTOC(pattern)
}
\arguments{
  \item{pattern}{
      Character, only datasets that contains this pattern in the description will be returned.
}
}
\value{
A data.frame with eight columns
\item{title}{The name of dataset of theme}
\item{code}{The codename of dataset of theme, will be used by the getEurostatRCV 
and getEurostatRaw functions.}
\item{type}{Is it a dataset, folder or table.}
\item{last.update.of.data, last.table.structure.change, data.start, data.end}{Dates.}
}
\author{
Przemyslaw Biecek
}
\seealso{
See Also as \code{\link{getEurostatRCV}}, \code{\link{getEurostatRaw}}, \code{\link{getEurostatTOC}}.
}
\examples{
\dontrun{
 tmp <- grepEurostatTOC("education")
 head(tmp)
}
}
\keyword{ database }
