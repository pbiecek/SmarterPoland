\name{getGoogleMapsAddress}
\alias{getGoogleMapsAddress}
\title{Geolocalisation with Google Maps}
\description{
Get geolocalisation (longitude, latitude) of a given adress 
with the use of Google Maps API.

The Google Maps API is used to determine the geolocalisation 
(longitude, latitude) of a given adress.
}
\usage{
getGoogleMapsAddress(street = "Banacha 2", city = "Warszawa", 
	country="Poland", positionOnly = TRUE, delay=1)
}
\arguments{
  \item{street}{  An address (street and building number)}
  \item{city}{  City}
  \item{country}{  Country}
  \item{positionOnly}{  What should be returned, vector with longitude, 
  latitude coordinates or the raw result from Google Maps API }
  \item{delay}{  Number of seconds to wait between api calls}
}
\value{
If positionOnly=TRUE then a vector with two values or a raw list from Google Maps otherwise.
}
\references{
The Google Maps API \code{https://developers.google.com/maps/}
}
\author{
Przemyslaw Biecek
}
\examples{
\dontrun{
 getGoogleMapsAddress()
}
}
\keyword{ geolocalisation }
