\name{cities_lon_lat}
\alias{cities_lon_lat}
\docType{data}
\title{Long and lat coordinated for largest cities}
\description{
A subset of world.cities{maps}. Extracted in order to shink number of dependencies. 
Only cities with pop > 50k are keept.
}
\author{
 Przemyslaw Biecek [based on world.cities]
}
\examples{
\donotrun{
	library(maps)
	data(world.cities)
	cities_lon_lat <- world.cities[!duplicated(world.cities$name),]
	rownames(cities_lon_lat) = cities_lon_lat[,1]
	cities_lon_lat <- cities_lon_lat[cities_lon_lat$pop > 50000,]
	cities_lon_lat <- cities_lon_lat[,4:5]
}
}
\keyword{datasets}
