\name{SmarterPoland-package}
\alias{SmarterPoland-package}
\alias{SmarterPoland}
\docType{package}
\title{
Tools for Accessing Various Datasets Developed by the Foundation SmarterPoland.pl
}
\description{
Tools for accessing and processing datasets prepared by the Foundation SmarterPoland.pl. Among all: access to API of Google Maps, Central Statistical Office of Poland, Eurostat, WHO and other sources.
}
\details{
\tabular{ll}{
Package: \tab SmarterPoland\cr
Type: \tab Package\cr
Version: \tab 1.7\cr
Date: \tab 2016-03-21\cr
License: \tab GPL-3 \cr
}
}
\author{
Author: Przemyslaw Biecek (Foundation SmarterPoland.pl)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>
}
\references{
http://www.smarterpoland.pl
}
\keyword{ package }
\seealso{
 \code{\link{getMillwardBrown}}, 
 \code{\link{getEurostatRCV}}, 
 \code{\link{getBDLseries}}, 
 \code{\link{getWeatherForecast}}
}
\examples{
\dontrun{
 # download the dataset 'Pupil/Student - teacher ratio and average class' from eurostat
 # for more developed API see https://github.com/rOpenGov/eurostat
 tmp <- getEurostatRCV(kod = "educ_iste")
 head(tmp)
 
 # download the dataset 'People killed in road accidents' from eurostat
 # and plot a maptable for selected countries
 # for more developed API see https://github.com/rOpenGov/eurostat
 library(ggplot2)
 t1 <- getEurostatRCV("tsdtr420")
 t1 <- t1[t1$geo %in% c("UK", "SK", "FR", "PL", "ES", "PT", "LV"), ]
 ggplot(t1, aes(time, value, color=sex, group=sex)) + 
 	geom_line() + facet_wrap(~geo)
}
}
