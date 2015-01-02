\name{SmarterPoland-package}
\alias{SmarterPoland-package}
\alias{SmarterPoland}
\docType{package}
\title{
A set of tools developed by the Foundation SmarterPoland.pl
}
\description{
A set of tools developed by the Foundation SmarterPoland.pl. Among all: functions for downloading data from eurostat database ec.europa.eu/eurostat, functions for downloading data from Google Maps and from GUS (Central Statistical Office of Poland).
}
\details{
\tabular{ll}{
Package: \tab SmarterPoland\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2015-01-05\cr
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
 # download the dataset 'Pupil/Student - teacher ratio and average class size' from eurostat
 tmp <- getEurostatRCV(kod = "educ_iste")
 head(tmp)
 
 # download the dataset 'People killed in road accidents' from eurostat
 # and plot a maptable for selected countries
 t1 <- getEurostatRCV("tsdtr420")
 tmp <- cast(t1, geo ~ time , mean, subset=victim=="KIL_MIO_POP")
 tmp2 <- tmp[c(1:10,14:30),1:19]
 tmp3 <- tmp2
 rownames(tmp3) <- tmp2[,1]
 tmp3 <- tmp3[c("UK", "SK", "FR", "PL", "ES", "PT", "LV"),]
 matplot(1991:2008,t(tmp3[,-1]), type="o", pch=19, lty=1, las=1, xlab="", ylab="", yaxt="n")
}
}
