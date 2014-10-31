\name{Dark Sky API for weather forecast}
\alias{getWeatherForecast}
\alias{getBDLtree}
\alias{getBDLtree}
\alias{getBDLsearch}
\title{
Access to weather forecasts with the use of Dark Sky API.
}
\description{
Access to hourly and daily weather forecasts with the use of Dark Sky API.
}
\usage{
getWeatherForecast(apiKey, lat = NA, lon = NA, city = NA)
}
\arguments{
  \item{apiKey}{
      You need to have Dark Sky apiKey in order to access weather forecasts. See here: https://developer.forecast.io/ hor more details.
}
  \item{lat}{
      The latitude coordinate for which prediction has to be made.
}
  \item{lon}{
      The longitude coordinate for which prediction has to be made.
}
  \item{city}{
      Instead of lat and lon you may specify name of the city for which prediction has to be made.
}
}
\value{
The function \code{getWeatherForecast()} returns list of three datasets.
now and by.hour datasets contains predictions. For each timepoint following information are collected:

time, summary, icon, precipIntensity, precipProbability, temperature, apparentTemperature, dewPoint, 
humidity, windSpeed, windBearing, visibility, cloudCover, pressure, ozone, temperatureCelsius, apparentTemperatureCelsius

Daily predictions (by.day component) contain following information:

time, summary, icon, sunriseTime, sunsetTime, moonPhase, precipIntensity, precipIntensityMax, precipProbability, temperatureMin, temperatureMinTime, temperatureMax, temperatureMaxTime, apparentTemperatureMin, apparentTemperatureMinTime, apparentTemperatureMax, apparentTemperatureMaxTime, dewPoint, humidity, windSpeed, windBearing, visibility, cloudCover, pressure, ozone, precipIntensityMaxTime, precipType, temperatureMaxCelsius, temperatureMinCelsius, apparentTemperatureMaxCelsius, apparentTemperatureMinCelsius
}
\references{
The Dark Sky API for weather forecasts is described as \code{https://developer.forecast.io/}
}
\author{
Przemyslaw Biecek
}
\examples{
\dontrun{
 # you have to have apiKey to execute these examples
library(scales)
library(ggplot2)

prognoza <- getWeatherForecast(apiKey, city='Warsaw')

ggplot(prognoza$by.hour, aes(y=temperatureCelsius, x=time)) + 
  geom_line() + geom_point() +
  geom_point(data=prognoza$now, size=10, color='red') +
  theme(axis.text=element_text(size=20),
        axis.text.x=element_text(angle=90)) + 
  scale_x_datetime(breaks = date_breaks(width = "1 hour"))

ggplot(prognoza$by.day, aes(ymin=temperatureMinCelsius, ymax=temperatureMaxCelsius, x=time)) + 
  geom_ribbon() +
  geom_point(data=prognoza$now, size=10, color='red', aes(y=temperatureCelsius, x=min(prognoza$by.day$time), ymin=0, ymax=0)) +
  theme(axis.text=element_text(size=20)) + xlab("") + ylab("temperatura w C")
}
}
\keyword{ database }
