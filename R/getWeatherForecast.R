addWeatherVariables <- function(df) {
  df$temperatureCelsius <- 5*(df$temperature-32)/9
  df$apparentTemperatureCelsius <- 5*(df$apparentTemperature-32)/9
  df$time <-  as.POSIXct(df$time, origin = "1970-01-01", tz = df$timezone)  
  df
}
addWeatherDailyVariables <- function(df) {
  df$temperatureMaxCelsius <- 5*(df$temperatureMax-32)/9
  df$temperatureMinCelsius <- 5*(df$temperatureMin-32)/9
  df$apparentTemperatureMaxCelsius <- 5*(df$apparentTemperatureMax-32)/9
  df$apparentTemperatureMinCelsius <- 5*(df$apparentTemperatureMin-32)/9
  df$temperatureMinTime <-  as.POSIXct(df$temperatureMinTime, origin = "1970-01-01", tz = df$timezone)  
  df$temperatureMaxTime <-  as.POSIXct(df$temperatureMaxTime, origin = "1970-01-01", tz = df$timezone)  
  df$apparentTemperatureMin <-  as.POSIXct(df$apparentTemperatureMin, origin = "1970-01-01", tz = df$timezone)  
  df$apparentTemperatureMax <-  as.POSIXct(df$apparentTemperatureMax, origin = "1970-01-01", tz = df$timezone)  
  df$time <-  as.POSIXct(df$time, origin = "1970-01-01", tz = df$timezone)  
  df
}


getWeatherForecast <- function(apiKey, lat = NA, lon = NA, city = NA) {
  if (!is.na(city)) {
    data(world.cities)
    cityInfo <- world.cities[world.cities$name == city,]
    forecast <- GET(paste0("https://api.forecast.io/forecast/",apiKey,"/",cityInfo$lat, ",", cityInfo$long))
  } else {
    if (!is.na(lat) & !is.na(long)) {
      forecast <- GET(paste0("https://api.forecast.io/forecast/",apiKey,"/",lat, ",", long))
    } else {
      error("You have to specify city or lat/lon")
    }
  }
  
  forecastJson <- rjson::fromJSON(rawToChar(forecast$content), method = "C")
  
  now = addWeatherVariables(as.data.frame(forecastJson$currently))
  by.hour = addWeatherVariables(forecastJson$hourly$data)
  by.day= addWeatherDailyVariables(forecastJson$daily$data)
  
  list(now = now, by.hour = by.hour, by.day = by.day)  
}

