braking_pattern <- function(data, laps, distance) {
  ggplot(data, aes( x = GPS_Latitude, y = GPS_Longitude)) + 
    geom_point(aes(color = BPS_Front))
}

throttle_position <- function(data, laps, distance) {
  ggplot(data, aes( x = GPS_Latitude, y = GPS_Longitude)) + 
    geom_point(aes(color = PE3_TPS))
}

