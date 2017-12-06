braking_pattern <- function(data, laps = 1, startdist = min(data$Distance), enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
  ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = BPS_Front))
}

throttle_position <- function(data, laps = 1, startdist = min(data$Distance), enddist = max(data$Distance)) {
  ggplot(data, aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = PE3_TPS))
}

