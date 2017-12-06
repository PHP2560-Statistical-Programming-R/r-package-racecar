

mapspeed <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(data$Lap == laps) %>%
    filter(startdist <= data$Distance) %>%
    filter(enddist >= data$Distance) %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) + 
      geom_point(aes(color = GPS_Speed)) 
  
}


airfuel <- function (data, laps)