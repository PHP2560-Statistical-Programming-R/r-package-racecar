

mapspeed <- function(data, laps, distance) {
  
  data <- data %>% filter(lap == laps)
  
  
  
  ggplot(data, aes( x = GPS_Latitude, y = GPS_Longitude)) + 
    geom_point(aes(color = GPS_Speed)) + facet_wrap(aes(~lap))
  
}


airfuel <- function (data, laps)