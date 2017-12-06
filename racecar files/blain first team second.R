

mapspeed <- function(data) {
  
  ggplot(data, aes( x = GPS_Latitude, y = GPS_Longitude)) + 
    geom_point(aes(color = GPS_Speed))
  
}

