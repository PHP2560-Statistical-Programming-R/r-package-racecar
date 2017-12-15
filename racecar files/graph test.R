
library(gridExtra)
library(dplyr)
library(ggplot2)
library(racecar)
library(plotly)
library(readr)
file1 <- normalizePath("~/Documents/coursework/R programming/r-package-racecar/racecar files/samplelap2.csv")
file2 <- normalizePath("~/Documents/coursework/R programming/r-package-racecar/racecar files/onelapsample.csv")
lap1 <- cleanSingleLap(file1, 1)
lap2 <- cleanSingleLap(file2)
laps <- cleanMultiLap(c(file1,file2))
names(lap1)


## breaking_pattern
braking_pattern <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    ## only look at data that is within the users specified start and end distance
    filter(Distance >= startdist & Distance <= enddist) %>%
    
    ## plot the map of the track
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    
    ##add color based on value of BPS_front - indicating the brake pressure
    geom_point(aes(color = BPS_Front), size = 3) +
    
    ## change color scale
    scale_colour_gradientn(colours = heat.colors(4))+
    
    ## change the theme color
    theme(plot.background = element_rect(fill = 'black', colour = 'red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black', colour = 'red')) +
    ## change axises color
    theme(axis.text.y = element_text(size = 10, colour = 'red'),
          axis.text.x = element_text(size = 10, colour = 'red'),
          axis.line = element_line(colour = "red"),
          axis.ticks = element_line(colour = "red", size = 0.5)) +
    ## change legend color and position
    theme(legend.background = element_rect(fill = "black", color = "black"),
          legend.text = element_text(color = "red"),
          legend.title = element_text(color = "red"),
          legend.position = "top") +
    labs(x = NULL, y = NULL) +
    ## change the strip color
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
}
braking_pattern(lap1)

## RPM_gear
RPM_gear <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    mutate(gear_floor = floor(Calculated_Gea)) %>%
    group_by(gear_floor) %>%
    ggplot(aes(colour = PE3_TPS)) +
    scale_colour_gradientn(colours=rainbow(4))+
    geom_point(aes(x = GPS_Speed, y = PE3_RPM), size = 1)+
    facet_wrap(~gear_floor) +
    ## change the theme color
    theme(plot.background = element_rect(fill = 'black', colour = 'red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black', colour = 'red')) +
    ## change axises color
    theme(axis.text.y = element_text(size = 10, colour = 'red'),
          axis.text.x = element_text(size = 10, colour = 'red'),
          axis.line = element_line(colour = "red"),
          axis.ticks = element_line(colour = "red", size = 0.5)) +
    ## change legend color and position
    theme(legend.background = element_rect(fill = "black", color = "black"),
          legend.text = element_text(color = "red"),
          legend.title = element_text(color = "red"),
          legend.position = "top") +
    ## change the strip color
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
}

RPM_gear(lap1)


## throttle position
throttle_position <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    ## only look at data that is within the users specified start and end distance
    filter(Distance >= startdist & Distance <= enddist) %>%
    
    ## plot the map of the track
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    
    ##add color based on value of PE3_TPS - indicating the throttle position
    geom_point(aes(color = PE3_TPS), size = 3) +
    
    ## change color scale (n is the number of color in the palette)
    scale_colour_gradientn(colours = heat.colors(4))+
    
    ## change the theme color
    theme(plot.background = element_rect(fill = 'black', colour = 'red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black', colour = 'red')) +
    ## change axises color
    theme(axis.text.y = element_text(size = 10, colour = 'red'),
          axis.text.x = element_text(size = 10, colour = 'red'),
          axis.line = element_line(colour = "red"),
          axis.ticks = element_line(colour = "red", size = 0.5)) +
    ## change legend color and position
    theme(legend.background = element_rect(fill = "black", color = "black"),
          legend.text = element_text(color = "red"),
          legend.title = element_text(color = "red"),
          legend.position = "top") +
    labs(x = NULL, y = NULL) +
    ## change the strip color
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
}

throttle_position(lap1)


## RPM_speed
library(gridExtra)

RPM_speed(lap1)

## lapspeed
lapspeed <- function(data,laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  p <- data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance, y = Lap)) +
    geom_point(aes(color = GPS_Speed), size = 8, pch = 15) +
    scale_colour_gradientn(colours=heat.colors(4)) +
    ## change the theme color
    theme(plot.background = element_rect(fill = 'black', colour = 'red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black', colour = 'red')) +
    ## change axises color
    scale_y_continuous(breaks= c(1, seq(1,length(laps),1))) +
    theme(axis.text.y = element_text(size = 10, colour = 'red'),
          axis.text.x = element_text(size = 10, colour = 'red'),
          axis.line = element_line(colour = "red"),
          axis.ticks = element_line(colour = "red", size = 0.5)) +
    ## change legend color and position
    theme(legend.background = element_rect(fill = "black", color = "black"),
          legend.text = element_text(color = "red"),
          legend.title = element_text(color = "red"),
          legend.position = "top")
  plotly(p)
}

lapspeed(lap1)

##mapspeed
mapspeed <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = GPS_Speed)) + facet_wrap(~Lap) +
    ## change color scale
    scale_colour_gradientn(colours = heat.colors(4))+
    
    ## change the theme color
    theme(plot.background = element_rect(fill = 'black', colour = 'red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black', colour = 'red')) +
    ## change axises color
    theme(axis.text.y = element_text(size = 10, colour = 'red'),
          axis.text.x = element_text(size = 10, colour = 'red'),
          axis.line = element_line(colour = "red"),
          axis.ticks = element_line(colour = "red", size = 0.5)) +
    ## change legend color and position
    theme(legend.background = element_rect(fill = "black", color = "black"),
          legend.text = element_text(color = "red"),
          legend.title = element_text(color = "red"),
          legend.position = "top") +
    labs(x = NULL, y = NULL) +
    ## change the strip color
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
  
}
mapspeed(lap1)


##airfuel
airfuel <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = PE3_RPM , y = PE3_LAMBDA)) + 
    geom_point(color = "#FFFF33", size = 1) + 
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~Lap) +
    ## change the theme color
    theme(plot.background = element_rect(fill = 'black', colour = 'red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black', colour = 'red')) +
    ## change axises color
    theme(axis.text.y = element_text(size = 10, colour = 'red'),
          axis.text.x = element_text(size = 10, colour = 'red'),
          axis.line = element_line(colour = "red"),
          axis.ticks = element_line(colour = "red", size = 0.5)) +
    ## change legend color and position
    theme(legend.background = element_rect(fill = "black", color = "black"),
          legend.text = element_text(color = "red"),
          legend.title = element_text(color = "red"),
          legend.position = "top") +
    labs(x = NULL, y = NULL) +
    ## change the strip color
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
}
airfuel(lap1)

## oilpressure

oilpressure <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    rename(oilpress = "Oil Pressure_Cal") %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) + geom_point(aes(color = oilpress)) +
    facet_wrap(~Lap) +
    ## change color scale
    scale_colour_gradientn(colours = heat.colors(4))+
    
    ## change the theme color
    theme(plot.background = element_rect(fill = 'black', colour = 'red'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black', colour = 'red')) +
    ## change axises color
    theme(axis.text.y = element_text(size = 10, colour = 'red'),
          axis.text.x = element_text(size = 10, colour = 'red'),
          axis.line = element_line(colour = "red"),
          axis.ticks = element_line(colour = "red", size = 0.5)) +
    ## change legend color and position
    theme(legend.background = element_rect(fill = "black", color = "black"),
          legend.text = element_text(color = "red"),
          legend.title = element_text(color = "red"),
          legend.position = "top") +
    labs(x = NULL, y = NULL) +
    ## change the strip color
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
  
}
oilpressure(lap1)
