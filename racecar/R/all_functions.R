
#######################cleanSingleLap Function#####################################
cleanSingleLap <- function(file, lapNum = 1) {
  ### read data into R
  options(warn=-1)
  lapData <- read_csv(file, skip = 15)

  ### This command gets rid of the extra column
  lapData <- lapData[,-ncol(lapData)]
  
  ### extract variable name information from the csv
  nameData <-  read_csv(file, skip = 12)
  nameData <- nameData[,-ncol(nameData)]

  ### add column names to dataframe
  names(lapData) <- nameData[1,]
  
  ### add column to indicate the lap number
  lapData$Lap <- lapNum
  return(lapData)
}

#######################cleanMultiLap Function#####################################

##files must be in sequential order
cleanMultiLap <- function(file_names) {
  options(warn=-1)
  ## create an empty tibble that will be used to merge all lap data
  all_laps <- tibble()
  
  ## determine how many laps worth of data we have
  lap_numbers <- 1:length(file_names)
  
  ## use cleanSingleLap function to clean data by lap, then merge into tibble
  for (i in 1:length(file_names)) {
    temp_lap<- cleanSingleLap(file_names[i], lap_numbers[i])
    all_laps <- bind_rows(all_laps, temp_lap)
  }
  
  return(all_laps)
}

#######################braking_pattern Function#####################################
braking_pattern <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap %in% laps) %>%
    ## only look at data that is within the users specified start and end distance
    filter(Distance >= startdist & Distance <= enddist) %>%
    
    ## plot the map of the track
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    
    facet_wrap(~Lap) +
    
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
          legend.position = "top")+
    labs(x = NULL, y = NULL) +
    ## change the strip color (if facet wrap)
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
}

#######################throttle_position Function#####################################
throttle_position <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%

    filter(Lap %in% laps) %>%

    ## only look at data that is within the users specified start and end distance
    filter(Distance >= startdist & Distance <= enddist) %>%
    
    ## plot the map of the track
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    
    facet_wrap(~Lap) +
    
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
    ## change the strip color (if facet wrap)
    theme(strip.background = element_rect(fill = "#333333", color = "red"),
          strip.text = element_text(color = "red"))
}

##########################Graph that compares lap speed#####################
lapspeed <- function(data,laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  p <- data %>%
    filter(Lap %in% laps) %>%
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
  ggplotly(p)
}



################### graphs that compare RPM in different gears ######################
RPM_gear <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  p <- data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    mutate(gear_floor = floor(Calculated_Gea)) %>%
    group_by(gear_floor) %>%
    ggplot(aes(colour = PE3_TPS)) +
    scale_colour_gradientn(colours=rainbow(4))+
    geom_point(aes(x = GPS_Speed, y = PE3_RPM), size = 1)+
    facet_wrap(~gear_floor, scales = "free_y") +
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
  ggplotly(p)
  
}

################### graphs that compare RPM and speed ######################

RPM_speed <- function(data, laps = 1, startdist = min(data$Distance), enddist = max(data$Distance)){
  p1 <- data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance)) +
    geom_line(aes(y = GPS_Speed), color = "#FFFF33", size = 1) +
    ## change plot backgroud color
    theme(plot.background = element_rect(fill = "black", color = "black"))+
    ## change panel color
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "red", size = 0.1),
          panel.grid.major.x =element_blank(),
          panel.background = element_rect(fill = "black", color = "red"))+
    scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
    #scale_x_continuous(breaks = c(0, seq(startdist, enddist, 1)))+
    theme(axis.text.y = element_text(color = "red", size = 10),
          axis.text.x = element_text(color = "red", size = 10),
          axis.ticks = element_line(colour = 'red', size = 0.5),
          axis.ticks.length = unit(.1, "npc"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())+
    labs(x = NULL, y = NULL) 
  
  p2 <- data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance)) +
    geom_line(aes(y = PE3_RPM), color = "#99FFFF", size = 1) +
    
    ## change plot backgroud color
    theme(plot.background = element_rect(fill = "black", color = "black"))+
    ## change panel color
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "red", size = 0.1),
          panel.grid.major.x =element_blank(),
          panel.background = element_rect(fill = "black", color = "red"))+
    ## change axises
    scale_y_continuous(expand = c(0,0), limits = c(0, 16000)) +
    #scale_x_continuous(breaks = c(0, seq(startdist, enddist, 1))) +
    theme(axis.text.y = element_text(color = "red", size = 10),
          axis.text.x = element_text(color = "red", size = 10),
          axis.ticks = element_line(colour = 'red', size = 0.5),
          axis.ticks.length = unit(.1, "npc"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL) 
  
  subplot(p1,plotly_empty(),p2, shareX = TRUE, nrows = 2 )%>% 
    layout(margin = list(l = 100))
  
}

################### graphs that compare RPM at point around the track ######################
maprpm <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = PE3_RPM), size = 3) + facet_wrap(~Lap) +
    
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

####################### Graph that compares speed at point around the track ###########################

mapspeed <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    
    ###only look at the specified laps
    filter(Lap == laps) %>%
    
    ###only look at the specified distance range 
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    
    ###create the ggplot
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = GPS_Speed), size = 3) + facet_wrap(~Lap) +
    
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

############### Graph that plots air fuel ratio and rpm ###############################

airfuel <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    
    ###only look at specified laps
    filter(Lap == laps) %>%
    
    ###only look at specified ditance range
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    
    ###create the ggplot
    ggplot(aes( x = PE3_RPM , y = PE3_LAMBDA)) + 
    geom_point(color = "#FFFF33", size = 3) + 
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


############# Graph that plots oil pressure around track ##############################

oilpressure <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  data %>%
    
    ###only look at specified laps
    filter(Lap == laps) %>%
    
    ###only look at specified distance range
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    
    ###remove the space in the oil pressure column
    rename(oilpress = "Oil Pressure_Cal") %>%
    
    ###create the ggplot
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) + 
    geom_point(aes(color = oilpress), size = 3) +
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
