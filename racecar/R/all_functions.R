
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
    ## only look at data that is within the users specified start and end distance
    filter(Distance >= startdist & Distance <= enddist) %>%
    
    ## plot the map of the track
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    
    ##add color based on value of BPS_front - indicating the brake pressure
    geom_point(aes(color = BPS_Front)) +
    
    ## change the theme color
    theme(panel.backgroud = theme_rect(fill = "black"),
            panel.grid.major = element_blank,
            panel.grid.minor = element_blank,
            plot.background = theme_rect(color = "black"))
}

#######################throttle_position Function#####################################
throttle_position <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    ## only look at data that is within the users specified start and end distance
    filter(Distance >= startdist & Distance <= enddist) %>%
    
    ## plot the map of the track
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    
    ##add color based on value of PE3_TPS - indicating the throttle position
    geom_point(aes(color = PE3_TPS))
}

##########################Graph that compares lap speed#####################
lapspeed <- function(data,laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance, y = Lap)) +
    geom_point(aes(color = GPS_Speed), size = 3, pch = 15) +
    scale_colour_gradientn(colours=rainbow(4)) +
    scale_y_continuous(breaks= c(1, seq(1,length(laps),1)))+
    ## change the theme color
    theme(panel.backgroud = element_blank,
            panel.grid.major = element_blank,
            panel.grid.minor = element_blank,
            plot.background = theme_rect(color = "black"))
}

################### graphs that compare RPM in different gears ######################
RPM_gear <- function(data, laps = 1, startdist = min(data$Distance), enddist = max(data$Distance)){
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist)%>%
    mutate(gear_floor = floor(Calculated_Gea)) %>%
    group_by(gear_floor) %>%
    ggplot(aes(colour = PE3_TPS)) +
    scale_colour_gradientn(colours=rainbow(4))+
    geom_point(aes(x = GPS_Speed, y = PE3_RPM), size = 0.1)+
    facet_wrap(~gear_floor)
}

################### graphs that compare RPM and speed ######################
### "gtable" method, lap argument can only be a number not vactor

RPM_speed <- function(data, laps = 1, startdist = min(data$Distance), enddist = max(data$Distance)){
  p1 <- data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance)) +
    geom_line(aes(y = GPS_Speed), color = "#0033FF", size = 0.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x =element_blank())+
    theme(panel.background = element_blank())+
    scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
    scale_x_continuous(breaks = c(0, seq(startdist, enddist, 1)))+
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.ticks = element_line(colour = 'gray50', size = 0.5),
          axis.ticks.length = unit(.25, "npc"),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_blank())+
    ggtitle("GPS_Speed\n") +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = - 0.2, vjust = 2.12, colour = "#0033FF", size = 12))

  p2 <- data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance)) +
    geom_line(aes(y = PE3_RPM), color = "#FF3333", size = 0.5) +
    ggtitle("RPM\n") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.major.x =element_blank()) +
    theme(panel.background = element_blank()) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 16000)) +
    scale_x_continuous(breaks = c(0, seq(startdist, enddist, 1))) +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.ticks = element_line(colour = 'gray50', size = 0.5),
          axis.ticks.length = unit(.25, "npc"),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = 0.85, vjust = 2.12, colour = "#FF3333", size = 12))

  grid.arrange(p1,p2)
  
}

##################Graph that compares speed among driver ###################
driver_speed <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  data %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    group_by(Distance)%>%
    sum(mean(GPS_Speed))%>%
    ggplot(aes(x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = GPS_Speed)) +
    ylab("Longitude") +
    xlab("Latitude")
}

####################### Graph that compares speed at point around the track ###########################

mapspeed <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = GPS_Speed)) 

}

############### Graph that plots air fuel ratio and rpm ###############################

airfuel <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = PE3_RPM , y = PE3_LAMBDA)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
}


############# Graph that plots oil pressure around track ##############################

oilpressure <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    rename(oilpress = "Oil Pressure_Cal") %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) + geom_point(aes(color = oilpress)) 
}
