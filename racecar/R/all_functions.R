#######################cleanSingleLap Function#####################################
cleanSingleLap <- function(file, lapNum) {
  lapData <- read_csv(file, skip = 15)

  ### This command gets rid of the extra column
  lapData <- lapData[,-ncol(lapData)]

  nameData <-  read_csv(file, skip = 12)
  nameData <- nameData[,-ncol(nameData)]

  #  names <- nameData[1,]
  #  units <- nameData[2,]

  #  combined <- c()
  #  for (i in 1:length(names)) {
  #    combined[i] <- paste(names[i], "(", units[i], ")")
  #  }
  names(lapData) <- nameData[1,]
  lapData$Lap <- lapNum
  return(lapData)
}

#######################cleanMultiLap Function#####################################

##files must be in sequential order
cleanMultiLap <- function(file_names) {
  all_laps <- tibble()
  lap_numbers <- 1:length(file_names)
  for (i in 1:length(file_names)) {
    temp_lap<- cleanSingleLap(file_names[i], lap_numbers[i])
    all_laps <- bind_rows(all_laps, temp_lap)
  }
  return(all_laps)
}

#######################braking_pattern Function#####################################
braking_pattern <- function(data, laps, distance) {
  ggplot(data, aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = BPS_Front))
}

#######################throttle_position Function#####################################
throttle_position <- function(data, laps, distance) {
  ggplot(data, aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = PE3_TPS))
}

##########################Graph that compares lap speed#####################
lapspeed <- function(data,lap, distance_start, distance_end){
  data %>%
    filter(Lap == lap) %>%
    filter(Distance_km >= distance_start & Distance_km <= distance_end) %>%
    ggplot(aes(x = Distance_km, y = Lap)) +
    geom_point(aes(color = GPS_Speed_mph), size = 3, pch = 15) +
    scale_colour_gradientn(colours=rainbow(4)) +
    scale_y_continuous(breaks= c(1, seq(1,length(lap),1)))
}

################### graphs that compare RPM in different gears ######################
RPM_speed <- function(data, lap){
  data %>%
    filter(Lap == lap) %>%
    mutate(gear_floor = floor(Calculated_Gea_)) %>%
    group_by(gear_floor) %>%
    ggplot(aes(colour = PE3_TPS_)) + 
    scale_colour_gradientn(colours=rainbow(4))+
    geom_point(aes(x = GPS_Speed_mph, y = PE3_RPM_rpm), size = 0.1)+
    facet_wrap(~gear_floor)
}

################### graphs that compare RPM and speed ######################
### "gtable" method
RPM_speed1 <- function(data, lap, distance_start, distance_end){
  p1 <- data %>%
    filter(Lap == lap) %>%
    filter(Distance_km >= distance_start & Distance_km <= distance_end) %>%
    ggplot(aes(x = Distance_km)) +
    geom_line(aes(y = GPS_Speed_mph), color = "#CC79A7", size = 0.8) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5), 
          panel.grid.major.x =element_blank())+
    theme(panel.background = element_blank())+
    scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
    scale_x_continuous(breaks = c(0, seq(100,10000,100)))+
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.ticks = element_line(colour = 'gray50', size = 0.5),
          axis.ticks.length = unit(.25, "mph"),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_blank())+
    ggtitle("GPS_Speed\n") +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = -0.16, vjust = 2.12, colour = "#CC79A7", size = 14))
  
  p2 <- data %>%
    filter(Lap == lap) %>%
    filter(Distance_km >= distance_start & Distance_km <= distance_end) %>%
    ggplot(aes(x = Distance_km)) +
    geom_line(aes(y = PE3_RPM_rpm), color = "#00A4E6", size = 0.8) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5), 
          panel.grid.major.x =element_blank()) +
    theme(panel.background = element_blank()) +
    scale_y_continuous(expand = c(0,0), limits = c(2000, 17000)) +
    scale_x_continuous(breaks = c(0, seq(100,10000,100))) +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10), 
          axis.ticks = element_line(colour = 'gray50', size = 0.5),
          axis.ticks.length = unit(.25, "npc"),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_blank()) +
    ggtitle("RPM\n") +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = -0.16, vjust = 2.12, colour = "#00A4E6", size = 14))
  
  ## make gtable objects from ggplot objects
  ## gtable object shows how grobs are put together to form a ggplot
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  ## get the location of the panel of p1
  ## so that the panel os p2 is positioned correctly on top of it
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  print(pp)
  ## superimpose p2 (the panel) on p1
  grob_p2 <- as.list(g2$grobs[[which(g2$layout$name == "panel")]])
  
  g <- gtable_add_grob(g1, grob_p2, pp$t, pp$l, pp$b, pp$l)
  print(g)
  ## extract the y-axis of p2
  ia <- which(g2$layout$name == "axis-l")
  
  ga <- g2$grobs[[ia]]
  ax <- as.list(ga$children)[[2]]
  
  ## flip it horizontally
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  
  ## add the flipped y-axis to the right
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  plt <- grid.draw(g)
  return(plt)
}

##################Graph that compares speed among driver ###################
driver_speed <- function(data, drivers, lap){
  data%>%
    filter(driver == drivers)%>%
    group_by(Distance_km)%>%
    sum(mean(GPS_Speed_mph))%>%
    ggplot(aes(x = GPS_Latitude_, y = GPS_Longitude_)) +
    geom_point(aes(color = GPS_Speed_mph)) +
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