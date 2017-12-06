library(readr)
library(dplyr)
##install.packages("gtable")
cleanSingleLap <- function(file, lapNum) {
  lapData <- read_csv(file, skip = 15)
  
  ### This command gets rid of the extra column
  lapData <- lapData[,-ncol(lapData)]
  
  nameData <-  read_csv(file, skip = 12)
  nameData <- nameData[,-ncol(nameData)]
  
  names <- nameData[1,]
  units <- nameData[2,]
  
  combined <- c()
  for (i in 1:length(names)) {
    combined[i] <- paste0(names[i], "_",units[i])
  }  
  names(lapData) <- combined
  lapData$Lap <- lapNum
  return(lapData)
}

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

file1 <- normalizePath("~/Documents/coursework/R programming/packages1/onelapsample.csv")
file2 <- normalizePath("~/Documents/coursework/R programming/packages1/samplelap2.csv")
lap1 <- cleanSingleLap(file1, 1)
laps <- cleanMultiLap(c(file1,file2))

names(lap1)

## delete regular expressions in column names
library(stringr)
library(rebus)
rename("GPS_Latitude_\xb0", "GPS_Latitude")
newnames <- c("Time_sec", "Distance_km", "BPS_Rear_psi", "BPS_Front_psi", "Longitudinal_a_g", "Lateral_acc_g", "Vertical_acc_g",
              "Calculated_Gea_#", "Datalogger_Tem_\xb0F", "Battery_V", "PE3_RPM_rpm", "PE3_TPS_%", "PE3_FUEL_TIME_ms", 
              "PE3_IGN_ANG_deg", "PE3_BARO_mbar", "PE3_MAP_mbar", "PE3_LAMBDA_lambda", "PE3_ANA_IN1_V", "PE3_ANA_IN2_V", "PE3_ANA_IN3_V", 
              "PE3_ANA_IN4_V", "PE3_ANA_IN5_V", "PE3_ANA_IN6_V", "PE3_ANA_IN7_V", "PE3_ANA_IN8_V", "PE3_FREQ1_Hz", "PE3_FREQ2_Hz", 
              "PE3_FREQ3_Hz", "PE3_FREQ4_Hz", "PE3_BATTVOLT_V", "PE3_IAT_\xb0F", "PE3_ECT_\xb0F", "PE3_THERMIST1_\xb0F", "PE3_THERMIST2_\xb0F", 
              "GPS_Speed_mph", "GPS_Nsat_#", "GPS_LatAcc_g", "GPS_LonAcc_g", "GPS_Slope_deg","GPS_Heading_deg", "GPS_Gyro_deg/s", "GPS_Altitude_m", 
              "GPS_PosAccuracy_m", "Max_Speed_WS_NA", "GPS_MPH_Unit", "SPS_Unit", "Oil Pressure_Cal_psi", "Speed_WS_NA", "AIM_Slip_Factor_NA", "Slip Ratio_%",
              "Average_Driven_Unit", "WSRPM_Unit", "GPS_Latitude_\xb0", "GPS_Longitude_\xb0", "GPS_Elevation_cm", "Lap")


test <- str_detect(newnames, pattern = char_class("\\","#","%","/"))
print(test)
newnames1 <- str_replace_all(newnames, pattern = char_class("\\xb0","#","%","/"), replacement = "")
print(newnames1)
names(laps) <- newnames1

library(ggplot2)
#Graph that compares lap speed
lapspeed <- function(data,lap){
  data %>%
    filter(Lap == lap) %>%
    ggplot(aes(x = Distance_km, y = Lap)) +
    geom_point(aes(color = GPS_Speed_mph), size = 3, pch = 15) +
    scale_colour_gradientn(colours=rainbow(4)) +
    scale_y_continuous(breaks= c(1, seq(1,length(lap),1)))
}
lapspeed(laps,c(1,2))

#Graph of RPM vs Speed
### "gtable" method
p1 <- laps %>%
  filter(Lap == 1) %>%
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

p2 <- laps %>%
  filter(Lap == 1) %>%
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


library(gtable)
library(ggplot2)
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

# flip it horizontally
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)

# add the flipped y-axis to the right
library(grid)
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
grid.draw(g)

## graphs that compare RPM and speed
RPM_speeed <- function(data, lap){
  data %>%
    filter(Lap == lap) %>%
    mutate(gear_floor = floor(Calculated_Gea_)) %>%
    group_by(gear_floor) %>%
    ggplot(aes(colour = PE3_TPS_)) + 
    scale_colour_gradientn(colours=rainbow(4))+
    geom_point(aes(x = GPS_Speed_mph, y = PE3_RPM_rpm), size = 0.1)+
    facet_wrap(~gear_floor)
}

laps %>%
  filter(Lap == 1) %>%
  mutate(gear_floor = floor(Calculated_Gea_)) %>%
  group_by(gear_floor) %>%
  ggplot(aes(colour = PE3_TPS_)) + 
  scale_colour_gradientn(colours=rainbow(4))+
  geom_point(aes(x = GPS_Speed_mph, y = PE3_RPM_rpm), size = 0.1)+
  facet_wrap(~gear_floor)



#Graph that compares speed among driver
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
