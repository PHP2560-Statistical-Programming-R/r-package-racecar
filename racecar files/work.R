library(readr)
library(dplyr)

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
