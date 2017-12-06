library(shiny)
library(tidyverse)

lap2 <- read_csv("lap2.csv")
lap2 = lap2 %>%
  mutate(lap =2)
lap3 <- read_csv("lap3.csv")
lap3 = lap3 %>% 
  mutate(lap = 3)
lap4 <- read_csv("lap4.csv")
lap4 = lap4 %>%
  mutate(lap = 4)
lap5 <- read_csv("lap5.csv")
lap5 = lap5 %>%
  mutate(lap = 5)
lap6 <- read_csv("lap6.csv")
lap6 = lap6 %>%
  mutate(lap=6)
lap7 <- read_csv("lap7.csv")
lap7 = lap7 %>%
  mutate(lap = 7)
lap8 <- read_csv("lap8.csv")
lap8 = lap8 %>%
  mutate(lap = 8)
lap9 <- read_csv("lap9.csv")
lap9 = lap9 %>%
  mutate(lap = 9)
lap10 <- read_csv("lap10.csv")
lap10 = lap10 %>%
  mutate(lap = 10)

laps = rbind(lap2, lap3, lap4, lap5, lap6, lap7, lap8,lap9,lap10)



ui = fluidPage(
  titlePanel("Brown FSAE"),
  sliderInput("lap", 
              label = h3("Choose Lap"),
              min = 2, 
              max = 10,
              value = 2),
  plotOutput("map"),
  plotOutput("trace")
)


server = function( input, output){
  
  output$map = renderPlot(
    laps %>%
      filter(lap == input$lap) %>%
      ggplot(aes(x = GPS_Latitude, y = GPS_Longitude)) +
      geom_point(aes(color = GPS_Speed), size = 2) +
      scale_color_gradient(low = "black", high = "red") +
      theme_dark() +
      ylab("Longitude") +
      xlab("Latitude")
  )
  output$trace = renderPlot(
    laps %>%
    ggplot(aes(x = Distance, y = lap)) + 
    geom_point(aes(color = GPS_Speed), size = 3, pch = 15) +
    geom_point(data = laps %>% filter(lap == input$lap), aes(color = GPS_Speed), size = 9, pch = 15) +
    scale_color_gradient(low = "black", high = "red") +
    ggtitle("Isaac Speed Trace") +
    theme_dark()
  )
}

shinyApp(ui = ui, server = server)
