library(shiny)

###requires install.packages("shinythemes")

ui <- fluidPage(theme = shinytheme("darkly"),
  
  fluidRow(column(width = 4,
                  tags$img(src = "logo.jpg")), 
           column(width = 6, 
                  tags$h1("raceR", style = "color:red;"))),

  fluidRow(column(width = 6,
                  fileInput("upload1", label = h4("Upload .csv here"))),
           column(width = 6,
                  fileInput("upload2", label = h4("Upload .csv here")))),


  fluidRow(column(width = 4,
                  selectInput("graphtype1", label = h5("Choose Graph"),
                              choices = list("Braking Map" = "braking", "Throttle Position Map" = "throttle",
                                             "Graph of Lap Speed" = "laps", "RPM by Gear" = "rpm_gear", 
                                             "RPM by Speed" = "rpm_speed", "Map of Lap Speed" = "mapspeed", 
                                             "Air to Fuel Ratio vs RPM" = "airfuel"),
                              selected = "mapspeed")),

           column(width = 4,
                  sliderInput("distrange", label = h5("Select Distance"), 
                              min = 0,
                              max = 3.5, value = c(0, 3.5))),


           column(width = 4,
                  selectInput("graphtype2", label = h5("Choose Graph"),
                        choices = list("Braking Map" = "braking", "Throttle Position Map" = "throttle",
                                        "Graph of Lap Speed" = "laps", "RPM by Gear" = "RPM_gear", 
                                         "RPM by Speed" = "RPM_speed", "Map of Lap Speed" = "mapspeed", 
                                         "Air to Fuel Ratio vs RPM" = "airfuel"),
                                          selected = "mapspeed"))),

  fluidRow(column(width = 6,
                  plotOutput("graph1")),
           column(width = 6,
                  plotOutput("graph2"))),
  
  fluidRow(column(width = 6,
                   tableOutput("table1")),
           column(width = 6,
                  tableOutput("table2"))
                    )


)


server <- function(input, output) {

  options(shiny.maxRequestSize = 50*1024^2)

  
  output$graph1 <- renderPlot( {
    input_data <- cleanSingleLap(input$upload1$datapath, 1)
    if(input$graphtype1 %in% c("laps")) {
    lapspeed(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
    
    } else if (input$graphtype1 %in% c("mapspeed")) {
    mapspeed(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype1 %in% c("RPM_speed")) {
      RPM_speed(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype1 %in% c("RPM_gear")) {
      RPM_gear(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype1 %in% c("throttle")) {
      throttle_position(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype1 %in% c("braking")) {
      breaking_pattern(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype1 %in% c("airfuel")) {
      airfuel(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    }
  })
  
  output$graph2 <- renderPlot({
    input_data <- cleanSingleLap(input$upload2$datapath, 1)
    if(input$graphtype2 %in% c("laps")) {
      lapspeed(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
    } else if (input$graphtype2 %in% c("mapspeed")) {
      mapspeed(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype2 %in% c("RPM_speed")) {
      RPM_speed(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype2 %in% c("RPM_gear")) {
      RPM_gear(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype2 %in% c("throttle")) {
      throttle_position(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
    
    } else if (input$graphtype2 %in% c("braking")) {
      breaking_pattern(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    } else if (input$graphtype2 %in% c("airfuel")) {
      airfuel(input_data, 1, startdist = input$distrange[1], enddist = input$distrange[2])
      
    }
    
})
  
  output$table1 <- renderTable( {
    input_data <- cleanSingleLap(input$upload1$datapath, 1)
    if(input$graphtype1 %in% c("laps")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        group_by(lap) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype1 %in% c("mapspeed")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype1 %in% c("RPM_speed")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
    } else if (input$graphtype1 %in% c("RPM_gear")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        group_by(Calculated_Gea) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
      
    } else if (input$graphtype1 %in% c("throttle")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Throttle = mean(PE3_TPS), Variance = sd(PE3_TPS))
      
    } else if (input$graphtype1 %in% c("braking")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Break = mean(BPS_Front), Variance = sd(BPS_Front))
      
      
    } else if (input$graphtype1 %in% c("airfuel")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Ratio = mean(PE3_LAMBDA), Variance = sd(PE3_LAMBDA))
      
    }
  })

  
  output$table2 <- renderTable( {
    input_data <- cleanSingleLap(input$upload2$datapath, 1)
    if(input$graphtype2 %in% c("laps")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        group_by(lap) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype2 %in% c("mapspeed")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype2 %in% c("RPM_speed")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
    } else if (input$graphtype2 %in% c("RPM_gear")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        group_by(Calculated_Gea) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
      
    } else if (input$graphtype2 %in% c("throttle")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Throttle = mean(PE3_TPS), Variance = sd(PE3_TPS))
      
    } else if (input$graphtype2 %in% c("braking")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Break = mean(BPS_Front), Variance = sd(BPS_Front))
      
      
    } else if (input$graphtype2 %in% c("airfuel")) {
      input_data %>%
        filter(Distance >= input$distrange[1]) %>%
        filter(Distance <= input$distrange[2]) %>%
        summarise(Ave_Ratio = mean(PE3_LAMBDA), Variance = sd(PE3_LAMBDA))
      
    }
  })
  
  
}


shinyApp(ui = ui, server = server)

