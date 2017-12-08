library(shiny)

ui <- fluidPage(

  fluidRow(column(width = 6,
                  fileInput("upload1", label = h4("Upload .csv here"))),
           column(width = 6,
                  fileInput("upload2", label = h4("Upload .csv here")))),


  fluidRow(column(width = 4,
                  selectInput("graphtype2", label = h5("Choose Graph"),
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                              selected = 1)),

           column(width = 4,
                  sliderInput("distrange", label = h5("Select Distance"), min = 0,
                              max = 100, value = c(40, 60))),


           column(width = 4,
                  selectInput("graphtype2", label = h5("Choose Graph"),
                              choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                              selected = 1))),

  fluidRow(column(width = 6,
                  plotOutput("graph1")),
           column(width = 6,
                  plotOutput("graph2")))


)


server <- function(input, output) {
  options(shiny.maxRequestSize = 50*1024^2)
  output$graph1 <- renderPlot( {
    input_data <- cleanSingleLap(input$upload1)

    lapspeed(input_data)
  })
}


shinyApp(ui = ui, server = server)

