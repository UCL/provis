library(shiny)

ui <- fluidPage(
  # 1) Choose investment type:  (road,rail,new settlement)
  selectInput("invest_type",label="Select investment type.",
              choices=c("Settlement","Road","Rail"),multiple=TRUE),
  fileInput("roadfile",label="Choose file containing road details."),
  fileInput("railfile",label="Choose file containing rail details."),
  fileInput("settlementfile",label="Choose file containing settlement details."),
  sliderInput("roadspeed",label="Average speed on new road (mph)",
              min=20,max=80,value=40),
  sliderInput("railspeed",label="Average speed on new rail (mph)",
              min=20,max=300,value=60),
  selectInput("roadtype",label="Choose type of road.",
              choices=c("Motorway","A road","Local")),
  plotOutput("heatmap1"),
  tableOutput("sumstats"),
  textOuptut("textout")
)
  # 4) if settlement
  #    a) coordinates
  #    b) land area
  #    c) pop density
  #    d) number of houses
  #    e) baseline settlement model
  #
  # 5) outputs
  #    a) new dataset
  #    b) new prices
  #    c) summary stats of change in value
  #    d) heatmap of change in value
  #    e) summary stats on assumptions
  #    f) sensitivities

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)