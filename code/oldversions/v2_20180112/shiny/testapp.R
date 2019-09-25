library(shiny)
library(ggmap)

ui <- fluidPage(
  actionButton("go",label="Go."),
  hr("Find name of postal_town for a vector of coordinates."),
  textOutput("text1"),
  textOutput("text2")
)

server <- function(input, output) {
  dest<-reactiveValues(coordinates=NULL,postal_town=NULL)
    
  observeEvent(input$go,{
    dest$coordinates<-c(0.121817,52.20534)
    fulldest<-revgeocode(dest$coordinates,output="more") 
    dest$postal_town<-fulldest$postal_town
  })
  
  output$text1<-renderText({
    if (is.null(dest$coordinates)) {
      "No coordinates selected."  
    } else {
      paste0("Coordinates are (",dest$coordinates[1],",",dest$coordinates[2],").")  
    }
  })
  output$text2<-renderText({
    if (!is.null(dest$coordinates)) {
      paste0("postal_town is ",dest$postal_town,".")
    }
  })
}

shinyApp(ui = ui, server = server)

