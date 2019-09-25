# Options for UI inputs

# Select regions using "selectInput"
region_options<-as.list(c(1:11))
names(region_options)<-c("Ca-MK-Ox","Cornwall and Devon",
                         "East Midlands","East of England",
                         "London","North East","North West",
                         "Sout East","South West","West Midlands",
                         "Yorkshire and the Humber")

selectInput("region",label="Choose region.",
            choices = region_options)

# Select regions using "radioButtons"
radioButtons("region",
             label = "Choose region.",
             choiceNames = list("CaMKOx","Cornwall and Devon","East Midlands",
                                "East of England","London","North East England",
                                "North West England","South East England",
                                "South West England",
                                "West Midlands","Yorkshire and the Humber"),
             choiceValues = as.list(c(1:11)),width="30%")

#--------------------------------------------------------------------
# Display PDF in shiny
#   OPTION 1
tags$iframe(style="height:600px; width:100%",
            src="http://localhost/ressources/pdf/R-Intro.pdf")

# Display PDF in shiny
#   OPTION 2
# UI
  textInput("pdfurl", "PDF URL")
  htmlOutput('pdfviewer')
# server
output$pdfviewer <- renderText({
  return(paste('<iframe style="height:600px; width:100%" src="',
               input$pdfurl, '"></iframe>', sep = ""))
})

# Option 3
#  1) put pdf in "www" directory
#  2) add renderUI statement to server
#  3) add uiOutput to ui
shinyServer(function(input, output) {
  
  observeEvent(input$generate, {
    output$pdfview <- renderUI({
      tags$iframe(style="height:600px; width:100%", src="foo.pdf")
    })
  })
})
shinyUI(fluidPage(
  
  titlePanel("Display a PDF"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Generate PDF")
    ),
    
    mainPanel(
      uiOutput("pdfview")
    )
  )
))

# PLOT MAP OF UK REGIONS
#    output$ukmap<-renderPlot({  
#      regions<-B5LoadRegions(dirs)
#      nregions<-nrow(regions@data)
#      plot(regions, col=rainbow(nregions),main="UK regions with data availability")
#      legend("left",legend = levels(regions@data$rgn15nm),
#             fill=rainbow(nregions),
#             cex=0.8)
#    })  

# Create UI that depends on user input
output$cityControls <- renderUI({
  cities <- getNearestCities(input$lat, input$long)
  checkboxGroupInput("cities", "Choose Cities", cities)
})

