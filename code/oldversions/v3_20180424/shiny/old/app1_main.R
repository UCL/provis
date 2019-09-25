library(shiny)

#   state        side             main
#   HOME         select region    UK map
#                submit
#   REGION       1) settlement    Regional map
#                2) rail          
#                3) road
#                4) submit
#                5) home
#   COMPUTING    same as REGION   CRAP TO READ
#   RESULTS      same as REGION   RESULTS   

# TODO
# 1) after submit region
#    a) show map of region
#    b) show choose investment page
#    c) 
ui <- fluidPage(
  sidebarPanel(
    #-------------------------------------------------------
    # P1: START PAGE:        SIDE BAR
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '1'",
      textOutput("p1_start_side_text"),
      hr(),
      radioButtons("region",
                   label = "Choose region.",
                   choiceNames = list("Cornwall and Devon","East Midlands",
                                      "East of England","London","North East England",
                                      "North West England","South East England",
                                      "South West England",
                                      "West Midlands","Yorkshire and the Humber",
                                      "CaMKOx"),
                   choiceValues = as.list(c(2:11,1)),width="100%"),
      actionButton("go_p2",label="Go.")
    ),
    #-------------------------------------------------------
    # P2: CHOOSE INVESTMENT:    SIDE BAR
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '2'",
      textOutput("p2_choose_invest_side_text"),hr(),
      actionButton("go_p1",label="Start over.")
    )
  ),
  mainPanel(
    h1("Property value uplift calculator"),
    #-------------------------------------------------------
    # P1: START PAGE                      MAIN PANEL
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '1'",
      textOutput("p1_start_main_text"),
      uiOutput("ukmap")
    ),
    #-------------------------------------------------------
    # P2: CHOOSE INVESTMENT PAGE           MAIN PANEL
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '2'",
      tabsetPanel(
        tabPanel("Map of region",
                 textOutput("p2_choose_invest_map_text"),hr(),
                 uiOutput("regionmap")),
        tabPanel("Instructions",
                 a(href="HowToDrawRoad.pdf", target="_blank", "Click to view instructions to draw a map using Google Maps."))
      )
    ),
    hr(),
    tags$footer(textOutput("page"))
  )
)

server <- function(input, output) {
  
  nregs<-11       # Number of regions
  all_regions<-data.frame(id=seq(1:nregs), 
                      str=c("CaMKOx", "CornwallDevon", 
                             "EastMid", "EastEng", 
                             "London", "NE", 
                             "NW", "SE",
                             "SW", "WestMid", "YorkshireHumber"),
                      name=c("Cambridge, Milton Keynes and Oxford",
                             "Cornwall and Devon",
                             "East Midlands",
                             "East of England",
                             "London",
                             "North East",
                             "North West",
                             "South East",
                             "South West",
                             "West Midlands","Yorkshire and the Humber"))
  
  
  # Reactive value:  number of page
  page_id <- reactiveValues(n="1")
  # reactive value:  region
  region <-reactiveValues(id=2,str="CornwallDevon",name="Cornwall and Devon")
  
  # Event:  go to page 2
  observeEvent(input$go_p2,{
    region$id   <- as.numeric(input$region)
    region$str  <- all_regions[region$id,2]
    region$name <- all_regions[region$id,3]
    page_id$n   <- "2"
  } )
  
  # Event: go to page 1
  observeEvent(input$go_p1,{
    page_id$n<-"1"
  } )
  
  # Update output$page
  output$page<-renderText({
    if (page_id$n=="1") {
      "1"
    } else if (page_id$n=="2") {
      "2"
    }  
  })
  
  #-------------------------------------------------------------
  # P1:  START PAGE
  
  # P1: SIDE BAR
  output$p1_start_side_text<-renderText({"This tool allows you to predict the impact on property values of investments in roads, rail or housing. Choose a region to begin." })
  
  # P1: MAIN PANEL
  output$p1_start_main_text<-renderText({"Below is a map of UK regions for which data is available." })
  output$ukmap <- renderUI({
    tags$iframe(style="height:600px; width:800px", src="regions.png",seamless=TRUE)
  })
  #--------------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------------------------
  # P2:   CHOOSE INVEST
  
  # P2:   SIDE BAR
  output$p2_choose_invest_side_text<-renderText({"Use Google Maps to draw a map of the project area. The map could be 1) an outline of a new settlement, 2) a number of road junctions, 3) a set of rail stations." })

  # P2:   MAIN PANEL  
  output$p2_choose_invest_map_text<-renderText({
    paste0("Below is a map of the ",region$name," region.")  
  })
  
  output$regionmap <- renderUI({
    src_file<- paste0("region",region$id,".png")
    tags$iframe(style="height:600px; width:800px", src=src_file)
  })
  
}

shinyApp(ui = ui, server = server)