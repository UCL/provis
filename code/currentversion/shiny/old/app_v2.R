
library(shiny)
library(knitr)
source("A0Setup.R",local=FALSE)

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
      textOutput("p1_side_text"),
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
    #     inputs: (infrastructure_type,fileflag1,settlement_kml1,nhouses,
    #              popdensity,model_laname,settlement_kml2,run_settlement,
    #              method,roadfile,speed,multiplier,run_transport,go_p1)
    #     outputs: (page,sidebar_introduction,select_model_la)
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '2'",
      h1(textOutput("sidebar_introduction")),hr(),
      radioButtons("infrastructure_type",label="Choose investment type.",
                   choices=c("settlement","road","rail"),width="100%"),
      # CHOOSE SETTLEMENT
      #    INPUTS:   (region,settlementfile,nhouses,popdensity,model_laname)
      #    OUTPUTS:  (run_settlment,text,table,plots,summary_report)
      conditionalPanel(
        condition = "input.infrastructure_type == 'settlement'",
        h1("New settlement."),
        radioButtons("fileflag1",label=NULL,
                     choiceNames=list("Upload kml file","Select existing kml file"),
                     choiceValues=list("upload","select")),
        conditionalPanel(
          condition = "input.fileflag1 == 'upload'",
          fileInput("settlement_kml1",label="Upload kml file containing settlement details."),
          sliderInput("nhouses",label = "Number of houses",min=5000,max=50000,value=10000),
          # TO DO:   add comments/instructions for population density
          #          add instructions for new settlement
          sliderInput("popdensity",label = "Population density",min=20,max=70,value=35),
          uiOutput("select_model_la")
        ),
        conditionalPanel(
          condition = "input.fileflag1 == 'select'",
          selectInput("settlement_kml2",label="Choose existing kml file.",
                      choices=c("bassingbourn.kml","bedford.kml",
                                "cambridgegb.kml","mksw.kml",
                                "mvridgemont","mvstewartby.kml",
                                "mvwoburnsands.kml","oxfordgb.kml",
                                "sandy.kml","southofbletchley.kml"))
        ),
        actionButton("run_settlement",label="Click to compute prices for new settlement.")
      ),
      conditionalPanel(
        condition = "input.infrastructure_type == 'road'",
        h1("Road"),
        radioButtons("method",label = "Choose method to change road network.",
                     choiceNames=list("new road","new speed"),
                     choiceValues=list("newroad","newspeed"),width="100%"),
        conditionalPanel(
          condition = "input.method == 'newroad'",
          fileInput("roadfile",label="Choose file containing road junction coordinates."),
          sliderInput("speed",label="Average speed on new road (mph).",
                      min=30,max=80,value=50)),
        conditionalPanel(
          condition = "input.method == 'newspeed'",
          fileInput("speedfile",label="Choose file containing travel improvement area."),
          sliderInput("multiplier",label="Travel time multiplier.",
                      min=0.8,max=1.2,value=0.9)),
        actionButton("run_transport",label="Click to compute impact of road investment.")
      ),hr(),
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
      textOutput("p1_main_text"),
      uiOutput("ukmap")
    ),
    #-------------------------------------------------------
    # P2: CHOOSE INVESTMENT PAGE           MAIN PANEL
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '2'",
      tabsetPanel(
        tabPanel("Map of region",
                 textOutput("p2_map_text"),hr(),
                 uiOutput("regionmap")),
        tabPanel("Instructions",
                 uiOutput("mapinstructions")
                 #                 a(href="HowToDrawRoad.pdf", target="_blank", "Click to view instructions to draw a map using Google Maps.")
                )
      )
    ),
    conditionalPanel(
      condition = "output.page == '3'",
      textOutput("p3_text"),hr(),
      actionButton("go_p4",label="Show results.")
    ),
    conditionalPanel(
      condition = "output.page == '4'",
      uiOutput("report_web"),hr(),
      actionButton("go_p4_to_p1",label="Start over.")
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
  #--------------------------------------------------------------------------
  
  # Reactive value:  number of page
  project <- reactiveValues(id=1,dir="www/pid1")
  page_id <- reactiveValues(n="1")
  results <- reactiveValues(flag=FALSE)
  # reactive value:  region
  region <-reactiveValues(id=2,str="CornwallDevon",name="Cornwall and Devon")
  outfile<-reactiveValues(report=NULL)
  shapefile<-reactiveValues(road=NULL,speed=NULL)
  
  # Generate new UI based on value of region
  output$select_model_la <- renderUI({
    load(paste0(RootDir,"/data/region_la_list.RData"))
    selectInput("la_model",label="Choose local authority to be used as statistical model.",
                choices=region_la_list[[region$id]])
  })  
  
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
  observeEvent(input$go_p4_to_p1,{
    page_id$n<-"1"
  })
  
  # Event: go to page 1
  observeEvent(input$go_p4,{
    page_id$n<-"4"
  } )
  
  #-------------------------------------------------------------
  # P1:  START PAGE
  
  # P1: SIDE BAR
  output$p1_side_text<-renderText({"This tool allows you to predict the impact on property values of investments in roads, rail or housing. Choose a region to begin." })
  
  # P1: MAIN PANEL
  output$p1_main_text<-renderText({"Below is a map of UK regions for which data is available." })
  output$ukmap <- renderUI({
    tags$iframe(style="height:600px; width:800px", src="regions.png",seamless=TRUE)
  })
  #--------------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------------------------
  # P2:   CHOOSE INVEST
  
  # P2:   SIDE BAR
  observeEvent(input$run_settlement,{
    
    # set project id
    if (file.exists("id.csv")) {
      project$id<-read.csv("id.csv")[1,]+1
    } else {
      project$id<-1 
    }  
    write.csv(project$id,"id.csv",row.names=FALSE)
    # create directory for current session
    project$dir<-paste0("www/pid",project$id)
    system(paste0("mkdir ",project$dir))

    # save parameters that are needed by D1Settlement.Rnw
    parms<-list(project$id,region$id,input$nhouses,input$popdensity,input$la_model,
                input$settlement_kml1$datapath)
    names(parms)<-c("project_id","region_id","nhouses","popdensity","la_model","mapfile")
    save(parms,file=paste0(project$dir,"/parms.RData"))

    # 1) Set names for tex file
    # 2) Change working directory
    # 3) set options for knit
    # 4) use knit to create tex file
    #    This step calls D1SettlementReport.Rnw to compute predictions and create graphs and tables
    # 5) use pdflatex to create pdf file
    texfile<-paste0("region",as.character(region$id),"_settlement",as.character(project$id))
    workdir<-getwd()
    setwd(project$dir)
    opts_knit$set(root.dir=".")
    knit(input="../../D1SettlementReport.Rnw",
         output=paste0(texfile,".tex"),
         envir=globalenv())
    # Compile twice to generate equation numbers
    system2("pdflatex", paste0(texfile,".tex"))
    system2("pdflatex", paste0(texfile,".tex"))
   
    # knit2pdf doesn't seem to work on some computers
    #knit2pdf(input = paste0(CodeDir,"/shiny/D1SettlementReport.Rnw"),
    #         output = paste0(CodeDir,"/shiny/www/region",region$id,
    #                         "_settlement",project$id,".tex"),
    #         clean = TRUE,envir=globalenv())

    # fullname of pdf file
    outfile$report<-paste0("pid",project$id,"/",texfile,".pdf")
    setwd(workdir)
    page_id$n<-3
  })
  
  observeEvent(input$run_transport,{
    
    # set project id
    if (file.exists("id.csv")) {
      project$id<-read.csv("id.csv")[1,]+1
    } else {
      project$id<-1 
    }  
    write.csv(project$id,"id.csv",row.names=FALSE)
    # create directory for current session
    project$dir<-paste0("www/pid",project$id)
    system(paste0("mkdir ",project$dir))

    if (input$method=="newroad") {
      shapefile$road<-input$roadfile$datapath
      shapefile$speed<-NULL
    } else if (input$method=="newspeed") {
      shapefile$road<-NULL
      if (!is.null(input$speedfile)) {
        shapefile$speed<-input$speedfile$datapath
      }
    }
    
    # Save parameters needed by .Rnw file
    parms<-list(project$id,region$id,input$infrastructure_type,input$method,
                shapefile$road,shapefile$speed,input$speed,input$multiplier)
    names(parms)<-c("project_id","region_id","infrastructure_type","method",
                    "roadfile","speedfile","speed","multiplier")
    save(parms,file=paste0(project$dir,"/parms.RData"))

    # 1) Set names for texfile and inputfile
    # 2) Change working directory
    # 3) set options for knit
    # 4) use knit to create tex file
    #    This step calls either D2RoadReport.Rnw or D3RoadSpeedReport.Rnw
    #    to compute predictions and create graphs and tables
    # 5) use pdflatex to create pdf file
    # 6) define outfile$report
    # 7) change working directory

    if (input$method=="newroad") {
      texfile<-paste0("region",as.character(region$id),"_road",as.character(project$id))
      inputfile<-"../../D2RoadReport.Rnw"
    } else if (input$method=="newspeed") {
      texfile<-paste0("region",as.character(region$id),"_roadspeed",as.character(project$id))
      inputfile<-"../../D3RoadSpeedReport.Rnw"
    }

    workdir<-getwd()
    setwd(project$dir)
    opts_knit$set(root.dir=".")
    knit(input=inputfile,
         output=paste0(texfile,".tex"),
         envir=globalenv())

    # Compile twice to generate equation numbers
    system2("pdflatex",paste0(texfile,".tex"))
    system2("pdflatex",paste0(texfile,".tex"))

    # fullname of pdf file
    outfile$report<-paste0("pid",project$id,"/",texfile,".pdf")
    setwd(workdir)
    page_id$n<-3
  })
  
  output$sidebar_introduction<-renderText({
    paste0("Current region is ",region$name,".")
  })
  output$sidebar_instructions<-renderText({
    "Choose type of investment and choose options."
  })
  output$main_introduction<-renderText({
    paste0("Current region is ",region$name,".")
  })
  
  output$main_instructions<-renderText({
    paste0("Current region is ",region$name,".")
  })
  
  output$p3_text<-renderText({ 
    paste0("Computing prices. This takes a few minutes.")
    })
  
  # P2:   MAIN PANEL  
  output$p2_map_text<-renderText({
    paste0("Below is a map of the ",region$name," region.")  
  })
  
  output$regionmap <- renderUI({
      src_file<- paste0("region",region$id,".png")
      tags$iframe(style="height:600px; width:800px", src=src_file)
  })
  
  output$mapinstructions<-renderUI({
    src_file="HowToDrawRoad.pdf"
    tags$iframe(style="height:1000px; width:1200px", src=src_file)
  })
  
  # Display report on web
  output$report_web <- renderUI({
    if (is.null(outfile$report)) return() 
    src_file<-outfile$report
    tags$iframe(style="height:1000px; width:1200px", src=src_file)
  })
  
  # Update output$page
  output$page<-renderText({
    if (page_id$n=="1") {
      "1"
    } else if (page_id$n=="2") {
      "2"
    } else if (page_id$n=="3") {
      "3"
    } else if (page_id$n=="4") {
      "4"
    }
  })
  
}

shinyApp(ui = ui, server = server)
