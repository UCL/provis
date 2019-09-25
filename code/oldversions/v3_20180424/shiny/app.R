
library(shiny)
library(knitr)
source("A0Setup.R",local=FALSE)

ui <- fluidPage(
  sidebarPanel(
    #-------------------------------------------------------
    # P1: START PAGE:        SIDE BAR
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '1'",
      textOutput("p1_side_text"),
      tags$b("EXPERIMENTAL VERSION: please send comments/questions to l.nesheim@ucl.ac.uk",style="color:red"),
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
      h1(textOutput("sidebar_introduction")),
      tags$b("EXPERIMENTAL VERSION: please send comments/questions to l.nesheim@ucl.ac.uk",style="color:red"),
      hr(),
      radioButtons("infrastructure_type",label="Choose investment type.",
                   choices=c("settlement","road","rail"),width="100%"),
      # CHOOSE SETTLEMENT
      #    INPUTS:   (region,settlementfile,nhouses,popdensity,model_laname)
      #    OUTPUTS:  (run_settlement,text,table,plots,summary_report)
      conditionalPanel(
        condition = "input.infrastructure_type == 'settlement'",
        h1("New settlement."),
        fileInput("settlement_kml1",
                  label=paste0("Upload kml file containing coordinates defining boundary of settlement, ",
                               "for example, a polygon centred around a point on the map. ",
                               "Use Google Maps to create the map. Instructions at tab at top right.")),
        sliderInput("nhouses",label = "Number of houses",min=100,max=50000,value=10000),
        # TO DO:   add comments/instructions for population density
        #          add instructions for new settlement
        sliderInput("popdensity",label = "Population density of new settlement",min=20,max=70,value=35),
        uiOutput("select_model_la"),
        actionButton("run_settlement",
                     label="Click to compute prices for new settlement.")
      ),
      # CHOOSE ROAD
      #    INPUTS:   (method,roadfile,speed,roadspeedfile,multiplier,run_transport)
      conditionalPanel(
        condition = "input.infrastructure_type == 'road'",
        h1("Road"),
        radioButtons("method",label = "Choose method to change road network.",
                     choiceNames=list("new road","travel improvement area"),
                     choiceValues=list("newroute","newspeed"),width="100%"),
        conditionalPanel(
          condition = "input.method == 'newroute'",
          fileInput("roadfile",label=paste0("Choose kml file containing road junction coordinates. ",
                                            "Use Google Maps to create the kml file. ",
                                            "Instructions in tab at top right.")),
          sliderInput("speed",label="Average speed on new road (mph).",
                      min=30,max=80,value=50)),
        conditionalPanel(
          condition = "input.method == 'newspeed'",
          fileInput("roadspeedfile",
                    label=paste0("Choose kml file defining travel improvement area ",
                                 "for example, a polygon centred around some town. ",
                                 "Use Google Maps to create kml file. Instructions at top right.")),
          sliderInput("multiplier",
                      label=paste0("Travel time multiplier. For example, new travel time equals ",
                                   "0.9 times original travel time."),
                      min=0.8,max=1.2,value=0.9)),
        actionButton("run_transport",label="Click to compute impact of road investment.")
      ),
      # CHOOSE RAIL
      #    INPUTS:   (method2,railfile,speed,railspeedfile,multiplier,run_rail)
      conditionalPanel(
        condition = "input.infrastructure_type == 'rail'",
        h1("Rail"),
        radioButtons("method2",label = "Choose method to change rail network.",
                     choiceNames=list("new rail line","new speed"),
                     choiceValues=list("newroute","newspeed"),width="100%"),
        conditionalPanel(
          condition = "input.method2 == 'newroute'",
          fileInput("railfile",label="Choose file containing rail station coordinates."),
          sliderInput("speed",label="Average speed on new rail line (mph).",
                      min=30,max=80,value=50)),
        conditionalPanel(
          condition = "input.method2 == 'newspeed'",
          fileInput("railspeedfile",label="Choose file containing rail travel improvement area."),
          sliderInput("multiplier",label="Travel time multiplier.",
                      min=0.8,max=1.2,value=0.9)),
        actionButton("run_rail",label="Click to compute impact of rail investment.")),
      hr(),
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
      tags$b("EXPERIMENTAL VERSION. Please send questions/comments to l.nesheim@ucl.ac.uk",style="color:red"),
      uiOutput("ukmap")
    ),
    #-------------------------------------------------------
    # P2: CHOOSE INVESTMENT PAGE           MAIN PANEL
    #-------------------------------------------------------
    conditionalPanel(
      condition = "output.page == '2'",
      tabsetPanel(
        tabPanel("Map of region",
                 textOutput("p2_map_text"),
                 tags$b("EXPERIMENTAL VERSION: please send comments/questions to l.nesheim@ucl.ac.uk",style="color:red"),
                 hr(),
                 uiOutput("regionmap")),
        tabPanel("Instructions to create map or new road or rail line.",
                 uiOutput("mapinstructions")
                 #                 a(href="HowToDrawRoad.pdf", target="_blank", "Click to view instructions to draw a map using Google Maps.")
                )
      )
    ),
    # Waiting for results
    conditionalPanel(
      condition = "output.page == '3'",
      textOutput("p3_text"),hr(),
      actionButton("go_p4",label="Show results.")
    ),
    # Display results
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
  project <- reactiveValues(id=1,dir="www/pid1",travelmodelbasis="cheb")
  page_id <- reactiveValues(n="1")
  results <- reactiveValues(flag=FALSE)
  # reactive value:  region
  region <-reactiveValues(id=2,str="CornwallDevon",name="Cornwall and Devon")
  outfile<-reactiveValues(report=NULL)
  shapefile<-reactiveValues(road=NULL,speed=NULL,rail=NULL)
  
  # Generate new UI based on value of region
  output$select_model_la <- renderUI({
    load(paste0(RootDir,"/data/region_la_list.RData"))
    selectInput("la_model",label="Choose local authority to be used as statistical model.",
                choices=region_la_list[[region$id]])
  })  
  
  # Event:  go to page 2: Select investment
  observeEvent(input$go_p2,{
    region$id   <- as.numeric(input$region)
    region$str  <- all_regions[region$id,2]
    region$name <- all_regions[region$id,3]
    page_id$n   <- "2"
  } )
  
  # Event: go to page 1: Select region
  observeEvent(input$go_p1,{
    page_id$n<-"1"
  } )
  observeEvent(input$go_p4_to_p1,{
    page_id$n<-"1"
  })
  
  # Event: go to page 1: Select region
  observeEvent(input$go_p4,{
    page_id$n<-"4"
  } )
  
  #------------------------------------------------------------
  # P1:  START PAGE
  
  # P1: SIDE BAR
#  output$p1_warning<-renderText({})
  output$p1_side_text<-renderText({"This tool allows you to predict the impact on property values of investments in roads, rail or housing. Choose a region to begin." })
  
  # P1: MAIN PANEL
  output$p1_main_text<-renderText({"Below is a map of UK regions for which data is available." })
  output$ukmap <- renderUI({
    tags$iframe(style="height:600px; width:800px", src="regions.png",seamless=TRUE)
  })
  #--------------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------------------------
  # P2:   CHOOSE INVEST
  
  # P2:   SIDE BAR:  Predict new settlement value
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

    # save parameters that are needed by D1SettlementReport.Rnw
    parms<-list(project$id,region$id,input$nhouses,input$popdensity,input$la_model,
                input$settlement_kml1$datapath,project$travelmodelbasis)
    names(parms)<-c("project_id","region_id","nhouses","popdensity","la_model",
                    "mapfile","travelmodelbasis")
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
   
    # fullname of pdf file
    outfile$report<-paste0("pid",project$id,"/",texfile,".pdf")
    setwd(workdir)
    page_id$n<-3
  })
  
  # Compute impact of road investment
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

    if (input$method=="newroute") {
      shapefile$road<-input$roadfile$datapath
      shapefile$speed<-NULL
    } else if (input$method=="newspeed") {
      shapefile$road<-NULL
      if (!is.null(input$roadspeedfile)) {
        shapefile$speed<-input$roadspeedfile$datapath
      }
    }
    
    # Save parameters needed by .Rnw file
    parms<-list(project$id,region$id,input$infrastructure_type,input$method,
                shapefile$road,shapefile$speed,input$speed,input$multiplier,
                project$travelmodelbasis)
    names(parms)<-c("project_id","region_id","infrastructure_type","method",
                    "roadfile","speedfile","speed","multiplier","travelmodelbasis")
    save(parms,file=paste0(project$dir,"/parms.RData"))

    # 1) Set names for texfile and inputfile
    # 2) Change working directory
    # 3) set options for knit
    # 4) use knit to create tex file
    #    This step calls either D2RoadReport.Rnw or D2RoadSpeedReport.Rnw
    #    to compute predictions and create graphs and tables
    # 5) use pdflatex to create pdf file
    # 6) define outfile$report
    # 7) change working directory

    if (input$method=="newroute") {
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
  
    # Compute impact of rail investment
  observeEvent(input$run_rail,{
    
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

    if (input$method2=="newroute") {
      shapefile$rail<-input$roadfile$datapath
      shapefile$speed<-NULL
    } else if (input$method2=="newspeed") {
      shapefile$rail<-NULL
      if (!is.null(input$railspeedfile)) {
        shapefile$speed<-input$railspeedfile$datapath
      }
    }
    
    # Save parameters needed by .Rnw file
    parms<-list(project$id,region$id,input$infrastructure_type,input$method2,
                shapefile$rail,shapefile$speed,input$speed,input$multiplier,
                project$travelmodelbasis)
    names(parms)<-c("project_id","region_id","infrastructure_type","method",
                    "railfile","speedfile","speed","multiplier","travelmodelbasis")
    save(parms,file=paste0(project$dir,"/parms.RData"))

    # 1) Set names for texfile and inputfile
    # 2) Change working directory
    # 3) set options for knit
    # 4) use knit to create tex file
    #    This step calls either D4RailReport.Rnw or D5RailSpeedReport.Rnw
    #    to compute predictions and create graphs and tables
    # 5) use pdflatex to create pdf file
    # 6) define outfile$report
    # 7) change working directory

    if (input$method2=="newroute") {
      texfile<-paste0("region",as.character(region$id),"_rail",as.character(project$id))
      inputfile<-"../../D4RailReport.Rnw"
    } else if (input$method2=="newspeed") {
      texfile<-paste0("region",as.character(region$id),"_railspeed",as.character(project$id))
      inputfile<-"../../D5RailSpeedReport.Rnw"
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
  # END OF PREDICTIONS

  # Print region
  output$sidebar_introduction<-renderText({
    paste0("Current region is ",region$name,".")
  })
  
  # Choose investment instructions
  output$sidebar_instructions<-renderText({
    "Choose type of investment and choose options."
  })
  
  # Introduction to ???
  output$main_introduction<-renderText({
    paste0("Current region is ",region$name,".")
  })
  
  output$main_instructions<-renderText({
    paste0("Current region is ",region$name,".")
  })
  
  output$p3_text<-renderText({ 
    paste0("Computing prices. This takes a few minutes.")
    })
  
  output$compute_instructions<-renderText({
    "Computation is a bit slow. After clicking you will have to wait 3-5 minutes for results. Apologies"  
  })
  # P2:   MAIN PANEL  
  output$p2_map_text<-renderText({
    paste0("EXPERIMENTAL VERSION. Below is a map of the ",region$name," region. Choose investment characteristics at ",
           "left. Then click at the bottom left to compute prices. After clicking you will have ",
           "to wait 3-5 minutes for the computation to complete. When the computation is complete",
           "a new window will open. Then, click to display the results. To start over, click",
           "Start over at the far bottom left.")  
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
