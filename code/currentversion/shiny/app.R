library(shiny)
library(shinyjs)
library(knitr)
source("A0Setup.R",local=FALSE)

ui <- fluidPage(
  useShinyjs(),
#-------------------------------------------------------------------------------------------------------------------------------------------
# BEGIN SIDE PANEL
#-------------------------------------------------------------------------------------------------------------------------------------------
  sidebarPanel(
    # Page 1: Select region
    conditionalPanel(condition = "output.page == '1'",textOutput("sidetext1"),
                     tags$b("EXPERIMENTAL VERSION: please send comments/questions to l.nesheim@ucl.ac.uk",style="color:red"),hr(),
                     radioButtons("region",label = "Choose region.",
                                  choiceNames = list("Cornwall and Devon","East Midlands","East of England","London","North East England",
                                                     "North West England","South East England","South West England","West Midlands",
                                                     "Yorkshire and the Humber","CaMKOx"),
                                   choiceValues = as.list(c(2:11,1)),width="100%"),
                     actionButton("go_p2",label="Go.")),
    # Page 2: Select investment type
    conditionalPanel(condition = "output.page == '2'",h1(textOutput("sidetext2")),
                     tags$b("EXPERIMENTAL VERSION: please send comments/questions to l.nesheim@ucl.ac.uk",style="color:red"),hr(),
                     radioButtons("infrastructure_type",label="Choose investment type.",
                                  choiceValues=c("settlement","road","rail"),
                                  choiceNames=c("settlement","road","public transport"),
                                  width="100%"),
    # Page 2.1:  Set settlement details
                     conditionalPanel(condition = "input.infrastructure_type == 'settlement'",h1("New settlement."),
                                      fileInput("settlement_kml1",label=paste0("Upload kml file containing coordinates defining boundary of settlement, ",
                                                                               "for example, a polygon centred around a point on the map. ",
                                                                               "Use Google Maps to create the map. Instructions at tab at top right.")),
                                      sliderInput("nproperties",label = "Total number of new properties (commercial and residential)",
                                                  min=100,max=50000,value=10000),
                                      sliderInput("fraction_commercial",label = "Fraction commercial (e.g. 0.05 = 5%)",min=0,max=0.10,value=0.05),
                                      sliderInput("popdensity",label = "Population density of new settlement",min=20,max=70,value=35),
                                      uiOutput("select_model_la"),actionButton("run_settlement",
                                      label="Click to compute prices for new settlement.")),
     # Page 2.2:  Set road details
                      conditionalPanel(condition = "input.infrastructure_type == 'road'",h1("Road"),
                                       radioButtons("method",label = "Choose method to change road network.",
                                                    choiceNames=list("new road","travel improvement area"),
                                                    choiceValues=list("newroute","newspeed"),width="100%"),
                                       conditionalPanel(condition = "input.method == 'newroute'",
                                                        fileInput("roadfile",label=paste0("Choose kml file containing road junction coordinates. ",
                                                                                          "Use Google Maps to create the kml file. ",
                                                                                          "Instructions in tab at top right.")),
                                                        sliderInput("speed",label="Average speed on new road (mph).",
                                                                    min=30,max=80,value=50)),
                                        conditionalPanel(condition = "input.method == 'newspeed'",
                                                         fileInput("roadspeedfile",label=paste0("Choose kml file defining travel improvement area ",
                                                                                                "for example, a polygon centred around some town. ",
                                                                                                "Use Google Maps to create kml file. Instructions at top right.")),
                                        sliderInput("multiplier",label="Choose percentage change in travel time.",
                                                    min=-40,max=20,value=-10,step=5),
                                        tags$p(paste0("For example, choose -10 for a 10% reduction in travel time ",
                                                      "or choose 10 for a 10% increase in travel time."))),
                                        actionButton("run_road",label="Click to compute impact of road investment.")),
        # Page 2.3:  Set rail details
                        conditionalPanel(condition = "input.infrastructure_type == 'rail'",
                                         h1("Public transport"),
                                         radioButtons("method2",label = "Choose method to change public transport.",
                                                      choiceNames=list("new rail line","travel improvement area"),
                                                      choiceValues=list("newroute","newspeed"),width="100%"),
                                         conditionalPanel(condition = "input.method2 == 'newroute'",
                                                          fileInput("railfile",label="Choose file containing new rail station coordinates."),
                                                          sliderInput("speed",label="Average speed on new rail line (mph).",
                                                                      min=30,max=80,value=50)),
                                         conditionalPanel(condition = "input.method2 == 'newspeed'",
                                                          fileInput("railspeedfile",label="Choose file containing public transport travel improvement area."),
                                                          sliderInput("multiplier",label="Choose percentage change in travel time.",
                                                                      min=-40,max=10,value=-10,step=5),
                                                          tags$p(paste0("For example, choose -10 for a 10% reduction in travel time ",
                                                                        "or choose 10 for a 10% increase in travel time."))),
                                                          actionButton("run_rail",label="Click to compute impact of public transport investment.")),
                    hr(),actionButton("go_p2_to_p1",label="Start over.")),
    conditionalPanel(condition = "output.page == '5'",actionButton("go_p5s_to_p1",label="Start over."))),
#-------------------------------------------------------------------------------------------------------------------------------------------
# BEGIN MAIN PANEL
#-------------------------------------------------------------------------------------------------------------------------------------------
mainPanel(h1("Property value uplift calculator"),
            # Page 1:  Main page
            conditionalPanel(condition = "output.page == '1'",textOutput("maintext1"),
                             tags$b("EXPERIMENTAL VERSION. Please send questions/comments to l.nesheim@ucl.ac.uk",style="color:red"),
                             uiOutput("ukmap"),hr()),
            # Page 2: Choose investment type
            conditionalPanel(condition = "output.page == '2'",
                             tabsetPanel(tabPanel("Map of region",textOutput("maintext2"),
                                                  tags$b("EXPERIMENTAL VERSION: please send comments/questions to l.nesheim@ucl.ac.uk",style="color:red"),hr(),
                                                  uiOutput("regionmap")),
                                         tabPanel("Instructions to create map for new road, new rail line, new settlement, or travel improvement area.",
                                                  uiOutput("mapinstructions")))),
            # Page 3: Wait for results
            conditionalPanel(condition = "output.page == '3'",textOutput("maintext3"),
                             textOutput("startTime"),hr()),
            # Page 4: Results complete. click to get output
            conditionalPanel(condition = "output.page == '4'",textOutput("maintext4"),hr(),actionButton("go_p5",label="Show results.")),
            # Page 5: Display results
            conditionalPanel(condition = "output.page == '5'",
                             tabsetPanel(tabPanel("Residential properties",uiOutput("report1")),
                                         tabPanel("Commercial properties",uiOutput("report2"))),
                             hr(),actionButton("go_p5_to_p1",label="Start over.")),
                             hr(),textOutput("page")
))

#-------------------------------------------------------------------------------------------------------------------------------------------
# BEGIN SERVER
#-------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output,session) {
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
  
  # Reactive values:  
  # project :   project details
  # page_id :   page number
  # runflag :   process status flags
  # etime          : elapsed time
  project       <- reactiveValues(id=1,dir="www/pid1",travelmodelbasis="cheb",nhouses=9500,nbusinesses=500)
  page_id       <- reactiveValues(n="1")
  region        <- reactiveValues(id=2,str="CornwallDevon",name="Cornwall and Devon")
  outfile       <- reactiveValues(report1=NULL,report2=NULL)
  shapefile     <- reactiveValues(road=NULL,speed=NULL,rail=NULL)
  runflag       <- reactiveValues(settlement=NULL,road=NULL,rail=NULL)
  etime         <- reactiveValues(t0=0,t1=0)

  # Generate new UI based on value of region
  output$select_model_la <- renderUI({
    load(paste0(RootDir,"/data/region_la_list.RData"))
    selectInput("la_model",label="Choose local authority to be used as statistical model.",
                choices=region_la_list[[region$id]])
  })  
  
  # Event 2:  go to page 2: Select investment
  observeEvent(input$go_p2,{
    region$id   <- as.numeric(input$region)
    region$str  <- all_regions[region$id,2]
    region$name <- all_regions[region$id,3]
    page_id$n   <- "2"
  } )
  
  # Event 1: go from 2 to 1: Select region
  observeEvent(input$go_p2_to_p1,{
    page_id$n<-"1"
  } )

  # Event 1: go from 5 to 1: Select region
  observeEvent(input$go_p5_to_p1,{
      page_id$n<-"1"
  })

  # Event 1: go from 5s to 1: Select region
  observeEvent(input$go_p5s_to_p1,{
    page_id$n<-"1"
  })

  # Event 5: go to page 5: Display results
  observeEvent(input$go_p5,{
      page_id$n<-"5"
  } )

  # Event 3: GO TO PAGE 3:    WAIT FOR RESULTS
  observeEvent(input$run_settlement,{
      # Advance to next page on display
      page_id$n<-3
      etime$t0<-Sys.time()
      etime$t1<-Sys.time()
  })
  
  # Event 3: Wait 1 second, before starting computations for new settlement
  observeEvent(input$run_settlement,{
    # Start computation after 1 second delay
    delay(1000,runflag$settlement<-TRUE)
  })

#------------------------------------------------------------------------------------------------------
# Event 3A: Computations for new settlement
#------------------------------------------------------------------------------------------------------
  observeEvent(runflag$settlement,{
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
      project$nbusinesses <- round(input$nproperties*input$fraction_commercial)
      project$nhouses     <- input$nproperties - project$nbusinesses
      
      # save parameters that are needed by D1SettlementReport.Rnw
      parms<-list(project$id,region$id,project$nhouses,input$popdensity,input$la_model,
      input$settlement_kml1$datapath,project$travelmodelbasis,project$nbusinesses)
      names(parms)<-c("project_id","region_id","nhouses","popdensity","la_model",
      "mapfile","travelmodelbasis","nbusinesses")
      save(parms,file=paste0(project$dir,"/parms.RData"))
      
      # 1) Set names for tex file
      # 2) Change working directory
      # 3) set options for knit
      # 4) use knit to create tex file
      #    This step calls D1SettlementReport.Rnw to compute predictions and create graphs and tables
      # 5) use pdflatex to create pdf file
      texfile1<-paste0("region",as.character(region$id),"_settlement1_",as.character(project$id))
      workdir<-getwd()
      setwd(project$dir)
      opts_knit$set(root.dir=".")
      
      # Residential property report
      knit(input="../../D1SettlementReport.Rnw",
      output=paste0(texfile1,".tex"),
      envir=globalenv())
      # Compile twice to generate equation numbers
      system2("pdflatex", paste0(texfile1,".tex"))
      system2("pdflatex", paste0(texfile1,".tex"))
      
      # fullname of pdf file
      outfile$report1<-paste0("pid",project$id,"/",texfile1,".pdf")
      
      # Commercial property report
      texfile2<-paste0("region",as.character(region$id),"_settlement2_",as.character(project$id))
      
      knit(input="../../E1NondomReport.Rnw",
      output=paste0(texfile2,".tex"),
      envir=globalenv())
      # Compile twice to generate equation numbers
      system2("pdflatex", paste0(texfile2,".tex"))
      system2("pdflatex", paste0(texfile2,".tex"))
      
      # fullname of pdf file
      outfile$report2<-paste0("pid",project$id,"/",texfile2,".pdf")
      setwd(workdir)
      page_id$n<-4
      etime$t1<-Sys.time()
  })

#------------------------------------------------------------------------------------------------------
# Event 3B: Computations for new road
#------------------------------------------------------------------------------------------------------
  # Event 3B.1:  Wait for results
  observeEvent(input$run_road,{
      # Advance to next page on display
      page_id$n<-3
      etime$t0<-Sys.time()
      etime$t1<-Sys.time()
  })
  # Event 3B.2: Wait 1 second, before computing
  observeEvent(input$run_road,{
    # Start computation after 1 second delay
    delay(1000,runflag$road<-TRUE)
  })

  # Event 3B.3: Compute new road results
  observeEvent(runflag$road,{
    
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
      texfile1<-paste0("region",as.character(region$id),"_road1_",as.character(project$id))
      inputfile<-"../../D2RoadReport.Rnw"
    } else if (input$method=="newspeed") {
      texfile1<-paste0("region",as.character(region$id),"_roadspeed1_",as.character(project$id))
      inputfile<-"../../D3RoadSpeedReport.Rnw"
    }

    workdir<-getwd()
    setwd(project$dir)
    opts_knit$set(root.dir=".")
    knit(input=inputfile,
         output=paste0(texfile1,".tex"),
         envir=globalenv())

    # Compile twice to generate equation numbers
    system2("pdflatex",paste0(texfile1,".tex"))
    system2("pdflatex",paste0(texfile1,".tex"))

    # fullname of pdf file
    outfile$report1<-paste0("pid",project$id,"/",texfile1,".pdf")
    
    # compute impact on commercial properties
    if (input$method=="newroute") {
      texfile2<-paste0("region",as.character(region$id),"_road2_",as.character(project$id))
      inputfile<-"../../E2RoadReport.Rnw"
    } else if (input$method=="newspeed") {
      texfile2<-paste0("region",as.character(region$id),"_roadspeed2_",as.character(project$id))
      inputfile<-"../../E3RoadSpeedReport.Rnw"
    }

    knit(input=inputfile,
         output=paste0(texfile2,".tex"),
         envir=globalenv())

    # Compile twice to generate equation numbers
    system2("pdflatex",paste0(texfile2,".tex"))
    system2("pdflatex",paste0(texfile2,".tex"))

    # fullname of pdf file
    outfile$report2<-paste0("pid",project$id,"/",texfile2,".pdf")
    
    setwd(workdir)
    page_id$n<-4
    etime$t1<-Sys.time()
  })

#------------------------------------------------------------------------------------------------------
# Event 3C: Computations for new rail
#------------------------------------------------------------------------------------------------------
  # Event 3C.1:  Wait for results
  observeEvent(input$run_rail,{
      # Advance to next page on display
      page_id$n<-3
      etime$t0<-Sys.time()
      etime$t1<-Sys.time()
  })
  # Event 3C.2: Wait 1 second, before computing
  observeEvent(input$run_rail,{
    # Start computation after 1 second delay
    delay(1000,runflag$rail<-TRUE)
  })

  # Event 3B.3: Compute new rail results
  observeEvent(runflag$rail,{
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
      shapefile$rail<-input$railfile$datapath
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
      texfile1<-paste0("region",as.character(region$id),"_rail1_",as.character(project$id))
      inputfile<-"../../D4RailReport.Rnw"
    } else if (input$method2=="newspeed") {
      texfile1<-paste0("region",as.character(region$id),"_railspeed1_",as.character(project$id))
      inputfile<-"../../D5RailSpeedReport.Rnw"
    }

    workdir<-getwd()
    setwd(project$dir)
    opts_knit$set(root.dir=".")
    knit(input=inputfile,
         output=paste0(texfile1,".tex"),
         envir=globalenv())

    # Compile twice to generate equation numbers
    system2("pdflatex",paste0(texfile1,".tex"))
    system2("pdflatex",paste0(texfile1,".tex"))

    # fullname of pdf file
    outfile$report1<-paste0("pid",project$id,"/",texfile1,".pdf")
    
    # Commercial property
    if (input$method2=="newroute") {
      texfile2<-paste0("region",as.character(region$id),"_rail2_",as.character(project$id))
      inputfile<-"../../E4RailReport.Rnw"
    } else if (input$method2=="newspeed") {
      texfile2<-paste0("region",as.character(region$id),"_railspeed2_",as.character(project$id))
      inputfile<-"../../E5RailSpeedReport.Rnw"
    }

    opts_knit$set(root.dir=".")
    knit(input=inputfile,
         output=paste0(texfile2,".tex"),
         envir=globalenv())

    # Compile twice to generate equation numbers
    system2("pdflatex",paste0(texfile2,".tex"))
    system2("pdflatex",paste0(texfile2,".tex"))

    # fullname of pdf file
    outfile$report2<-paste0("pid",project$id,"/",texfile2,".pdf")

    setwd(workdir)
    page_id$n<-4
    etime$t1<-Sys.time()
  })

#------------------------------------------------------------------------------------------------------------------------
# END OF PREDICTIONS
#------------------------------------------------------------------------------------------------------------------------
# Generate text and other outputs
  # P1: SIDE BAR
  output$sidetext1<-renderText({paste0("This tool allows you to predict the impact on property values of investments in ",
                                          "roads, public transport or housing. Choose a region to begin.") })
  # Print region
  output$sidetext2<-renderText({
    paste0("Current region is ",region$name,".")
  })
  
  # P1: MAIN PANEL
  output$maintext1<-renderText({"Below is a map of UK regions for which data is available." })
  output$ukmap <- renderUI({
    tags$iframe(style="height:600px; width:800px", src="regions.png",seamless=TRUE)
  })
  # P2:   MAIN PANEL
  output$maintext2<-renderText({
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

  # P3: MAIN PANEL
  output$maintext3<-renderText({
    if (input$infrastructure_type=="settlement") {
      paste0("Computing prices. The server is constructing a database of ", 
             input$nproperties," properties, matching the new properties to geographic information, ",
             "and predicting residential prices and commercial rateable values.",
             "This process usually takes 3-8 minutes. ",
             "The screen will refresh when the computation is complete.")
    } else {
      paste0("Computing prices. The server is predicting new travel times for the database of properties ",
            "and is predicting residential prices and commercial rateable values. ",
            "This process usually takes 1-3 minutes. The screen will refresh when the computation is complete.")
    }
  })
  output$maintext4<-renderText({
      paste0("Price computation complete. Elapsed time = ",as.character(round(etime$t1-etime$t0))," seconds.")
  })
  
  # Timing text
  output$startTime<-renderText({
    paste0("Start time: ",etime$t0)  
  })
  
  output$currentTime<-renderText({
    invalidateLater(1000,session)
    paste0("Current time: ",Sys.time())
  })

  # Display report on web
  output$report1 <- renderUI({
    if (is.null(outfile$report1)) return()
    src_file<-outfile$report1
    tags$iframe(style="height:1000px; width:1200px", src=src_file)
  })

  output$report2 <- renderUI({
      if (is.null(outfile$report2)) return()
      src_file<-outfile$report2
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
    } else if (page_id$n=="5") {
      "5"
    }
  })
  
}

shinyApp(ui = ui, server = server)
