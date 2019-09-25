# TO DO
#   1) for settlement, for each region, display relevant list of LA's for model LA
#   2) when click submit:
#       a) on main page, display text saying what is happening
#       b) display tabs: 1) computation, 2) heatmap results, 3) density results
#   3) add button to return to start
#   4) output report to new tab

library(shiny)
# 2) after choose region switch to map of that region
# 1) stop stargazer from display output
# 3) more details of progress on webpage
# 6) option to display density instead of heatmap
# 7) logs versus levels
# 8) compare multiple investments
# 9) correct labels for settlements other than bedford
# 10) create pdf/latex

# Inputs
#   1) infrastructure_type
#   2) Settlement
#      a) settlementfile,run_settlement
#   3) Road
#      a) method, roadfile,speed,multiplier,run_transport
#      b) roadfile
#   4) Results
#      a) plot_type1,plot_type2
# Outputs
#   1) sidebar_introduction
#   2) sidebar_instructions
#   3) main_introduction 
#   4) sumstats,heatmap1

ui <- fluidPage(
  sidebarPanel(
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
                   choiceNames=list("Upload kml file","Select existing file"),
                   choiceValues=list("upload","select")),
      conditionalPanel(
        condition = "input.fileflag1 == 'upload'",
        fileInput("settlement_kml",label="Upload kml file containing settlement details."),
        sliderInput("nhouses",label = "Number of houses",min=5000,max=50000,value=10000),
        # TO DO:   add comments/instructions for population density
        #          add instructions for new settlement
        sliderInput("popdensity",label = "Population density",min=20,max=70,value=35),
        selectInput("model_laname",label = "Model local authority",
                    choices=c("Nottingham","Derby"))
      ),
      conditionalPanel(
        condition = "input.fileflag1 == 'select'",
        uiOutput("select_model_LA"),
        selectInput("settlement_csv",label="Choose file containing settlement details.",
                    choices=c("bassingbourn.csv","bedford.csv",
                              "cambridgegb.csv","mksw.csv",
                              "mvridgemont","mvstewartby.csv",
                              "mvwoburnsands.csv","oxfordgb.csv",
                              "sandy.csv","southofbletchley.csv"))
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
        sliderInput("multiplier",label="Travel time multiplier.",
                  min=0.8,max=1.2,value=0.9)),
      actionButton("run_transport",label="Click to compute impact of road investment.")
    )
  ),  
  mainPanel(
    tabsetPanel(
      tabPanel("Instructions.",textOutput("main_instructions")),
      tabPanel("Results (heatmap)",
               tableOutput("sumstats"),
               plotOutput("heatmap1")),
      tabPanel("Results (density)",
               plotOutput("density1"))
    )
  )
)

server <- function(input, output) {

  #----------------------------------------------------------------------------
  # SET PATH, LOAD LIBRARIES, LOAD FUNCTIONS
  host<-system("hostname",intern=TRUE)
  if (host=="dh-230-mac.econ.ucl.ac.uk") {
    RootDir<-"/Users/uctpln0/Documents/research/hedonic/NIC"
  } else if (host=="minelava") {
    RootDir<-"C:/a/research/hedonic/NIC"
  } else if (host=="DH-G06-03") {
    RootDir<-"U:/NICProject"
  } else if (host=="jake.local" | host=="vic.local") {
    RootDir<-"/home/uctpln0/hedonic/NIC"
  } else {
    info_sys<-Sys.info()
    user<-info_sys["user"]
    if (user=="uctpln0") {
      RootDir<-"/home/uctpln0/hedonic/NIC"
    } else {
      RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
    }
  }
  source(paste0(RootDir,"/code/model/B1LoadLibrary.R"))
  source(paste0(RootDir,"/code/model/B2SetPath.R"))
  source(paste0(RootDir,"/code/model/B3CreateDestinations.R"))
  source(paste0(RootDir,"/code/model/A4CreateSettlement.R"))
  source(paste0(RootDir,"/code/model/A5CreateTransport.R"))
  source(paste0(RootDir,"/code/model/A6PredictPrice.R"))
  source(paste0(RootDir,"/code/model/A7NewRoad.R"))
  #A8RoadReport.Rnw
  #A9RailReport.Rnw
  #A10SettlementReport.Rnw

  region  <- reactiveValues(id=2,str="CornwallDevon",name="Cornwall and Devon")
  newdata <- reactiveValues(data=NULL)
  logprice<-reactiveValues(data=NULL)

  # Generate new UI based on value of region
  output$select_model_LA <- renderUI({
    LA_list <- GetRegion_LA_list(region_id)
    selectInput("LA_model",label="Choose local authority to be used as statistical model.",
                choices=LA_list)
  })  
  
  observeEvent(input$run_settlement,{
 
    # set region id 
    # set path to load data for current region
    # set path to save outputs for current region
    region_id<-region$id
    region_str<-region$str
    dirs<-B2SetPath(RootDir,region_id)

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Begin creating dataset for new settlement", value = 0)
    
    # Extract and capitalise town name
    town<-gsub("_new.csv","",input$settlementfile)
    town<-paste0(toupper(substr(town,1,1)),substr(town,2,nchar(town)))
  
    # Create title for tables and output file name  
    title<-c("Summary statistics:  Old location",
             paste0("Summary statistics:  New ",town))
    outfile<-paste0("summary_statistics_",tolower(town),".tex")
    
    print("Begin creating dataset for new settlement.")

    newdata$data<-A4CreateSettlement(region_id,dirs,input$settlementfile,title,outfile)
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1/2, detail = "Dataset creation complete. Begin prediction.")
    print("Dataset creation complete.")
    print("Begin predicting prices.")
    logprice$data<-A6PredictPrice(newdata$data$m1data_new,newdata$data$m2data_new,dirs$outdir)
    progress$inc(1, detail = "Price prediction complete.")
    print("Price prediction complete.")
  })

  observeEvent(input$run_transport,{
    
    # set region id 
    # set path to load data for current region
    # set path to save outputs for current region
    region_id  <- region$id
    region_str <- region$str
    dirs<-B2SetPath(RootDir,region_id)
#    browser()
    print("Begin creating dataset for new transport project.")
    routefile<-input$roadfile$datapath
    newdata$data<-A5CreateTransport(region_id,input$infrastructure_type,input$method,
                                    routefile=routefile,
                                    input$multiplier,input$speed,dirs,usemapdir=FALSE)
    print("Dataset creation complete.")
    print("Begin predicting prices.")
    out0<-A6PredictPrice(newdata$data$m1data_new,newdata$data$m2data_old,dirs$outdir) 
    out1<-A6PredictPrice(newdata$data$m1data_new,newdata$data$m2data_new,dirs$outdir) 
    
    logprice$data<-out1
    print("Price prediction complete.")
  })
  
  # Outputs
  #   1) sidebar_introduction
  #   2) sidebar_instructions
  #   3) main_introduction 
  #   4) sumstats,heatmap1
  
  output$sidebar_introduction<-renderText({
    paste0("Current region is ",region$name,".")
  })
  output$sidebar_instructions<-renderText({
    "Choose type of investment and choose options."
  })
  output$main_introduction<-renderText({
    paste0("Current region is ",region$str,".")
  })
  
  output$main_instructions<-renderText({
    paste0("Current region is ",region$str,".")
  })
  
  output$sumstats<-renderTable({
    if (is.null(logprice$data)) return()
    xtable(summary(logprice$data)) })
  
  output$density1<-renderPlot({
    if (is.null(logprice$data)) return()
    plot(density(logprice$data$logprice_new,na.rm=TRUE))
  })
  
  output$heatmap1<-renderPlot({  
    if (is.null(logprice$data)) return()
    
    filter<- !is.na(logprice$data$logprice_new)
    zlabel<-"Predicted log price"
    x<-newdata$data$m2data_new$longitude[filter]
    y<-newdata$data$m2data_new$latitude[filter]
    z<-logprice$data$logprice_new[filter]
    resolution<-0.01
    map1 <- interp(x=x,
                   y=y,
                   z=z,
                   yo=seq(min(y),max(y),by=resolution),
                   xo=seq(min(x),max(x),by=resolution),
                   duplicate="mean")
    filled.contour(map1, color.palette=terrain.colors,
                   plot.title={
                     title(xlab="Longitude",cex.lab=1)
                     mtext("Latitude",2,cex=1,line=3,las=0)
                     mtext(zlabel,4,cex=1,line=0.8,las=0)
                   })
  })
  
  
}

shinyApp(ui = ui, server = server)
