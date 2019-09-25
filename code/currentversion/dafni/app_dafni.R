library(knitr)
source("A0Setup.R")

input            <-list()
input$regionID   <- 2              # region id number:  options = list(1:11)
input$investType <- "settlement"   # investment type:   options = list("settlement","road","rail")
input$projectId  <- 1              # project id number

# Step 1: users chooses region
# page displays map of England and list of regions
# user selects one
# regionNames <- list("Cornwall and Devon","East Midlands","East of England","London","North East England",
#                    "North West England","South East England","South West England","West Midlands",
#                    "Yorkshire and the Humber","CaMKOx")
# regionID    <- c(2:11,1)

# Step 2: User chooses investment type
#  investTypes <- c("settlement","road","rail")
#  investNames <- c("settlement","road","public transport")
if (input$investType=="settlement") {
  # Step 3A: user chooses settlement details
  #         1) uploads kml file
  #         2) number of properties
  #         3) fraction commercial
  #         4) popdensity
  #         5) model_la
  input$mapfile            <- "kml/region2_settlement1.kml"    # name of user uploaded kml input file
  input$nproperties        <- 5000                             # options in range: 100 - 50,000
  input$fractionCommercial <- 0.05                             # options in range: 0.00 - 0.20 
  input$popdensity         <- 35                               # options in range: 20 - 70

  # User selects model LA
  load(paste0(RootDir,"/data/region_la_list.RData"))
  laOptions <- region_la_list[[region$id]]
  # display:   "Choose local authority to be used as statistical model."
  # user chooses i1 from list on line 105
  input$i1       <- 1                  # user chooses element from laOptions
  input$laModel <- laOptions[i1]
  
} else if (input$investType=="road") {
  # Step 3B: user chooses road investment details
  #          1) investment type
  #          2a) if type=="newroute"
  #              i) upload road kml file
  #              ii) average speed
  #          2b) if type=="newspeed"
  #              i) upload area kml file
  #              ii) speed multiplier
  input$methodType <- "newroute"               # options = list("newroute","newspeed")
  if (input$methodType=="newroute") {
    input$mapfile <- "kml/region2_road1.kml"   # user uploaded kml file defining road
    input$speed    <- 50                       # range from: 30 to 80
  } else if (input$methodType=="newspeed") {
    input$mapfile <- "kml/region2_area1.kml"   # user uploaded kml file defining geo. area
    input$speedMultiplier <- -10               # range from -40 to 20
  }
} else if (input$investType=="rail") {
  # Step 3C: user chooses rail investment details
  #          1) investment type
  #          2a) if type=="newroute"
  #              i) upload road kml file
  #              ii) average speed
  #          2b) if type=="newspeed"
  #              i) upload area kml file
  #              ii) speed multiplier
  input$methodType <- "newroute"                 # options = list("newroute","newspeed")
  if (input$methodType=="newroute") {
    # upload kml file
    input$mapfile <- "kml/region2_rail1.kml"     # user uploaded kml file defining rail line
    input$speed    <- 50                         # range from 30 to 80
  } else if (input$methodType=="newspeed") {
    input$mapfile   <- "kml/region2_area1.kml"   # user uploaded kml file defining geo area
    input$speedMultiplier <- -10                 # range from -40 to 20
  }
}  

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
  
  # project :   project details
  # page_id :   page number
  # runflag :   process status flags
  # etime          : elapsed time
  project       <- list(id=1,dir="www/pid1",travelmodelbasis="cheb",nhouses=9500,nbusinesses=500)
  region        <- list(id=2,str="CornwallDevon",name="Cornwall and Devon")
  outfile       <- list(report1=NULL,report2=NULL)
  etime         <- list(t0=0,t1=0)

if (input$investType=="settlement") {  
  #------------------------------------------------------------------------------------------------------ 
  # Event 3A: Computations for new settlement
  #------------------------------------------------------------------------------------------------------
  # set project id
  etime$t0<-Sys.time()
  etime$t1<-Sys.time()
  
  project$id          <- input$projectId
  # create directory for current session
  project$dir         <-paste0("www/pid",project$id)
  system(paste0("mkdir ",project$dir))
  project$nbusinesses <- round(input$nproperties*input$fractionCommercial)
  project$nhouses     <- input$nproperties - project$nbusinesses
      
  # save parameters that are needed by D1SettlementReport.Rnw
  parms<-list(project$id,region$id,project$nhouses,input$popdensity,input$laModel,
              input$mapfile,project$travelmodelbasis,project$nbusinesses)
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
  etime$t1<-Sys.time()
} else if (input$investType=="road") {
  #------------------------------------------------------------------------------------------------------
  # Event 3B: Computations for new road
  #------------------------------------------------------------------------------------------------------
  etime$t0<-Sys.time()
  etime$t1<-Sys.time()
    
  project$id<-input$projectId
  # create directory for current session
  project$dir<-paste0("www/pid",project$id)
  system(paste0("mkdir ",project$dir))

  # Save parameters needed by .Rnw file
  parms<-list(project$id,region$id,input$investType,input$methodType,
              input$mapfile,input$speed,input$speedMultiplier,
              project$travelmodelbasis)
  names(parms)<-c("project_id","region_id","infrastructure_type","method",
                  "mapfile","speed","multiplier","travelmodelbasis")
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
  if (input$methodType=="newroute") {
    texfile1<-paste0("region",as.character(region$id),"_road1_",as.character(project$id))
    inputfile<-"../../D2RoadReport.Rnw"
  } else if (input$methodType=="newspeed") {
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
  if (input$methodType=="newroute") {
    texfile2<-paste0("region",as.character(region$id),"_road2_",as.character(project$id))
    inputfile<-"../../E2RoadReport.Rnw"
  } else if (input$methodType=="newspeed") {
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
  etime$t1<-Sys.time()
} else if (input$investType=="rail") {
  #------------------------------------------------------------------------------------------------------
  # Event 3C: Computations for new rail
  #------------------------------------------------------------------------------------------------------
  etime$t0<-Sys.time()
  etime$t1<-Sys.time()

  project$id<-input$projectId
  # create directory for current session
  project$dir<-paste0("www/pid",project$id)
  system(paste0("mkdir ",project$dir))

  # Save parameters needed by .Rnw file
  parms<-list(project$id,region$id,input$investType,input$methodType,
              input$mapfile,input$speed,input$speedMultiplier,
              project$travelmodelbasis)
  names(parms)<-c("project_id","region_id","infrastructure_type","method",
                  "mapfile","speed","multiplier","travelmodelbasis")
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

  if (input$methodType=="newroute") {
    texfile1<-paste0("region",as.character(region$id),"_rail1_",as.character(project$id))
    inputfile<-"../../D4RailReport.Rnw"
  } else if (input$methodType=="newspeed") {
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
  if (input$methodType=="newroute") {
    texfile2<-paste0("region",as.character(region$id),"_rail2_",as.character(project$id))
    inputfile<-"../../E4RailReport.Rnw"
  } else if (input$methodType=="newspeed") {
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
    etime$t1<-Sys.time()
}

output<-list()  
# Generate text and other outputs
  # P1: SIDE BAR
output$sidetext1<-paste0("This tool allows you to predict the impact on property values of investments in ",
                         "roads, public transport or housing. Choose a region to begin.")
# Print region
output$sidetext2<-paste0("Current region is ",region$name,".")

# P1: MAIN PANEL
output$maintext1<-"Below is a map of UK regions for which data is available."
output$ukmap <- "regions.png"

# P2:   MAIN PANEL
output$maintext2<-paste0("EXPERIMENTAL VERSION. Below is a map of the ",region$name," region. Choose investment characteristics at ",
           "left. Then click at the bottom left to compute prices. After clicking you will have ",
           "to wait 3-5 minutes for the computation to complete. When the computation is complete",
           "a new window will open. Then, click to display the results. To start over, click",
           "Start over at the far bottom left.")  

output$regoinmap <- paste0("region",region$id,".png")

output$mapinstructions<-"HowToDrawRoad.pdf"

# P3: MAIN PANEL
if (input$investType=="settlement") {
  output$maintext3<-paste0("Computing prices. The server is constructing a database of ", 
             input$nproperties," properties, matching the new properties to geographic information, ",
             "and predicting residential prices and commercial rateable values.",
             "This process usually takes 3-8 minutes. ",
             "The screen will refresh when the computation is complete.")
} else {
  output$maintext3<-paste0("Computing prices. The server is predicting new travel times for the database of properties ",
            "and is predicting residential prices and commercial rateable values. ",
            "This process usually takes 1-3 minutes. The screen will refresh when the computation is complete.")
}

output$maintext4<-paste0("Price computation complete. Elapsed time = ",as.character(round(etime$t1-etime$t0))," seconds.")

output$startTime<-paste0("Start time: ",etime$t0)  
output$currentTime<-paste0("Current time: ",Sys.time())

# Display report on web
output$report1 <-outfile$report1
output$report2 <-outfile$report2
