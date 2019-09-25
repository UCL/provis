host<-system("hostname",intern=TRUE)
#browser()
if (host=="dh-230-mac.econ.ucl.ac.uk") {
  RootDir<-"/Users/uctpln0/Documents/research/hedonic/NIC"
} else if (host=="rserver.econ.ucl.ac.uk") {
  RootDir<-"/srv/shiny-server"
} else if (host=="minelava") {
  RootDir<-"C:/a/research/hedonic/NIC"
} else if (host=="DH-G06-03") {
  RootDir<-"U:/NICProject"
} else if (host=="jake.local" | host=="vic.local") {
  RootDir<-"/home/uctpln0/hedonic/NIC"
} else if (host=="polly's house") {
  RootDir<-"C:/.../NIC"
} else {
  info_sys<-Sys.info()
  user<-info_sys["user"]
  if (user=="uctpln0") {
    RootDir<-"/home/uctpln0/hedonic/NIC"
  } else {
    RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
  }
}
CodeDir<-paste0(RootDir,"/code/currentversion")
if (host=="rserver.econ.ucl.ac.uk") {
  CodeDir<-paste0(RootDir,"/LVU1")
}

source(paste0(CodeDir,"/model/B1LoadLibrary.R"))
source(paste0(CodeDir,"/model/B2SetPath.R"))
source(paste0(CodeDir,"/model/B3CreateDestinations.R"))
source(paste0(CodeDir,"/model/B4GetVarList.R"))
source(paste0(CodeDir,"/model/A1PredictValue.R"))
source(paste0(CodeDir,"/model/A1Plot.R"))
source(paste0(CodeDir,"/model/A1NewUseCode.R"))

source(paste0(CodeDir,"/model/A2Model2_newvariables.R"))
source(paste0(CodeDir,"/model/A3Model2_specification0.R"))
source(paste0(CodeDir,"/model/A3Model2_specification1.R"))
source(paste0(CodeDir,"/model/A3Model2_vlist0.R"))
source(paste0(CodeDir,"/model/A3Model2_vlist1.R"))

source(paste0(CodeDir,"/model/A4CreateSettlement.R"))
source(paste0(CodeDir,"/model/A5CreateTransport.R"))
source(paste0(CodeDir,"/model/A6PredictPrice.R"))
source(paste0(CodeDir,"/model/A7NewTransportRoute.R"))
source(paste0(CodeDir,"/model/C1CreateSettlement.R"))
source(paste0(CodeDir,"/model/F1CreateNondom.R"))
source(paste0(CodeDir,"/model/F2CreateNondom.R"))


