B2SetPath<-function(RootDir,CodeDir,region_id=0) {

  # Assumptions
  #   datadir    = RootDir/data/region_"region_id"
  #   newdatadir = datadir/settlements
  #   mapdir     = RootDir/data/maps
  #   traveldir  = RootDir/data/TravelTime/results
  #   outdir     = CodeDir/output
  #   wwwdir     = CodeDir/shiny/www
  #   farmdir    = RootDir/data/RICS
  #   
  if (missing(region_id) | is.null(region_id)) {
    region_id=0
  }
  if (region_id>0) {
    DataDir<-paste0(RootDir,"/data1/region",as.character(region_id))
  } else if (region_id==0) {
    DataDir<-paste0(RootDir,"/data1")
  }  
  NewDataDir<-paste0(RootDir,"/newdata1/region",as.character(region_id))
  MapDir<-paste0(RootDir,"/data/maps")

  TravelDir<-paste0(RootDir,"/data/TravelTime/results")
  if (region_id>0) {
    OutDir<-paste0(CodeDir,"/output/region",as.character(region_id))
  } else if (region_id==0) {
    OutDir<-paste0(CodeDir,"/output")
  }

  if (!file.exists(paste0(CodeDir,"/output"))) {
    dir.create(paste0(CodeDir,"/output"))  
  }
  
  if (!file.exists(OutDir)){
    dir.create(OutDir)
  }

  if (!file.exists(NewDataDir)){
    dir.create(NewDataDir)
  }

  # directory for pdf's for use by shiny
  wwwdir <- paste0(CodeDir,"/shiny/www")
  
  # RICS data direcory
  farmdir<-paste0(RootDir,"/data/RICS")
  
  roaddir<-paste0(RootDir,"/data/roads")
  riverdir<-paste0(RootDir,"/data/rivers")
  # AONB = "Area of Outstanding Natural Beauty"
  AONBdir <-paste0(RootDir,"/data/AONB.gdb")
  
  # Put all directories in a single list
  dirs<-list(RootDir,DataDir,NewDataDir,MapDir,TravelDir,OutDir,
            wwwdir,farmdir,roaddir,riverdir,AONBdir)
  names(dirs)<-c("rootdir",
                 "datadir",
                 "newdatadir",
                 "mapdir",
                 "traveldir",
                 "outdir",
                 "wwwdir",
                 "farmdir",
                 "roaddir",
                 "riverdir",
                 "AONBdir")
  

  return(dirs)
}
