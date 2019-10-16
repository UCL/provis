B2SetPath<-function(RootDir,CodeDir,DataRoot,region_id=0,datastub="m11") {

  # Assumptions
  #   m11: domestic
  #     datadir    = DataRoot/data/region_"region_id"
  #     newdatadir = DataRoot/newdata/region_"region_id"
  #     outdir     = CodeDir/output/region_"region_id"
  #   nondom: non-domestic
  #     datadir    = DataRoot/data/nondom/region_"region_id"
  #     newdatadir = DataRoot/newdata/nondom/regino_"region_id"
  #     outdir     = CodeDir/output/nondom/region_"region_id"
  #   mapdir     = DataRoot/data/maps
  #   traveldir  = DataRoot/data/TravelTime/results
  #   wwwdir     = CodeDir/shiny/www
  #   farmdir    = DataRoot/data/RICS
  #   
  if (region_id>0 & datastub=="m11") {
    # domestic  
    DataDir<-paste0(DataRoot,"/data/region",as.character(region_id))
  } else if (region_id>0 & datastub=="nondom") {
    # non-domestic  
    DataDir<-paste0(DataRoot,"/data/nondom/region",as.character(region_id))
  } else if (region_id==0) {
    DataDir<-paste0(DataRoot,"/data")
  }  
  if (datastub=="m11") {
    NewDataDir<-paste0(DataRoot,"/newdata/region",as.character(region_id))
  } else if (datastub=="nondom") {
    NewDataDir<-paste0(DataRoot,"/newdata/nondom/region",as.character(region_id))
  }
  MapDir<-paste0(DataRoot,"/data/maps")

  TravelDir<-paste0(DataRoot,"/data/TravelTime/results")
  if (region_id>0 & datastub=="m11") {
    OutDir<-paste0(CodeDir,"/output/region",as.character(region_id))
  } else if (region_id>0 & datastub=="nondom") {
    OutDir<-paste0(CodeDir,"/output/nondom/region",as.character(region_id))
  } else if (region_id==0) {
    OutDir<-paste0(CodeDir,"/output")
  }

  if (!file.exists(paste0(CodeDir,"/output"))) {
    dir.create(paste0(CodeDir,"/output"))  
  }
  if (!file.exists(paste0(CodeDir,"/output/nondom"))) {
    dir.create(paste0(CodeDir,"/output/nondom"))  
  }
  
  if (!file.exists(OutDir)){
    dir.create(OutDir)
  }

  if (!file.exists(paste0(DataRoot,"/newdata"))) {
    dir.create(paste0(DataRoot,"/newdata"))  
  }
  if (!file.exists(paste0(DataRoot,"/newdata/nondom"))) {
    dir.create(paste0(DataRoot,"/newdata/nondom"))  
  }
  
  if (!file.exists(NewDataDir)){
    dir.create(NewDataDir)
  }

  # directory for pdf's for use by shiny
  wwwdir <- paste0(CodeDir,"/shiny/www")
  
  # RICS data direcory
  farmdir<-paste0(DataRoot,"/data/RICS")
  
  roaddir<-paste0(DataRoot,"/data/roads")
  raildir<-paste0(DataRoot,"/data/rail")
  riverdir<-paste0(DataRoot,"/data/rivers")
  # AONB = "Area of Outstanding Natural Beauty"
  AONBdir <-paste0(DataRoot,"/data/AONB.gdb")
  # national park directory
  parkdir <- paste0(DataRoot,"/data/nationalparks")
  
  # Put all directories in a single list
  dirs<-list(RootDir,DataDir,NewDataDir,MapDir,TravelDir,OutDir,
            wwwdir,farmdir,roaddir,raildir,riverdir,AONBdir,parkdir)
  names(dirs)<-c("rootdir",
                 "datadir",
                 "newdatadir",
                 "mapdir",
                 "traveldir",
                 "outdir",
                 "wwwdir",
                 "farmdir",
                 "roaddir",
                 "raildir",
                 "riverdir",
                 "AONBdir", 
                 "parkdir")
  return(dirs)
}
