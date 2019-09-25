B2SetPath<-function(RootDir,CodeDir,region_id=0,datastub="m11") {

  # Assumptions
  #   m11: domestic
  #     datadir    = RootDir/data/region_"region_id"
  #     newdatadir = RootDir/newdata/region_"region_id"
  #     outdir     = CodeDir/output/region_"region_id"
  #   nondom: non-domestic
  #     datadir    = RootDir/data/nondom/region_"region_id"
  #     newdatadir = RootDir/newdata/nondom/regino_"region_id"
  #     outdir     = CodeDir/output/nondom/region_"region_id"
  #   mapdir     = RootDir/data/maps
  #   traveldir  = RootDir/data/TravelTime/results
  #   wwwdir     = CodeDir/shiny/www
  #   farmdir    = RootDir/data/RICS
  #   
  if (region_id>0 & datastub=="m11") {
    # domestic  
    DataDir<-paste0(RootDir,"/data/region",as.character(region_id))
  } else if (region_id>0 & datastub=="nondom") {
    # non-domestic  
    DataDir<-paste0(RootDir,"/data/nondom/region",as.character(region_id))
  } else if (region_id==0) {
    DataDir<-paste0(RootDir,"/data")
  }  
  if (datastub=="m11") {
    NewDataDir<-paste0(RootDir,"/newdata/region",as.character(region_id))
  } else if (datastub=="nondom") {
    NewDataDir<-paste0(RootDir,"/newdata/nondom/region",as.character(region_id))
  }
  MapDir<-paste0(RootDir,"/data/maps")

  TravelDir<-paste0(RootDir,"/data/TravelTime/results")
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

  if (!file.exists(paste0(RootDir,"/newdata"))) {
    dir.create(paste0(RootDir,"/newdata"))  
  }
  if (!file.exists(paste0(RootDir,"/newdata/nondom"))) {
    dir.create(paste0(RootDir,"/newdata/nondom"))  
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
