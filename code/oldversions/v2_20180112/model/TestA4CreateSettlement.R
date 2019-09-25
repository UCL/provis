source(paste0(CodeDir,"/model/A4CreateSettlement.R"))
source(paste0(CodeDir,"/model/A6PredictPrice.R"))

# Names of files in NewDataDir
files<-list.files(dirs$newdatadir)
filename<-files[1]

# Extract and capitalise town name
town<-gsub("_new.csv","",filename)
town<-paste0(toupper(substr(town,1,1)),substr(town,2,nchar(town)))

title<-c("Summary statistics:  Old location",
         paste0("Summary statistics:  New ",town))
outfile<-paste0("summary_statistics_",tolower(town),".tex")
newdata<-A4CreateSettlement(region_id,dirs,filename,title,outfile)
out0<-A6PredictPrice(newdata$m1data_old,newdata$m2data_new,dirs$outdir)
out1<-A6PredictPrice(newdata$m1data_new,newdata$m2data_new,dirs$outdir)
