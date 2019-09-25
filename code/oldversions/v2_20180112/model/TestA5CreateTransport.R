# Test A5CreateTransport
source(paste0(CodeDir,"/model/A5CreateTransport.R"))
source(paste0(CodeDir,"/model/A6PredictPrice.R"))
source(paste0(CodeDir,"/model/A7NewTransportRoute.R"))

infrastructure_type <- "road"      # "road" or "rail"
method              <- "newroute"  # "newroute" or "newspeed"
multiplier          <- 0.9
speed               <- 50 # miles per hour
routefile           <- "region2_road1.kml"

newdata<-A5CreateTransport(region_id,infrastructure_type,method,routefile,multiplier,speed,dirs)

out0<-A6PredictPrice(newdata$m1data_new,newdata$m2data_old,dirs$outdir) 
out1<-A6PredictPrice(newdata$m1data_new,newdata$m2data_new,dirs$outdir) 
