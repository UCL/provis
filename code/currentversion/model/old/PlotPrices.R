# TODO
# 1) add file_map* as optional inputs with default values
file_map0<-paste0(OutDir,"/map0_logprice0.pdf")  # predicted logprice0:  baseline 
file_map1<-paste0(OutDir,"/map1_logprice2.pdf")  # predicted logprice2:  new location price
file_map2<-paste0(OutDir,"/map2_logprice_new.pdf")


# Create map of predictions
resolution <- 0.01 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
i1<- is.na(m2data_new$location_value)
map0<-heatmap1(m2data_new$longitude,m2data_new$latitude,m2data_new$location_value,
               filter=!i1,resolution,
               "Baseline log location price",file_map0)

i1<-is.na(logprice2$fit[,1])
map1<-heatmap1(m2data_new$longitude,m2data_new$latitude,logprice2$fit[,1],filter=!i1,resolution,
               "Predicted log location price: Model 2",file_map1)
