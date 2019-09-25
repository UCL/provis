# Use caret machine learning package
library(caret)


i_location_value<-grep("location_value",colnames(m2data))
i_lu<-grep("\\<lu",colnames(m2data))[1:9]
i_plan<-grep("localplan",colnames(m2data))
i_pop<-grep("popdensityOA",colnames(m2data))
i_imd<-grep("imddecile",colnames(m2data))
i_flood<-grep("prob_4band",colnames(m2data))
i_noise<-grep("noise",colnames(m2data))
i_green<-grep("greenbelt",colnames(m2data))
i_drive<-grep("drive_",colnames(m2data))
i_trans<-grep("trans_",colnames(m2data))
i_coast<-grep("distance_coast",colnames(m2data))

icols<-c(i_location_value,i_lu,i_plan,i_pop,i_imd,i_flood,
         i_noise,i_green,i_drive,i_trans,i_coast)
traindata<-m2data[,icols]

m3.1<-train(location_value~.,
            data=traindata,
            method="lm",
            na.action=na.exclude)
