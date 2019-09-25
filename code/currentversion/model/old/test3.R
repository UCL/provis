#m2data$xdrive_DBY1<- m2data$drive_DBY*(m2data$rankdrive_DBY==1)*(m2data$rankdrive_DBY==2)
#                         *(rankdrive_DBY==3))   

attach(m2data)

str5a<-paste0("location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate",
              "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
              "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
              "+greenbelt+I((distance_coast<20)*log(distance_coast))",
              "+log(drive_station)+I(log(drive_station)^2)",
              "+I((drive_town<30)*log(drive_town))+I((distance_motorway<20)*log(distance_motorway))",
              "+I((drive_aroad<30)*log(drive_aroad))",
              "+I((distance_EMA<20))",
              "+I(distance_PBH<20)+I(distance_SFD<20)")

str5b<-paste0("+rankdrive+ranktrans",
              "+I(drive_COV*(rankdrive_COV==1))+I(drive_COV*(rankdrive_COV==2))",
              "+I(drive_COV*(rankdrive_COV==3))",
              "+I(drive_DBY*(rankdrive_DBY==1))+I(drive_DBY*(rankdrive_DBY==2))",
              "+I(drive_DBY*(rankdrive_DBY==3))",
              "+I(drive_GHM*(rankdrive_GHM==1))+I(drive_GHM*(rankdrive_GHM==2))",
              "+I(drive_GHM*(rankdrive_GHM==3))", 
              "+I(drive_LEI*(rankdrive_LEI==1))+I(drive_LEI*(rankdrive_LEI==2))",
              "+I(drive_LEI*(rankdrive_LEI==3))",   
              "+I(drive_LIN*(rankdrive_LIN==1))+I(drive_LIN*(rankdrive_LIN==2))",
              "+I(drive_LIN*(rankdrive_LIN==3))",   
              "+I(drive_NHN*(rankdrive_NHN==1))+I(drive_NHN*(rankdrive_NHN==2))",
              "+I(drive_NHN*(rankdrive_NHN==3))",   
              "+I(drive_NOT*(rankdrive_NOT==1))+I(drive_NOT*(rankdrive_NOT==2))",
              "+I(drive_NOT*(rankdrive_NOT==3))",   
              "+I(drive_PBH*(rankdrive_PBH==1))+I(drive_PBH*(rankdrive_PBH==2))",
              "+I(drive_PBH*(rankdrive_PBH==3))",   
              "+I(drive_SHF*(rankdrive_SHF==1))+I(drive_SHF*(rankdrive_SHF==2))",
              "+I(drive_SHF*(rankdrive_SHF==3))",
              "+I(drive_SFD*(rankdrive_SFD==1))+I(drive_SFD*(rankdrive_SFD==2))",
              "+I(drive_SFD*(rankdrive_SFD==3))")
str5c<-paste0("+I((rankdrive_COV==1)*drive_COV^2)+I((rankdrive_COV==2)*drive_COV^2)",
              "+I((rankdrive_LEI==1)*drive_LEI^2)+I((rankdrive_LEI==2)*drive_LEI^2)",
              "+I((rankdrive_NOT==1)*drive_NOT^2)+I((rankdrive_NOT==2)*drive_NOT^2)",
              "+I(trans_NOT^2)")
str5d<-paste0("+trans_BIR+trans_LEI+trans_LIN",
              "+trans_NOT+trans_PBH+trans_SHF")
formula5<-as.formula(paste0(str5a,str5b,str5d,str5c))
m2ols5<-lm(formula5,data=m2data,na.action=na.exclude)
summary(m2ols5)
p5<-predict(m2ols5,na.action=na.exclude)
e5<-m2data$location_value-p5

heatmap1(m2data$longitude,m2data$latitude,e5,filter=!is.na(e5),resolution=0.01,zlabel="e5",outfile="e5.pdf")

