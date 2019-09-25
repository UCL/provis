#m2data$xdrive_DBY1<- m2data$drive_DBY*(m2data$rankdrive_DBY==1)*(m2data$rankdrive_DBY==2)
#                         *(rankdrive_DBY==3))   

attach(m2data)

#OK  "+I((drive_coast<30)*log(drive_coast))+I((drive_coast<30)*log(drive_coast)^2)",
#OK "+I((distance_coast<30)*log(distance_coast))+I((distance_coast<30)*log(distance_coast)^2)",
#like 1 "+I((drive_coast<30)*drive_coast)+I((drive_coast<30)*drive_coast^2)",
#"+I((distance_coast<30)*distance_coast)+I((distance_coast<30)*distance_coast^2)",

str5a<-paste0("location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate",
              "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
              "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
              "+greenbelt",
              "+I((drive_coast<30)*log(drive_coast))+I((drive_coast<30)*log(drive_coast)^2)",
              "+I((drive_station<30)*log(drive_station))+I((drive_station<30)*log(drive_station)^2)",
              "+I((drive_town<30)*log(drive_town))+I((drive_town<30)*log(drive_town)^2)",
              "+I((distance_motorway<30)*log(distance_motorway))+I((distance_motorway<30)*log(distance_motorway)^2)",
              "+I((distance_aroad<30)*log(distance_aroad))+I((distance_aroad<30)*log(distance_aroad)^2)",
              "+I((distance_STD<30)*log(distance_STD))+I((distance_STD<30)*log(distance_STD)^2)",
              "+I((distance_LUT_AIR<30)*log(distance_LUT_AIR))+I((distance_LUT_AIR<30)*log(distance_LUT_AIR)^2)")

str5b<-paste0("+rankdrive",
              "+I(drive_CAM*(rankdrive_CAM==1))+I(drive_CAM*(rankdrive_CAM==2))",
              "+I(drive_CAM*(rankdrive_CAM==3))",
              "+I(drive_COL*(rankdrive_COL==1))+I(drive_COL*(rankdrive_COL==2))",
              "+I(drive_COL*(rankdrive_COL==3))",
              "+I(drive_NWH*(rankdrive_NWH==1))+I(drive_NWH*(rankdrive_NWH==2))",
              "+I(drive_NWH*(rankdrive_NWH==3))",
              "+I(drive_LUT*(rankdrive_LUT==1))+I(drive_LUT*(rankdrive_LUT==2))",
              "+I(drive_LUT*(rankdrive_LUT==3))")
#,
#              "+I(drive_PBH*(rankdrive_PBH==1))+I(drive_PBH*(rankdrive_PBH==2))",
#              "+I(drive_PBH*(rankdrive_PBH==3))",
#              "+I(drive_IPS*(rankdrive_IPS==1))+I(drive_IPS*(rankdrive_IPS==2))",
#              "+I(drive_IPS*(rankdrive_IPS==3))")
str5c<-paste0("+trans_CAM",
              "+trans_COL+I((ranktrans_NWH==1)*trans_NWH)",
              "+trans_LON+I(trans_CAM*trans_LON)")
str5d<-paste0("+I((rankdrive_CAM==1)*drive_CAM^2)+I((rankdrive_CAM==2)*drive_CAM^2)",
              "+I((rankdrive_COL==1)*drive_COL^2)+I((rankdrive_COL==2)*drive_COL^2)",
              "+I((rankdrive_NWH==1)*drive_NWH^2)+I((rankdrive_NWH==2)*drive_NWH^2)",
              "+I((rankdrive_LUT==1)*drive_LUT^2)+I((rankdrive_LUT==2)*drive_LUT^2)")
#,
#              "+I((rankdrive_PBH==1)*drive_PBH^2)+I((rankdrive_PBH==2)*drive_PBH^2)",
#              "+I((rankdrive_IPS==1)*drive_IPS^2)+I((rankdrive_IPS==2)*drive_IPS^2)")
formula5<-as.formula(paste0(str5a,str5b,str5c,str5d))

m2ols5<-lm(formula5,data=m2data,na.action=na.exclude)
summary(m2ols5)
p5<-predict(m2ols5,na.action=na.exclude)
e5<-m2data$location_value-p5

heatmap1(m2data$longitude,m2data$latitude,e5,filter=!is.na(e5),resolution=0.01,zlabel="e5",outfile="e5.pdf")

# 1) logs vs levels
# 2) distance vs drive time
str5a<-paste0("location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate",
              "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
              "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
              "+greenbelt",
              "+I((distance_coast<30)*distance_coast)+I((distance_coast<30)*distance_coast^2)",
              "+AONB+drive_AONB+I(drive_AONB^2)",
              "+drive_station+I(drive_station^2)",
              "+I((drive_town<30)*drive_town)+I((drive_town<30)*drive_town^2)",
              "+I((distance_motorway<30)*distance_motorway)+I((distance_motorway<30)*distance_motorway^2)",
              "+I((distance_aroad<30)*distance_aroad)+I((distance_aroad<30)*distance_aroad^2)",
              "+I((distance_STD<20)*log(distance_STD))")

