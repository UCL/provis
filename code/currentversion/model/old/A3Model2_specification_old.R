#' Create formula for model 5
#' @param region   region
#' @return formula5
#' @examples 
#' formula5 <- A3Model2_specification(region)
#' 
A3Model2_specification<-function(region) {

if (region==2) {  
# Region 2: Model 5 specification   
  vlist1<-paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate",
              "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
              "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
              "+I((distance_aroad<0.5)*log(distance_aroad))+I((distance_aroad<0.5)*log(distance_aroad)^2)",
              "+I((distance_motorway<10)*log(distance_motorway))+I((distance_motorway<10)*log(distance_motorway)^2)",
              "+AONB+I((drive_AONB<30)*log(1+drive_AONB))+I((drive_AONB<30)*log(1+drive_AONB)^2)",
              "+natpark",
              "+I((distance_coast<3)*log(distance_coast))+I((distance_coast<3)*log(distance_coast)^2)",
              "+I((drive_station<30)*log(drive_station))+I((drive_station<30)*log(drive_station)^2)")
  vlist2<-paste0("+rankdrive+ranktrans",
              "+I(drive_BSP*(rankdrive_BSP==1))+I(drive_BSP*(rankdrive_BSP==2))",
              "+I(drive_BSP*(rankdrive_BSP==3))",
              "+I(drive_EXE*(rankdrive_EXE==1))+I(drive_EXE*(rankdrive_EXE==2))",
              "+I(drive_EXE*(rankdrive_EXE==3))",
              "+I(drive_MHD*(rankdrive_MHD==1))+I(drive_MHD*(rankdrive_MHD==2))",
              "+I(drive_MHD*(rankdrive_MHD==3))",
              "+I(drive_PAD*(rankdrive_PAD==1))+I(drive_PAD*(rankdrive_PAD==2))",
              "+I(drive_PAD*(rankdrive_PAD==3))",
              "+I(drive_PNZ*(rankdrive_PNZ==1))+I(drive_PNZ*(rankdrive_PNZ==2))",
              "+I(drive_PNZ*(rankdrive_PNZ==3))",   
              "+I(drive_PLY*(rankdrive_PLY==1)*(drive_PLY<30))+I(drive_PLY*(rankdrive_PLY==2))",
              "+I(drive_PLY*(rankdrive_PLY==3))",
              "+I(drive_STA*(drive_STA<30)*(rankdrive_STA==1))+I(drive_STA*(drive_STA<30)*(rankdrive_STA==2))",
              "+I(drive_SIV*(rankdrive_SIV==1))+I(drive_SIV*(rankdrive_SIV==2))",
              "+I(drive_SIV*(rankdrive_SIV==3))",
              "+I(drive_TBY*(rankdrive_TBY==1))+I(drive_TBY*(rankdrive_TBY==2))",
              "+I(drive_TBY*(rankdrive_TBY==3))")
  vlist3<-paste0("+trans_BSP+trans_EXE+",
               "+trans_PNZ+trans_PLY+trans_SIV",
               "+trans_TAU+trans_TBY+trans_TRU")
  vlist4<-paste0("+I((rankdrive_BSP==1)*drive_BSP^2)+I((rankdrive_BSP==2)*drive_BSP^2)",
               "+I((rankdrive_EXE==1)*drive_EXE^2)+I((rankdrive_EXE==2)*drive_EXE^2)",
               "+I((rankdrive_PNZ==1)*drive_PNZ^2)+I((rankdrive_PNZ==2)*drive_PNZ^2)",
               "+I((rankdrive_TBY==1)*drive_TBY^2)+I((rankdrive_TBY==2)*drive_TBY^2)",
              "+I((rankdrive_PLY==1)*(drive_PLY<30)*drive_PLY^2)+I((rankdrive_PLY==2)*drive_PLY^2)",
              "+I((rankdrive_STA==1)*(drive_STA<30)*drive_STA^2)+I((rankdrive_STA==2)*(drive_STA<30)*drive_STA^2)")
  vlist5<-paste0("+I(trans_BSP^2)+I(trans_EXE^2)+",
               "+I(trans_PNZ^2)+I(trans_PLY^2)+I(trans_SIV^2)",
               "+I(trans_TAU^2)+I(trans_TBY^2)+I(trans_TRU^2)")
  formula5<-as.formula(paste0(vlist1,vlist2,vlist3,vlist4,vlist5))
  return(formula5)
} # if (region==2) { 
} # end of function
