A3Model2_vlist0<-function(region=2) {
  spec<-"linear"
  
  if (spec=="linear") {
    if (region!=1 & region!=5) {
      # All regions excluding London and CAMKOX
      vlist1<-paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+greenbelt",
                     "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
                     "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
                     "+I(distance_airport>=10)",
                     "+I((distance_airport<10)*log(distance_airport))",
                     "+I(distance_motorway>=10)",
                     "+I((distance_motorway<10)*log(distance_motorway))",
                     "+I(distance_aroad>=10)",
                     "+I((distance_aroad<10)*log(distance_aroad))",
                     "+AONB",
                     "+I((drive_AONB>=30))",
                     "+I((drive_AONB<30)*log(1+drive_AONB))",
                     "+natpark",
                     "+I((drive_natpark>=30))",
                     "+I((drive_natpark<30)*log(1+drive_natpark))", 
                     "+I(distance_coast>=30)",
                     "+I((distance_coast<30)*log(distance_coast))",
                     "+I(drive_station>=30)",
                     "+I((drive_station<30)*log(1+drive_station))",
                     "+I((drive_town>=30))",
                     "+I((drive_town<30)*log(1+drive_town))")
    } else if (region==1) {
      # CAMKOX
      vlist1<-paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+greenbelt",
                     "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
                     "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
                     "+I(distance_airport>=10)",
                     "+I((distance_airport<10)*log(distance_airport))",
                     "+I(distance_motorway>=10)",
                     "+I((distance_motorway<10)*log(distance_motorway))",
                     "+I(distance_aroad>=10)",
                     "+I((distance_aroad<10)*log(distance_aroad))",
                     "+AONB",
                     "+I((drive_AONB>=30))",
                     "+I((drive_AONB<30)*log(1+drive_AONB))",
                     "+I(drive_station>=30)",
                     "+I((drive_station<30)*log(1+drive_station))",
                     "+I((drive_town>=30))",
                     "+I((drive_town<30)*log(1+drive_town))")
    } else if (region==5) {
      # London
      vlist1<-paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+greenbelt",
                     "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
                     "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
                     "+I(distance_airport>=5)",
                     "+I((distance_airport<5)*log(distance_airport))",
                     "+I(distance_motorway>=5)",
                     "+I((distance_motorway<5)*log(distance_motorway))",
                     "+I(distance_station)",
                     "+distance_tubestation",
                     "+I((drive_town>=21))",
                     "+I((drive_town<21)*log(1+drive_town))")
    } # end if (region!=1)
  } else if (spec=="quadratic") {
    
  if (region!=1 & region!=5) {
    # All regions excluding London and CAMKOX
    vlist1<-paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+greenbelt",
                 "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
                 "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
                "+I(distance_airport>=10)",
                "+I((distance_airport<10)*log(distance_airport))",
               "+I((distance_airport<10)*log(distance_airport)^2)",
               "+I(distance_motorway>=10)",
               "+I((distance_motorway<10)*log(distance_motorway))",
               "+I((distance_motorway<10)*log(distance_motorway)^2)",
               "+I(distance_aroad>=10)",
               "+I((distance_aroad<10)*log(distance_aroad))",
               "+I((distance_aroad<10)*log(distance_aroad)^2) ",
               "+AONB",
               "+I((drive_AONB>=30))",
               "+I((drive_AONB<30)*log(1+drive_AONB))",
               "+I((drive_AONB<30)*log(1+drive_AONB)^2)",
               "+natpark",
               "+I((drive_natpark>=30))",
               "+I((drive_natpark<30)*log(1+drive_natpark))", 
               "+I((drive_natpark<30)*log(1+drive_natpark)^2)",
               "+I(distance_coast>=30)",
               "+I((distance_coast<30)*log(distance_coast))",
               "+I((distance_coast<30)*log(distance_coast)^2)",
               "+I(drive_station>=30)",
               "+I((drive_station<30)*log(1+drive_station))",
               "+I((drive_station<30)*log(1+drive_station)^2)",
               "+I((drive_town>=30))",
               "+I((drive_town<30)*log(1+drive_town))",
               "+I((drive_town<30)*log(1+drive_town)^2)")
  } else if (region==1) {
    # CAMKOX
    vlist1<-paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+greenbelt",
                   "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
                   "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
                   "+I(distance_airport>=10)",
                   "+I((distance_airport<10)*log(distance_airport))",
                   "+I((distance_airport<10)*log(distance_airport)^2)",
                   "+I(distance_motorway>=10)",
                   "+I((distance_motorway<10)*log(distance_motorway))",
                   "+I((distance_motorway<10)*log(distance_motorway)^2)",
                   "+I(distance_aroad>=10)",
                   "+I((distance_aroad<10)*log(distance_aroad))",
                   "+I((distance_aroad<10)*log(distance_aroad)^2) ",
                   "+AONB",
                   "+I((drive_AONB>=30))",
                   "+I((drive_AONB<30)*log(1+drive_AONB))",
                   "+I((drive_AONB<30)*log(1+drive_AONB)^2)",
                   "+I(drive_station>=30)",
                   "+I((drive_station<30)*log(1+drive_station))",
                   "+I((drive_station<30)*log(1+drive_station)^2)",
                   "+I((drive_town>=30))",
                   "+I((drive_town<30)*log(1+drive_town))",
                   "+I((drive_town<30)*log(1+drive_town)^2)")
  } else if (region==5) {
  # London
  vlist1<-paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+greenbelt",
                 "+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr",
                 "+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass",
                 "+I(distance_airport>=10)",
                 "+I((distance_airport<10)*log(distance_airport))",
                 "+I((distance_airport<10)*log(distance_airport)^2)",
                 "+I(distance_motorway>=10)",
                 "+I((distance_motorway<10)*log(distance_motorway))",
                 "+I((distance_motorway<10)*log(distance_motorway)^2)",
                 "+I(distance_aroad>=10)",
                 "+I((distance_aroad<10)*log(distance_aroad))",
                 "+I((distance_aroad<10)*log(distance_aroad)^2) ",
                 "+I(drive_station>=30)",
                 "+I((drive_station<30)*log(1+drive_station))",
                 "+I((drive_station<30)*log(1+drive_station)^2)",
                 "+I((drive_town>=30))",
                 "+I((drive_town<30)*log(1+drive_town))",
                 "+I((drive_town<30)*log(1+drive_town)^2)")
} # end if (region!=1)
} # end if (spec=="linear")
  return(vlist1)
}