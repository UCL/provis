#' Create formula for model 5: using avgtime to destinations
#' @param region   region
#' @return formula5
#' @examples 
#' formula5 <- A3Model2_specification1(region)
#' 
A3Model2_specification1<-function(region,datastub="m11") {
  vlist1<-A3Model2_vlist1(region,datastub)

  vlist2 <- "+rankavgtime"
  avgtimevar<- colnames(m2data[,grep("\\<avgtime_[^cts]",colnames(m2data))])
  ranktimevar<- colnames(m2data[,grep("\\<rankavgtime_[^cts]",colnames(m2data))])
  for (i1 in 1:length(avgtimevar)) {
    for (i2 in 1:3) {
      t1 <- paste0("I(",avgtimevar[i1],"*(",ranktimevar[i1],"==",i2,"))")
      vlist2<-paste(vlist2,t1,sep="+")
    }
  }
  
if (region==1) {
  # Region 1: Model 5 specification 

  #vlist2<-paste0("+rankavgtime",
  #               "+I(avgtime_CAM*(rankavgtime_CAM==1))",
  #               "+I(avgtime_CAM*(rankavgtime_CAM==2))",
  #               "+I(avgtime_CAM*(rankavgtime_CAM==3))",
  #               "+I(avgtime_OX*(rankavgtime_OX==1))+I(avgtime_OX*(rankavgtime_OX==2))",
  #               "+I(avgtime_OX*(rankavgtime_OX==3))",
  #               "+I(avgtime_LON*(rankavgtime_LON==1))+I(avgtime_LON*(rankavgtime_LON==2))",
  #               "+I(avgtime_LON*(rankavgtime_LON==3))")
} else if (region==2) {  
  # Region 2: Model 5 specification   
  vlist2 <- "+rankavgtime"
  avgtimevar<- colnames(m2data[,grep("\\<avgtime_[^cts]",colnames(m2data))])
  ranktimevar<- colnames(m2data[,grep("\\<rankavgtime_[^cts]",colnames(m2data))])
  for (i1 in 1:length(avgtimevar)) {
    for (i2 in 1:2) {
      t1 <- paste0("I(",avgtimevar[i1],"*(",ranktimevar[i1],"==",i2,"))")
      vlist2<-paste(vlist2,t1,sep="+")
    }
  }
  
  #vlist2<-paste0("+rankavgtime",
  #            "+I(avgtime_FAL*(rankavgtime_FAL==1))+I(avgtime_FAL*(rankavgtime_FAL==2))",
  #            "+I(avgtime_FAL*(rankavgtime_FAL==3))",
  #            "-I(avgtime_GTR*(rankavgtime_GTR==1))-I(avgtime_GTR*(rankavgtime_GTR==2))",
  #            "-I(avgtime_GTR*(rankavgtime_GTR==3))",
  #            "+I(avgtime_KND*(rankavgtime_KND==1))+I(avgtime_KND*(rankavgtime_KND==2))",
  #            "+I(avgtime_NQY*(rankavgtime_NQY==1))+I(avgtime_NQY*(rankavgtime_NQY==2))",
  #            "+I(avgtime_KND*(rankavgtime_KND==3))",
  #            "+I(avgtime_NQY*(rankavgtime_NQY==3))",
  #            "+I(avgtime_PAD*(rankavgtime_PAD==1))+I(avgtime_PAD*(rankavgtime_PAD==2))",
  #            "+I(avgtime_PAD*(rankavgtime_PAD==3))",
  #            "+I(avgtime_PNZ*(rankavgtime_PNZ==1))+I(avgtime_PNZ*(rankavgtime_PNZ==2))",
  #            "+I(avgtime_PNZ*(rankavgtime_PNZ==3))",   
  #            "+I(avgtime_SIV*(rankavgtime_SIV==1))+I(avgtime_SIV*(rankavgtime_SIV==2))",
  #            "+I(avgtime_SIV*(rankavgtime_SIV==3))",
  #            "+I(avgtime_SAL*(rankavgtime_SAL==1))+I(avgtime_SAL*(rankavgtime_SAL==2))",
  #            "+I(avgtime_SAL*(rankavgtime_SAL==3))",
  #            "+I(avgtime_TOP*(rankavgtime_TOP==1))+I(avgtime_TOP*(rankavgtime_TOP==2))",
  #            "+I(avgtime_TOP*(rankavgtime_TOP==3))",
  #            "+I(avgtime_WHI*(rankavgtime_WHI==1))+I(avgtime_WHI*(rankavgtime_WHI==2))",
  #            "+I(avgtime_WHI*(rankavgtime_WHI==3))")
} else if (region==3) {
#  vlist2<-paste0("+rankavgtime",
#                 "+I(avgtime_DAV*(rankavgtime_DAV==1))",
#                 "+I(avgtime_DAV*(rankavgtime_DAV==2))",
#                 "+I(avgtime_DAV*(rankavgtime_DAV==3))",
#                 "+I(avgtime_EYM*(rankavgtime_EYM==1))",
#                 "+I(avgtime_EYM*(rankavgtime_EYM==2))",
#                 "+I(avgtime_EYM*(rankavgtime_EYM==3))",
#                 "+I(avgtime_NHN*(rankavgtime_NHN==1))",
##                 "+I(avgtime_NHN*(rankavgtime_NHN==2))",
#                 "+I(avgtime_NHN*(rankavgtime_NHN==3))",
#                 "+I(avgtime_OAD*(rankavgtime_OAD==1))",
#                 "+I(avgtime_OAD*(rankavgtime_OAD==2))",
#                 "+I(avgtime_OAD*(rankavgtime_OAD==3))",
#                 "+I(avgtime_SFD*(rankavgtime_SFD==1))",
#                 "+I(avgtime_SFD*(rankavgtime_SFD==2))",
#                 "+I(avgtime_SFD*(rankavgtime_SFD==3))",
#                 "+I(avgtime_WBR*(rankavgtime_WBR==1))",
#                 "+I(avgtime_WBR*(rankavgtime_WBR==2))",
##                 "+I(avgtime_WBR*(rankavgtime_WBR==3))")

} else if (region==4) {
  #vlist2<-paste0("+rankavgtime",
  #               "+I(avgtime_CAM*(rankavgtime_CAM==1))+I(avgtime_CAM*(rankavgtime_CAM==2))",
  #               "+I(avgtime_CAM*(rankavgtime_CAM==3))",
  #               "+I(avgtime_SOS*(rankavgtime_SOS==1))+I(avgtime_SOS*(rankavgtime_SOS==2))",
  #               "+I(avgtime_SOS*(rankavgtime_SOS==3))",
  #               "+I(avgtime_NWH*(rankavgtime_NWH==1))+I(avgtime_NWH*(rankavgtime_NWH==2))",
  ##               "+I(avgtime_NWH*(rankavgtime_NWH==3))",   
  #               "+I(avgtime_COL*(rankavgtime_COL==1))+I(avgtime_COL*(rankavgtime_COL==2))",
  #               "+I(avgtime_COL*(rankavgtime_COL==3))",
  #               "+I(avgtime_LON*(rankavgtime_LON==1))+I(avgtime_LON*(rankavgtime_LON==2))",
  #               "+I(avgtime_LON*(rankavgtime_LON==3))")
} else if (region==5) {

} else if (region==6 & datastub=="m11") {
  vlist2<-paste0("+rankavgtime",
                 "+I(avgtime_ANN*(rankavgtime_ANN==1)*(avgtime_ANN<50))",
                 "+I(avgtime_ANN*(rankavgtime_ANN==2)*(avgtime_ANN<50))",
                 "+I(avgtime_ANN*(rankavgtime_ANN==3)*(avgtime_ANN<50))",
                 "+I(avgtime_COR*(rankavgtime_COR==1)*(avgtime_COR<50))",
                 "+I(avgtime_COR*(rankavgtime_COR==2)*(avgtime_COR<50))",
                 "+I(avgtime_COR*(rankavgtime_COR==3)*(avgtime_COR<50))",
                 "+I(avgtime_CRA*(rankavgtime_CRA==1))+I(avgtime_CRA*(rankavgtime_CRA==2))",
                 "+I(avgtime_CRA*(rankavgtime_CRA==3))",
                 "+I(avgtime_DAR*(rankavgtime_DAR==1))+I(avgtime_DAR*(rankavgtime_DAR==2))",
                 "+I(avgtime_DAR*(rankavgtime_DAR==3))",
                 "+I(avgtime_NUN*(rankavgtime_NUN==1))+I(avgtime_NUN*(rankavgtime_NUN==2))",
                 "+I(avgtime_NUN*(rankavgtime_NUN==3))")
} else if (region==7 & datastub=="m11") {
  vlist2<-paste0("+rankavgtime",
                 "+I(avgtime_BUR*(rankavgtime_BUR==1))+I(avgtime_BUR*(rankavgtime_BUR==2))",
                 "+I(avgtime_BUR*(rankavgtime_BUR==3))",
                 "+I(avgtime_SEA*(rankavgtime_SEA==1))+I(avgtime_SEA*(rankavgtime_SEA==2))",
                 "+I(avgtime_SEA*(rankavgtime_SEA==3))",
                 "+I(avgtime_TEB*(rankavgtime_TEB==1))+I(avgtime_TEB*(rankavgtime_TEB==2))",
                 "+I(avgtime_TEB*(rankavgtime_TEB==3))",
                 "+I(avgtime_WAR*(rankavgtime_WAR==1))+I(avgtime_WAR*(rankavgtime_WAR==2))",
                 "+I(avgtime_WAR*(rankavgtime_WAR==3))",
                 "+I(avgtime_WID*(rankavgtime_WID==1))+I(avgtime_WID*(rankavgtime_WID==2))",
                 "+I(avgtime_WID*(rankavgtime_WID==3))",
                 "+I(avgtime_WMW*(rankavgtime_WMW==1))+I(avgtime_WMW*(rankavgtime_WMW==2))",
                 "+I(avgtime_WMW*(rankavgtime_WMW==3))",
                 "+I(avgtime_WIN*(rankavgtime_WIN==1))+I(avgtime_WIN*(rankavgtime_WIN==2))",
                 "+I(avgtime_WIN*(rankavgtime_WIN==3))",
                 "+I(avgtime_WIR*(rankavgtime_WIR==1))+I(avgtime_WIR*(rankavgtime_WIR==2))",
                 "+I(avgtime_WIR*(rankavgtime_WIR==3))")

} else if (region==8 & datastub=="m11") {
  # southeast
  vlist2<-paste0("+rankavgtime",
                 "+I((rankavgtime_LON>3)*avgtime_LON)",
                 "+I((rankavgtime_LON>3)*avgtime_LON^2)",
                 "+I(avgtime_LON*(rankavgtime_LON==1))+I(avgtime_LON*(rankavgtime_LON==2))",
                 "+I(avgtime_LON*(rankavgtime_LON==3))",
                 "+I(avgtime_BRI*(rankavgtime_BRI==1))+I(avgtime_BRI*(rankavgtime_BRI==2))",
                 "+I(avgtime_BRI*(rankavgtime_BRI==3))",
                 "+I(avgtime_CAN*(rankavgtime_CAN==1))+I(avgtime_CAN*(rankavgtime_CAN==2))",
                 "+I(avgtime_CAN*(rankavgtime_CAN==3))",
                 "+I(avgtime_CSG*(rankavgtime_CSG==1))+I(avgtime_CSG*(rankavgtime_CSG==2))",
                 "+I(avgtime_CSG*(rankavgtime_CSG==3))",
                 "+I(avgtime_DOR*(rankavgtime_DOR==1))+I(avgtime_DOR*(rankavgtime_DOR==2))",
                 "+I(avgtime_DOR*(rankavgtime_DOR==3))",
                 "+I(avgtime_HCP*(rankavgtime_HCP==1))+I(avgtime_HCP*(rankavgtime_HCP==2))",
                 "+I(avgtime_HCP*(rankavgtime_HCP==3))",
                 "+I(avgtime_GFD*(rankavgtime_GFD==1))+I(avgtime_GFD*(rankavgtime_GFD==2))",
                 "+I(avgtime_GFD*(rankavgtime_GFD==3))",
                 "+I(avgtime_OXF*(rankavgtime_OXF==1))+I(avgtime_OXF*(rankavgtime_OXF==2))",
                 "+I(avgtime_OXF*(rankavgtime_OXF==3))")
  
} else if (region==9 & datastub=="m11") {  
  vlist2<-paste0("+rankavgtime",
                 "+I(avgtime_ALT*(rankavgtime_ALT==1))+I(avgtime_ALT*(rankavgtime_ALT==2))",
                 "+I(avgtime_ALT*(rankavgtime_ALT==3))",
                 "+I(avgtime_BTH*(rankavgtime_BTH==1))+I(avgtime_BTH*(rankavgtime_BTH==2))",
                 "+I(avgtime_BTH*(rankavgtime_BTH==3))",
                 "+I(avgtime_CCP*(rankavgtime_CCP==1))+I(avgtime_CCP*(rankavgtime_CCP==2))",
                 "+I(avgtime_CCP*(rankavgtime_CCP==3))",
                 "+I(avgtime_CHR*(rankavgtime_CHR==1))+I(avgtime_CHR*(rankavgtime_CHR==2))",
                 "+I(avgtime_CHR*(rankavgtime_CHR==3))",
                 "+I(avgtime_CLF*(rankavgtime_CLF==1))+I(avgtime_CLF*(rankavgtime_CLF==2))",
                 "+I(avgtime_CLF*(rankavgtime_CLF==3))",
                 "+I(avgtime_DUN*(rankavgtime_DUN==1))+I(avgtime_DUN*(rankavgtime_DUN==2))",
                 "+I(avgtime_DUN*(rankavgtime_DUN==3))",
                 "+I(avgtime_HUN*(rankavgtime_HUN==1))+I(avgtime_HUN*(rankavgtime_HUN==2))",
                 "+I(avgtime_HUN*(rankavgtime_HUN==3))",
                 "+I(avgtime_MIN*(rankavgtime_MIN==1))+I(avgtime_MIN*(rankavgtime_MIN==2))",
                 "+I(avgtime_MIN*(rankavgtime_MIN==3))",
                 "+I(avgtime_POR*(rankavgtime_POR==1))+I(avgtime_POR*(rankavgtime_POR==2))",
                 "+I(avgtime_POR*(rankavgtime_POR==3))",
                 "+I(avgtime_WLT*(rankavgtime_WLT==1))+I(avgtime_WLT*(rankavgtime_WLT==2))",
                 "+I(avgtime_WLT*(rankavgtime_WLT==3))",
                 "+I(avgtime_WSF*(rankavgtime_WSF==1))+I(avgtime_WSF*(rankavgtime_WSF==2))",
                 "+I(avgtime_WSF*(rankavgtime_WSF==3))")

} else if (region==10 & datastub=="m11") {
  vlist2 <- "+rankavgtime"
  avgtimevar<- colnames(m2data[,grep("\\<avgtime_[^cts]",colnames(m2data))])
  ranktimevar<- colnames(m2data[,grep("\\<rankavgtime_[^cts]",colnames(m2data))])
  for (i1 in 1:length(avgtimevar)) {
    for (i2 in 1:3) {
      if (i1!=3 | i2!=1) {
        t1 <- paste0("I(",avgtimevar[i1],"*(",ranktimevar[i1],"==",i2,"))")
        vlist2<-paste(vlist2,t1,sep="+")
      }
    }
  }

} else if (region==11 & datastub=="m11") {
  vlist2<-paste0("+rankavgtime",
                 "+I(avgtime_DON*(rankavgtime_DON==1))+I(avgtime_DON*(rankavgtime_DON==2))",
                 "+I(avgtime_DON*(rankavgtime_DON==3))",
                 "+I(avgtime_HRG*(rankavgtime_HRG==1))+I(avgtime_HRG*(rankavgtime_HRG==2))",
                 "+I(avgtime_HRG*(rankavgtime_HRG==3))",
                 "+I(avgtime_KSN*(rankavgtime_KSN==1))+I(avgtime_KSN*(rankavgtime_KSN==2))",
                 "+I(avgtime_KSN*(rankavgtime_KSN==3))",
                 "+I(avgtime_OSM*(rankavgtime_OSM==1))+I(avgtime_OSM*(rankavgtime_OSM==2))",
                 "+I(avgtime_OSM*(rankavgtime_OSM==3))",
                 "+I(avgtime_SHF*(rankavgtime_SHF==1))+I(avgtime_SHF*(rankavgtime_SHF==2))",
                 "+I(avgtime_SHF*(rankavgtime_SHF==3))",
                 "+I(avgtime_YRK*(rankavgtime_YRK==1))+I(avgtime_YRK*(rankavgtime_YRK==2))",
                 "+I(avgtime_YRK*(rankavgtime_YRK==3))",
                 "+I(avgtime_SCU*(rankavgtime_SCU==1))+I(avgtime_SCU*(rankavgtime_SCU==2))",
                 "+I(avgtime_SCU*(rankavgtime_SCU==3))",
                 "+I(avgtime_SKP*(rankavgtime_SKP==1))+I(avgtime_SKP*(rankavgtime_SKP==2))",
                 "+I(avgtime_SKP*(rankavgtime_SKP==3))",
                 "+I(avgtime_DRF*(rankavgtime_DRF==1))+I(avgtime_DRF*(rankavgtime_DRF==2))",
                 "+I(avgtime_DRF*(rankavgtime_DRF==3))")

} # if (region==1) 
  
  formula5<-as.formula(paste0(vlist1,vlist2))
  
  return(formula5)
  
} # end of function
