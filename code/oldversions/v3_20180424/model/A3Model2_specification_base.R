#' Create formula for model 5
#' @param region   region
#' @return formula5
#' @examples 
#' formula5 <- A3Model2_specification(region)
#' 
A3Model2_specification<-function(region) {
  vlist1<-A3Model2_vlist1(region)

if (region==1) {
  # Region 1: Model 5 specification 
  vlist2<-paste0("+rankdrive",
                 "+I(drive_BED*(rankdrive_BED==1))+I(drive_BED*(rankdrive_BED==2))",
                 "+I(drive_BED*(rankdrive_BED==3))",
                 "+I(drive_CAM*(rankdrive_CAM==1))+I(drive_CAM*(rankdrive_CAM==2))",
                 "+I(drive_CAM*(rankdrive_CAM==3))",
                 "+I(drive_LUT*(rankdrive_LUT==1))+I(drive_LUT*(rankdrive_LUT==2))",
                 "+I(drive_LUT*(rankdrive_LUT==3))",
                 "+I(drive_MK*(rankdrive_MK==1))+I(drive_MK*(rankdrive_MK==2))",
                 "+I(drive_MK*(rankdrive_MK==3))",
                 "+I(drive_NOR*(rankdrive_NOR==1))+I(drive_NOR*(rankdrive_NOR==2))",
                 "+I(drive_NOR*(rankdrive_NOR==3))",   
                 "+I(drive_OX*(rankdrive_OX==1))+I(drive_OX*(rankdrive_OX==2))",
                 "+I(drive_OX*(rankdrive_OX==3))",
                 "+I(drive_PBH*(rankdrive_PBH==1))+I(drive_PBH*(rankdrive_PBH==2))",
                 "+I(drive_PBH*(rankdrive_PBH==3))",
                 "+I(drive_REA*(rankdrive_REA==1))+I(drive_REA*(rankdrive_REA==2))",
                 "+I(drive_REA*(rankdrive_REA==3))")
  vlist3<-paste0("+ranktrans",
                 "+I(trans_CAM*(ranktrans_CAM==1))",
                 "+I(trans_CAM*(ranktrans_CAM==2))",
                 "+I(trans_CAM*(ranktrans_CAM==3))",
                 "+I(trans_LUT*(ranktrans_LUT==1))",
                 "+I(trans_LUT*(ranktrans_LUT==2))",
                 "+I(trans_LUT*(ranktrans_LUT==3))",
                 "+I(trans_OX*(ranktrans_OX==1))",
                 "+I(trans_OX*(ranktrans_OX==2))",
                 "+I(trans_OX*(ranktrans_OX==3))",
                 "+I(trans_PBH*(ranktrans_PBH==1))",
                 "+I(trans_PBH*(ranktrans_PBH==2))",
                 "+I(trans_PBH*(ranktrans_PBH==3))",
                 "+I(trans_REA*(ranktrans_REA==1))",
                 "+I(trans_REA*(ranktrans_REA==2))",
                 "+I(trans_REA*(ranktrans_REA==3))",
                 "+I(trans_LON*(ranktrans_LON==1))",
                 "+I(trans_LON*(ranktrans_LON==2))",
                 "+I(trans_LON*(ranktrans_LON==3))")
} else if (region==2) {  
  # Region 2: Model 5 specification   
  vlist2<-paste0("+rankdrive",
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
              "+I(drive_PLY*(rankdrive_PLY==1))+I(drive_PLY*(rankdrive_PLY==2))",
              "+I(drive_PLY*(rankdrive_PLY==3))",
              "+I(drive_STA*(rankdrive_STA==1))+I(drive_STA*(rankdrive_STA==2))",
              "+I(drive_STA*(rankdrive_STA==3))",
              "+I(drive_SIV*(rankdrive_SIV==1))+I(drive_SIV*(rankdrive_SIV==2))",
              "+I(drive_SIV*(rankdrive_SIV==3))",
              "+I(drive_TBY*(rankdrive_TBY==1))+I(drive_TBY*(rankdrive_TBY==2))",
              "+I(drive_TBY*(rankdrive_TBY==3))")
  vlist3<-paste0("+ranktrans+trans_BSP+trans_EXE+",
               "+trans_PNZ+trans_PLY+trans_SIV",
               "+trans_TAU+trans_TBY+trans_TRU")
} else if (region==3) {
  vlist2<-paste0("+rankdrive",
                 "+I(drive_BIR*(rankdrive_BIR==1))+I(drive_BIR*(rankdrive_BIR==2))",
                 "+I(drive_BIR*(rankdrive_BIR==3))",
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

  vlist3<-paste0("+ranktrans",
                 "+I((ranktrans_BIR==1)*trans_BIR)",
                 "+I((ranktrans_LEI==1)*trans_LEI)",
                 "+I((ranktrans_LIN==1)*trans_LIN)",
                 "+I((ranktrans_NOT==1)*trans_NOT)",
                 "+I((ranktrans_PBH==1)*trans_PBH)",
                 "+I((ranktrans_SHF==1)*trans_SHF)")
} else if (region==4) {
  vlist2<-paste0("+rankdrive",
                 "+I(drive_BAS*(rankdrive_BAS==1))+I(drive_BAS*(rankdrive_BAS==2))",
                 "+I(drive_BAS*(rankdrive_BAS==3))",
                 "+I(drive_BED*(rankdrive_BED==1))+I(drive_BED*(rankdrive_BED==2))",
                 "+I(drive_BED*(rankdrive_BED==3))",
                 "+I(drive_CAM*(rankdrive_CAM==1))+I(drive_CAM*(rankdrive_CAM==2))",
                 "+I(drive_CAM*(rankdrive_CAM==3))",
                 "+I(drive_SOS*(rankdrive_SOS==1))+I(drive_SOS*(rankdrive_SOS==2))",
                 "+I(drive_SOS*(rankdrive_SOS==3))",
                 "+I(drive_NWH*(rankdrive_NWH==1))+I(drive_NWH*(rankdrive_NWH==2))",
                 "+I(drive_NWH*(rankdrive_NWH==3))",   
                 "+I(drive_IPS*(rankdrive_IPS==1))+I(drive_IPS*(rankdrive_IPS==2))",
                 "+I(drive_IPS*(rankdrive_IPS==3))",
                 "+I(drive_COL*(rankdrive_COL==1))+I(drive_COL*(rankdrive_COL==2))",
                 "+I(drive_COL*(rankdrive_COL==3))",
                 "+I(drive_CHF*(rankdrive_CHF==1))+I(drive_CHF*(rankdrive_CHF==2))",
                 "+I(drive_CHF*(rankdrive_CHF==3))",
                 "+I(drive_LUT*(rankdrive_LUT==1))+I(drive_LUT*(rankdrive_LUT==2))",
                 "+I(drive_LUT*(rankdrive_LUT==3))",
                 "+I(drive_PBH*(rankdrive_PBH==1))+I(drive_PBH*(rankdrive_PBH==2))",
                 "+I(drive_PBH*(rankdrive_PBH==3))")
  
  vlist3<-paste0("+ranktrans",
                 "+I((ranktrans_CAM==1)*trans_CAM)",
                 "+I((ranktrans_NWH==1)*trans_NWH)",
                 "+I((ranktrans_IPS==1)*trans_IPS)",
                 "+I((ranktrans_COL==1)*trans_COL)",
                 "+I((ranktrans_LUT==1)*trans_LUT)",
                 "+I((ranktrans_PBH==1)*trans_PBH)",
                 "+trans_LON")
  formula5<-as.formula(paste0(vlist1,vlist2,vlist3))
} else if (region==5) {

} else if (region==6) {
  vlist2<-paste0("+rankdrive",
                 "+I(drive_MDB*(rankdrive_MDB==1))+I(drive_MDB*(rankdrive_MDB==2))",
                 "+I(drive_MDB*(rankdrive_MDB==3))",
                 "+I(drive_NWC*(rankdrive_NWC==1))+I(drive_NWC*(rankdrive_NWC==2))",
                 "+I(drive_NWC*(rankdrive_NWC==3))",
                 "+I(drive_MAN*(rankdrive_MAN==1))+I(drive_MAN*(rankdrive_MAN==2))",
                 "+I(drive_MAN*(rankdrive_MAN==3))",
                 "+I(drive_LDS*(rankdrive_LDS==1))+I(drive_LDS*(rankdrive_LDS==2))",
                 "+I(drive_LDS*(rankdrive_LDS==3))")
  vlist3<-paste0("+ranktrans",
                 "+I(trans_MDB*(ranktrans_MDB==1))",
                 "+I(trans_MDB*(ranktrans_MDB==2))",
                 "+I(trans_MDB*(ranktrans_MDB==3))",
                 "+I(trans_NWC*(ranktrans_NWC==1))",
                 "+I(trans_NWC*(ranktrans_NWC==2))",
                 "+I(trans_NWC*(ranktrans_NWC==3))",
                 "+I(trans_MAN*(ranktrans_MAN==1))",
                 "+I(trans_MAN*(ranktrans_MAN==2))",
                 "+I(trans_MAN*(ranktrans_MAN==3))",
                 "+I(trans_LDS*(ranktrans_LDS==1))",
                 "+I(trans_LDS*(ranktrans_LDS==2))",
                 "+I(trans_LDS*(ranktrans_LDS==3))")
} else if (region==7) {
  vlist2<-paste0("+rankdrive",
                 "+I(drive_MAN*(rankdrive_MAN==1))+I(drive_MAN*(rankdrive_MAN==2))",
                 "+I(drive_MAN*(rankdrive_MAN==3))",
                 "+I(drive_LIV*(rankdrive_LIV==1))+I(drive_LIV*(rankdrive_LIV==2))",
                 "+I(drive_LIV*(rankdrive_LIV==3))",
                 "+I(drive_WMR*(rankdrive_WMR==1))+I(drive_WMR*(rankdrive_WMR==2))",
                 "+I(drive_WMR*(rankdrive_WMR==3))")
  vlist3<-paste0("+ranktrans",
                 "+I(trans_MAN*(ranktrans_MAN==1))",
                 "+I(trans_MAN*(ranktrans_MAN==2))",
                 "+I(trans_MAN*(ranktrans_MAN==3))",
                 "+I(trans_LIV*(ranktrans_LIV==1))",
                 "+I(trans_LIV*(ranktrans_LIV==2))",
                 "+I(trans_LIV*(ranktrans_LIV==3))")
  
} else if (region==8) {
  # southeast
  vlist2<-paste0("+rankdrive",
                 "+I((rankdrive_LON>3)*drive_LON)",
                 "+I((rankdrive_LON>3)*drive_LON^2)",
                 "+I(drive_LON*(rankdrive_LON==1))+I(drive_LON*(rankdrive_LON==2))",
                 "+I(drive_LON*(rankdrive_LON==3))",
                 "+I(drive_BRI*(rankdrive_BRI==1))+I(drive_BRI*(rankdrive_BRI==2))",
                 "+I(drive_BRI*(rankdrive_BRI==3))",
                 "+I(drive_OXF*(rankdrive_OXF==1))+I(drive_OXF*(rankdrive_OXF==2))",
                 "+I(drive_OXF*(rankdrive_OXF==3))",
                 "+I(drive_SOU*(rankdrive_SOU==1))+I(drive_SOU*(rankdrive_SOU==2))",
                 "+I(drive_SOU*(rankdrive_SOU==3))",
                 "+I(drive_GFD*(rankdrive_GFD==1))+I(drive_GFD*(rankdrive_GFD==2))",
                 "+I(drive_GFD*(rankdrive_GFD==3))")
  
  vlist3<-paste0("+ranktrans",
                 "+I((ranktrans_LON==1)*trans_LON)",
                 "+I((ranktrans_LON==2)*trans_LON)",
                 "+I((ranktrans_LON==3)*trans_LON)",
                 "+I((ranktrans_BRI==1)*trans_BRI)",
                 "+I((ranktrans_BRI==2)*trans_BRI)",
                 "+I((ranktrans_BRI==3)*trans_BRI)",
                 "+I((ranktrans_CAN==1)*trans_CAN)",
                 "+I((ranktrans_CAN==2)*trans_CAN)",
                 "+I((ranktrans_CAN==3)*trans_CAN)",
                 "+I((ranktrans_OXF==1)*trans_OXF)",
                 "+I((ranktrans_OXF==2)*trans_OXF)",
                 "+I((ranktrans_OXF==3)*trans_OXF)",
                 "+I((ranktrans_SOU==1)*trans_SOU)",
                 "+I((ranktrans_SOU==2)*trans_SOU)",
                 "+I((ranktrans_SOU==3)*trans_SOU)",
                 "+I((ranktrans_GFD==1)*trans_GFD)",
                 "+I((ranktrans_GFD==2)*trans_GFD)",
                 "+I((ranktrans_GFD==3)*trans_GFD)")
} else if (region==9) {  
  vlist2<-paste0("+rankdrive",
                 "+I(drive_BSL*(rankdrive_BSL==1))+I(drive_BSL*(rankdrive_BSL==2))",
                 "+I(drive_BSL*(rankdrive_BSL==3))",
                 "+I(drive_BMH*(rankdrive_BMH==1))+I(drive_BMH*(rankdrive_BMH==2))",
                 "+I(drive_BMH*(rankdrive_BMH==3))",
                 "+I(drive_VIC*(rankdrive_VIC==1))+I(drive_VIC*(rankdrive_VIC==2))",
                 "+I(drive_VIC*(rankdrive_VIC==3))",
                 "+I(drive_RDG*(rankdrive_RDG==1))+I(drive_RDG*(rankdrive_RDG==2))",
                 "+I(drive_RDG*(rankdrive_RDG==3))")
  vlist3<-paste0("+ranktrans",
                 "+I(trans_BSL*(ranktrans_BSL==1))",
                 "+I(trans_BSL*(ranktrans_BSL==2))",
                 "+I(trans_BSL*(ranktrans_BSL==3))",
                 "+I(trans_BMH*(ranktrans_BMH==1))",
                 "+I(trans_BMH*(ranktrans_BMH==2))",
                 "+I(trans_BMH*(ranktrans_BMH==3))",
                 "+I(trans_RDG*(ranktrans_RDG==1))",
                 "+I(trans_RDG*(ranktrans_RDG==2))",
                 "+I(trans_RDG*(ranktrans_RDG==3))")
  
} else if (region==10) {
  vlist2<-paste0("+rankdrive",
                 "+I(drive_BHM*(rankdrive_BHM==1))+I(drive_BHM*(rankdrive_BHM==2))",
                 "+I(drive_BHM*(rankdrive_BHM==3))",
                 "+I(drive_WHN*(rankdrive_WHN==1))+I(drive_WHN*(rankdrive_WHN==2))",
                 "+I(drive_WHN*(rankdrive_WHN==3))",
                 "+I(drive_COV*(rankdrive_COV==1))+I(drive_COV*(rankdrive_COV==2))",
                 "+I(drive_COV*(rankdrive_COV==3))",
                 "+I(drive_STO*(rankdrive_STO==1))+I(drive_STO*(rankdrive_STO==2))",
                 "+I(drive_STO*(rankdrive_STO==3))")
  
  vlist3<-paste0("+ranktrans+I((ranktrans_BHM==1)*trans_BHM)",
                 "+I((ranktrans_WHN==1)*trans_WHN)",
                 "+I((ranktrans_COV==1)*trans_COV)",
                 "+I((ranktrans_STO==1)*trans_STO)")
  
} else if (region==11) {
  vlist2<-paste0("+rankdrive",
                 "+I(drive_BFD*(rankdrive_BFD==1))+I(drive_BFD*(rankdrive_BFD==2))",
                 "+I(drive_BFD*(rankdrive_BFD==3))",
                 "+I(drive_HRG*(rankdrive_HRG==1))+I(drive_HRG*(rankdrive_HRG==2))",
                 "+I(drive_HRG*(rankdrive_HRG==3))",
                 "+I(drive_LDS*(rankdrive_LDS==1))+I(drive_LDS*(rankdrive_LDS==2))",
                 "+I(drive_LDS*(rankdrive_LDS==3))",
                 "+I(drive_SHF*(rankdrive_SHF==1))+I(drive_SHF*(rankdrive_SHF==2))",
                 "+I(drive_SHF*(rankdrive_SHF==3))",
                 "+I(drive_YRK*(rankdrive_YRK==1))+I(drive_YRK*(rankdrive_YRK==2))",
                 "+I(drive_YRK*(rankdrive_YRK==3))",   
                 "+I(drive_RPN*(rankdrive_RPN==1))+I(drive_RPN*(rankdrive_RPN==2))",
                 "+I(drive_RPN*(rankdrive_RPN==3))",
                 "+I(drive_MBH*(rankdrive_MBH==1))+I(drive_MBH*(rankdrive_MBH==2))",
                 "+I(drive_MBH*(rankdrive_MBH==3))")

  vlist3<-paste0("+ranktrans+I((ranktrans_LDS==1)*trans_LDS)",
                 "+I((ranktrans_SHF==1)*trans_SHF)",
                 "+I((ranktrans_YRK==1)*trans_YRK)")
} # if (region==1) 
  
  formula5<-as.formula(paste0(vlist1,vlist2,vlist3))
  
  return(formula5)
  
} # end of function
