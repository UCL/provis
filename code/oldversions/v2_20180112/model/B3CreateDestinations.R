B3CreateDestinations<-function(region_id) {
  
if (region_id==1) {
  # CaMKOx
  dest_name<-c("Bedford, England", 
               "Cambridge, England",
               "Milton Keynes, England",
               "Oxford, England",
               "Luton Airport, Luton",
               "Stansted Airport, Stansted",
               "Liverpool Street Station, Liverpool Street",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "London King's Cross, Euston Road")
  dest_name_short<- c("BED","CAM","MK","OX","LUT",
                      "STD","LIV","STP","MAR","EUS","KGX")
  idest<-c(2,4)
} else if (region_id==2) {
  # CornwallDevon
  dest_name<-c("Barnstaple, England","Exeter, England",
               "Falmouth, England",
               "Newquay, England","Penzance, England","Plymouth, England",
               "St Ives, England","Torbay, England","Truro, England")
  dest_name_short<-c("BSP","EXE","FAL","NQY","PZE","PLY","STV","TBY","TRO")
  idest<-c(1,2,3,6)
} else if (region_id==3) {
  # East Midlands
  dest_name<-c("Birmingham, England","Coventry, England","Derby, England", "Leicester, England",
               "Northampton, England","Nottingham, England",
               "Stoke-on-Trent, England","Sheffield, England","East Midlands Airport",
               "Peak District National Park, UK","Oxford, UK",
               "Grantham, UK","Peterborough, UK",
               "Loughborough, UK","Milton Keynes, UK","Kettering, UK",
               "Banbury, UK")
  dest_name_short<-c("BIR","COV","DBY","LEI","NHN","NOT",
                     "STK","SHF","EMA","PEAK","OX","GHM","PBH","LBH","MK","KET","BAN") 
  idest<-c(4,6,10,12,17)
}  else if (region_id==4) {
  # East of England
  dest_name<-c("Bedford, England", 
               "Cambridge, England",
               "Southend-on-Sea, England",
               "Norwich, England",
               "Ipswich, England",
               "Colchester, England",
               "Chelmsford, England",
               "Luton Airport, Luton",
               "Stansted Airport, Stansted",
               "Liverpool Street Station, Liverpool Street",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "London King's Cross, Euston Road")
  dest_name_short<- c("BED","CAM","SOS","NWH","IPS",
                      "COL","CHF","LUT","STD","LIV","STP","MAR","EUS","KGX")
  
  idest<-c(2,4,10,14)
  
} else if (region_id==5) {
  # london
  dest_name<-c("Trafalgar Square, London",
               "Heathrow Airport",
               "Watford, England",
               "Luton Airport, Luton",
               "Stansted Airport, Stansted",
               "Gatwick Airport",
               "Liverpool Street Station, Liverpool Street",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "London King's Cross, Euston Road")
  dest_name_short<- c("TFG","LHW","WAT","LUT","STD","GAT","LIV",
                      "STP","MAR","EUS","KGX")
  
  idest<-c(1,2,7)
  
} else if (region_id==6) {
  dest_name<-c("Middlesbrough, England",
               "Newcastle upon Tyne, England",
               "Sunderland, England",
               "Durham, England",
               "Manchester, England",
               "Leeds, England",
               "York, England")
  dest_name_short<-c("MDB","NWC","SUN","DUR",
                     "MAN","LDS","YRK")
  idest<-c(1,2,3,4)
} else if (region_id==7) {
  # Northwest
  dest_name<-c("Manchester, England",
               "Liverpool, England",
               "Blackburn, England",
               "Burnley, England",
               "Preston, England",
               "Blackpool, England",
               "Warrington, England",
               "Carlisle, England",
               "Windermere, England",
               "Manchester Airport",
               "Liverpool John Lennon Airport")
  dest_name_short<-c("MAN","LIV","BBN","BLY",
                     "PRN","BPL","WRN","CLL","WMR","MCA","LPA")
  idest<-c(1,2,9)
} else if (region_id==8) {
  # Southeast
  dest_name<-c("Brighton, England",
               "Canterbury, England",
               "Chichester, England",
               "Oxford, England",
               "Portsmouth, England",
               "Southampton, England",
               "Winchester, England",
               "Gatwick Airport",
               "Luton Airport",
               "Heathrow Airport",
               "Southampton Airport",
               "Kent International Airport",
               "Euston Station, London",
               "Victoria Station, London",
               "London Bridge Station, London",
               "Waterloo Station, London",
               "Paddington Station, London",
               "Marylebone Station, London",
               "Guildford, England")
  dest_name_short<-c("BRI","CAN","CHI","OXF","PMH",
                     "SOU","WIN","GAT","LUT","LHR",
                     "SNA","KIA","EUS","VIC","LBR",
                     "WAT","PAD","MAR","GFD")
  idest<-c(1,6,13,14,19)     
} else if (region_id==9) {
  dest_name<-c("Bath, England",
               "Bristol, England",
               "Swindon, England",
               "Gloucester, England",
               "Cheltenham, England",
               "Bournemouth, England",
               "Bristol Airport",
               "Bournemouth Airport",
               "Paddington Station, London",
               "Victoria Station, London",
               "Reading, England")
  dest_name_short<-c("BTH","BSL","SDN","GLO","CHM","BMH","BRA","BOA","PAD","VIC","RDG")
  idest<-c(2,6,10,11)
} else if (region_id==10) {
  # West Midlands
  dest_name<-c("Birmingham, England",
               "Wolverhampton, England",
               "Coventry, England",
               "Stoke on Trent, England",
               "Birmingham Airport, England")
  dest_name_short<-c("BHM","WHN","COV","STO","BMA")
  idest<-c(1,2,3,4)
} else if (region_id==11) {
  # Yorkshire and the Humber
  dest_name<-c("Bradford, England",
               "Harrogate, England",
               "Kingston upon Hull, England",
               "Leeds, England",
               "Ripon, England",
               "Sheffield, England",
               "York, England",
               "Middlesbrough, England",
               "North York Moors, England",
               "Leeds Bradford International Airport")
  dest_name_short<-c("BFD","HRG","KSN","LDS","RPN","SHF","YRK","MBH","NYM","LBA")
  idest<-c(1,2,4,5,6,7,8)
}
dest_coordinates<- geocode(dest_name,output="latlon",source="google")  
attempt<-0
while (any(is.na(dest_coordinates))) {
  for (i1 in 1:nrow(dest_coordinates)) {  
    if (is.na(dest_coordinates[i1,1])) {
      dest_coordinates[i1,]<- geocode(dest_name[i1],output="latlon",source="google")  
    }   
  }  
  attempt<-attempt+1
  if (attempt>=10) {
    break
  }
}
names(dest_coordinates)<-c("longitude","latitude")

# Logical indicator for which destinations are on shortlist
shortlist<-matrix(FALSE,nrow=length(dest_name),ncol=1)
shortlist[idest]<-TRUE

dest<-data.frame(dest_name,dest_name_short,dest_coordinates,shortlist)
colnames(dest)<-c("name","shortname","longitude","latitude","shortlist")

return(dest)
}
