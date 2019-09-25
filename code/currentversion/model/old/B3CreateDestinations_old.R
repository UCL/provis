B3CreateDestinations<-function(region_id) {
  
  # Create bounding box for spatial data
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  ukgrid = "+init=epsg:27700"
  
if (region_id==1) {
  # CaMKOx
  dest_name<-c("Bedford, England", 
               "Cambridge, England",
               "Luton, UK",
               "Milton Keynes, England",
               "Northampton, UK",
               "Oxford, England",
               "Peterborough, UK",
               "Reading, UK",
               "Luton Airport, Luton",
               "Stansted Airport, Stansted",
               "Liverpool Street Station, Liverpool Street",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "Paddington Station, London",
               "London King's Cross, Euston Road")
  dest_name_short<- c("BED","CAM","LUT","MK","NOR","OX","PBH","REA","LTA",
                      "STD","LIV","STP","MAR","EUS","PAD","KGX")
#  idest<-c(1,2,3,4,5,6,7,8)
  idest<-c(2,6,11:16)
#  raildest<-c(2,3,6,7,8,11:16)
  raildest<-c(2,6,11:16)
  airport<-c(9,10)
} else if (region_id==2) {
  # CornwallDevon
  dest_name<-c("Barnstaple, England",
               "Bude, UK",
               "Dartmoor National Park, UK",
               "Exeter, England",
               "Exmoor National Park, UK",
               "Falmouth, England",
               "Newquay, England",
               "Minehead, UK",
               "Padstow, UK",
               "Penzance, England",
               "Plymouth, England",
               "Redruth, UK",
               "St Austell, UK",
               "St Ives, England",
               "Taunton, UK",
               "Torbay, England",
               "Truro, England",
               "Exeter Airport, UK",
               "Newquay Airport, UK")
  dest_name_short<-c("BSP","BUD","DMR","EXE","EXM",
                     "FAL","NQY","MHD","PAD","PNZ",
                     "PLY","RED","STA","SIV","TAU",
                     "TBY","TRU","EXE_AIR","NQY_AIR")
  idest<-c(1,4,8,9,10,11,13,14,16,17)
  raildest<-c(1,4,8,9,10,11,13,14,15,16,17)
  airport<-c(18,19)
} else if (region_id==3) {
  # East Midlands
  dest_name<-c("Banbury, England","Birmingham, England","Coventry, England","Derby, England",
               "Grantham, UK","Leicester, England","Lincoln, UK",
               "Northampton, England","Nottingham, England",
               "Peterborough, UK","Sheffield, England",
               "Stamford, UK",
               "East Midlands Airport","Peak District National Park, UK",
               "West Bridgford, England","Oadby, England","Daventry, England")
  dest_name_short<-c("BAN","BIR","COV","DBY","GHM",
                     "LEI","LIN","NHN","NOT","PBH",
                     "SHF","SFD","EMA","PEAK","WBR",
                     "OAD","DAV")
#  idest<-c(1:11)
  idest<-c(1,5,12,14,15,16)
  raildest<-c(4,6,7,9,11)
#  raildest<-c(1,5,6,8,9,10)
  airport<-c(12)
}  else if (region_id==4) {
  # East of England
  dest_name<-c("Basildon, UK",
               "Bedford, England", 
               "Cambridge, England",
               "Southend-on-Sea, England",
               "Norwich, England",
               "Ipswich, England",
               "Colchester, England",
               "Chelmsford, England",
               "Luton, UK",
               "Peterborough, UK",
               "Liverpool Street Station, Liverpool Street",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "London King's Cross, Euston Road",
               "Luton Airport, UK",
               "Stansted Airport, UK",
               "Norwich International Airport, UK",
               "London Southend Airport, UK")
  dest_name_short<- c("BAS","BED","CAM","SOS","NWH","IPS",
                      "COL","CHF","LUT","PBH","LIV","STP","MAR","EUS","KGX","LUT_AIR",
                      "STD","NWH_AIR","SEN_AIR")
  idest<-c(1,2,3,4,5,6,7,8,9,10)
  raildest<-c(3,5,6,7,9,10,11:15)
  airport<-c(15:18)
} else if (region_id==5) {
  # london
  dest_name<-c("Trafalgar Square, London",
               "Charing Cross Station, London",
               "Euston Station, London",
               "King's Cross Station, Euston Road",
               "Liverpool Street Station, Liverpool Street",
               "London Bridge Station, London",
               "Marylebone Station, London",
               "Paddington Station, London",
               "St Pancras Station, Euston Road",
               "Waterloo Station, London",
               "Heathrow Airport",
               "Luton Airport, Luton",
               "Stansted Airport, Stansted",
               "Gatwick Airport",
               "London City Airport, UK")
  dest_name_short<- c("TFG",
                      "CHX","EUS","KGX","LIV","LBG","MYB",
                      "PAD","STP","WAT",
                      "LHW","LUT","STD","GTW","LCY")
  
  idest<-c(1:10)
  raildest<-c(2:10)
  airport<-c(11:15)
} else if (region_id==6) {
  dest_name<-c("Middlesbrough, England",
               "Newcastle upon Tyne, England",
               "Sunderland, England",
               "Durham, England",
               "Manchester, England",
               "Leeds, England",
               "York, England",
               "Leeds Bradford Airport, UK",
               "Newcastle Airport, UK")
  dest_name_short<-c("MDB","NWC","SUN","DUR",
                     "MAN","LDS","YRK","LBA","NCL_AIR")
  idest<-c(1,2,5,6)
  raildest<-c(1,2,5,6)
  airport<-c(8,9)
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
               "Manchester Airport, UK",
               "Liverpool John Lennon Airport")
  dest_name_short<-c("MAN","LIV","BBN","BLY",
                     "PRN","BPL","WRN","CLL","WMR","MAN_AIR","LPL_AIR")
  idest<-c(1,2,9)
  raildest<-c(1,2)
  airport<-c(10,11)
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
  idest<-c(1,4,6,13,14,15,16,17,18,19)
  raildest<-c(1,2,4,6,13:19)
  airport<-c(8,9,10,11,12)
} else if (region_id==9) {
  dest_name<-c("Bath, England",
               "Bristol, England",
               "Swindon, England",
               "Gloucester, England",
               "Cheltenham, England",
               "Bournemouth, England",
               "Bristol Airport, UK",
               "Bournemouth Airport, UK",
               "Paddington Station, London",
               "Victoria Station, London",
               "Reading, England")
  dest_name_short<-c("BTH","BSL","SDN","GLO","CHM","BMH","BRA","BOA","PAD","VIC","RDG")
  idest<-c(2,6,10,11)
  raildest<-c(2,6,11)
  airport<-c(7,8)
} else if (region_id==10) {
  # West Midlands
  dest_name<-c("Birmingham, England",
               "Wolverhampton, England",
               "Coventry, England",
               "Stoke on Trent, England",
               "Birmingham Airport, UK")
  dest_name_short<-c("BHM","WHN","COV","STO","BMX")
  idest<-c(1,2,3,4)
  raildest<-c(1,2,3,4)
  airport<-c(5)
} else if (region_id==11) {
  # Yorkshire and the Humber
  dest_name<-c("Bradford, England",
               "Doncaster, England",
               "Harrogate, England",
               "Kingston upon Hull, England",
               "Leeds, England",
               "Osmotherley, UK",
               "Ripon, England",
               "Sheffield, England",
               "York, England",
               "Middlesbrough, England",
               "North York Moors, England",
               "Scunthorpe, England",
               "Leeds Bradford International Airport",
               "Skipton, England",
               "Driffield, England")
  dest_name_short<-c("BFD","DON","HRG","KSN","LDS","OSM","RPN","SHF","YRK",
                     "MBH","NYM","SCU","LBA","SKP","DRF")
  idest<-c(2,3,4,6,8,9,12,14,15)
  raildest<-c(4,5,8,9,10)
  airport<-c(13)
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
  if (attempt>=20) {
    break
  }
}
names(dest_coordinates)<-c("longitude","latitude")

# Logical indicator for which destinations are on shortlist
shortlist<-matrix(FALSE,nrow=length(dest_name),ncol=1)
shortlist[idest]<-TRUE

# Easting and northing
long_lat<-SpatialPoints(dest_coordinates,CRS(wgs.84))
east_north<-spTransform(long_lat,CRS(ukgrid))
dest<-data.frame(dest_name,dest_name_short,dest_coordinates,coordinates(east_north),shortlist)
colnames(dest)<-c("name","shortname","longitude","latitude","easting","northing","shortlist")
dest$raildest<-FALSE
dest$raildest[raildest]<-TRUE
dest$airport<-FALSE
dest$airport[airport]<-TRUE
return(dest)
}

