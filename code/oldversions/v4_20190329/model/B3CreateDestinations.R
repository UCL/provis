B3CreateDestinations<-function(region_id,datastub="m11") {
  
  # Create bounding box for spatial data
  wgs.84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  ukgrid <- "+init=epsg:27700"

if (region_id==1) {
  # CaMKOx
  dest_name<-c("Badby, UK",
               "Bletsoe Bedford, UK",
               "Botley, UK",
               "Cambridge, England",
               "Colmworth, UK",
               "Ducklington, UK",
               "Fringford, UK",
               "Fritwell, UK",
               "Great Chishill, UK",
               "Great Houghton Northampton, UK",
               "Hadenham, UK",
               "Heythrop, UK",
               "Oxford, England",
               "Stoke Mandeville, UK",
               "Upper Basildon, UK",
               "Wheatley, UK",
               "Woburn, UK",
               "Worlds End Newbury, England",
               "Liverpool Street Station, England",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "Paddington Station, London",
               "London King's Cross, Euston Road",
               "Luton Airport, Luton",
               "Stansted Airport, Stansted")
  dest_name_short<- c("BAD","BLT","BOT","CAM","CLM",
                      "DUC","FRG","FRT","GCH","GHN",
                      "HAD","HTP","OXF","STO","UPB",
                      "WHE","WOB","WOR","LIV","STP",
                      "MAR","EUS","PAD","KGX","LTA",
                      "STD")
  if (datastub=="m11") {
    idest<-c(1:24)
    raildest<-idest
  } else if (datastub=="nondom") {
    idest <-c(4,13,18,19:24)
    raildest<-idest
  }
  #raildest<-c(2,6,8,11:16)
  airport<-c(25:26)
} else if (region_id==2) {
  # CornwallDevon
  dest_name<-c("Bude, UK",
               "Challacombe, UK",
               "Crockernwell, UK",
               "Croyde, UK",
               "Helford, UK",
               "Kingsand, UK",
               "Otterton, UK",
               "Padstow, UK",
               "Salcombe, UK",
               "St Agnes, UK",
               "St Austell, UK",
               "St Ives, England",
               "Exeter Airport, UK",
               "Newquay Airport, UK",
               "Exeter, England", 
               "Plymouth, England",
               "Trevrose Head Heritage Coast, England",
               "Truro, England")
  dest_name_short<-c("BUD","CHA","CRK","CRY","HEL",
                     "KND","OTT","PAD","SAL","SAG",
                     "STA","SIV","EXA","NQA","EXE",
                     "PLY","THC","TRU")
  if (datastub=="m11") {
    idest<-c(1:12)
    raildest<-idest
  } else if (datastub=="nondom") {
    idest<-c(4,9,12,15,16,17,18)
    raildest<-idest
  }  
  airport<-c(13:14)
} else if (region_id==3) {
  # East Midlands
  if (datastub=="m11") {
    dest_name<-c("Banbury, England","Birmingham, England","Coventry, England","Derby, England",
                 "Grantham, UK","Leicester, England","Lincoln, UK",
                 "Northampton, England","Nottingham, England",
                 "Peterborough, UK","Sheffield, England",
                 "Stamford, UK",
                 "East Midlands Airport","Eyam, UK",
                 "West Bridgford, England","Oadby, England","Daventry, England")
    dest_name_short<-c("BAN","BIR","COV","DBY","GHM",
                       "LEI","LIN","NHN","NOT","PBH",
                       "SHF","SFD","EMA","EYM","WBR",
                       "OAD","DAV")
    idest     <-c(8,12,14:17)
    raildest <-c(8,12,14:17)
    airport<-c(13)
  } else if (datastub=="nondom") {
    # East Midlands
    dest_name<-c("Arnold, England","RAF College Cranwell, England",
                 "Alvaston Derby, England","Derby Rail Station, England",
                 "Glenfield, England","Desborough, England","Leicester Rail Station, England",
                 "Lincoln, England","Spalding, England",
                 "Egerton Park Cricket Club, Melton Mowbray, England",
                 "Northampton, England","Nottingham Rail Station, England",
                 "Rugby Rail Station, England","Anwick, England","East Midlands Airport")
    dest_name_short<-c("ARN","CRA","ALV","DBY","GLE",
                       "DES","LEI","LIN","SPA","MEL",
                       "NHN","NOT","RUG","AWK","EMA")
    idest    <-c(1,2,3,5,6,8,10,11,13,14)
    raildest <-c(4,7,8,11,12,13)
    airport<-c(15)
  }    
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
               "Liverpool Street Station, England",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "London King's Cross, Euston Road",
               "Luton Airport, UK",
               "Stansted Airport, UK",
               "Norwich International Airport, UK",
               "London Southend Airport, UK",
               "Biggleswade, England",
               "Great Chesterford, England",
               "Hainault London, England",
               "Watford, England",
               "Harold Park, England")
  dest_name_short<- c("BAS","BED","CAM","SOS","NWH",
                      "IPS","COL","CHF","LUT","PBH",
                      "LIV","STP","MAR","EUS","KGX",
                      "LUT_AIR","STD","NWH_AIR","SEN_AIR","BIG",
                      "GCH","HAI","WFD","HPK")
  if (datastub=="m11") {
    idest    <- c(3,4,5,7,11:15)
    raildest <- c(3,4,5,7,11:15)
  } else if (datastub=="nondom") {
    idest <- c(5,7,8,10,11:15,16,17,20:24)
    raildest<- idest
  }
  airport<-c(16:19)
} else if (region_id==5) {
  # london
  dest_name<-c("Trafalgar Square, London",
               "Charing Cross Station, London",
               "Euston Station, London",
               "King's Cross Station, Euston Road",
               "Liverpool Street Station, London",
               "London Bridge Station, London",
               "Marylebone Station, London",
               "Paddington Station, England",
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
  
  idest    <- c(1:10)
  raildest <- c(1:10)
  airport<-c(11:15)
} else if (region_id==6) {
  dest_name<-c("Akeld, UK",
               "Alnmouth, UK",
               "Annitsford, UK",
               "Bowburn, UK",
               "Bowes, UK",
               "Brancepeth, UK",
               "Chollerford, UK",
               "Corbridge, UK",
               "Craster, UK",
               "Darlington, UK",
               "Durham, England",
               "Hartlepool, UK",
               "Middlesbrough, England",
               "Newcastle upon Tyne, England",
               "Nunthorpe, UK",
               "Pegswood, UK",
               "Sandhoe, UK",
               "Sunderland, England",
               "Warrenby, UK",
               "Washington, UK",
               "Wooler, UK",
               "Leeds Bradford Airport, UK",
               "Newcastle Airport, UK",
               "Ravensworth Golf Course, England",
               "Newcastle Racecourse, England")
  dest_name_short<-c("AKD","ALN","ANN","BBN","BOW",
                     "BPH","CFD","COR","CRA","DAR",
                     "DUR","HAR","MDB","NCL","NUN",
                     "PEG","SDH","SUN","WBY","WAS",
                     "WOO","LBA","NCA","GHD","LBN")
  if (datastub=="m11") {
    idest    <- c(3,8,9,10,15)
    raildest <- idest
  } else if (datastub=="nondom") {
    idest<-c(10,11,22,23,24,25)
    raildest<-idest
  }
  airport<-c(22,23)
} else if (region_id==7) {
  # Northwest
  
  dest_name<-c("Blackburn, England",
               "Blackpool, England",
               "Bolton, UK",
               "Burnley, England",
               "Bury, UK",
               "Carlisle, England",
               "Eskdale, UK",
               "Kendal, UK",
               "Lancaster, UK",
               "Liverpool, England",
               "Manchester, England",
               "Preston, England",
               "Seatoller, UK",
               "Stretford, UK",
               "Tebay, UK",
               "Warrington, England",
               "Whitehaven, UK",
               "Widnes, UK",
               "Wigan, UK",
               "Wilmslow, UK",
               "Windermere, England",
               "Wirral, UK",
               "Manchester Airport, UK",
               "Liverpool John Lennon Airport",
               "Altrincham, England")
  dest_name_short<-c("BBN","BPL","BTN","BRN","BUR",
                     "CAR","ESK","KEN","LAN","LIV",
                     "MAN","PRE","SEA","STR","TEB",
                     "WAR","WHI","WID","WIG","WMW",
                     "WIN","WIR","MAN_AIR","LPL_AIR","ALT")
  if (datastub=="m11") {
    idest    <- c(5,13,15,16,18,20,21,22)
    raildest <- c(5,13,15,16,18,20,21,22)
  } else if (datastub=="nondom") {
    idest <- c(16,17,18,22,24,25)
    raildest <- idest
  }
  airport  <- c(23,24)
} else if (region_id==8) {
  # Southeast

  dest_name<-c("Beaconsfield, UK",
               "Brighton, England",
               "Canterbury, England",
               "Chalfont St Giles, UK",
               "Dorking, UK",
               "Hampton Court Palace, UK",
               "Guildford, UK",
               "Lane End, UK",
               "Kingsclere, UK",
               "Oxford, England",
               "Oxted, UK",
               "Salisbury, UK",
               "Southampton, England",
               "Staplefield, UK",
               "Winchfield, UK",
               "Euston Station, London",
               "London Bridge Station, London",
               "Marylebone Station, London",
               "Paddington Station, London",
               "Victoria Station, London",
               "Waterloo Station, London",
               "Gatwick Airport",
               "Luton Airport",
               "Heathrow Airport",
               "Southampton Airport",
               "Kent International Airport",
               "Bluewater Shopping Centre, England",
               "Windsor, England",
               "Woking, England",
               "Reading, England",
               "Bracknell, England",
               "Farnham, England",
               "Basingstoke, England")
  dest_name_short<-c("BEA","BRI","CAN","CSG","DOR",
                     "HCP","GFD","LNE","KGC","OXF",
                     "OXT","SAL","SOU","STP","WFD",
                     "EUS","LBR","MAR","PAD","VIC",
                     "WAT","GTW","LTA","LHR","SHA",
                     "KIA","BSC","WSR","WOK","RDG",
                     "BRK","FNM","BSG")
  if (datastub=="m11") {
    idest    <- c(2,3,4,5,6,7,10,16:21)
    raildest <- idest
  } else if (datastub=="nondom") {
    idest <- c(2,5,7,13,16:21,22,27:33) 
    raildest<-idest
  }
#  idest    <- c(1:21)
#  raildest <- c(1:21)
  airport  <- c(22:26)
} else if (region_id==9) {
  dest_name<-c("Alton Pancras, UK",
               "Bath, England",
               "Bournemouth, England",
               "Bristol, England",
               "Chipping Campden, UK",
               "Christchurch, UK",
               "Clifton, UK",
               "Dundry, UK",
               "Hungerford, UK",
               "Ledbury, UK",
               "Minehead, UK",
               "Portishead, UK",
               "West Littleton, UK",
               "West Stafford, UK",
               "Bristol Airport, UK",
               "Bournemouth Airport, UK",
               "Southampton Airport, UK",
               "Asda Patchway Supercentre, England",
               "Askerwell, England",
               "Tormarton, England",
               "Puddletown, England",
               "Poole, England",
               "Lyneham, England",
               "Royal Wooton Bassett, England",
               "Sutton Benger, England",
               "Highnam, England",
               "Brockhampton, England")
  dest_name_short<-c("ALT","BTH","BOU","BSC","CCP",
                     "CHR","CLF","DUN","HUN","LED",
                     "MIN","POR","WLT","WSF","BAI",
                     "BOA","SAI","BSL","ASK","TOR",
                     "PUD","POO","LYN","RWB","SUT",
                     "HNM","BRK")
  if (datastub=="m11") {
    idest    <- c(1,2,5,6,7,8,9,11,12,13,14)
    raildest <- idest
  } else if (datastub=="nondom") {
    idest<-c(2,8,18:27)
    raildest <- idest
  }
  airport  <- c(15:17)
} else if (region_id==10) {
  # West Midlands
  dest_name<-c("Abberley, UK",
               "Belfry Hotel, UK",
               "Buxton, UK",
               "Chilcote, UK",
               "Kenilworth, UK",
               "Kilpeck, UK",
               "Lapworth, UK",
               "Lichfield Trent Valley, UK",
               "Moreton-in-March, UK",
               "Pipe Aston, UK",
               "Princethorpe, UK",
               "Shrewsbury, UK",
               "Sutton Coldfield, UK",
               "Weethley, UK",
               "Wellesbourne, UK",
               "Weston Heath, UK",
               "Wetwood Stafford, UK",
               "Wilmcote, UK",
               "Birmingham Airport, UK",
               "Birmingham International Rail Station, England",
               "Solihull, England",
               "Henley-in-Arden, England",
               "Claverdon, England",
               "Stratford-upon-Avon, England",
               "Welford-on-Avon, England",
               "Warwick, England",
               "Elford, England",
               "Redditch, England",
               "Sytchampton, England",
               "Shifnal, England")
  dest_name_short<-c("ABB","BEL","BUX","CHI","KEN",
                     "KIL","LAP","LCH","MOR","PIP",
                     "PRI","SHR","SUT","WEE","WBN",
                     "WES","WET","WIL","BMX","BMR",
                     "SOL","HEN","CLA","STR","WEL",
                     "WAR","ELF","RED","SYT","SHI")
  if (datastub=="m11") {
    idest    <- c(1:18)
    raildest <- idest
  } else if (datastub=="nondom") {
    idest<- c(8,12,20:30)
    raildest<-idest
  }
  airport<-c(19)
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
               "Driffield, England",
               "Snainton, England",
               "Pannal, England",
               "Wetherby, England",
               "Moortown, England",
               "Selby, England",
               "Hutton Cranswick, England",
               "North Dalton, England",
               "University of Sheffield, England",
               "Edenthorpe, England",
               "Helmsley, England",
               "Kildale, England",
               "Brompton North Allerton, England",
               "Thirsk Rail Station, England")
  dest_name_short<-c("BFD","DON","HRG","KSN","LDS",
                     "OSM","RPN","SHF","YRK","MBH",
                     "NYM","SCU","LBA","SKP","DRF",
                     "SNA","PAN","WTH","MOO","SEL",
                     "HUT","NDN","USH","EDT","HLM",
                     "KIL","NAL","TSK")
  if (datastub=="m11") {
    idest    <- c(2,3,4,5,6,8,9,10,12,14,15)
    raildest <- c(2,3,4,5,6,8,9,10,12,14,15)
  } else if (datastub=="nondom") {
    idest<- c(9,12,16:28)
    raildest<-idest
  }
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

