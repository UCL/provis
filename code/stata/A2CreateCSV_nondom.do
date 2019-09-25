if strmatch("$S_OS","MacOSX") {
*  global RootDir "/Users/larsnesheim/Documents/research/hedonic/NIC"
*  global DataDir "${RootDir}/data"
} 
else if strmatch("$S_OS","Unix") {
*  global RootDir "/home/uctpln0/hedonic/NIC"
*  global DataDir "${RootDir}/data"
}
else {
  global RootDir "C:\a\research\hedonic\NIC"
  global HPCDir "O:\NICProject\NonDomesticModel"
  global DataDir "${RootDir}\data"
}

global model 2
global nregions 11

local i1 0
foreach x in CaMKOx CornwallDevon EastMid EastEng London NE NW SE SW WestMid YorkshireHumber {
  local i1 = `i1'+1
  if ${model}==2 {
    if strmatch("$S_OS","MacOSX") {
      *use "${DataDir}/model2/m11_`x'.dta", clear
      *export delimited using "${DataDir}/model2/m11_`x'.csv", replace  
    } 
    else if strmatch("$S_OS","Unix") {
      *use "${DataDir}/model2/m11_`x'.dta",clear
      *export delimited using "${DataDir}/model2/m11_`x'.csv",replace
    }
    else {
      use "${HPCDir}\ndmodel2_`x'.dta", clear
      export delimited using "${HPCDir}\nondom_`x'.csv", replace  	
    }
  }
}
