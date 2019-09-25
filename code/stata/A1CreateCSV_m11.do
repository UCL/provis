if strmatch("$S_OS","MacOSX") {
  global RootDir "/Users/larsnesheim/Documents/research/hedonic/NIC"
  global DataDir "${RootDir}/data"
} 
else if strmatch("$S_OS","Unix") {
  global RootDir "/home/uctpln0/hedonic/NIC"
  global DataDir "${RootDir}/data"
}
else {
  global RootDir "C:\a\research\hedonic\NIC"
  global DataDir "${RootDir}\data"
}

global model 2
global nregions 11

local i1 0
foreach x in CaMKOx CornwallDevon EastMid EastEng London NE NW SE SW WestMid YorkshireHumber {
  local i1 = `i1'+1
  if ${model}==1 {
    use "${DataDir}/model1/model1_`x'.dta", clear
    export delimited using "${DataDir}/region`i1'/model1_`x'.csv", replace
  }
  else {
    if strmatch("$S_OS","MacOSX") {
      use "${DataDir}/model2/m11_`x'.dta", clear
      export delimited using "${DataDir}/model2/m11_`x'.csv", replace  
    } 
    else if strmatch("$S_OS","Unix") {
      use "${DataDir}/model2/m11_`x'.dta",clear
      export delimited using "${DataDir}/model2/m11_`x'.csv",replace
    }
    else {
      use "${DataDir}\model2\m11_`x'.dta", clear
      export delimited using "${DataDir}\region`i1'\m11_`x'.csv", replace  	
    }
  }
}
if ${model}==1 {
  use "${DataDir}/model1/model1_Londonplus.dta", clear
  export delimited using "${DataDir}/region5/model1_Londonplus.csv", replace
}
