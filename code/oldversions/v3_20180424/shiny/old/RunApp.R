host<-system("hostname",intern=TRUE)
if (host=="dh-230-mac.econ.ucl.ac.uk") {
  RootDir<-"/Users/uctpln0/Documents/research/hedonic/NIC"
} else if (host=="minelava") {
  RootDir<-"C:/a/research/hedonic/NIC"
} else if (host=="DH-G06-03") {
  RootDir<-"U:/NICProject"
} else if (host=="jake.local" | host=="vic.local") {
  RootDir<-"/home/uctpln0/hedonic/NIC"
} else {
  info_sys<-Sys.info()
  user<-info_sys["user"]
  if (user=="uctpln0") {
    RootDir<-"/home/uctpln0/hedonic/NIC"
  } else {
    RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
  }
}
source(paste0(RootDir,"/code/model/B1LoadLibrary.R"))
source(paste0(RootDir,"/code/model/B2SetPath.R"))
source(paste0(RootDir,"/code/model/A4CreateSettlement.R"))
source(paste0(RootDir,"/code/model/A5CreateTransport.R"))
source(paste0(RootDir,"/code/model/A6PredictPrice.R"))
source(paste0(RootDir,"/code/model/A7NewRoad.R"))

runApp('~/Documents/research/hedonic/NIC/code/shiny/app2_choose_investment.R')


# Call knitr from shiny
\documentclass{article}

\begin{document}

<<names>>=
  input$firstname
input$lastname
@
  
\end{document}

shinyServer(function(input, output) {
  output$report = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('input.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
})

shinyUI(basicPage(
  textInput('firstname', 'First name', value = 'Jimmy'),
  textInput('lastname', 'Last name', value = 'John'),
  downloadButton('report')
))



