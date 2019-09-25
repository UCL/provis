#' Create list of unique variables included in model (e.g. stripping out log transformations)
#' @param  varlist  list of variables in model (including transformations)
#' @return varlist  list of unique variables in model
#' @export
#' @examples 
#' varlist0<-B4GetVarList(names(m2ols0$model))
B4GetVarList<-function(varlist) {
  varlist<-gsub(">= 10","",varlist)
  varlist<-gsub(">= 30","",varlist)
  varlist<-gsub("< 30","",varlist)
  varlist<-gsub("< 20","",varlist)
  varlist<-gsub("< 10","",varlist)
  
  varlist<-gsub("log\\(","",varlist)
  varlist<-gsub("\\)\\^2","",varlist)
  varlist<-gsub("\\)\\^3","",varlist)
  varlist<-gsub("I\\(","",varlist)
  varlist<-gsub("\\)","",varlist)
  varlist<-gsub("\\*","",varlist)
  varlist<-gsub("1 \\+ ","",varlist)
  varlist<-gsub("== 1","",varlist)
  varlist<-gsub("== 2","",varlist)
  varlist<-gsub("== 3","",varlist)
  varlist<-gsub("\\(","",varlist)
  varlist<-gsub("\\^2","",varlist)
  varlist<-gsub("as.character","",varlist)
  varlist<-gsub("== \\\"DARTMOOR\\\"","",varlist)
  varlist<-str_squish(varlist)
  v1<-word(varlist)
  v2<-word(varlist,-1)
  v3<-c(v1,v2)  
  varlist<-unique(v3)
  return(varlist)
}