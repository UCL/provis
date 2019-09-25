#' Create list of unique variables included in model (e.g. stripping out log transformations)
#' @param  varlist  list of variables in model (including transformations)
#' @return varlist  list of unique variables in model
#' @export
#' @examples 
#' varlist0<-B4GetVarList(names(m2ols0$model))
B4GetVarList<-function(varlist) {
  varlist<-gsub("log\\(","",varlist)
  varlist<-gsub("\\)\\^2","",varlist)
  varlist<-gsub("\\)\\^3","",varlist)
  varlist<-gsub("I\\(","",varlist)
  varlist<-gsub("\\)","",varlist)
  varlist<-unique(varlist)
  return(varlist)
}