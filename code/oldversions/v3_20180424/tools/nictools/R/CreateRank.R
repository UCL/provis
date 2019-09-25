#' Rank each variable of type travelmode.
#' @param m2data      dataframe, with columns of travel times
#' @param travelmode  c("drive","trans")
#' @return m2data
#' @examples
#' Finds all columns with travel times of current type, and ranks the travel times
#' from lowest to highest. For example, if travelmode=="drive", it finds all columns
#' containing drive_times and ranks them. Creates new variables in dataframe
#' containing the rankings.#'
#' m2data<-CreateRank(m2data,"drive")
#' m2data<-CreateRank(m2data,"trans")
CreateRank<-function(m2data,travelmode) {
  if (travelmode=="drive") {
    itemp<-grep("\\<drive_[^acmnst]",colnames(m2data))
    iAONB<-grep("\\<drive_AONB",colnames(m2data))
    if (length(iAONB)>0) {
      itemp<-itemp[itemp!=iAONB]
    }
  } else if (travelmode=="trans") {
    itemp<-grep("\\<trans_[^acmnst]",colnames(m2data))
  }

  f3<-function(x) rank(m2data[x,itemp])

  travelrank<-data.frame(t(sapply(1:nrow(m2data),f3,simplify=TRUE)))
  n1<-colnames(travelrank)
  n1<-gsub(travelmode,paste0("rank",travelmode),n1)
  colnames(travelrank)<-n1
  m2data<-cbind(m2data,travelrank)

  m2data$temprank<-NA
  for (i1 in 1:length(itemp)) {
    m2data$temp<-m2data[,itemp[i1]] * (travelrank[,i1]<=3)
    i2<-ncol(m2data)
    colnames(m2data)[i2]<-paste0("x",colnames(m2data[,itemp][i1]))
    m2data$temprank[travelrank[,i1]==1] <- i1
  }
  m2data$temprank<-factor(m2data$temprank)
  i1<-grep("temprank",colnames(m2data))
  colnames(m2data)[i1]<-paste0("rank",travelmode)
  return(m2data)
}
