#' Compute logit based average travel time to each location
#' @param  m2data   input data frame
#' @return m2data   output data frame with new average time variables 
ComputeAverageTime <- function(m2data) {
  idrive <- grep("\\<drive_",colnames(m2data))
  itrans <- grep("\\<trans_",colnames(m2data))
  
  # tran prob = (pi(1) + pi(6)) / (pi(1) + pi(6) + pi(3) + pi(4)  + pi(7))
  # pi(1) = bus
  # pi(6) = rail
  # pi(4) = drive
  # pi(5) = passenger
  # pi(7) = taxi
  # pi(2) = walk
  # pi(3) = cycle
  # pi(i1) = exp(alpha[i1] + beta * time[i1]) / D
  # modes <- c("bus","walk","cycle","drive","passenger","rail","taxi")
  alpha <- c(0,
              1.355064,
              -1.426877,
              1.623076,
              .9763002,
              -2.975188,
              -2.11932)
  beta <- -.0162493
  
  for (i1 in idrive) {
    # extract name of destination
    destname <- strsplit(colnames(m2data)[i1],"_")[[1]][2]
    transname <- paste0("\\<trans_",destname)
    i2 <- grep(transname,colnames(m2data))
    if (length(i2)>0) {
      # average time, weighted by mode choice probability
      drive_time<-m2data[,i1]
      trans_time<-m2data[,i2]
      pi1 <- exp(alpha[4]+beta*drive_time/60)+
             exp(alpha[5]+beta*drive_time/60)+
             exp(alpha[7]+beta*drive_time/60)
      pi2 <- exp(beta*trans_time/60) + exp(alpha[6]+beta*trans_time/60)
      driveprob <- pi1/(pi1+pi2)
      m2data$temp <- driveprob*drive_time +
                     (1-driveprob)*trans_time
      itemp <-grep("temp",colnames(m2data))
      colnames(m2data)[itemp] <- paste0("avgtime_",destname)
      m2data$driveprob <- driveprob
      itemp <- grep("driveprob",colnames(m2data))
      colnames(m2data)[itemp] <- paste0("probdrive_",destname)
    }
  } # for (i1 in idrive)
  return(m2data)
}
