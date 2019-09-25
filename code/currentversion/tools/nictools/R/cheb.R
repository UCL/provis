#' Compute matrix of chebyshev polynomials
#' @param x     vector of values betwen -1 and 1
#' @param n     n-1 = highest order polynomial
#' @return cheb (nx x n) matrix of results
#' @examples
#' x<-matrix(2*c(0:10)/11-1,11,1)
#' c1< cheb(x,5)
cheb<-function(x,n) {
  nx<-nrow(as.matrix(x))
  cheb <-matrix(rep(1,n*nx),nx,n)
  cheb[,2] <- as.matrix(x)

  for (j in 3:n) {
    cheb[,j] <- 2.0*as.matrix(x) * cheb[,j-1]-cheb[,j-2]
  }
  cheb[,1]<- 0.5*cheb[,1]
  return(cheb)
}
