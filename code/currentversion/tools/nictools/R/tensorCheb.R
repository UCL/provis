#' Compute tensor product of Chebyshev polynomials
#' @param xdata  (n x 4) matrix of data
#' @param k      k[1]-1 =highest order of polynomial
#'               k[2]^3*k[1] = dimension of subset of tensor product
#' @return x     (n x k[1]*k[2]^3)  matrix of subset of tensor product of Chebyshev polynomials
#' @examples
#' xdata<-ttsample[,c("origin_x","origin_y","dest_x","dest_y")]
#' k<-c(25,6)
#' x <- tensorCheb(xdata,k)
tensorCheb<-function(xdata,k,lo=NULL,hi=NULL) {

  nx<-ncol(xdata)
  if (is.null(lo)) {
    lo<-vector("numeric",4)
    hi<-vector("numeric",4)
    for (i1 in 1:nx) {
      lo[i1]<-min(xdata[,i1])
      hi[i1]<-max(xdata[,i1])
    }
  }
  for (i1 in 1:nx) {
    x<-2*(xdata[,i1]-lo[i1])/(hi[i1]-lo[i1])-1
    assign(paste0('b',as.character(i1)),cheb(x,k[1]))
  }

  x<-kronecker(b1,matrix(rep(1,k[2]^3),1,k[2]^3))
  temp<-kronecker(b2,matrix(rep(1,k[2]^2),1,k[2]^2))
  temp<-kronecker(matrix(rep(1,k[2]),1,k[2]),temp)
  x<-x*temp
  temp<-kronecker(b3,matrix(rep(1,k[2]),1,k[2]))
  temp<-kronecker(matrix(rep(1,k[2]^2),1,k[2]^2),temp)
  x<-x*temp
  x<-x*kronecker(matrix(rep(1,k[2]^3),1,k[2]^3),b4)

  rm(temp)
  y<-list(x,lo,hi)
  names(y)<-c("x","lo","hi")
  return(y)
}
