#' myclt
#'
#' @param n size
#' @param iter number of iterations
#' @param a lower limit
#' @param b upper limit
#'
#' @return a histogram
#' @export
#'
#' @examples
#' \dontrun{w=myclt(n=50,iter=10000,a=5,b=10)}
#'
#' @importFrom stats runif
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
