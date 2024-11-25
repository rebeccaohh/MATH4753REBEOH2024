#' myci
#'
#' @param x data set
#'
#' @return a 95% confidence interval
#' @export
#'
#' @examples
#' \dontrun{myci(x)}
#'
#' @importFrom stats qt
myci = function(x){
  n=length(x)
  t=qt(0.975,n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
}
