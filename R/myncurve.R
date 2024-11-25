#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return a normal distribution curve
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=4, sigma=3)}
#'
#' @importFrom stats dnorm
#' @importFrom graphics curve
myncurve = function(mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))
  list(mu = mu, sigma = sigma)
}
