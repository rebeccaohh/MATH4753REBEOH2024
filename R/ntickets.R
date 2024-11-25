#' ntickets
#'
#' @param N Number of seats on a flight
#' @param gamma the probability of overbooking
#' @param p the probability a passenger will show
#'
#' @return a list of nc, nd, N, p, gamma, and a plot
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=200, gamma=0.02, p=0.95)}
#'
#' @importFrom stats qbinom pbinom qnorm
#' @importFrom graphics abline barplot
ntickets = function(N, gamma, p){
  # discrete distribution
  nd_range = seq(N, N+50, by = 1) # setting up a range of numbers n could be
  qbinom_range = qbinom(1-gamma, nd_range, p) # gives us qbinom for each n created from n_range
  nd = nd_range[qbinom_range >= N] # in n_range, it gives us where qbinom is equal to or greater than N
  nd = nd[1] # indexing nd to return the first value in n_valid. This is the first n value where the qbinom is greater than or equal to N

  plot(1-gamma-pbinom(N, nd_range, p), main="Objective vs n to find optimal tickets sold (412) gamma=0.02,N=400 discrete", xlab="n added to N", ylab="Objective")
  abline(h=0, v=12, col="red")

  # normal approximation
  nc_range = seq(N, N+50, by=1) # setting up a range of numbers n could be
  nc_mean = nc_range*p # finding the mean, μ=n*p
  nc_sd = sqrt(nc_range*p*(1-p)) # finding the standard deviation, σ=sqrt(np(1-p))
  nc = nc_mean + qnorm(1-gamma) * nc_sd
  nc = nc_range[nc >= N]
  nc = nc[1]


  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}






