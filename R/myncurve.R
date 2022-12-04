#' Curve Functioni
#'
#' This function will make it easier to plot a shaded area within a curve by calculating the area for you given the certain parameters.
#'
#' @param mu = mean
#' @param sigma = standard deviation
#' @param a = bounds
#' @return a curve with a shaded area
#' @export
#'

myncurve = function(mu, sigma, a){
  x = NULL
  curve(dnorm(x, mean = mu, sd = sigma), xlim=c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu-3*sigma,a,length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon (c(mu-3*sigma, xcurve, a),c(0,ycurve,0),col="Red")
  prob=round(pnorm(a, mu, sigma),4)

}
