#' cheby
#'
#' @param x = uses data given to solve for the percentage of data within a standard deviation
#'
#' @return a numerical value stating the amount of data present within a certain number of standard deviation
#' @export
#'
#' @examples
cheb = function(x){
  cheb = 1 - (1/x^2)
}
