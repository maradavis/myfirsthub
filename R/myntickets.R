#' Ntickets Function
#'
#' @param N Number of seats, default 200
#' @param gamma Probability of passenger not showing up, default 0.02
#' @param p Probability that N or fewer passengers do show up, default 0.95
#'
#' @return Produces a graph for the binomial and normal distribution for objective v n Also prints a named list of nd, nc, N, p, and gamma.
#' @export
#'
#' @examples
#'
ntickets <- function(N = 200, gamma = 0.02, p = 0.95){
  n <- seq(N, floor(N + N/10), by = 1)

  # Binomial
  tmpbin = qbinom(p = 1-gamma, size = n, prob = p)
  tmplen = length(tmpbin)

  #for loop applied
  for (i in 1:tmplen) {
    if (tmpbin[i] >= N) {
      nd = tmpbin[i] + i
      break
    }
  }

  tmp <- 1 - gamma - pbinom(q = N, size = n, prob = p)
  ind <- which.min(abs(tmp))
  plot(x = n, y = tmp, xlab = "n", ylab = "Objective", type = "b", pch = 19)
  title(main = "Objective Vs n to find optimal tickets sold that is discrete")

  #creating the horizontal and vertical lines on the first plot
  abline(v = nd, col = 'Red')
  abline(a = abs(tmp[ind]), b = 0, col = 'Red')

  # Normal approximation
  n2 <- seq(N, floor(N + N/10), by = 1)

  tmpnorm = qnorm(1 - gamma, n * p, (n * p * (1-p))^(0.5)) + 0.5
  tmpnlen = length(tmpnorm)

  for (i in 1:tmpnlen) {
    if (tmpnorm[i] >= N + 0.5) {
      nc = tmpnorm[i] + i - 1
      break
    }
  }

  #placing in the normal distribution into a new tmp function
  tmp2 <- 1 - gamma - pnorm(q = N, mean = n * p, sd = (n * p * (1-p))^(0.5))
  ind2 <- which.min(abs(tmp2))
  curve(1 - gamma - pnorm(q = N, mean = x * p, sd = (x * p * (1-p))^(0.5)) , xlim = c(N, N + N/10))
  plot(x = n2, y = tmp2, xlab = "n", ylab = "Objective", type ='l')
  title(main = "Objective Vs n to find optimal tickets sold that is continuous")

  #plotting the "points" on the graph
  abline(v = nc, col = 'Blue')
  abline(a = abs(tmp2[ind2]), b = 0, col = 'Blue')

  #prints out the named list of values
  listn <- list(nd = nd, nc = nc, N = N, gamma = gamma, p = p)
  print(listn)
}
