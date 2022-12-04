#' Hypergeometric Function
#'
#' This function will make it easier to plot one sample using the hypergeomtric distribution
#'
#' @param n = sample size
#' @param N = total size of group
#' @param r = specific sub group
#' @param iter = iterations
#' @return a histogram plot
#' @export
#'
#' @examples
#'
hypergeo <- function(iter = 100, N = 20, r = 12, n = 6){
    sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
    succ=c()
    for(i in 1:iter){
      sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
      succ[i]=sum(sam.mat[,i])
    }
    succ.tab=table(factor(succ,levels=0:n))
    barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
    succ.tab/iter
}
