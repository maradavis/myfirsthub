#' CLT Mean Function
#'
#' This will generate an n-sized random sample from the uniform distribution for a given
#' amount of iterations. After doing this, it will calculate the mean and then create a histogram for that sample.
#'
#' @param n the size of the random sample
#' @param iter the number of iterations within the simulation
#'
#' @return the vector containing the vector of mean values for each iteration's
#' random sample
#'
#' @export
#'
mycltMean=function(n,iter){

  #generates random sample from uniform of (n*iter) size
  y=runif(n*iter,0,5)

  #transforms Y into a (n x iter) matrix
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)

  #calculates the mean of each column
  mn=apply(data,2,mean)

  #forms histogram of the mean
  hist(mn)

  #mean of each iteration's random sample
  mn
}
