#' Calculate Confidence Interval for the Mean
#'
#' @param x Numeric vector of data.
#'
#' @return A numeric vector representing the confidence interval for the mean.
#' @export
#' @importFrom stats qt sd
#' @examples
#'
#' x = rnorm(30,mean=10,sd=12)
myci = function(x){
  alpha1<-0.05
  n<-length(x)
  t1<-qt(1-alpha1/2,n-1)
  mp<-c(-1,1)
  mean(x)+mp*t1*sd(x)/sqrt(n)

}
