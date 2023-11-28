#' Make a normal
#'
#' This function will make a plot of a random sample
#'
#' @param n Sample size for each iteration.
#' @param p Probability of success for each trial.
#' @param iter Number of iterations to simulate.
#'
#' @return A histogram
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#' @examples
#'
myf=function(iter = 1000, n = 10, p = 0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  #Make a vector to hold the number of successes in each trial
  succ = c()
  for(i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i] = sample(c(1,0), n, replace = TRUE, prob = c(p,1-p))

    #Calculate a statistic from the sample (this case it is the sum)
    succ[i] = sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab = table(factor(succ, levels = 0:n))

  #Make a barplot of the proportions
  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(succ.tab/(iter), col = rainbow(n+1), main = "Binomial simulation", sub = lab, ylab="Number of successes")
  succ.tab/iter
}
