#' Project 1 function
#'
#' @param N The number of tickets.
#' @param gamma The gamma parameter for the calculation.
#' @param p The probability parameter for the calculation.
#'
#' @return The result of the calculation.
#' @export
#' @importFrom graphics layout
#' @importFrom stats pbinom pnorm qbinom qnorm uniroot
#' @examples ntickets(N=400, gamma = 0.02, p = 0.95)
ntickets <- function(N,gamma,p) {
  # Variable initialization finding n
  n <- seq(1,N+20,length=N+20)

  # Finding nd
  nd <- abs(qbinom(1-gamma, n, p)-N)
  mnd <- which.min(nd) # using which mean

  # Finding nc to print on the table
  nc <- function(n) {
    sigma = sqrt(n * p * (1-p))
    mu = n * p
    nc_result <- qnorm(1-gamma, mu, sigma) - N - 0.5
  }

  # create equation to graph discrete
  fnd <- 1-gamma-pbinom(N,n,p)
  fnd1 <- which.min(abs(fnd))

  #To draw a graph for continous
  fnc <- function(n) {
    sigma = sqrt(n * p * (1-p))
    mu = n * p
    fnc_result <- 1-gamma-pnorm(N+0.5, mu, sigma)
  }

  nc_uni<-uniroot(nc,c(1, N+20))
  nc_root<- nc_uni$root
  fnc_uni<-uniroot(fnc,c(1, N+20))
  fnc_root<- fnc_uni$root

  #lay out to make two plots
  layout(matrix(c(1:2),nrow=2,ncol=2))

  # Using plot for discrete
  plot(fnd, col='black', xlim=c(N,N+20), xlab = "n", ylab = "Objective", main = paste("Objective function Vs n to find tickets sold","\n","(",fnd1,")","gamma=",gamma,"N=",N, "discrete"), lwd=1, pch=16, cex=1, type = "b")
  abline(v=fnd1,h=fnd[fnd1],lwd=2,col='red')

  # Using plot for continuous
  plot(fnc, xlim=c(N,N+20), xlab = "n", ylab = "Objective", main = paste("Objective function Vs n to find tickets sold","\n","(",fnc_root,")","gamma=",gamma,"N=",N, "continuous"), col='black', lwd=1, type = "l")
  abline(v=fnc_root,h=0,lwd=1, col='blue')

  # print the table include nc, nd, N, p, and gamma
  list(nd = mnd, ncol = nc_root, N = N, p = p, gamma = gamma)
}
