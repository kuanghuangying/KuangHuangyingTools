#' MLE
#' Computes the liklihood of a given distribution for data x
#' @param x vector
#' @param fun a distribution function
#' @param interval the interval where the optimization shoul search among
#'
#' @return return the log of the ppdf or pmf
#' @export
#' @examples
#' x <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
#' f <- function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' interval <- c(0,100)
#' func7(x,f,interval)
#'
func7 <- function(x,fun,interval){
  logl <- function(theta,x) sum(fun(theta,x))
  oout<-optimize(logl,maximum=TRUE, interval,x= x)
  return(oout$maximum)
}
