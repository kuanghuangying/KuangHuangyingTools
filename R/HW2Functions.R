#' Given a numeric matrix A and a numeric vector x, calculates xTA−1x
#'
#' @param A matrix
#' @param x vector
#'
#' @return value of xTA−1x
#' @export
#' @examples
#' hw2_1(matrix(rnorm(20),nc=5,nr=4),c(7,8,9))
hw2_1 <- function(A, x){
  stopifnot(is.numeric(A))
  stopifnot(is.numeric(x))
  stopifnot(!is.na(A))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(A))
  stopifnot(!is.nan(x))
  stopifnot(!is.infinite(A))
  stopifnot(!is.infinite(x))
  a_row = nrow(A)
  a_col = ncol(A)
  if (a_row == a_col && a_row == length(x)){
    x_vec <- matrix(x,ncol=1)
    val <- t(x_vec) %*% solve(A) %*% x_vec
    return(val)
  }
  else{
    print('matrix or x has unmathable dimensions')
  }
}

#' Given a numeric matrix A and a numeric vector x, calculates xTA−1x (binary operator VERSION)
#'
#' @param A matrix
#' @param x vector
#'
#' @return value of xTA−1x
#' @export
#' @examples
#' matrix(rnorm(20),nc=5,nr=4) %question2% c(7,8,9)
"%question2%" <- function(A, x){
  stopifnot(is.numeric(A))
  stopifnot(is.numeric(x))
  stopifnot(!is.na(A))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(A))
  stopifnot(!is.nan(x))
  stopifnot(!is.infinite(A))
  stopifnot(!is.infinite(x))
  a_row = nrow(A)
  a_col = ncol(A)
  if (a_row == a_col && a_row == length(x)){
    x_vec <- matrix(x,ncol=1)
    val <- t(x_vec) %*% solve(A) %*% x_vec
    return(val)
  }
  else{
    print('matrix or x has unmathable dimensions')
  }
  cat(A," ",x)
}

#' Column standardization
#'
#' Takes a numeric matrix and standardizes its columns
#'
#' @param m matrix
#'
#' @return standardized matrix
#' @export
#' @examples
#' q3(matrix(rnorm(20),nc=5,nr=4))
q3 <- function(m){
  stopifnot(nrow(m) > 1)
  for (i in 1:ncol(m)){
    m[,i] = (m[,i] - mean(m[,i]))/sd(m[,i])
  }
  return(m)
}

#' Column standardization (not using loop)
#'
#' Takes a numeric matrix and standardizes its columns
#'
#' @param m matrix
#'
#' @return standardized matrix
#' @export
#' @examples
#' q4(matrix(rnorm(20),nc=5,nr=4))
q4 <- function(m){
  stopifnot(nrow(m) > 1)
  val = apply(a,2,function(x) (x-mean(x))/sd(x))
  return(val)
}


#' A function similar to apply()
#'
#' @param X matrix of any type
#' @param MARGIN either (the number) 1 or (the number) 2
#' @param FUN R function that maps vectors to vectors
#' @param ... optional parameters for plotting
#'
#' @return list
#' @export
#' @examples
#' myapply(matrix(1:6, nrow = 3, ncol = 2),1,mean)
myapply <- function (X, MARGIN,FUN,...){
  if (!MARGIN %in% c(1,2)){
    stop("margin is not 1 or 2")
  }
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)

  if (MARGIN ==1 ){
    result = list()
    for (i in 1:R){
      result[[i]] = f(X[i,],...)
    }
  }
  else if(MARGIN ==2 ){
    result = list()
    for (j in 1:C){
      result[[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}
