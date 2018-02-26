
#' Plot my data
#' @export
plotMyData <- function () {
  #plo = match.fun(ggplot2::ggplot)
  #plo(...) + geom_point() 
  ggplot2::ggplot(cleanData,aes(x = Petal.Length, y = Petal.Width)) + geom_point() 
}
