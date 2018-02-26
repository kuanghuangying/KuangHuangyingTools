
#' slice my data and select 3 columns
#' @export
selectData <- function () {
  selectedData <- cleanData %>% 
     dplyr::select(Petal.Length, Petal.Width, Species)
  return(selectedData)
}
