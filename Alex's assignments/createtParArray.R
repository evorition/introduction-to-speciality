createtParArray <- function(tParCount){
  if (tParCount <= 1){
    stop("tParCount must be 2 or greather")
  }

  tParCount <- tParCount - 1
  tParArray <- seq(from = 0, to = 1-1/tParCount, by = 1/tParCount)
  tParArray <- c(tParArray, 1)

  return(tParArray)
}
