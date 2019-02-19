createSandBoxBounds <- function(tParArray, bandWidth){
  # create empty array with lenghth = 2
  sandBoxBounds <- array(data = 0, dim = 2)

  # assign sandbox boundaries from tParArray index
  sandBoxBounds[1] <- min(which(tParArray > bandWidth))
  sandBoxBounds[2] <- max(which(tParArray < 1 - bandWidth))

  return(sandBoxBounds)
}
