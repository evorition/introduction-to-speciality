createDiagandHorizSamples <- function(sampleSize = 1000,
                                      tParCount = 100,
                                      sigma = 2,
                                      mean = 0){
  # create noise
  noise <- rnorm(sampleSize, mean, sigma)

  # compute vectorized implementation of horizontalSample
  # final product will be two-dimensional array with size tParCount*sampleSize
  horizontalSample <- t(noise %*% t(seq(tParCount)/tParCount))

  # compute vectorized implementation of diagonalSample
  # final product will be one-dimensional array with size sampleSize
  diagonalSample <- seq(sampleSize)/sampleSize*noise

  return(list(horizontalSample, diagonalSample))
}
