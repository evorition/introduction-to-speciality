computeBetaMatrix <- function(sample,
                              psi,
                              sigma,
                              lag,
                              tPar){
  size <- length(sample)
  betaMatrix <- array(data = 0, dim = size-1)

  # create rho
  if(lag == 0){
    rho <- 1
  } else{
    rho <- 0
  }

  # calculate gamma
  gamma <- tPar^2 * sigma^2

  for(i in 1:size-1){
    betaMatrix[i] <- sample[i] * (sample[i+lag] - sample[i]*rho)/gamma
  }

  return(betaMatrix[!is.na(betaMatrix)])
}
