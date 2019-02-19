horizDiagandTrueSingleN <- function(tParCount = 100,
                                    sampleSize = 1000,
                                    sigma = 2,
                                    lag = 2,
                                    mySeed = 42){

  # Create output directory for image if it doesn't exist
  if (!dir.exists("output")){
    dir.create("output", recursive = T)
  }

  # Set a logical variable to display
  # whether the seed is fixed or not in the legend
  if(mySeed == "random"){
    seed <- F
  } else{
    set.seed(mySeed)
    seed <- T
  }

  # compute bandWidth with given constant beta = -0.26
  bandWidth <- 1 * sampleSize^(-0.26)

  # compute batchSize with given constant alpha = 0.2
  batchSize <- 1 * round(sampleSize^(0.2))

  # compute horizontal and diagonal samples,
  # assign them to horizSample and diagSample
  horizAndDiag <- createDiagandHorizSamples(sampleSize,
                                            tParCount,
                                            sigma,
                                            mean = 0)
  horizSample <- horizAndDiag[[1]]
  diagSample <- horizAndDiag[[2]]

  # create tParArray with real numbers fro 0 to 1 with length tParCount
  tParArray <- createtParArray(tParCount)

  # create sandbox bounds
  sandBoxBounds <- createSandBoxBounds(tParArray, bandWidth)

  bounds <- sandBoxBounds[2] - sandBoxBounds[1]

  if(bounds <= 0){
    warning("SandBoxBounds is too small. Use a larger sample size.")
    return(NULL)
  }

  # create empty arrays for true LRV and LRV with hat
  trueLRVarray <- array(data = 0, dim = bounds)
  horizLRVarray <- array(data = 0, dim = bounds)
  diagLRVarray <- array(data = 0, dim = bounds)

  for(i in (sandBoxBounds[1]:sandBoxBounds[2])){
    # create index for arrays
    idx <-  i - sandBoxBounds[1] + 1

    # calculate psi
    psi <- sin(i/sampleSize)

    # first compute true value, but it always be 1
    # so we just add it to the array
    trueLRVarray[idx] <- 1

    # calculate horizontal beta matrix
    horizBeta <- computeBetaMatrix(horizSample[i,],
                                   psi,
                                   sigma,
                                   lag,
                                   tParArray[i])

    # find the squares of block sums
    horizSquaredBatchSum <- computeSquaredBatchSum(horizBeta, batchSize)

    # compute kernel array
    kernelArray <- computeKernelArray(horizSquaredBatchSum,
                                      tParArray[i],
                                      bandWidth)

    # find LRV hat for horizontal
    horizLRVarray[idx] <- computeLRVKernelHat(horizSquaredBatchSum,
                                              kernelArray,
                                              batchSize)

    # calculate diagonal beta matrix
    diagBeta <- computeBetaMatrix(diagSample,
                                  psi,
                                  sigma,
                                  lag,
                                  tParArray[i])

    # find the squares of block sums
    diagSquaredBatchSum <- computeSquaredBatchSum(diagBeta, batchSize)

    # compute kernel array
    kernelArray <- computeKernelArray(diagSquaredBatchSum,
                                      tParArray[i],
                                      bandWidth)

    # find LRV hat for horizontal
    diagLRVarray[idx] <- computeLRVKernelHat(diagSquaredBatchSum,
                                             kernelArray,
                                             batchSize)
  }

  # Make the name of the output graph
  nameOfFile <- paste0("output/BetaMatrixSingleN(lag=", lag,
                       ",sampleSize=", sampleSize,
                       ",tParCount=", tParCount,").svg")

  # create a graphic
  createLrvPlotScenario3SingleN(lrvXArray = diagLRVarray,
                                lrvYArray = horizLRVarray,
                                trueLrvArray = trueLRVarray,
                                tParArray = (tParArray[sandBoxBounds[1]:
                                                         sandBoxBounds[2]]),
                                sampleSize = sampleSize,
                                fileName = nameOfFile,
                                b = bandWidth,
                                m = batchSize,
                                mySeed = seed)
}
