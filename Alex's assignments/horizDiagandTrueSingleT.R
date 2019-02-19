horizDiagandTrueSingleT <- function(tParCount = 11,
                                    tIndex = 6,
                                    minSize = 100,
                                    maxSize = 1000,
                                    sampleSizeStep = 100,
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

  sampleSizeSeq <- seq(from = minSize, to = maxSize, by = sampleSizeStep)

  # create tParArray with real numbers fro 0 to 1 with length tParCount
  tParArray <- createtParArray(tParCount)

  # compute true value, will be equal to 1 so we just assign it
  trueLRV <- 1

  # create empty arrays for true LRV and LRV with hat
  horizLRVarray <- array(data = 0, dim = length(sampleSizeSeq))
  diagLRVarray <- array(data = 0, dim = length(sampleSizeSeq))

  for (sample in 1:length(sampleSizeSeq)){
    # calculate psi
    psi <- sin(tIndex/sample)
    # compute bandWidth with given constant beta = -0.26
    bandWidth <- 1 * sampleSizeSeq[sample]^(-0.26)
    # compute batchSize with given constant alpha = 0.2
    batchSize <- round(1 * sampleSizeSeq[sample]^(0.2))

    if((tParArray[tIndex] < bandWidth) | (tParArray[tIndex] > 1-bandWidth)){
      cat("Error: The selected t is outside the 'sandbox'. ")
      return(NULL)
    }

    # compute horizontal and diagonal samples,
    # assign them to horizSample and diagSample
    horizAndDiag <- createDiagandHorizSamples(sampleSizeSeq[sample],
                                              tParCount,
                                              sigma,
                                              mean = 0)
    horizSample <- horizAndDiag[[1]]
    diagSample <- horizAndDiag[[2]]

    # calculate horizontal beta matrix
    horizBeta <- computeBetaMatrix(horizSample[tIndex,],
                                   psi,
                                   sigma,
                                   lag,
                                   tParArray[tIndex])

    # find the squares of block sums
    horizSquaredBatchSum <- computeSquaredBatchSum(horizBeta, batchSize)

    # compute kernel array
    kernelArray <- computeKernelArray(horizSquaredBatchSum,
                                      tParArray[tIndex],
                                      bandWidth)

    # find LRV hat for horizontal
    horizLRVarray[sample] <- computeLRVKernelHat(horizSquaredBatchSum,
                                                 kernelArray,
                                                 batchSize)

    # calculate diagonal beta matrix
    diagBeta <- computeBetaMatrix(diagSample,
                                  psi,
                                  sigma,
                                  lag,
                                  tParArray[tIndex])

    # find the squares of block sums
    diagSquaredBatchSum <- computeSquaredBatchSum(diagBeta, batchSize)

    # compute kernel array
    kernelArray <- computeKernelArray(diagSquaredBatchSum,
                                      tParArray[tIndex],
                                      bandWidth)

    # find LRV hat for horizontal
    diagLRVarray[sample] <- computeLRVKernelHat(diagSquaredBatchSum,
                                                kernelArray,
                                                batchSize)

    # Make the name of the output graph
    nameOfFile <- paste0("output/singleT(",
                         "maxsampleSize=", sampleSizeSeq[sample], ").svg")

    # create a graphic
    createLrvPlotScenario3SingleT(lrvXArray = diagLRVarray[1:sample],
                                  lrvYArray = horizLRVarray[1:sample],
                                  trueLrv = trueLRV,
                                  tPar = tParArray[tIndex],
                                  sampleSize = sampleSizeSeq[1:sample],
                                  fileName = nameOfFile,
                                  b = bandWidth,
                                  m = batchSize,
                                  mySeed = seed)
    msg = paste0("There are (", length(diagLRVarray) - sample,
                 ") iterations left until the end of the program\n")
    cat(msg)
  }
}
