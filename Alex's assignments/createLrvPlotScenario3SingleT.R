createLrvPlotScenario3SingleT <- function(lrvXArray,
                                          lrvYArray,
                                          trueLrv,
                                          sampleSizeArray,
                                          fileName,
                                          tPar,
                                          b,
                                          m,
                                          mySeed)
{
  arraySize <- length(lrvXArray)
  color1Array <- array(data = "blue", dim = arraySize)
  color2Array <- array(data = "red", dim = arraySize)

  svg(fileName)
  plot(x = c(sampleSizeArray, sampleSizeArray), y = c(lrvXArray, lrvYArray),
       col = c(color1Array, color2Array), xlab = "Sample size", ylab = "LRV",
       xlim = c(0, sampleSizeArray[length(sampleSizeArray)] + sampleSizeArray[1]), ylim = c(0, 4),
       main = "Graph for single-t", pch = 21, bg = c(color1Array, color2Array))
  abline(h = trueLrv, col = "black")
  legend("topright", legend = c("true LRV", "diagonal", "horizontal"),
         col = c("black", "blue", "red"), lty=1:1, cex = 0.8)

  tLegend <- paste("t =", tPar)
  pointCountLegend <- paste("number of points =", arraySize)
  lastBandWidthLegend <- paste("last b =", round(b, 4))
  lastbatchSizeLegend <- paste("last m =", m)
  maxSampleSizeLegend <- paste("Max sample size =", max(sampleSizeArray))

  if(mySeed){
    seed <- "seed is const"
  }else{
    seed <- "seed is random"
  }

  lines(sampleSizeArray, lrvXArray, col = "blue")
  lines(sampleSizeArray, lrvYArray, col = "red")


  legend("topleft", legend = c(tLegend, pointCountLegend,
                               maxSampleSizeLegend,
                               seed),
         cex = 0.8)

  dev.off()
}
