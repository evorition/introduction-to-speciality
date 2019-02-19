createLrvPlotScenario3SingleN <- function(lrvXArray,
                                          lrvYArray,
                                          trueLrvArray,
                                          sampleSize,
                                          fileName,
                                          tParArray,
                                          b,
                                          m,
                                          mySeed)
{
  arraySize <- length(lrvXArray)
  color1Array <- array(data = "blue", dim = arraySize)
  color2Array <- array(data = "red", dim = arraySize)
  color3Array <- array(data = "black", dim = arraySize)

  svg(fileName)
  plot(x = c(tParArray, tParArray, tParArray), y = c(lrvXArray, lrvYArray, trueLrvArray),
       col = c(color1Array, color2Array, color3Array), xlab = "tPar", ylab = "LRV",
       xlim = c(0, max(tParArray)+tParArray[1]), ylim = c(0, max(c(lrvXArray, lrvYArray, trueLrvArray))),
       main = "Graph for single-N", pch = 21, bg = c(color1Array, color2Array, color3Array))

  legend("bottomright",
         legend = c("true LRV", "diagonal", "horizontal"),
         col = c("black", "blue", "red"), lty=1:1, cex = 0.8)

  sampleSizeLegend <- paste("Sample size =", sampleSize)
  pointCountLegend <- paste("number of points =", arraySize)
  lastBandWidthLegend <- paste("b =", round(b, 4))
  lastbatchSizeLegend <- paste("m =", m)

  if(mySeed){
    seed <- "seed is const"
  }else{
    seed <- "seed is random"
  }

  legend("topleft",
         legend = c(sampleSizeLegend, pointCountLegend,
                    lastBandWidthLegend, lastbatchSizeLegend,
                    seed),
         cex = 0.8)

  dev.off()
}
