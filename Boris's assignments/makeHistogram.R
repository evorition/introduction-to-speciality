makeHistogram <- function(obj, nameOfFile, title){
  path <- file.path("/home", "/maxim/", "experiments_with_R", 
                    paste(nameOfFile, sep=""))
  png(path, width=700, height=700, units="px")
  hist(x=obj, main=title)
  dev.off()
}