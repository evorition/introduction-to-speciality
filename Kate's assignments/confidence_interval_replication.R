confidence_interval_replication <- function(n = 30,
                                            mean = 2,
                                            sd = sqrt(1/3),
                                            r = 1000,
                                            gamma = 0.95){
  
  t <- abs(qnorm((1-gamma)/2))
  
  failed_intervals <- 0
  
  upper_bound <- c()
  lower_bound <- c()
  
  for(counter in seq(r)){
    X <- rnorm(n = n, mean = mean, sd = sd)
    a_hat <- mean(X)
    sd_hat <- sd(X)
    delta <- t * sd_hat / sqrt(n)
    upper <- a_hat + delta
    lower <- a_hat - delta
    
    if(lower > mean || upper < mean){
      failed_intervals <- failed_intervals + 1
    }
    
    upper_bound <- c(upper_bound, upper)
    lower_bound <- c(lower_bound, lower)
  }
  
  cat(failed_intervals, "\n")
}