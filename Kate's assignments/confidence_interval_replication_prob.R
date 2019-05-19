confidence_interval_replication_prob <- function(n = 25,
                                                 p = 0.5,
                                                 r = 10000,
                                                 gamma = 0.95){
  
  t <- abs(qnorm((1 - gamma)/2))
  
  upper_bound <- c()
  lower_bound <- c()
  
  failed_intervals <- 0
  
  for(counter in seq(r)){
    X <- rbinom(n = n, size = 1, prob = p)
    m <- sum(X == 1)
    w <- m/n
    lower <- n/(t^2 + n) * (w + t^2/(2 * n) - t * sqrt((w * (1 - w))/n) + (t/(2 * n))^2)
    upper <- n/(t^2 + n) * (w + t^2/(2 * n) + t * sqrt((w * (1 - w))/n) + (t/(2 * n))^2)
    
    if(lower > p || upper < p){
      failed_intervals <- failed_intervals + 1
    }
    
    upper_bound <- c(upper_bound, upper)
    lower_bound <- c(lower_bound, lower)
  }
  cat(failed_intervals, "\n")
}