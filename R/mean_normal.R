sampling <- function(samples=50, sample_size=100){
  samplemeans <- rep(NA, samples)
  for (i in 1:samples){
    x <- rnorm(sample_size, mean = 0, sd = 1)
    samplemeans[i] = mean (x)
  }
  return(samplemeans)
}
