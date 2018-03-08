sampling <- function(samples=50, sample_size=100){
  samplemeans <- rep(NA, samples)
  for (i in 1:samples){
    x <- rnorm(sample_size, mean = 0, sd = 1)
    samplemeans[i] = mean (x)
  }
  return(samplemeans)
}

df <- data.frame(
  s50 = sampling (200, 50)
  ,s100 = sampling (200, 100)
  ,s200 = sampling (200, 200)
  ,s300 = sampling (200, 500)
) %>%
  gather(sample_size, samplemeans)
 ggplot(df,mapping= aes(x = samplemeans))+
  geom_density (alpha = .5)
