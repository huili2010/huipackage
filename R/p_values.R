PValue <- function(samples){
  nsamples=1000
  Ps = rep(NA, nsamples)
  for (i in 1:nsamples){
    x <- rnorm(10000)
    TT <- t.test(x)
    Ps[i] = TT$p.value
    TT$p.value
  }
  return(Ps)
}
