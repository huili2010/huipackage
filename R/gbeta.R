g_beta = function(x, beta){
  n = nrow(data.mela)
  gbeta = sum(x^beta*log(x))/sum(x^beta)- 1/beta- sum(log(x))/n
  return(gbeta)
}
