NRiteration = function(x=data.mela, beta0=1, stopval=1e-7, i_max=90){

  for(i in 1:i_max){

    gbeta = sum(x^beta0*log(x))/sum(x^beta0)- 1/beta0- sum(log(x))/n
    gbeta_d1 = 1/(beta0^2)
    beta1 = beta0-gbeta/gbeta_d1

    if(abs(beta1-beta0) < stopval){

      gtheta = (sum(x^beta1/n))^(1/beta1)
      gbeta_d2 = -n/(beta1^2)-(sum((x/gtheta)^beta1*log(x/gtheta)^2))
      gtheta_d2 = (n*beta1)/gtheta^2-beta1*(sum((x)^beta1))*(beta1+1)/(gtheta^(beta1+1)*gtheta)
      gbeta.theta = -n/gtheta+sum(x^beta1)/(gtheta^(beta1+1))-beta1*(sum((x)^beta1))*log(gtheta)/(gtheta^(beta1+1))+beta1*(sum((x^beta1)*log(x)))/gtheta^(beta1+1)
      Obs_InfMatrix <- matrix(c(-gbeta_d2, -gbeta.theta, -gbeta.theta, -gtheta_d2), nrow=2, ncol=2)
      Exp_InfMatrix <- solve(Obs_InfMatrix)
      varbeta <- Exp_InfMatrix[1]
      vartheta <- Exp_InfMatrix[4]
      sdbeta<- sqrt(varbeta)
      sdtheta <- sqrt(vartheta)
      beta_upperCI <- beta1 + 1.96*sdbeta
      beta_lowerCI <- beta1 - 1.96*sdbeta
      theta_upperCI <- gtheta + 1.96*sdtheta
      theta_lowerCI <- gtheta - 1.96*sdtheta
    } else {
      beta0 <- beta1
    }
  }
