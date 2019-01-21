data {
  int<lower=0> n;          // number of observations
  real y[n];               // observed mercury
  matrix[n,numReg] Xmu;	   // the model matrix for regions
  matrix[n,numReg] Xbeta;  // the model matrix for guano effect by region
}

parameters {
  vector[numReg] mu;	// region effect
  vector[numReg] beta;	// guano effect for each region
  real<lower=0> sigma;	// standard deviation
}

transformed parameters {
  vector[N] yhat = Xmu*mu + Xbeta*beta;    // school treatment effects
}

model {
  for (i in 1:numReg)
    mu[i] ~ normal(0, 100)
  for (i in 1:numReg)
    beta[i] ~ normal(0, 100)

  
    
  target += normal_lpdf(eta | 0, 1);       // prior log-density
  target += normal_lpdf(y | theta, sigma); // log-likelihood
}
