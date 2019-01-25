data {
  int<lower=1> numObs;     // number of observations
  int<lower=1> numReg;     // number of regions
  real y[numObs];               // observed mercury
  matrix[numObs,numReg] Xmu;	   // the model matrix for regions
  matrix[numObs,numReg] Xbeta;  // the model matrix for guano effect by region
  int<lower=1> nu;   // d.f. for t distribution likelihood
}

parameters {
  vector[numReg] mu;	// region effect
  vector[numReg] beta;	// guano effect for each region
  real<lower=0> sigma;	// standard deviation
}

transformed parameters{
  vector[numObs] yhat;
  yhat = (Xmu*mu) + (Xbeta*beta);    // fitted values
}

model {

  mu ~ normal(0.5, 100);
  beta ~ normal(0.25, 100);
  sigma ~ exponential(1.0);

  y ~ student_t(nu, yhat, sigma);   	 // likelihood
}

generated quantities {
  vector[numObs] resid;
  for (i in 1:numObs){
    resid[i] = y[i] - yhat[i];   // residuals
  }
}
