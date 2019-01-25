data {
  int<lower=1> numObs;     // number of observations
  int<lower=1> numReg;     // number of regions
  int<lower=1> numCav;   // number of caves
  real y[numObs];               // observed mercury
  matrix[numObs,numReg] Xmu;	   // the model matrix for regions
  matrix[numObs,numReg] Xbeta;  // the model matrix for guano effect by region
  matrix[numObs, numCav] Xtheta; // the model matrix for cave effects
}

parameters {
  vector[numReg] mu;	// region effect
  vector[numReg] beta;	// guano effect for each region
  vector[numCav] theta;	// cave effect
  real<lower=0> sigma;	// standard deviation in likelihood
  real<lower=0> tauTheta;    // standard deviation of cave effects
}

transformed parameters {
  vector[numObs] yhat = Xmu*mu + Xbeta*beta;    // fitted values
}

model {
  mu ~ normal(0.5, 100);
  beta ~ normal(0.25, 100);
  sigma ~ exponential(1.0);
  tauTheta ~ exponential(10);
  y ~ normal(yhat, sigma);
}
