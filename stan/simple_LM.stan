//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'. AKA = data block
data {
  int<lower=0> N; //number of data
  vector[N] x; //covariates 
  vector[N] y; //variate
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'. AKA = parameter block
parameters {
  real alpha; //intercept
  real beta; //slope
  real<lower=0> sigma; //error scale or scatter; it has to be positive
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'. AKA = model block
model { 
  //priors of the parameters
  alpha ~ normal(0, 10); 
  beta ~ normal(0, 10); 
  sigma ~ normal(0, 1);
  
  //Likelihood
  //Y is drawn from a Gaussian distribution of the predicted Y
  // given X and the parameters Alpha, Beta and the standard deviation Sigma
  y ~ normal(alpha + beta * x, sigma);
}

