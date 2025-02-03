// specify data inputs
data {
  int<lower=0> n;
  int<lower=0> y[n];
  vector[n] x;
}

// and model params
parameters {
  real<lower=0, upper=1> theta;
  real alpha;
  real beta;
}

// now add some transformed model parameters
transformed parameters {
  vector[n] lambda;
  
  // set up a linear predictor on the rate (non-zero component)
  lambda = alpha + beta * x;
  
}

// set model likelihood
model {

  // set priors
  alpha ~ std_normal();
  beta ~ std_normal();

  // work through each observation
  for (i in 1:n) {
        
      if (y[i] == 0) {
          
        // calculate probability of zero observations
        //   (sum of structural zero versus zero from the Poisson)
        target += log_sum_exp(
          bernoulli_lpmf(1 | theta),
          bernoulli_lpmf(0 | theta) +
            poisson_log_lpmf(y[i] | lambda[i])
        );
                
      } else {
            
        // otherwise calculate the probabilty of a non-zero
        //   observation as a structural non-zero multiplied
        //   by the Poisson value
        target += bernoulli_lpmf(0 | theta) +
          poisson_log_lpmf(y[i] | lambda[i]);
      }
    }
}

// add a transformed theta (which is probabliity of zero) to
//   give a probability of non-zero value
generated quantities {
  real<lower=0, upper=1> p = 1 - theta;
}
