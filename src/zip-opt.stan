// define a function to calculate the number of zeros
//   in the response vector
functions {
  int num_zeros(int[] y) {
    int sum = 0;
    for (i in 1:size(y))
      sum += (y[i] == 0);
    return sum;
  }
}

// specify data inputs
data {
  int<lower=0> n;
  int<lower=0> y[n];
  vector[n] x;
}

// make a new vector of non-zero values
transformed data {
  int<lower = 0> n_zero = num_zeros(y);
  int<lower = 0> n_nonzero = n - n_zero;
  int<lower = 1> y_nonzero[n_nonzero];
  int<lower = 0> ones_zero[n_zero];
  int<lower = 0> zeros_zero[n_zero];
  vector[n_zero] x_zero;
  vector[n_nonzero] x_nonzero;
  int i_nonzero = 0;
  int i_zero = 0;
  for (i in 1:n) {
    if (y[i] == 0) {
      i_zero += 1;
      x_zero[i_zero] = x[i];   
      ones_zero[i_zero] = 1;
      zeros_zero[i_zero] = 0;
    } else {
      i_nonzero += 1;
      y_nonzero[i_nonzero] = y[i];
      x_nonzero[i_nonzero] = x[i];
    }
  }
}

// and model params
parameters {
  real<lower=0, upper=1> theta;
  real alpha;
  real beta;
}

// now add some transformed model parameters
transformed parameters {
  vector[n_zero] lambda_zero;
  vector[n_nonzero] lambda_nonzero;
  
  // set up a linear predictor separately for zero and
  //   non-zero values
  lambda_zero = alpha + beta * x_zero;
  lambda_nonzero = alpha + beta * x_nonzero;
  
}

// set model likelihood
model {
    
  // set priors
  alpha ~ std_normal();
  beta ~ std_normal();
    
  // calculate a summed probability of zero and non-zero values together
  target += log_sum_exp(
     bernoulli_lpmf(ones_zero | theta),
     bernoulli_lpmf(zeros_zero | theta) + 
       poisson_log_lpmf(zeros_zero | lambda_zero)
   );
   target += n_nonzero * bernoulli_lpmf(0 | theta);
   target += poisson_log_lpmf(y_nonzero | lambda_nonzero);
   
}

// add a transformed theta (which is probabliity of zero) to
//   give a probability of non-zero value
generated quantities {
  real<lower=0, upper=1> p = 1. - theta;
}
