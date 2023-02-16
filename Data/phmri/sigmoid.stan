data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}

parameters {
  real a;
  real<lower=0, upper=60> b;
  real<lower=0> c;
//  real d;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] mu;
  for (k in 1:N)
      mu[k] = a / (1 + exp(- b * (x[k] - c))) + 1;
//    mu[k] = a / (1 + exp(- b * (x[k] - c))) + d;
}

model {
  for(i in 1:N)
    y[i] ~ normal(mu[i], sigma);
}

