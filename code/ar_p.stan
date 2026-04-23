data {
  int<lower=1> N;
  int<lower=1> p;
  int<lower=1> H;
  vector[N] y;
  matrix[N,p] X;          // forecast horizon
}

parameters {
  real alpha;            // intercept
  vector[p] phi;         // AR coefficients
  real<lower=0> sigma;   // noise sd
}

transformed parameters {
  vector[N] mu;
  mu = alpha + X * phi;
}

model {

  // priors
  alpha ~ normal(0, 10);
  phi ~ normal(0, 1);
  sigma ~ exponential(1);

  // likelihood
  y ~ normal(alpha + X * phi, sigma);
}

generated quantities {

  array[N] real y_rep;      // in-sample posterior predictive
  array[H] real y_forecast; // out-of-sample forecasts
  vector[p] lags;
  real mu_f;

  // in-sample predictions
  y_rep = normal_rng(mu, sigma);

  // initialize last lags
  lags = X[N]';

  // forecasts
  for (h in 1:H) {
    mu_f = alpha + dot_product(phi, lags);
    y_forecast[h] = normal_rng(mu_f, sigma);

    // update lag vector
    if (p > 1){
      lags[2:p] = lags[1:(p-1)];
    }
    lags[1] = y_forecast[h];
  }
}
