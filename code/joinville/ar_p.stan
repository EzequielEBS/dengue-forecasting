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

model {

  // priors
  alpha ~ normal(0, 10);
  phi ~ normal(0, 1);
  sigma ~ inv_gamma(2, 1);

  // likelihood
  y ~ normal(alpha + X * phi, sigma);
}

generated quantities {

  vector[N] y_rep;      // in-sample posterior predictive
  vector[H] y_forecast; // out-of-sample forecasts
  vector[p] lags;

  // in-sample predictions
  for (n in 1:N)
    y_rep[n] = normal_rng(alpha + X[n] * phi, sigma);

  // initialize last lags
  for (j in 1:p)
    lags[j] = X[N, j];

  // forecasts
  for (h in 1:H) {

    real mu = alpha + dot_product(phi, lags);
    y_forecast[h] = normal_rng(mu, sigma);

    // update lag vector
    if (p > 1){
       for (j in p:2)
         lags[j] = lags[j-1];
    }
    lags[1] = y_forecast[h];
  }
}
