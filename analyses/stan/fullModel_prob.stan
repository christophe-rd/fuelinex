// Started 16 February 2026
// By Ken

// Combined allometry and carryover model

data{
  int <lower = 0> N_allo;
  vector[N_allo] d_allo;
  vector[N_allo] h_allo;
  int <lower = 0> N_spp;
  array[N_allo] int spp_allo;
  array[N_allo] real agb_allo;
  
  int <lower = 0> N;
  vector[N] d0; // initial diameter measurements
  vector[N] h0; // initial height measurements
  vector[N] d1;
  vector[N] h1;
  vector[N] d2;
  vector[N] h2;
  array[N] int trt;
  array[N] int spp;
  
}

parameters{
  // allometry model
  vector<lower = 0> [N_spp] b1;
  vector<lower = 0> [N_spp] b2;
  real<lower   = 0> sigma_allo;

  // treatment effects on year 1 growth
  vector<lower = 0>[N_spp] acc1;
  vector<lower = 0>[N_spp] awc1;
  vector<lower = 0>[N_spp] acw1;
  vector<lower = 0>[N_spp] aww1;

  // treatment effects on year 2 growth
  vector<lower = 0>[N_spp] acc2;
  vector<lower = 0>[N_spp] awc2;
  vector<lower = 0>[N_spp] acw2;
  vector<lower = 0>[N_spp] aww2;

  array[N] real <lower = 0> agb0;
  array[N] real <lower = 0> agb1;
  array[N] real <lower = 0> agb2;

  real <lower = 0> sigma_y;
}

transformed parameters{
  array[N_allo] real agb_allo_pred;
  
  for(i in 1:N_allo){
    agb_allo_pred[i] = b1[spp_allo[i]] * (d_allo[i]^2 * h_allo[i]) ^ b2[spp_allo[i]];
  }
  
  array[N] real delta1;
  array[N] real delta2;
  array[N] real delta1_pred;
  array[N] real delta2_pred;
  
  for(i in 1:N){
    
    
    delta1[i] = agb1[i] - agb0[i];
    delta2[i] = agb2[i] - agb1[i];

    if(trt[i] == 1){
      delta1_pred[i] = acc1[spp[i]];
      delta2_pred[i] = acc2[spp[i]];
    } else if(trt[i] == 2) {
      delta1_pred[i] = awc1[spp[i]];
      delta2_pred[i] = awc2[spp[i]];
    } else if(trt[i] == 3) {
      delta1_pred[i] = acw1[spp[i]];
      delta2_pred[i] = acw2[spp[i]];
    } else {
      delta1_pred[i] = aww1[spp[i]];
      delta2_pred[i] = aww2[spp[i]];
    }
  }

}

model{	
  // allometry model
  for(i in 1:N) {
  agb0[i] ~ normal(b1[spp[i]] * (d0[i]^2*h0[i])^b2[spp[i]], sigma_allo);
  agb1[i] ~ normal(b1[spp[i]] * (d1[i]^2*h1[i])^b2[spp[i]], sigma_allo);
  agb2[i] ~ normal(b1[spp[i]] * (d2[i]^2*h2[i])^b2[spp[i]], sigma_allo);
  }
  
  b1 ~ lognormal(log(0.5), 0.3);
  b2 ~ normal(0.7, 0.2);
  sigma_allo ~ normal(0, 2);
  
  for(i in 1:N_allo){
    target += lognormal_lpdf(agb_allo[i] | log(agb_allo_pred[i]), sigma_allo);
  }

  acc1 ~ lognormal(1, 1);
  awc1 ~ lognormal(1, 1);
  acw1 ~ lognormal(1, 1);
  aww1 ~ lognormal(1, 1);

  acc2 ~ lognormal(1, 1);
  awc2 ~ lognormal(1, 1);
  acw2 ~ lognormal(1, 1);
  aww2 ~ lognormal(1, 1);

  agb0 ~ normal(10, 5);
  agb1 ~ normal(10, 5);
  agb2 ~ normal(10, 5);
  
  sigma_y ~ lognormal(0, 0.5);

for(i in 1:N){
  target += normal_lpdf(delta1[i] | delta1_pred[i], sigma_y);
  target += normal_lpdf(delta2[i] | delta2_pred[i], sigma_y);
  }
}

generated quantities{

  array[N] real delta1_rep;
  array[N] real delta2_rep;

  // Prior draws
  vector[N_spp] b1_prior;
  vector[N_spp] b2_prior;

  vector[N_spp] acc1_prior;
  vector[N_spp] awc1_prior;
  vector[N_spp] acw1_prior;
  vector[N_spp] aww1_prior;

  vector[N_spp] acc2_prior;
  vector[N_spp] awc2_prior;
  vector[N_spp] acw2_prior;
  vector[N_spp] aww2_prior;

  real sigma_allo_prior;
  real sigma_y_prior;

  for(i in 1:N){

    delta1_rep[i] = normal_rng(delta1_pred[i], sigma_y);
    delta2_rep[i] = normal_rng(delta2_pred[i], sigma_y);

  }

  for(s in 1:N_spp){

    b1_prior[s] = lognormal_rng(log(0.5), 0.3);
    b2_prior[s] = normal_rng(0.7, 0.2);

    acc1_prior[s] = lognormal_rng(1,1);
    awc1_prior[s] = lognormal_rng(1,1);
    acw1_prior[s] = lognormal_rng(1,1);
    aww1_prior[s] = lognormal_rng(1,1);

    acc2_prior[s] = lognormal_rng(1,1);
    awc2_prior[s] = lognormal_rng(1,1);
    acw2_prior[s] = lognormal_rng(1,1);
    aww2_prior[s] = lognormal_rng(1,1);

  }

  sigma_allo_prior = fabs(normal_rng(0,2));
  sigma_y_prior = lognormal_rng(0,0.5);

}
