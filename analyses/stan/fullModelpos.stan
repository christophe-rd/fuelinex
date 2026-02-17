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
  vector<lower = 0>[N_spp] b1;
  vector[N_spp] b2;
  vector<lower = 0>[N_spp] s_allo;
  
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
  
  vector<lower = 0>[N_spp] s_y;
}

transformed parameters{
  array[N_allo] real agb_allo_pred;
  
  for(i in 1:N_allo){
    agb_allo_pred[i] = b1[spp_allo[i]] * (d_allo[i]^2 * h_allo[i])^b2[spp_allo[i]];
  }
  
  array[N] real agb0;
  array[N] real agb1;
  array[N] real agb2;
  array[N] real delta1;
  array[N] real delta2;
  array[N] real delta1_pred;
  array[N] real delta2_pred;
  
  for(i in 1:N){
    agb0[i] = b1[spp[i]] * (d0[i]^2 * h0[i])^b2[spp[i]];
    agb1[i] = b1[spp[i]] * (d1[i]^2 * h1[i])^b2[spp[i]];
    agb2[i] = b1[spp[i]] * (d2[i]^2 * h2[i])^b2[spp[i]];
    
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
  
  vector[N_spp] a1;
  vector[N_spp] as1;
  vector[N_spp] af1;
  vector[N_spp] asf1;
  
  vector[N_spp] a2;
  vector[N_spp] as2;
  vector[N_spp] af2;
  vector[N_spp] asf2;
  
  for(i in 1:N_spp){
    a1[i] = acc1[i];
    as1[i] = awc1[i] - acc1[i];
    af1[i] = acw1[i] - acc1[i];
    asf1[i] = aww1[i] - a1[i] - as1[i] - af1[i];
    
    a2[i] = acc2[i];
    as2[i] = awc2[i] - acc2[i];
    af2[i] = acw2[i] - acc2[i];
    asf2[i] = aww2[i] - a2[i] - as2[i] - af2[i];
  }
}

model{	
  // allometry model
  b1 ~ lognormal(log(0.5), 0.3);
  b2 ~ normal(0.7, 0.2);
  s_allo ~ normal(0, 2);
  
  for(i in 1:N_allo){
    target += lognormal_lpdf(agb_allo[i] | log(agb_allo_pred[i]), s_allo[spp_allo[i]]);
  }
  
  acc1 ~ lognormal(1, 1);
  awc1 ~ lognormal(1, 1);
  acw1 ~ lognormal(1, 1);
  aww1 ~ lognormal(1, 1);
  
  acc2 ~ lognormal(1, 1);
  awc2 ~ lognormal(1, 1);
  acw2 ~ lognormal(1, 1);
  aww2 ~ lognormal(1, 1);
  
  s_y ~ lognormal(0, 0.5);
  
  for(i in 1:N){
    if(delta1[i] > 0){
      target += lognormal_lpdf(delta1[i] | log(delta1_pred[i]), s_y[spp[i]]);
    } else {
      target += 0.001;
    }
    
    if(delta2[i] > 0){
      target += lognormal_lpdf(delta2[i] | log(delta2_pred[i]), s_y[spp[i]]);
    } else {
      target += 0.001;
    }
    
  }
}	

generated quantities{
  array[N] real delta1_trt;
  array[N] real delta2_trt;
  
  for(i in 1:N){
    delta1_trt[i] = lognormal_rng(log(delta1_pred[i]), s_y[spp[i]]);
    delta2_trt[i] = lognormal_rng(log(delta2_pred[i]), s_y[spp[i]]);
  }
}
