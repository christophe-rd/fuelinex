// Started 16 February 2026
// By Ken

// Combined allometry and carryover model

data{
  int <lower = 0> N;
  int <lower = 0> N_spp;
  array[N] real delta1;
  array[N] real delta2;
  array[N] int trt;
  array[N] int spp;
}

parameters{
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
  array[N] real delta1_pred;
  array[N] real delta2_pred;
  
  for(i in 1:N){
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
  acc1 ~ lognormal(2, 1);
  awc1 ~ lognormal(2, 1);
  acw1 ~ lognormal(2, 1);
  aww1 ~ lognormal(2, 1);
  
  acc2 ~ lognormal(2, 1);
  awc2 ~ lognormal(2, 1);
  acw2 ~ lognormal(2, 1);
  aww2 ~ lognormal(2, 1);
  
  s_y ~ lognormal(0, 0.5);
  
for(i in 1:N){
  target += normal_lpdf(delta1[i] | delta1_pred[i], s_y[spp[i]]);
  target += normal_lpdf(delta2[i] | delta2_pred[i], s_y[spp[i]]);
  }
}
generated quantities{
  array[N] real delta1_trt;
  array[N] real delta2_trt;

  for(i in 1:N){
  delta1_trt[i] = normal_rng(delta1_pred[i], s_y[spp[i]]);
  delta2_trt[i] = normal_rng(delta2_pred[i], s_y[spp[i]]);
  }
}
