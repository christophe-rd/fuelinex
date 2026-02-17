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
  vector[N] s; // dummy variable for spring 
  vector[N] f; // dummy variable for fall
  vector[N] sf; // dummy variable for interation between wspring and wfall
  array[N] int spp;
}

parameters{
  // allometry model
  vector<lower = 0>[N_spp] b1;
  vector[N_spp] b2;
  vector[N_spp] s_allo;
  
  // treatment effects on year 1 growth
  vector[N_spp] a1;
  vector[N_spp] as1;
  vector[N_spp] af1;
  vector[N_spp] asf1;
  
  // treatment effects on year 2 growth
  vector[N_spp] a2;
  vector[N_spp] as2;
  vector[N_spp] af2;
  vector[N_spp] asf2;
  
  vector[N_spp] s_y;
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
    
    delta1_pred[i] = a1[spp[i]] + as1[spp[i]]*s[i] + af1[spp[i]]*f[i] + asf1[spp[i]]*sf[i];
    delta2_pred[i] = a2[spp[i]] + as2[spp[i]]*s[i] + af2[spp[i]]*f[i] + asf2[spp[i]]*sf[i];
  }
}

model{	
  // allometry model
  b1 ~ lognormal(log(0.5), 0.3);
  b2 ~ normal(0.7, 0.2);
  s_allo ~ normal(0, 2);
  
  for(i in 1:N_allo){
    target += normal_lpdf(agb_allo[i] | agb_allo_pred[i], s_allo[spp_allo[i]]);
  }
  
  a1 ~ normal(0, 1);
  as1 ~ normal(0, 1);
  af1 ~ normal(0, 1);
  asf1 ~ normal(0, 1);
  
  a2 ~ normal(0, 1);
  as2 ~ normal(0, 1);
  af2 ~ normal(0, 1);
  asf2 ~ normal(0, 1);
  
  s_y ~ normal(0, 1);
  
  for(i in 1:N){
    target += normal_lpdf(delta1[i] | delta1_pred[i], s_y[spp[i]]);
    target += normal_lpdf(delta2[i] | delta2_pred[i], s_y[spp[i]]);
  }
}	

generated quantities{
  array[N] real delta1_trt;
  array[N] real delta2_trt;
  
  for(i in 1:N){
    delta1_trt[i] = normal_rng(a1[spp[i]] + as1[spp[i]]*s[i] + af1[spp[i]]*f[i] + asf1[spp[i]]*sf[i], s_y[spp[i]]);
    delta2_trt[i] = normal_rng(a2[spp[i]] + as2[spp[i]]*s[i] + af2[spp[i]]*f[i] + asf2[spp[i]]*sf[i], s_y[spp[i]]);
  }
}
