// Fuelinex biomass model!
// By CRD 
// Started 21 November 2025

data{
int<lower=0> N; 	// number of total observations
vector[N] height; 	// height
vector[N] dia; 	// diameter
int<lower=0> Nspp; 	
array[N] int spp;
array[N] real y; 		// biomass (response)
}

parameters{
vector[Nspp] b1;        
vector[Nspp] b2; // temperature spring
real<lower=0> sigma_y;
}

transformed parameters{
array[N] real ypred;
for (i in 1:N){ 
    ypred[i]=
        b1[spp[i]]*(dia[i]^2*height[i])^b2[spp[i]];
    }
}

model{	
  // b1 ~ normal(30, 5);
  sigma_y ~ normal(0, 1);
  y ~ normal(ypred, sigma_y);
}	
