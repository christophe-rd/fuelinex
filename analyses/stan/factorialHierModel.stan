// Fuelinex biomass model!
// By CRD 
// Started 21 November 2025

data{
int<lower=0> N; 	// number of total observations
int<lower=0> Nspp; 	// number of species (grouping factor)
array[N] int species; 	// species identity, coded as int
vector[N] s; 	// dummy variable for spring temperature
vector[N] f; 	// dummy variable for fall temperature
vector[N] sf; 	// dummy variable for interaction between spring and fall temperature
array[N] real y; 		// biomass (response)
}

parameters{
real b;        
vector[Nspp] bs; // temperature spring
vector[Nspp] bf; // temperature fall
vector[Nspp] bsf; // interaction parameter between the temperature of fall and spring
real<lower=0> sigma_y;
}

transformed parameters{
array[N] real ypred;
for (i in 1:N){ 
    ypred[i]=
        b + 
        bs[species[i]]*s[i] +
        bf[species[i]]*f[i] + 
        bsf[species[i]]*sf[i];
    }
}

model{	
  b ~ normal(30, 5); // prior on reference value
  sigma_y ~ normal(0, 5);
  y ~ normal(ypred, sigma_y);
}	
