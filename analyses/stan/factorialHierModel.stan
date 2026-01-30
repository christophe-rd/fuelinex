// Two-level (1 hierarchical grouping) `random' intercept model
// Partial pooling on species and 

// Updated for new version of Stan (2025!)

//do this: change everything! a lot of work before I should play here

data{
int<lower=0> N; 	// number of total observations
// int<lower=0> Nspp; 	// number of species (grouping factor)
// array[N] int species; 	// species identity, coded as int
// int<lower=0> Nid;  // number of tree ids (grouping factor)
// array[N] int id;   // tree id identity, coded as int
vector[N] s; 	// dummy variable for spring temperature
vector[N] f; 	// dummy variable for fall temperature
vector[N] sf; 	// dummy variable for interaction between spring and fall temperature
array[N] real y; 		// biomass (response)
}

parameters{
real b;        
real bs; // temperature spring
real bf; // temperature fall
real bsf; // interaction parameter between the temperature of fall and spring
// vector[Nspp] bs_spp ;
real<lower=0> sigma_y;
}

transformed parameters{
array[N] real ypred;
for (i in 1:N){ 
    ypred[i]=
        b + 
        bs*s[i] +
        bf*f[i] + 
        bsf*sf[i];
    }
}

model{	
  b ~ normal(30, 5);
  sigma_y ~ normal(0, 5);
  
  y ~ normal(ypred, sigma_y); // this creates an error model where error is normally distributed
}	
