// Two-level (1 hierarchical grouping) `random' intercept model
// Partial pooling on species and 

// Updated for new version of Stan (2025!)

//do this: change everything! a lot of work before I should play here

data{
int<lower=0> N; 	// number of total observations
int<lower=0> Nspp; 	// number of species (grouping factor)
array[N] int species; 	// species identity, coded as int
int<lower=0> Nsite;  // number of sites (grouping factor)
array[N] int site;   // site identity, coded as int
int<lower=0> Ntreeid;  // number of tree ids (grouping factor)
array[N] int treeid;   // tree id identity, coded as int
vector[N] gdd; 	// gdd (predictor for slope)
array[N] real y; 		// day of year of phenological event (response)
}

parameters{
real b;        // slope
real a;		// mean intercept across everything
real<lower=0> sigma_bsp;
real<lower=0> sigma_asp;	// variation of intercept across species	
real<lower=0> sigma_atreeid;    // variation of intercept across tree ids
real<lower=0> sigma_y; 	// measurement error, noise etc. 	
vector[Nspp] bsp;
vector[Nspp] asp; 		// defining transformed asp
vector[Ntreeid] atreeid;       //the intercept for each tree id
}

transformed parameters{
array[N] real ypred;
for (i in 1:N){ 
    ypred[i]=
        a + 
        atreat[treat[i]] +
        asp[species[i]] + 
        atreeid[treeid[i]];
    }
}

model{	
  atreat ~ normal(0, sigma_atreat); // this creates the partial pooling on intercepts for sites
  atreeid ~ normal(0, sigma_atreeid); // this creates the partial pooling on intercepts for tree ids
  asp ~ normal(0, 1); 
  a ~ normal(15, 1);
  sigma_asp ~ normal(0, 0.3);
  sigma_atreat ~ normal(0, 0.3);
  sigma_atreeid ~ normal(0, 0.1);
  sigma_y ~ normal(0, 1);
  
  y ~ normal(ypred, sigma_y); // this creates an error model where error is normally distributed
}	
