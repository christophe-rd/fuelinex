# allometry temporary snipet


# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
options(max.print = 200) 
options(digits = 3)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(ggplot2)
library(rstan)
library(wesanderson)
library(patchwork)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
set.seed(123)

if (length(grep("christophe_rouleau-desrochers", getwd())) > 0) {
  setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
} else if (length(grep("lizzie", getwd())) > 0) {
  setwd("/Users/lizzie/Documents/git/projects/others/coringtreespotters/wildchrokie/analyses")
} else  {
  setwd("/home/crouleau/wildchrokie/analyses")
}


util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)


# Simulate data
sigma_y <- 0.1

n_sp <- 10
n_per_spp <- 50

spp <- rep(rep(1:n_sp, each = n_per_spp))

ids <- 1: length(spp)

N <- length(ids)

error <- rnorm(N, 0, sigma_y)

# set coefficients per spp
b1 <- rlnorm(n_sp, log(0.1), 0.2)
b2 <- rnorm(n_sp, 0.05, 0.2)

simdf <- data.frame(
  ids = ids,
  spp = spp,
  error = error
)

simdf$b1 <- b1[simdf$spp]
simdf$b2 <- b2[simdf$spp]

# add height and diameter mean increment for each species
hmean <- abs(rnorm(n_sp, 0.3, 0.1)) # in meters
dmean <- abs(rnorm(n_sp, 0.2, 0.1)) # in cm

# individual level variations
simdf$height <- abs(rnorm(N, hmean[simdf$spp], 0.1))
simdf$dia <- abs(rnorm(N, dmean[simdf$spp], 0.1))

# join everything together
simdf$biom <- simdf$b1*(simdf$dia^2*simdf$height)^b2

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit Sim Data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
y <- simdf$biom
N <- nrow(simdf)
Nids <- length(unique(simdf$ids))
Nspp <- length(unique(simdf$spp))
spp <- simdf$spp
height <- simdf$height
dia <- simdf$dia


table(spp)
# Nspp <- length(unique(emp$spp_num))
# species <- as.numeric(as.character(emp$spp_num))

# assign bounds to tell the initial values to the sampler
inits <- function(chain_id){
  params <- list("b1" = as.array(rlnorm(n_sp, log(0.1), 1)),
                 "b2" = as.array(rnorm(n_sp, 0.05, 1)),
                 "sigma_y" = abs(rnorm(1,0,1)))
  return(params)
}
inits(1)

fit <- stan("stan/allometryModel.stan", 
            data=c("N","y",
                   "spp","Nspp",
                   "height", "dia"),
            init = inits,
            iter = 4000, chains=4, cores=4,
            warmup = 2000)

