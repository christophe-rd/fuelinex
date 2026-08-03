## Started 16 February 2026
## By Ken and CRD

## STAT 547 model

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

#setwd
if(length(grep("christophe", getwd()) > 0)) { 
  setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")
} else if(length(grep("Ken", getwd()) > 0)){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/fuelinex/analyses")
}

library(ggplot2)
library(rstan)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

runfulljointmodel <- F

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)
source('rcode/tools.R', local=util)

mea <- read.csv("output/cleanedMeasurements.csv", sep = ",", header = TRUE)

mea$spp_num <- match(mea$genus, unique(mea$genus))
mea$treeid_num <- match(mea$tree_ID, unique(mea$tree_ID))
mea <- mea[which(!is.na(mea$height) & !is.na(mea$diameter)),]
mea$height   <- as.numeric(mea$height)
mea$diameter <- as.numeric(mea$diameter)
mea <- mea[, -1]

biom <- read.csv("input/biomass.csv")
biom2 <- biom[!is.na(as.numeric(biom$aboveGroundWeight)),]

table(biom2$genus, biom2$bloc)
# biomass now
biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)

d <- reshape(mea, timevar = 'year',
             idvar = c('tree_ID', 'bloc', 'treatment', 'genus', 'species',
                       'spp_num', 'treeid_num'),
             direction = 'wide')

d$vol.2023 <- d$diameter.2023^2 * d$height.2023
d$vol.2024 <- d$diameter.2024^2 * d$height.2024
d$vol.2025 <- d$diameter.2025^2 * d$height.2025
d$volinc1 <- d$vol.2024 - d$vol.2023
d$volinc2 <- d$vol.2025 - d$vol.2024

d$s <- NA
d$f <- NA
trt <- unique(d$treatment)
s <- c(0, 1, 0, 1, 0, 1)
f <- c(0, 0, 1, 1, 0, 1)
for(i in 1:length(trt)){
  idx <- which(d$treatment == trt[i])
  d$s[idx] <- s[i]
  d$f[idx] <- f[i]
}
# warm spring warm fall
d$sf <- d$s * d$f


d <- subset(d, volinc1 > 0 & volinc2 > 0 & 
              treatment %in% trt[1:4] & 
              spp_num %in% 1:7)

d$trt_num <- match(d$treatment, unique(d$treatment))

biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)
d_allo <- subset(mea, year == "2025")
d_allo <- merge(d_allo, biom[, c("tree_ID","aboveGroundWeight")], by = "tree_ID")

d_allo <- subset(d_allo, !is.na(diameter) & !is.na(height) & 
                   treatment %in% trt[1:4] & # excluding nitro boost for now
                   aboveGroundWeight > 0 & spp_num %in% 1:7)

# Fit model
data <- list("N_allo" = nrow(d_allo),
             "d_allo" = d_allo$diameter,
             "h_allo" = d_allo$height,
             "N_spp" = length(unique(d$species)),
             "spp_allo" = d_allo$spp_num,
             "agb_allo" = d_allo$aboveGroundWeight,
             "N" = nrow(d),
             "d0" = d$diameter.2023,
             "h0" = d$height.2023,
             "d1" = d$diameter.2024,
             "h1" = d$height.2024,
             "d2" = d$diameter.2025,
             "h2" = d$height.2025,
             "trt" = d$trt_num,
             "spp" = d$spp_num)

set.seed(1)

inits <- function(chain_id){
  params <- list("b1" = as.array(rlnorm(unique(d$spp_num), log(0.5), 0.3)),
                 "b2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "sigma_allo" = abs(rnorm(1, 0, 1)),
                 "acc1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "awc1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acw1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "aww1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acc2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "awc2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acw2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "aww2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "sigma_y" = abs(rnorm(1, 0, 1))
  )
  return(params)
}
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit Model ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (runfulljointmodel) {
  data$sigma_y <- runif(1)
  fit <- stan("stan/fullModelpos.stan",
              data = data, 
              # init = inits, # fill readd later when I figure out why the bound on b2 messes it up
              seed = 1,
              warmup = 1000, iter = 2000, refresh = 500, chains = 4)
  # saveRDS(fit, "output/stanOutput/fullJoint.rds")
  # saveRDS(fit, "output/stanOutput/fullJoint_justTreat.rds")
  # saveRDS(fit, "output/stanOutput/fullJoint_justAllometry.rds")
  # names(fit)[grepl("sigma_y", name(sigma_y))]
}
fit <- readRDS("output/stanOutput/fullJoint_justAllometry.rds")
fit_full <- readfull_fit_normalLikelihood_bound0B2
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Diagnostics ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
names <- c(grep('^b1', names(samples), value = TRUE),
           grep('^b2', names(samples), value = TRUE),
           grep('sigma_allo', names(samples), value = TRUE),
           grep('acc1', names(samples), value = TRUE),
           grep('awc1', names(samples), value = TRUE),
           grep('acw1', names(samples), value = TRUE),
           grep('aww1', names(samples), value = TRUE),
           grep('acc2', names(samples), value = TRUE),
           grep('awc2', names(samples), value = TRUE),
           grep('acw2', names(samples), value = TRUE),
           grep('aww2', names(samples), value = TRUE),
           grep('sigma_y', names(samples), value = TRUE))

# just delta 1s
idtocheck <- which(d$spp_num == 1)
deltanames <- paste0('delta1[', idtocheck, ']')
deltadata <- sapply(deltanames, function(f_name) c(t(samples[[f_name]]), 
                                                   recursive = TRUE))

base_samples <- util$filter_expectands(samples, names)
print(util$check_all_expectand_diagnostics(base_samples))
summary(fit)
# library(shinystan)
# launch_shinystan(fit)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Marginal posterior #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# 
# pdf("figures/modelDiagnostics/marginalPost.pdf", height = 9, width = 9)
# par(mfrow = c(3, 3))
# for(i in 1:length(names)){
#   a <- min(samples[[names[i]]])
#   b <- max(samples[[names[i]]])
#   util$plot_expectand_pushforward(expectand_vals = samples[[names[i]]],
#                                   B = 100,
#                                   display_name = names[i],
#                                   flim = c(a, b))
# }
# dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Pairs plot #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# pdf('figures/modelDiagnostics/pairs.pdf', height = 9, width = 9)
# util$plot_div_pairs(names, names, samples, diagnostics)
# dev.off()
# 
# # with bound on b1
# namesallo <- c(grep('b1', names(samples), value = TRUE),
#            grep('b2', names(samples), value = TRUE),
#            grep('sigma_allo', names(samples), value = TRUE),
#            grep('sigma_y', names(samples), value = TRUE))
# namesallo <- namesallo[!grepl("agb", namesallo)]
# 
# pdf('figures/modelDiagnostics/pairs_normLikelihood_bound.pdf', height = 9, width = 9)
# util$plot_div_pairs(namesallo, namesallo, samples, diagnostics)
# dev.off()
# 
# # no bound on b2
# fitnobound <- readRDS("output/stanOutput/full_fit_normalLikelihood.rds")
# samplesnobound <- util$extract_expectand_vals(fitnobound)
# 
# pdf('figures/modelDiagnostics/pairs_normLikelihood_NoBound.pdf', height = 9, width = 9)
# util$plot_div_pairs(namesallo, namesallo, samplesnobound, diagnostics)
# dev.off()


df_fit <- as.data.frame(fit)

# grab parameter estimates
cols <- colnames(df_fit)[!grepl("ypred", colnames(df_fit))]
cols <- cols[!grepl("y_rep", cols)]
cols <- cols[!grepl("lp__", cols)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For sigma_y
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# grab parameter estimates
sigma_cols <- cols[grepl("sigma_y", cols) ]

sigmavec <- as.vector(df_fit[, colnames(df_fit) %in% sigma_cols])

sigma_df <- data.frame(
  mean = mean(sigmavec), 
  per5 = quantile(sigmavec, probs = 0.05),
  per25 = quantile(sigmavec, probs = 0.25),
  per75 = quantile(sigmavec, probs = 0.75),
  per95 = quantile(sigmavec, probs = 0.95)
)
sigma_df

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For coefficient B1 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
b1_cols <- cols[grepl("^b1", cols)]

b1_df <- df_fit[, colnames(df_fit) %in% b1_cols]

# change their names
colnames(b1_df) <- as.numeric(sub("b1\\[|\\]", "", sub("\\]", "", colnames(b1_df))))

# empty treat dataframe
b1_df2 <- data.frame(
  spp = character(ncol(b1_df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
b1_df2

for (i in 1:ncol(b1_df)) { # i = 1
  b1_df2$spp[i] <- colnames(b1_df)[i]         
  b1_df2$mean[i] <- round(mean(b1_df[[i]]),3)  
  b1_df2$per5[i] <- round(quantile(b1_df[[i]], probs = 0.05), 3)
  b1_df2$per25[i] <- round(quantile(b1_df[[i]], probs = 0.25), 3)
  b1_df2$per75[i] <- round(quantile(b1_df[[i]], probs = 0.75), 3)
  b1_df2$per95[i] <- round(quantile(b1_df[[i]], probs = 0.95), 3)
}
b1_df2

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For coefficient B2
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
b2_cols <- cols[grepl("^b2", cols)]

b2_df <- df_fit[, colnames(df_fit) %in% b2_cols]

# change their names
colnames(b2_df) <- as.numeric(sub("b2\\[|\\]", "", sub("\\]", "", colnames(b2_df))))

# empty treat dataframe
b2_df2 <- data.frame(
  spp = character(ncol(b2_df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
b2_df2

for (i in 1:ncol(b2_df)) { # i = 1
  b2_df2$spp[i] <- colnames(b2_df)[i]         
  b2_df2$mean[i] <- round(mean(b2_df[[i]]),3)  
  b2_df2$per5[i] <- round(quantile(b2_df[[i]], probs = 0.05), 3)
  b2_df2$per25[i] <- round(quantile(b2_df[[i]], probs = 0.25), 3)
  b2_df2$per75[i] <- round(quantile(b2_df[[i]], probs = 0.75), 3)
  b2_df2$per95[i] <- round(quantile(b2_df[[i]], probs = 0.95), 3)
}
b2_df2

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Compare allometry alone vs with joint model ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
fit <- readRDS("output/stanOutput/fullJoint_justAllometry.rds")
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Recover and plot parameters SOS restricted vs full #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
d_joint <- as.data.frame(fit)
d_joint <- d_joint[,names(d_joint)[grepl("^b2|^b1|sigma", names(d_joint))]]

# summary
d_joint_sum <- data.frame(
  prm = colnames(d_joint),
  mu  = round(sapply(d_joint, mean), 3),
  p05 = round(sapply(d_joint, quantile, probs = 0.05), 3),
  p25 = round(sapply(d_joint, quantile, probs = 0.25), 3),
  p75 = round(sapply(d_joint, quantile, probs = 0.75), 3),
  p95 = round(sapply(d_joint, quantile, probs = 0.95), 3),
  row.names = NULL
)

# just allometry model
d_allo <- as.data.frame(readRDS("output/stanOutput/allometryModel"))

d_allo <- d_allo[,names(d_allo)[grepl("^b2|^b1|sigma", names(d_allo))]]

# summary
d_allo_sum <- data.frame(
  prm = colnames(d_allo),
  mu  = round(sapply(d_allo, mean), 3),
  p05 = round(sapply(d_allo, quantile, probs = 0.05), 3),
  p25 = round(sapply(d_allo, quantile, probs = 0.25), 3),
  p75 = round(sapply(d_allo, quantile, probs = 0.75), 3),
  p95 = round(sapply(d_allo, quantile, probs = 0.95), 3),
  row.names = NULL
)

# subset out the sigmas for now
d_joint_sum <- subset(d_joint_sum, !prm %in% d_joint_sum$prm[grepl("sigma", d_joint_sum$prm)])
d_allo_sum <- subset(d_allo_sum, !prm %in% d_allo_sum$prm[grepl("sigma", d_allo_sum$prm)])

# Open device
jpeg("figures/empiricalData_plots/diagnostics/jointVSallom.jpeg", 
     width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(1,1), oma = c(0, 2, 0, 0))

prm_type <- ifelse(grepl("^b1", d_joint_sum$prm), "b1", "b2")
cols <- ifelse(prm_type == "b1", "#0a6a3c", "#b5651d")

plot(d_joint_sum$mu, d_allo_sum$mu,
     xlab = "joint model", ylab = "allometry model only", 
     main = "", type = "n", frame = FALSE,
     ylim = range(c(d_joint_sum$p25, d_joint_sum$p75)),
     xlim = range(c(d_allo_sum$p25, d_allo_sum$p75)))
arrows(x0 = d_joint_sum$mu, y0 = d_allo_sum$p25,
       x1 = d_joint_sum$mu, y1 = d_allo_sum$p75,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
arrows(x0 = d_joint_sum$p25, y0 = d_allo_sum$mu,
       x1 = d_joint_sum$p75, y1 = d_allo_sum$mu,
       angle = 90, code = 3, length = 0, lwd = 1.5, col = "darkgray")
points(d_joint_sum$mu, d_allo_sum$mu,
       pch = 16, col = cols, cex = 1.5)
abline(0, 1, lty = 2, col = "black", lwd = 2)
legend("topleft", legend = c("b1", "b2"),
       pch = 16, col = c("#0a6a3c", "#b5651d"), bty = "n")

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model with betas from a the separate fit ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
d_joint_sum$spp <- substr(d_joint_sum$prm, 4, 4)
b1 <- subset(d_joint_sum, grepl("b1", d_joint_sum$prm))
b2 <- subset(d_joint_sum, grepl("b2", d_joint_sum$prm))

data$spp_b_idx <- 1:7

data$b1 <- b1$mu[match(data$spp_b_idx, b1$spp)]
data$b2 <- b2$mu[match(data$spp_b_idx, b2$spp)]
# data$sigma_y <- runif(7, min = 0.5, max =1.5)


fit <- stan("stan/fullModel_noBCal.stan", 
            data = data, 
            # init = inits, # fill readd later when I figure out why the bound on b2 messes it up
            seed = 1,
            warmup = 1000, iter = 2000, refresh = 500, chains = 4)
saveRDS(fit, "output/stanOutput/fullJoint_CO_noBcal")
fitnob <- readRDS("output/stanOutput/fullJoint_CO_noBcal")

fit[grepl("sigma", names(fit))]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit model with probabilistic allometry model ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
data <- list("N_allo" = nrow(d_allo),
             "d_allo" = d_allo$diameter,
             "h_allo" = d_allo$height,
             "N_spp" = length(unique(d$species)),
             "spp_allo" = d_allo$spp_num,
             "agb_allo" = d_allo$aboveGroundWeight,
             "N" = nrow(d),
             "d0" = d$diameter.2023,
             "h0" = d$height.2023,
             "d1" = d$diameter.2024,
             "h1" = d$height.2024,
             "d2" = d$diameter.2025,
             "h2" = d$height.2025,
             "trt" = d$trt_num,
             "spp" = d$spp_num)

set.seed(1)

inits <- function(chain_id){
  params <- list("b1" = as.array(rlnorm(unique(d$spp_num), log(0.5), 0.3)),
                 "b2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "sigma_allo" = abs(rnorm(1, 0, 1)),
                 "acc1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "awc1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acw1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "aww1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acc2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "awc2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acw2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "aww2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "sigma_y" = abs(rnorm(1, 0, 1))
  )
  return(params)
}

# run model
fit <- stan("stan/fullModel_prob.stan", 
            data = data, 
            # init = inits, # fill readd later when I figure out why the bound on b2 messes it up
            seed = 1,
            warmup = 1000, iter = 2000, refresh = 500, chains = 4)
fitprobabilistic <- saveRDS(fit, "output/stanOutput/fullJointProbabilistic")
diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diagnostics)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Diagnostics ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
samples <- util$extract_expectand_vals(fit)
nuts_params(fit)

util$plot_div_pairs("zatreeid[1]", "sigma_atreeid", samples_gdd, diagnostics_gdd, transforms = list("sigma_atreeid" = 1))

# check b1
pdf(file = "figures/empiricalData_plots/diagnostics/pairsB1.pdf", 
    width = 8, height = 10)
b1 <- paste0("b1[", 1:7, "]")
util$plot_div_pairs(b1, "sigma_allo", samples, diagnostics)
dev.off()

# check b2
pdf(file = "figures/empiricalData_plots/diagnostics/pairsB2.pdf", 
    width = 8, height = 10)
b2 <- paste0("b2[", 1:7, "]")
util$plot_div_pairs(b2, "sigma_allo", samples, diagnostics)
dev.off()

# check agb0
pdf(file = "figures/empiricalData_plots/diagnostics/pairsAGB0.pdf", 
    width = 8, height = 10)
agb0 <- paste0("agb0[", sample(1:300, 50), "]")
util$plot_div_pairs(agb0, "sigma_y", samples, diagnostics)
dev.off()

# check agb1
pdf(file = "figures/empiricalData_plots/diagnostics/pairsAGB1.pdf", 
    width = 8, height = 10)
agb1 <- paste0("agb1[", sample(1:300, 50), "]")
util$plot_div_pairs(agb1, "sigma_y", samples, diagnostics)
dev.off()

# check agb2
pdf(file = "figures/empiricalData_plots/diagnostics/pairsAGB2.pdf", 
    width = 8, height = 10)
agb2 <- paste0("agb2[", sample(1:300, 50), "]")
util$plot_div_pairs(agb2, "sigma_y", samples, diagnostics)
dev.off()



# check asite vs a
asite <- paste0("asite[", 1:4, "]")
util$plot_div_pairs(asite, "a", samples_gdd, diagnostics_gdd)

# check asite vs aspp
par(mfrow = c(4,4))
pdf("figures/growthModelsMain/diagnostics/pairsSiteVSaspp.pdf", 
    width = 6, height = 9)
util$plot_div_pairs(asite, aspp, samples_gdd, diagnostics_gdd)
dev.off()
util$plot_div_pairs(asite, aspp, samples_gsl, diagnostics_gsl)
util$plot_div_pairs(asite, aspp, samples_sos, diagnostics_sos)
util$plot_div_pairs(asite, aspp, samples_eos, diagnostics_eos)
dev.off()  

# check bspp
bspp <- paste0("bsp[", 1:4, "]")
util$plot_div_pairs(bspp, bspp, samples_gdd, diagnostics_gdd)
util$plot_div_pairs(bspp, bspp, samples_gsl, diagnostics_gsl)
util$plot_div_pairs(bspp, bspp, samples_sos, diagnostics_sos)
util$plot_div_pairs(bspp, bspp, samples_eos, diagnostics_eos)

# check bsppyr
bsppyr <- paste0("bspyr[", 1:4, "]")
util$plot_div_pairs(bsppyr, bsppyr, samples, diagnostics)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Check some priors #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
dfit <- as.data.frame(fit)

# full posterior
columns <- colnames(dfit)[!grepl("prior", colnames(dfit))]
dsigma <- dfit[, columns[grepl("sigma", columns)]]
db <- dfit[, columns[grepl("^b1|^b2", columns)]]
agb <- dfit[, grepl("agb", columns) & !grepl("pred", columns)]

agbsub <- agb[, sample(names(agb), 100)]

# change colnames
colnames(bspp_df_gdd) <- 1:ncol(bspp_df_gdd)
colnames(treeid_df_gdd) <- 1:ncol(treeid_df_gdd)
colnames(aspp_df_gdd) <- 1:ncol(aspp_df_gdd)
colnames(site_df_gdd) <- 1:ncol(site_df_gdd)
colnames(ayear_df_gdd) <- 1:ncol(ayear_df_gdd)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Plot posterior vs priors for gdd fit #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf(file = "figures/empiricalData_plots/diagnostics/priorVSposteriorAGB.pdf", 
    width = 8, height = 10)

pal <- wes_palette("AsteroidCity1")[3:4]

par(mfrow = c(4, 2))

for (i in names(agbsub)) {
  
  x <- dfit[[i]]
  
  plot(
    density(agb[, "agb_prior"]),
    col = pal[1],
    lwd = 2,
    main = paste("Posterior:", i),
    xlab = i
  )
  
}
dev.off()
# a
plot(density(dfit[, names(agbsub)]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_a", 
     # xlab = "a", 
     ylim = c(0, 1))
lines(density(dfit[, "a"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_atreeid
plot(density(dfit[, "sigma_atreeid_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_atreeid", 
     xlab = "sigma_atreeid", ylim = c(0,4))
lines(density(dfit[, "sigma_atreeid"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_asite
plot(density(dfit[, "sigma_asite_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_asite", 
     xlab = "sigma_asite", ylim = c(0,4))
lines(density(dfit[, "sigma_asite"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# sigma_y
plot(density(dfit[, "sigma_y_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_sigma_y", 
     xlab = "sigma_y", ylim = c(0, 4))
lines(density(dfit[, "sigma_y"]), col = pal[2], lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# aspp
plot(density(dfit[, "aspp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_aspp", 
     xlab = "aspp", 
     # xlim = c(-5, 5), 
     ylim = c(0, 0.3))
for (col in colnames(aspp_df_gdd)) {
  lines(density(aspp_df_gdd[, col]), col = pal[2], lwd = 1)
} 
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# asite
plot(density(dfit[, "asite_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_asite", 
     xlab = "asite", xlim = c(-6, 6), ylim = c(0, 1))
for (col in colnames(site_df_gdd)) {
  lines(density(site_df_gdd[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# bsp
plot(density(dfit[, "bsp_prior"]), 
     col = pal[1], lwd = 2, 
     main = "priorVSposterior_bsp", 
     xlab = "bsp", ylim = c(0, 5))
for (col in colnames(bspp_df_gdd)) {
  lines(density(bspp_df_gdd[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

# ayear
plot(density(dfit[, "ayear_prior"]),
     col = pal[1], lwd = 2,
     main = "priorVSposterior_ayear",
     xlab = "ayear", xlim = c(-3, 3), ylim = c(0, 1))
for (col in colnames(ayear_df_gdd)) {
  lines(density(ayear_df_gdd[, col]), col = pal[2], lwd = 1)
}
legend("topright", legend = c("Prior", "Posterior"), col = pal, lwd = 2)

dev.off()
