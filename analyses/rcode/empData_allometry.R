# 6 February 2026
# CRD and Ken
# Goal is to estimate allometric coefficients for fuelinex trees

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
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

# flags
fitmodel <- FALSE
plotpriors <- FALSE
retro <- FALSE

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Loop up empirical data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
mea <- read.csv2("output/cleanedMeasurements.csv", sep = ",", header = TRUE)

mea$spp_num <- match(mea$genus, unique(mea$genus))
mea$treeid_num <- match(mea$tree_ID, unique(mea$tree_ID))

# remove nas
mea <- mea[which(!is.na(mea$height) & !is.na(mea$diameter)),]
mea$height   <- as.numeric(mea$height)
mea$diameter <- as.numeric(mea$diameter)

biom <- read.csv("input/biomass.csv")
# biomass now
biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)

f25 <- subset(mea, year == "2025")

df25 <- merge(f25, biom[, c("tree_ID","aboveGroundWeight")], by = "tree_ID")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit Empirical Data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
df25 <- df25[which(!is.na(df25$aboveGroundWeight)),]
y <- df25$aboveGroundWeight
N <- nrow(df25)
Nids <- length(unique(df25$tree_ID))
Nspp <- length(unique(df25$spp_num))
spp <- df25$spp_num
height <- df25$height
dia <- df25$dia

# assign bounds to tell the initial values to the sampler
if (fitmodel) {
inits <- function(chain_id){
  params <- list("b1" = as.array(rlnorm(Nspp, log(0.5), 1)),
                 "b2" = as.array(rnorm(Nspp, 0.5, 1)),
                 "sigma_y" = abs(rnorm(1,0,1)))
  return(params)
}
inits(1)

fit <- stan("stan/allometryModel.stan", 
            data=c("N","y",
                   "spp","Nspp",
                   "height", "dia"),
            init = inits,
            iter = 4000, chains = 4, cores = 4,
            warmup = 2000)

saveRDS(fit, "output/stanOutput/allometryModel")
}
fit <- readRDS("output/stanOutput/allometryModel")
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (retro){
  
# util$plot_inv_metric(fit, 75)
samples <- util$extract_expectand_vals(fit)
jpeg(
  filename = "figures/empiricalData_allometry/retrodictiveCheckHist.jpeg",
  width = 2400,      
  height = 2400,
  res = 300          
)
util$plot_hist_quantiles(samples, "y_rep", 
                         -20, # lower x axis limit
                         100, # upper x axis limit
                         5, # binning
                         baseline_values = y,
                         xlab = "Biomass")
dev.off()
}
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Recover parameters ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
df_fit <- as.data.frame(fit)

# grab parameter estimates
cols <- colnames(df_fit)[!grepl("ypred", colnames(df_fit))]
cols <- cols[!grepl("y_rep", cols)]
cols <- cols[!grepl("lp__", cols)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For sigma_y
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# grab parameter estimates
sigma_cols <- cols[grepl("sigma", cols)]

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
b1_cols <- cols[grepl("b1", cols)]

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
b2_cols <- cols[grepl("b2", cols)]

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
# Plot posterior vs prior #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
###### Plot sigma_y prior vs posterior ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
sigma_priorvpost <- data.frame(
  prior = rnorm(length(sigmavec), 0, 2),
  posterior = sigmavec
)

sigmapriorvspostplot <- ggplot(sigma_priorvpost) +
  geom_density(aes(x = prior, colour = "Prior at N(0, 2)"),
               linewidth = 0.8) +
  geom_density(aes(x = posterior, colour = "Posterior"),
               linewidth = 0.8) +
  labs(title = "priorVSposterior_sigmas",
       x = "", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
sigmapriorvspostplot

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
###### Plot b1 prior vs posterior ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# convert posterior distribution to long format
if (plotpriors) {

b1_long <- reshape(
  b1_df,
  direction = "long",
  varying = list(names(b1_df)),
  v.names = "value",
  timevar = "spp",
  times = names(b1_df),
  idvar = "draw"
)
b1_long

# aspp prior
b1_prior <- rlnorm(nrow(b1_df), log(0.5), 0.3)

b1priorvspostplot <- ggplot() +
  geom_density(data = data.frame(b1_prior = b1_prior),
               aes(x = b1_prior, colour = "Prior at logN(log(0.1), 0.5)"),
               linewidth = 0.8) +
  geom_density(data = b1_long,
               aes(x = value, colour = "Posterior", group = spp),
               linewidth = 0.5) +
  # facet_wrap(~spp) + 
  labs(title = "priorVSposterior_b1",
       x = "b1", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  xlim(c(0, 1)) +
  theme_minimal()
b1priorvspostplot

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
###### Plot b2 prior vs posterior ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# convert posterior distribution to long format
b2_long <- reshape(
  b2_df,
  direction = "long",
  varying = list(names(b2_df)),
  v.names = "value",
  timevar = "spp",
  times = names(b2_df),
  idvar = "draw"
)
b2_long

# aspp prior
b2_prior <- rnorm(nrow(b2_df), 0.7, 0.2)

b2priorvspostplot <- ggplot() +
  geom_density(data = data.frame(b2_prior = b2_prior),
               aes(x = b2_prior, colour = "Prior at logN(log(0.1), 0.5)"),
               linewidth = 0.8) +
  geom_density(data = b2_long,
               aes(x = value, colour = "Posterior", group = spp),
               linewidth = 0.5) +
  # facet_wrap(~spp) + 
  labs(title = "priorVSposterior_b2",
       x = "b2", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  # xlim(c(-20, 20)) +
  theme_minimal()
b2priorvspostplot

# make some checks

#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
combined <- (sigmapriorvspostplot + b1priorvspostplot + b2priorvspostplot)
combined
ggsave("figures/empiricalData_allometry/priorVSposteriorCombined.jpeg", combined, width = 12, height = 8, units = "in", dpi = 300)
}
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Estimate biomass for year 2023 and 2024 ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### 2023 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
df23 <- subset(mea, year == "2023")

sub <- df23[, c("treeid_num", "spp_num", "height", "diameter")]

X <- df23$diameter^2 * df23$height

n_draws <- nrow(b1_df)
n_trees <- nrow(df23)

biomass_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_trees)

for(i in seq_len(n_trees)) {
  spp <- as.character(df23$spp_num[i])
  Xi  <- X[i]
  
  mu <- b1_df[[spp]] * (Xi ^ b2_df[[spp]])
  
  biomass_mat[, i] <- rnorm(n_draws, mu, sigma_df[,1])
}

# reintegrate in mesurement df23
# empty treat dataframe
b23 <- data.frame(
  treeid_num = df23$treeid_num,
  mean =  colMeans(biomass_mat),  
  per5  = apply(biomass_mat, 2, quantile, probs = 0.05), 
  per25 = apply(biomass_mat, 2, quantile, probs = 0.25),
  per75 = apply(biomass_mat, 2, quantile, probs = 0.75),
  per95 = apply(biomass_mat, 2, quantile, probs = 0.95)
)
b23

df23$agb <- b23$mean[match(df23$treeid_num, b23$treeid_num)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### 2024 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
df24 <- subset(mea, year == "2024")

sub24 <- df24[, c("treeid_num", "spp_num", "height", "diameter")]

X <- df24$diameter^2 * df24$height

n_draws <- nrow(b1_df)
n_trees <- nrow(df24)

biomass_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_trees)

for(i in seq_len(n_trees)) {
  spp <- as.character(df24$spp_num[i])
  Xi  <- X[i]
  
  mu <- b1_df[[spp]] * (Xi ^ b2_df[[spp]])
  
  biomass_mat[, i] <- rnorm(n_draws, mu, sigma_df[,1])
}

# reintegrate in mesurement df24
# empty treat dataframe
b24 <- data.frame(
  treeid_num = df24$treeid_num,
  mean =  colMeans(biomass_mat),  
  per5  = apply(biomass_mat, 2, quantile, probs = 0.05), 
  per25 = apply(biomass_mat, 2, quantile, probs = 0.25),
  per75 = apply(biomass_mat, 2, quantile, probs = 0.75),
  per95 = apply(biomass_mat, 2, quantile, probs = 0.95)
)
b24

df24$agb <- b24$mean[match(df24$treeid_num, b24$treeid_num)]
df24$mul <- df24$diameter * df24$diameter * df24$height
# ggplot(df24, aes(x = mul, y = agb, color = genus, fill = genus)) +
#   geom_point(aes(y = agb), alpha = 0.7) +
#   facet_wrap(~ genus, scales = "free") +
#   theme_minimal()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# ##### 2025 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
df25$vol <- df25$diameter^2 * df25$height

dmax  <- aggregate(vol ~ spp_num, df25, FUN = max)
dmax$vol <- dmax$vol/1000
dmax$vol <- ceiling(dmax$vol)
dmax$vol <- dmax$vol*1000
dmax$spp_num <- as.character(dmax$spp_num)

n_draws <- nrow(b1_df)
n_trees <- nrow(df25)
nsp <- nrow(dmax)

biomass_simu <- list()
for(i in seq_len(nsp)) {
  Xi  <- seq(from = 0, to = dmax$vol[i], by = 100)
  biomass_simu[[i]] <- data.frame(spp = rep(i, length(Xi)), vol = Xi)
}
biomass_simu <- do.call(rbind, biomass_simu)
calc <- matrix(NA_real_, nrow = nrow(biomass_simu), ncol = n_draws)

for(i in seq_len(nrow(biomass_simu))) { # i = 1 
  spp <- as.character(biomass_simu$spp[i])
  
  mu <- b1_df[[spp]] * (biomass_simu$vol[i] ^ b2_df[[spp]])
  calc[i, ] <- rnorm(n_draws, mu, sigma_df[,1])
}

biomass_simu <- cbind(biomass_simu, calc)

biomass_simu2 <- biomass_simu[, 1:2]

biomass_simu2$mean <- rowMeans(biomass_simu[3:ncol(biomass_simu)])
biomass_simu2$per5  = apply(biomass_simu, 1, quantile, probs = 0.05)
biomass_simu2$per25  = apply(biomass_simu, 1, quantile, probs = 0.25)
biomass_simu2$per75  = apply(biomass_simu, 1, quantile, probs = 0.75)
biomass_simu2$per95  = apply(biomass_simu, 1, quantile, probs = 0.95)

# === === === === === === === === === === === === === === === === === === === 
# Plotting Posterior Predictive Checks ####
# === === === === === === === === === === === === === === === === === === === 
cols <- c("#88a0dc", "#381a61", "#7c4b73", "#ed968c", "#ab3329","#e78429", "#f9d14a")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Retrodictive no histogram #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf(file = "figures/empiricalData_allometry/slopesRetrodictiveCheck.pdf", 
    width = 10, height = 8)

biomass_simu2$sppname <- df25$species[match(biomass_simu2$spp, df25$spp_num)]
spp_levels <- unique(biomass_simu2$spp)

# Panel layout similar to facet_wrap
n <- length(spp_levels)
ncol <- 3
nrow <- 3

par(mfrow = c(nrow, ncol), mar = c(4,4,3,1))

for(sp in spp_levels){
  
  df <- biomass_simu2[biomass_simu2$spp == sp, ]
  df <- df[order(df$vol), ]   
  
  plot(df$vol, df$mean, type = "n",
       ylim = range(c(df$per25, df$per75), na.rm = TRUE),     
       xlab = "Diameter(mm)^2*Height(cm3)",
       ylab = "Above Ground Biomass (gr)",
       main = df$sppname[sp])

  #   ribbons
  polygon(
    c(df$vol, rev(df$vol)),
    c(df$per25, rev(df$per75)),
    col = adjustcolor(cols[sp], alpha.f = 0.3),
    border = NA
  )
  # lines
  lines(df$vol, df$mean,
        col = cols[sp],
        lwd = 2)
  
  # points for empirical data
  pts <- df25[df25$spp_num == sp, ]
  
  points(
    pts$vol,
    pts$aboveGroundWeight,
    col = adjustcolor(cols[sp], alpha.f = 0.7),
    pch = 16
  )
}
dev.off()


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Retrodictive with histogram #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
if (retro) {
  pdf(file = "figures/empiricalData_allometry/slopesRetrodictiveCheck_hist.pdf", 
      width = 10, height = 8)
  
  biomass_simu2$sppname <- df25$species[match(biomass_simu2$spp, df25$spp_num)]
  spp_levels <- unique(biomass_simu2$spp)
  
  # Panel layout similar to facet_wrap
  n <- length(spp_levels)
  ncol <- 3
  nrow <- 3
  
  par(mfrow = c(nrow, ncol), mar = c(4,4,3,1))
  
  for(sp in spp_levels){
    
    df <- biomass_simu2[biomass_simu2$spp == sp, ]
    df <- df[order(df$vol), ]   
    
    plot(df$vol, df$mean, type = "n",
         ylim = range(c(df$per25, df$per75), na.rm = TRUE),     
         xlab = "Diameter(mm)^2*Height(cm3)",
         ylab = "Above Ground Biomass (gr)",
         main = df$sppname[sp])
    
    vol <- hist(d$vol.2023[which(d$spp_num == sp)], breaks = seq(0, dmax$vol[sp], by = 100), plot = FALSE)
    vol_counts <- vol$counts / max(vol$counts) * (range(c(df$per25, df$per75), na.rm = TRUE)[2] - range(c(df$per25, df$per75), na.rm = TRUE)[1]) / 4
    
    rect(xleft = vol$breaks[1:(length(vol$breaks) - 1)],
         ybottom = rep(range(c(df$per25, df$per75), na.rm = TRUE)[1], length(vol_counts)),
         xright = vol$breaks[2:length(vol$breaks)],
         ytop = vol_counts + range(c(df$per25, df$per75), na.rm = TRUE)[1],
         col = 'green')
    
    v23 <- vol_counts
    
    vol <- hist(d$vol.2024[which(d$spp_num == sp)], breaks = seq(0, dmax$vol[sp], by = 100), plot = FALSE)
    vol_counts <- vol$counts / max(vol$counts) * (range(c(df$per25, df$per75), na.rm = TRUE)[2] - range(c(df$per25, df$per75), na.rm = TRUE)[1]) / 4
    
    rect(xleft = vol$breaks[1:(length(vol$breaks) - 1)],
         ybottom = rep(range(c(df$per25, df$per75), na.rm = TRUE)[1], length(vol_counts)),
         xright = vol$breaks[2:length(vol$breaks)],
         ytop = vol_counts + range(c(df$per25, df$per75), na.rm = TRUE)[1],
         col = 'red')
    
    v24 <- vol_counts
    
    vol <- hist(d$vol.2025[which(d$spp_num == sp)], breaks = seq(0, dmax$vol[sp], by = 100), plot = FALSE)
    vol_counts <- vol$counts / max(vol$counts) * (range(c(df$per25, df$per75), na.rm = TRUE)[2] - range(c(df$per25, df$per75), na.rm = TRUE)[1]) / 4
    
    rect(xleft = vol$breaks[1:(length(vol$breaks) - 1)],
         ybottom = rep(range(c(df$per25, df$per75), na.rm = TRUE)[1], length(vol_counts)),
         xright = vol$breaks[2:length(vol$breaks)],
         ytop = vol_counts + range(c(df$per25, df$per75), na.rm = TRUE)[1],
         col = 'grey')
    
    lines(rep(vol$breaks, each = 2),
          c(0, rep(v23, each = 2), 0) + range(c(df$per25, df$per75), na.rm = TRUE)[1],
          col = 'green')
    
    lines(rep(vol$breaks, each = 2),
          c(0, rep(v24, each = 2), 0) + range(c(df$per25, df$per75), na.rm = TRUE)[1],
          col = 'red')
    
    #   ribbons
    polygon(
      c(df$vol, rev(df$vol)),
      c(df$per25, rev(df$per75)),
      col = adjustcolor(cols[sp], alpha.f = 0.3),
      border = NA
    )
    
    
    # lines
    lines(df$vol, df$mean,
          col = cols[sp],
          lwd = 2)
    
    # points for empirical data
    pts <- df25[df25$spp_num == sp, ]
    
    points(
      pts$vol,
      pts$aboveGroundWeight,
      col = adjustcolor(cols[sp], alpha.f = 0.7),
      pch = 16
    )
  }
  dev.off()
  
}