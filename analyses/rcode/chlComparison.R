# CRD
# 27 May 2025
# Goal is to start find a convertion factor from two tools to measure chlorophyll

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 100) 

# --- --# --- --# --- --# --- --# --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(ggplot2)
library(rstanarm)
library(rethinking)
library(ggplot2)
library(wesanderson)
# --- --# --- --# --- --# --- --# --- --- --- --- --- --- --- --- --- --- --- --

# Set the path to your directory folder 
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

runsimdata <- FALSE

# read csvs
chl212 <- read.csv("input/2025-212_Chltoocomparison.csv")
chl235 <- read.csv("input/2025-212_Chltoocomparison.csv")
chl290 <- read.csv("input/2025-290_Chltoocomparison.csv")

# add column with doy
chl212$doy <- 212
chl235$doy <- 235
chl290$doy <- 290

# create unique ids 
chl212$tree_ID_doy <- paste(chl212$tree_ID, chl212$doy, sep = "_")
chl235$tree_ID_doy <- paste(chl235$tree_ID, chl235$doy, sep = "_")
chl290$tree_ID_doy <- paste(chl290$tree_ID, chl290$doy, sep = "_")

# rbind 
chl <- rbind(chl212, chl235,chl290)

# convert minolta and ccm200plus cols to numeric
chl$minolta <- as.numeric(as.character(chl$minolta))
chl$ccm200plus <- as.numeric(as.character(chl$ccm200plus))

# remove NA rows
chl <- chl[!is.na(chl$minolta), ]


if (runsimdata) {
# Simulate data ####
set.seed(124)
a <- 2
b <- 3
sigma_y <- 0.8

n_spp <- 50
replicates <- 30
N <- n_spp*replicates

# error
error <- rnorm(N, 0, 1.5)

spp <- rep(1:n_spp, each = replicates)
ids <- rep(1:replicates, times = n_spp)

# a_spp
a_spp <- rnorm(n_spp, 0, sigma_y)
a_spp_values <- round(a_spp[spp], 3)

# ccm values
ccm <- round(rnorm(N, 8, 3), 3)
dens(ccm)

minol <- ccm * b + a + a_spp_values + error
sim <- data.frame(spp, ids, a, a_spp_values, b, sigma_y, ccm, minol)
sim

sim$log_minol <- log(sim$minol)
sim$log_ccm <- log(sim$ccm)

chl$log_minolta <- log(chl$minolta)
chl$log_ccm <- log(chl$ccm)

jpeg("figures/chlempirical.jpeg", width = 8, height = 6, units = "in", res = 300, pointsize = 12)
plot(chl$minolta ~ chl$ccm200plus)
dev.off()

##### Fit simulated data #####
fit <- stan_lmer(
  minol ~ log_ccm + (1|spp),
  data = sim,
  chains = 4,
  iter = 4000,
  cores = 4
)

##### Recover simulated data parameters #####
fit_fixef <- fixef(fit)
fit_ranef <- ranef(fit)

spp_df <- fit_ranef$spp

# Now extract the tree IDs and intercepts
a_sppwithranef <- data.frame(
  spp = rownames(spp_df),
  fit_a_spp = spp_df[["(Intercept)"]]
)

# recover only conf intervals spp from previously created df
messyinter <- as.data.frame(posterior_interval(fit))
messyinter$messyids <- rownames(messyinter)
a_spp_messyinter <- subset(messyinter, grepl("spp:", messyids))
a_spp_messyinter$spp <- sub(".*spp:([0-9]+)\\].*", "\\1", a_spp_messyinter$messyids)

# remove unecessary columns
a_spp_messyinter <- a_spp_messyinter[, c("spp", "5%", "95%")]

# change 5 and 95% names
colnames(a_spp_messyinter) <- c("spp", "per5", "per95")

# merge!
fit_merged <- merge(a_sppwithranef, a_spp_messyinter, by = "spp")

# Get sim data ready to merge #####
simcoeftoplot <- sim[, c("spp", "a_spp_values")]
colnames(simcoeftoplot) <- c("spp", "sim_a_spp")
simcoeftoplot <- simcoeftoplot[!duplicated(simcoeftoplot),]

# now merge
a_spp_fit_sim <- merge(simcoeftoplot, fit_merged, by = "spp")

# plot!
plot_a_spp_fit_sim <- ggplot(a_spp_fit_sim, aes(x = sim_a_spp, y = fit_a_spp)) +
  geom_point(color = "blue", size = 2) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, color = "darkgray", alpha=0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Simulated a_spp", y = "Model a_spp", title = "") +
  theme_minimal()
plot_a_spp_fit_sim
ggsave("figures/Chl_a_spp_recovery.jpeg", plot_a_spp_fit_sim, width = 8, height = 6)

# === === === === === === === === === === === === === === === === === === === 

# === === === === === === === === === === === === === === === === === === === 

}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit to empirical data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
plot(log10(chl$ccm200plus) ~ chl$minolta)

fitempir <- stan_lmer(
  log10(chl$ccm200plus) ~ minolta + (1|species),
  data = chl,
  chains = 4,
  iter = 4000,
  cores = 4
)

##### Recover empirical model parameters #####
df_fit <- as.data.frame(fitempir)
aspp_cols <- colnames(df_fit)[grepl("species", colnames(df_fit))]
aspp_cols <- aspp_cols[!grepl("Sigma", aspp_cols)]

aspp_df <- df_fit[, colnames(df_fit) %in% aspp_cols]
# change their names
colnames(aspp_df) <- species_names <- sub(".*species:([a-z_]+)\\]", "\\1", colnames(aspp_df))

#empty aspp df
aspp_df2 <- data.frame(
  species = character(ncol(aspp_df)),
  fit_a_spp = numeric(ncol(aspp_df)),  
  fit_a_spp_per5 = NA, 
  fit_a_spp_per25 = NA,
  fit_a_spp_per75 = NA,
  fit_a_spp_per95 = NA
)
for (i in 1:ncol(aspp_df)) { # i = 1
  aspp_df2$species[i] <- colnames(aspp_df)[i]         
  aspp_df2$fit_a_spp[i] <- round(mean(aspp_df[[i]]),3)  
  aspp_df2$fit_a_spp_per5[i] <- round(quantile(aspp_df[[i]], probs = 0.05), 3)
  aspp_df2$fit_a_spp_per25[i] <- round(quantile(aspp_df[[i]], probs = 0.25), 3)
  aspp_df2$fit_a_spp_per75[i] <- round(quantile(aspp_df[[i]], probs = 0.75), 3)
  aspp_df2$fit_a_spp_per95[i] <- round(quantile(aspp_df[[i]], probs = 0.95), 3)
}

aspp_df2$b<- mean(df_fit$minolta)
aspp_df2$a<- mean(df_fit$`(Intercept)`)
aspp_df2$total_a <- aspp_df2$fit_a_spp+aspp_df2$a

mergedempirfit <- merge(chl, aspp_df2[, c("species",
                                "fit_a_spp", 
                                "b",
                                "a",
                                "total_a")], by = "species")

ggplot(mergedempirfit) +
  geom_point(aes(x = minolta, y = log10(ccm200plus), colour = species)) +
  geom_abline(aes(intercept = total_a, slope = b, colour = species), 
              linewidth = 0.5) +
  labs(title = "", x = "minolta", y = "log(ccm200plus)") +
  scale_colour_manual(values = wes_palette("Darjeeling1")) +
  facet_wrap(~ species) + 
  theme_minimal()
ggsave("figures/chlObsFit.jpeg", width = 8, height = 6)


# transform chl2024 measurements of ccm200plus to the scale of minolta

# pull back the parameter values
a <- aspp_df2$a
b <- aspp_df2$b

a_aspp_acne <- aspp_df2$total_a[which(aspp_df2$species == "acer_negundo")]
a_aspp_bepa <- aspp_df2$total_a[which(aspp_df2$species == "betula_papyrifera")]
a_aspp_poba <- aspp_df2$total_a[which(aspp_df2$species == "populus_balsamifera")]
a_aspp_prvi <- aspp_df2$total_a[which(aspp_df2$species == "prunus_virginiana")]
a_aspp_quma <- aspp_df2$total_a[which(aspp_df2$species == "quercus_macrocarpa")]

# minolta = (ccm - a - asp)/b
chl24 <- read.csv("output/chl24.csv")
chl24
# subset for ccm200plus values
ccm <- subset(chl24, meter == "ccm200plus")
minolta <- subset(chl24, meter == "minolta")
ccm$chlValuecorrected <- ccm$chlValue

ccm$chlValuecorrected[which(ccm$meter == "ccm200plus")] 

ccm_acer <- subset(ccm, genus == "acer")
ccm_acer$chlValuecorrected <- (log10(ccm_acer$chlValue) - a_aspp_acne)/b

ccm_bepa <- subset(ccm, genus == "betula")
ccm_bepa$chlValuecorrected <- (log10(ccm_bepa$chlValue) - a_aspp_bepa)/b

hist(ccm_acer$chlValuecorrected)
hist(ccm_acne$chlValue)

# slope = 0.0282
# a_spp = 0.019
# a = 0.137

