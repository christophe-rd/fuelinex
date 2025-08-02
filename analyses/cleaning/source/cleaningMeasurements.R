# Started on 1 February 2025
# CRD
# Goal Cleaning tree measurements and assessing the comments and irregularies

# house keeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

### ####
# do this: need to add validity somehow into the script


# Load libraries
library(ggplot2)

# Set Working Directory
# Set Working Directory
if(length(grep("christophe", getwd()) > 0)) {
} else if(length(grep("christophe", getwd())) > 0){
  setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/")
} 

# read it
d<-read.csv("input/TreeMeasurements.csv", header=TRUE)

# clean the weird rows that have nothing in them
d <- subset(d, !is.na(bloc))

# start small with 24 only
w2024 <- d[, c(1:11)]

# add year column
w2024$year <- 2024
# add month col
w2024$month <- "february"

# standardize colnames
colnames(w2024) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "notes", "year", "month")

# 2025
w2025 <- d[, c(1:5, 12:16, 19)]

# add year column
w2025$year <- 2025
w2025$month <- ifelse(w2025$doy > 31 & w2025$doy < 60, "february", "march")

# standardize colnames
colnames(w2025) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "notes", "year", "month")

# bind both dfs 
binded <- rbind(w2024, w2025)

ggplot(binded) +
  geom_point(aes(x = year, y = diameter, color = species)) +
  facet_wrap(species~treatment, ncol = 6, nrow = 7) +
  ylim(0, 20)
