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
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

# read it
d<-read.csv("input/TreeMeasurements.csv", header=TRUE)

# clean the weird rows that have nothing in them
d <- subset(d, !is.na(bloc))

# start small with 24 only
w2024 <- d[, c(1:11, 17:18)]

# add year column
w2024$year <- 2024
# add month col
w2024$month <- "february"

# standardize colnames
colnames(w2024) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "notes", "valid_height", "valid_diameter", "year", "month")

# Winter 2025
w2025 <- d[, c(1:5, 12:16, 19, 17:18)]

# add year column
w2025$year <- 2025
w2025$month <- ifelse(w2025$doy > 31 & w2025$doy < 60, "february", "march")

# standardize colnames
colnames(w2025) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "notes", "valid_height", "valid_diameter", "year", "month")

# Fall of 2025 
f2025 <- d[, c(1:5, 12:16, 19, 19:ncol(d))]

# add year column
f2025$year <- 2025
f2025$month <- ifelse(f2025$doy > 31 & f2025$doy < 60, "february", "march")

# 
test <- aggregate(
  diameterFall25 ~ species,
  f2025,
  FUN = function(x) sum(is.na(x)),
  na.action = na.pass
)
test

# quick checks for measured species 
pist <- subset(f2025, species == "pinus_strobus")
pist$noteFall25[which(is.na(pist$diameterFall25))]

prvi <- subset(f2025, species == "prunus_virginiana")
prvi$noteWinter25[which(is.na(prvi$diameterFall25))]
prvi$tree_ID[which(is.na(prvi$diameterFall25))]
# the missing prvi are all dead

quma <- subset(f2025, species == "quercus_macrocarpa")
quma$noteWinter25[which(is.na(quma$diameterFall25))]

# standardize colnames
colnames(f2025) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "notes", "valid_height", "valid_diameter", "year", "month")



# bind both dfs 
binded2 <- rbind(w2024, w2025)

# remove all the dead trees
binded <- subset(binded2, notes != "dead")

#### Go through the notes ####
# make changes if necessary
notesdf <- subset(binded, notes != "")

binded$valid_height[which(binded$notes == "measured shoot is dead, another shoot is measured")] <- "no"

# get invalidy for 2025 and remove measurements for 2024
### height
binded$Height <- binded$height
idheighttochange <- binded$tree_ID[which(binded$year == "2025" & binded$valid_height == "no")]
binded$Height[which(binded$tree_ID %in% idheighttochange & binded$year == "2024")] <- NA
#### diameter
binded$Diameter <- binded$diameter
iddiamtochange <- binded$tree_ID[which(binded$year == "2025" & binded$valid_diameter == "no")]
binded$Diameter[which(binded$tree_ID %in% iddiamtochange & binded$year == "2024")] <- NA

#

# write up csv
### reorganize to its nicer to play with
bindedtowrite <- binded[, c("tree_ID", "bloc", "treatment", "genus", "species", "year", "month", "doy", "Height", "Diameter", "notes")]

write.csv2(bindedtowrite, "output/cleanedMeasurements.csv", row.names = FALSE)

