# Started on 1 February 2025
# CRD
# Goal Cleaning tree measurements and assessing the comments and irregularies

# house keeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)
options(max.print = 150) 
options(digits = 5)

### ####
# do this: need to add validity somehow into the script


# Load libraries
library(ggplot2)

# Set Working Directory
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

# read it
meas <- read.csv("input/TreeMeasurements.csv", header=TRUE)
meas[meas == ""] <- NA
biom <- read.csv("input/biomass.csv", header=TRUE)
biom[biom == ""] <- NA
biom$aboveGroundWeight[biom$aboveGroundWeight == "na"] <- NA
biom$belowGroundWeight[biom$belowGroundWeight == "na"] <- NA

biom <- biom[, 1:ncol(biom)-1]

# clean the weird rows that have nothing in them
meas <- subset(meas, !is.na(bloc))

# start small with 24 only
w2024 <- meas[, c(1:11, 17:18)]

# add year column
w2024$year <- 2024
# add month col
w2024$month <- "february"
w2024$season <- "winter"

# standardize colnames
colnames(w2024) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "notes", "valid_height", "valid_diameter", "year", "month", "season")

# Winter 2025
w2025 <- meas[, c(1:5, 12:16, 19, 17:18)]

# add year column
w2025$year <- 2025
w2025$month <- ifelse(w2025$doy > 31 & w2025$doy < 60, "february", "march")
w2025$season <- "winter"
# standardize colnames
colnames(w2025) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "notes", "valid_height", "valid_diameter", "year", "month", "season")

# Fall of 2025 
f2025 <- meas[, c(1:5, 20:ncol(meas))]

# add year column
f2025$year <- 2025
f2025$season <- "fall"
colnames(f2025) <- c("tree_ID", "bloc", "treatment", "genus", "species", "diameter", "diameterTrunk2", "height", "heightTrunk2", "doy", "valid_height", "valid_diameter", "notes", "year", "season")

# add missing doy for measurements
f2025$doy[which(is.na(f2025$doy) & f2025$genus == "betula")] <- 317
f2025$doy[which(is.na(f2025$doy) & f2025$genus == "populus")] <- 328

f2025$month <- ifelse(f2025$doy > 274 & f2025$doy < 305, "october", "november")

# quick checks
test <- aggregate(
  diameter ~ species,
  f2025,
  FUN = function(x) sum(is.na(x)),
  na.action = na.pass
)
test
test2 <- aggregate(
  height ~ species,
  f2025,
  FUN = function(x) sum(is.na(x)),
  na.action = na.pass
)
test2

heightna <- f2025$tree_ID[which(is.na(f2025$height) & 
                      f2025$genus != "betula" & 
                      f2025$genus != "populus" & 
                      f2025$genus != "pinus")]
diameterna <- f2025$tree_ID[which(is.na(f2025$diameter) & 
                                  f2025$genus != "betula" & 
                                  f2025$genus != "populus" &
                                  f2025$genus != "pinus")]
setdiff(heightna, diameterna)

# Clean biomass ####
vec_chr <- biom$tree_ID[which(is.na(biom$aboveGroundWeight) & 
                                biom$genus != "betula" &
                                biom$genus != "populus" &
                                biom$genus != "pinus")]
vec_chr
for (i in 7:ncol(biom)) {
  as.numeric(biom[[i]])
}
vec_numabov <- biom$tree_ID[which(is.na(biom$aboveGroundWeight) & 
                                biom$genus != "betula" &
                                biom$genus != "populus" &
                                biom$genus != "pinus")]

vec_numbelow <- biom$tree_ID[which(is.na(biom$belowGroundWeight) & 
                                biom$genus != "betula" &
                                biom$genus != "populus" &
                                biom$genus != "pinus")]

t <- setdiff(diameterna, vec_numabov)
setdiff(diameterna, vec_numbelow)
tsub <- subset(biom, tree_ID %in% t)

biomna_above <- biom$tree_ID[which(is.na(biom$aboveGroundWeight) & 
                                  biom$genus != "betula" & 
                                  biom$genus != "populus")]
biomna_below <- biom$tree_ID[which(is.na(biom$belowGroundWeight) & 
                               biom$genus != "betula" & 
                               biom$genus != "populus")]
setdiff(biomna_above, biomna_below)

# quick checks for measured species 
pist <- subset(f2025, species == "pinus_strobus")
pist$noteFall25[which(is.na(pist$diameterFall25))]

prvi <- subset(f2025, species == "prunus_virginiana")
prvi$noteWinter25[which(is.na(prvi$diameterFall25))]
prvi$tree_ID[which(is.na(prvi$diameterFall25))]
# the missing prvi are all dead

quma <- subset(f2025, species == "quercus_macrocarpa")
quma$noteWinter25[which(is.na(quma$diameterFall25))]


# bind both dfs 
binded <- rbind(w2024, w2025, f2025)

# remove all the dead trees
# binded <- subset(binded2, notes != "dead")

#### Go through the notes ####
# make changes if necessary
notesdf <- subset(binded, notes != "")

binded$valid_height[which(binded$notes == "measured shoot is dead, another shoot is measured")] <- "no"

# get invalidy for winter 2025 and remove measurements for winter 2024
### height
binded$Height <- binded$height
idheighttochange <- binded$tree_ID[which(binded$year == "2025" &
                                           binded$month != "october" &
                                           binded$month != "november" &
                                           binded$valid_height == "no")]

idheighttochange
binded$Height[which(binded$tree_ID %in% idheighttochange & 
                      binded$year == "2024")] <- NA

#### diameter
binded$Diameter <- binded$diameter

iddiamtochange <- binded$tree_ID[which(binded$year == "2025" &
                                           binded$month != "october" &
                                           binded$month != "november" &
                                           binded$valid_diameter == "no")]
binded$Diameter[which(binded$tree_ID %in% iddiamtochange & 
                        binded$year == "2024")] <- NA

# get invalidy for fall 2025 and remove measurements for winter 2025
### height
binded$Height <- binded$height
idheighttochange <- binded$tree_ID[which(binded$year == "2025" &
                                           binded$month != "february" &
                                           binded$month != "march" &
                                           binded$valid_height == "no")]

binded$Height[which(binded$tree_ID %in% idheighttochange & 
                      binded$year == "2024")] <- NA

# Decision: the year of the measurements will be corresponding to their previous GS.e.g. winter2024 will be 2023
binded$Year <- binded$year
binded$Year[which(binded$year == "2024")] <- 2023
binded$Year[which(binded$year == "2025" & 
                    binded$season == "winter")] <- 2024
binded$Year[which(binded$year == "2025" & 
                    binded$season == "fall")] <- 2025

# write up csv
### reorganize to its nicer to play with
bindedtowrite <- binded[, c("tree_ID", "bloc", "treatment", "genus", "species", "Year", "Height", "Diameter", "notes")]

# rename for consistency
colnames(bindedtowrite) <- c("tree_ID", "bloc", "treatment", "genus", "species", "year", "height", "diameter", "notes")
write.csv2(bindedtowrite, "output/cleanedMeasurements.csv", row.names = FALSE)

