#=== === === === === === === === === === === === === === === === === === === ===
## Title:           Phenostages FUELINEX
##  
##
## Project:         FUELINEX 
## Date:            May 20, 2024
## Autor:           Christophe 
#=== === === === === === === === === === === === === === === === === === === ===

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(readxl)
library(ggplot2)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder 
directory <-"/Users/christophe_rouleau-desrochers/Documents/github/fuelinex/"
setwd(directory)
list.files()

# Read data
phenostages <- read.csv2("data/phenological_monitoring/phenological_monitoring.csv", header = TRUE, sep = ",", check.names = FALSE)
head(phenostages)
str(phenostages)

# Function to convert all doy columns to numeric without having tons of warnings: 
#source: https://stackoverflow.com/questions/32846176/applying-as-numeric-only-to-elements-of-a-list-that-can-be-coerced-to-numeric-i
convert_to_numeric <- function(x) {
  as.numeric(ifelse(grepl("^[0-9]+$", x), x, NA))
}
# Convert to numeric
for (i in 7:ncol(phenostages)) {
  phenostages[, i] <- convert_to_numeric(phenostages[, i])
}

#### ACNE ####
acne <- subset(phenostages, genus =="acer")
head(acne)
# Select columns
acne.sel <- acne[, c(1, 3, 7:ncol(acne))]
head(acne.sel)
#Convert to long format
acne.long <- melt(setDT(acne.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
acne.mean <- aggregate(value ~ treatment + doy, data = acne.long, FUN = mean, na.omit =TRUE )
head(acne.mean)
# Quick plot
acne.mean.plot <- ggplot(acne.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("Acne total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
acne.mean.plot

# only for spring or fall
acne.sf <- acne.sel
head(acne.sf)
acne.sf$springtreatment <- ifelse(grepl('^CoolS', acne.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment spcified by >length(colnames(acne.sf)) is at second position and that all days are included 
acne.sf <- acne.sf[, c(1, 27, 3:12)]
head(acne.sf)
#Convert to long format
acne.long <- melt(setDT(acne.sf), id.vars = c("tree_ID","springtreatment"), variable.name = "doy")
head(acne.long)
# Mean of treatments + doy
acne.mean <- aggregate(value ~ springtreatment + doy, data = acne.long, FUN = mean, na.omit =TRUE )
# Standard deviation of treatments + doy
acne.sd <- aggregate(value ~ springtreatment + doy, data = acne.long, FUN = sd)
# Merge dfs mean and sd
acne.merged <- merge(acne.mean, acne.sd, by=c("springtreatment","doy"))
# change colnames
colnames(acne.merged) <- c("springtreatement", "doy", "mean", "sd")
head(acne.merged)


ggplot(acne.long, ) + 
  geom_histogram()

stat_boxplot(geom = "errorbar",
             width = 0.15) + 
