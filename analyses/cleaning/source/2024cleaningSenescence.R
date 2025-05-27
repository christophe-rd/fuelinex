# 27 May 2025
# CRD
# Goal is to clean senescence monitoring in 2024

# housekeeping 
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder  
directory_path <- "/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/"

# Set Working Directory
setwd(directory_path)

# Load librairies
library(ggplot2)
library(dplyr)

# Read csv
shoot <- read.csv2("input/2024ShootElongation.csv", header = TRUE, sep = ",", check.names = FALSE)
head(shoot)
colnames(shoot)[6] <- "notes"