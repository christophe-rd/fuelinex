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
phenostages <- read.csv2("data/phenological_monitoring.csv", header = TRUE, sep = ",")

# Subset for quma, warm spring
quma <- subset(phenostages, genus =="quercus")
head(quma)
quma_warmspring <- subset(quma, treatment == "WarmS/WarmF_nitro")


