# 28 May 2025
# CRD
# Goal is to start data simulation for fuelinex treatments

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load library 
library(rstanarm)
library(ggplot2)
library(arm)

runmodels <- FALSE
runoldcode <- FALSE

setwd("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses")

# === === === === === === === === === === === === === === === === 
#### Step 1. Come up with a model ####
# === === === === === === === === === === === === === === === === 
# try to predict budburst and budset dates based on treatments. We'll start with one species first 

# === === === === === === === === === === === === === === === === 
#### Step 2. Simulate data ####
# === === === === === === === === === === === === === === === === 
n_trees <- 30  # 30 trees
treatment <- 2
n <- n_trees * n_trees

