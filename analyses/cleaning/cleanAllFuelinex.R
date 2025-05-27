# Started 27 May 2025
# CRD

# get all fuelinex data into one file 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")
} 
  # else if(length(grep("lizzie", getwd()) > 0)) {
  # setwd("/Users/lizzie/Documents/git/projects/egret/analyses")
# } 

# set source to false for now as they are not ready to be added yet
source <- FALSE

### === === === === === === === === === === ###
#### Get cleaned data ! ####
### === === === === === === === === === === ###

### === === === === === === === === === === ###
##### Start with 2024 #####
### === === === === === === === === === === ###
# 1. Get phenostages
source("cleaning/source/2024cleaningPhenostages.R") 

if (source){
  # 2. Get senescence
  source("cleaning/source/2024cleaningSenescence.R") 
  # 3. Get shootelongation 
  source("cleaning/source/2024cleaningShootElongation.R") 
}

