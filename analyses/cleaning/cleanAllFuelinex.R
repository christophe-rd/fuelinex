# Started 27 May 2025
# CRD

# get all fuelinex data into one file 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

if(length(grep("christophe_rouleau-desrochers", getwd()) > 0)) {
  setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")
} else if(length(grep("nolanmeier", getwd()) > 0)) {
setwd("/Users/nolanmeier/git/fuelinex/analyses")
}

# set source to false for now as they are not ready to be added yet
source <- FALSE

### === === === === === === === === === === ###
#### Get cleaned data ! ####
### === === === === === === === === === === ###

### === === === === === === === === === === ###
##### Start with 2024 #####
### === === === === === === === === === === ###
# 1. Get phenostages: named d
source("cleaning/source/2024cleaningPhenostages.R") 

# 2. Get senesence: named leafdrop24, greenleafcover24 and chl24
source("cleaning/source/2024cleaningSenescence.R") 

# 3. Get shoot elongation: named shootelongation24
source("cleaning/source/2024cleaningShootElongation.R") 
if (source){
  # 2. Get senescence
  
  # 3. Get shootelongation 
  
}

