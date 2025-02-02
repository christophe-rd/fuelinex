### Cleaning tree measurements and assessing the comments and irregularies
# CRD on 1 February 2025

# house keeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)


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
str(d)
# start small
dcut <- d[1:180, c("tree_ID","species", "heightWinter24", "heightWinter25")]
dput(dcut)
# Start by looking at height increment!
g <- ggplot(d, aes(x = Year, y = Height, fill = Species)) +
  geom_boxplot() 
  
# create df for 2024 measurements
d24<-d[,c("tree_ID", "species", "heightWinter24", "heightWinter24_trunk2", "diameterWinter24", "diameterWinter24_trunk2", "noteWinter24")]

# add year

d24$year <- 2024

# Usage example
new_data <- reshape_data(your_dataframe)

