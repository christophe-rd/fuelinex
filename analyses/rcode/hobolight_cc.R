#*======================================================================================================
## Hobo light loggers for the climate chambers
##  
##
## Project:        FUELINEX
## Date:           07.01.2024
## Author:         Christophe 
#*======================================================================================================

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
list.files()
#*------------------------------------------------------------------------------------------------------
# Set the path to your directory folder called PhotoChain
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github/fuelinex/data/hobo_light/2024-07-01"

# Set Working Directory
setwd(directory_path)

# Package
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(scales)

# Read totem field data from the past 30 years
cc3 <- read.csv2("cc321715492 2024-07-01 16_55_01 PDT (Data PDT).csv", header = TRUE, sep = ",")
cc5 <- read.csv2("cc521713595 2024-07-01 16_55_40 PDT (Data PDT).csv", header = TRUE, sep = ",")
walkin <- read.csv2("walkin21715357 2024-07-01 16_55_08 PDT (Data PDT).csv", header = TRUE, sep = ",")

# Change colum names
colnames(cc3) <- c("id", "date.time", "temperature", "light.lux")
colnames(cc5) <- c("id", "date.time", "temperature","light.lux")
colnames(walkin) <- c("id", "date.time", "temperature","light.lux")

# Change chr columns to numeric
cc3$light.lux <- as.numeric(cc3$light.lux)
cc5$light.lux <- as.numeric(cc5$light.lux)
walkin$light.lux <- as.numeric(walkin$light.lux)

# Change date format to posixct (calendar time)
cc3$date.time <- as.POSIXct(cc3$date.time, format = "%m-%d-%Y %H:%M:%S")
cc5$date.time <- as.POSIXct(cc5$date.time, format = "%m-%d-%Y %H:%M:%S")
walkin$date.time <- as.POSIXct(walkin$date.time, format = "%m-%d-%Y %H:%M:%S")

# Add column with date only
cc3$date <- as.POSIXct(cc3$date.time, format = "%m-%d-%Y")
cc5$date <- as.POSIXct(cc5$date.time, format = "%m-%d-%Y")
walkin$date <- as.POSIXct(walkin$date.time, format = "%m-%d-%Y")
head(cc3)
# Add column with hour only 
cc3$hour <- format(cc3$date.time, "%H")
cc5$hour <- format(cc5$date.time, "%H")
walkin$hour <- format(walkin$date.time, "%H")


