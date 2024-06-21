#*======================================================================================================
## Temperature decision for climate chambers
##  
##
## Project:        FUELINEX
## Date:           06.15.2023
## Author:         Christophe 
#*======================================================================================================

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
list.files()
#*------------------------------------------------------------------------------------------------------
# Set the path to your directory folder called PhotoChain
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github/fuelinex/data/hobo_light/"

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
B1_5913<- read.csv2("B1_21715913 2024-06-19 13_56_09 PDT (Data PDT)(1).csv", header = TRUE, sep = ",")
B1_5921 <- read.csv2("B1_21715921 2024-06-19 13_55_41 PDT (Data PDT).csv", header = TRUE, sep = ",")
B1_5358 <- read.csv2("B1_21715358 2024-06-19 13_56_34 PDT (Data PDT)(1).csv", header = TRUE, sep = ",")
B3_5910 <- read.csv2("B3_21715910 2024-06-19 16_14_43 PDT (Data PDT)(1).csv", header = TRUE, sep = ",")
str(B1_5921)
# Change colum names
colnames(B1_5921) <- c("id", "date.time", "temperature", "light.lux")
colnames(B1_5358) <- c("id", "date.time", "light.lux")
colnames(B3_5910) <- c("id", "date.time", "light.lux")

# Change chr columns to numeric
B1_5921$light.lux <- as.numeric(B1_5921$light.lux)
B1_5358$light.lux <- as.numeric(B1_5358$light.lux)
B3_5910$light.lux <- as.numeric(B3_5910$light.lux)

# Change date format to posixct (calendar time)
B1_5921$date.time <- as.POSIXct(B1_5921$date.time, format = "%m-%d-%Y %H:%M:%S")
B1_5358$date.time <- as.POSIXct(B1_5358$date.time, format = "%m-%d-%Y %H:%M:%S")
B3_5910$date.time <- as.POSIXct(B3_5910$date.time, format = "%m-%d-%Y %H:%M:%S")

# Add column with date only
B1_5921$date <- as.POSIXct(B1_5921$date.time, format = "%m-%d-%Y")
B1_5358$date <- as.POSIXct(B1_5358$date.time, format = "%m-%d-%Y")
B3_5910$date <- as.POSIXct(B3_5910$date.time, format = "%m-%d-%Y")

# Add column with hour only 
B1_5921$hour <- format(B1_5921$date.time, "%H")
B1_5358$hour <- format(B1_5358$date.time, "%H")
B3_5910$hour <- format(B3_5910$date.time, "%H")

# Mean of each hour for each day
B1_5921.hourly.means <- aggregate(light.lux ~ date + hour, data = B1_5921, FUN = mean)
B1_5358.hourly.means <- aggregate(light.lux ~ date + hour, data = B1_5358, FUN = mean)
B3_5910.hourly.means <- aggregate(light.lux ~ date + hour, data = B3_5910, FUN = mean)
head(B3_5910.hourly.means)



# Preliminary plot
ggplot() +
  geom_jitter(data = B3_5910.hourly.means,
              aes(hour, light.lux), 
              colour= "blue", alpha = 0.5,
              width = 1,
              height = 0,) +
  geom_jitter(data = B1_5921.hourly.means, 
              aes(hour, light.lux), 
              colour= "red", alpha = 0.5,
              width = 1,
              height = 0,)


