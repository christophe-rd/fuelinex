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
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github/PhaenoFlex/treatment_overview/R"

# Set Working Directory
setwd(directory_path)

# Package
library(geosphere)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)

# Read totem field data from the past 30 years
climate_totem <- read.csv2("raw_data/TotemField_30Years_cleaned.csv", header = TRUE, sep = ",")

# Change columns
climate_totem$Tair_max <- as.numeric(climate_totem$Tair_max)
climate_totem$Tair_min <- as.numeric(climate_totem$Tair_min)
climate_totem$Tair_mean <- rowMeans(climate_totem[c("Tair_max", "Tair_min")], na.rm = TRUE)

# Change date format
climate_totem$date <- as.POSIXlt(strptime(climate_totem$Date, format = "%d-%b-%y"))
# Add column for year
climate_totem$year <- format(climate_totem$date, format = "%Y")
# Add column for month
climate_totem$month <- format(climate_totem$date, format = "%m")
# Add column for Julian day
climate_totem$julian <- as.numeric(format(climate_totem$date, "%j"))


### Create mean daily values for the last 30 years
mean_30years <- climate_totem %>%
  group_by(julian) %>%
  summarise(meantemp = mean(Tair_mean, na.rm = TRUE),
            maxtemp = mean(Tair_max, na.rm = TRUE),
            mintemp = mean(Tair_min, na.rm = TRUE))

cut <- mean_30years[c(60:121),]

ggplot(cut) +
  geom_line(aes(x=julian, y= meantemp))+
  geom_ribbon(aes(ymin = mintemp, ymax = maxtemp),
              alpha = 0.1, na.rm = TRUE) 

ggplot(data = cut, aes(x = julian, y = meantemp)) +
  geom_ribbon(aes(ymin = mintemp, ymax = maxtemp),
              alpha = 0.1, na.rm = TRUE) + 
  geom_line(aes(x=julian, y= meantemp))+
  geom_line(aes(x=julian, y= 20))

# Select from 92 to 121 (April)
# Max
head(climate_totem)
max <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 92 & julian <= 121) %>%
  summarise(Tair_max = max(Tair_max, na.rm=TRUE)) 

ggplot(max) + 
  geom_point(aes(x=year, y=Tair_max))

mean(max$Tair_max)
