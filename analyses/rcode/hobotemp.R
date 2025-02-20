#*======================================================================================================
## Hobo light loggers for the greenhouse
##  
##
## Project:        FUELINEX
## Date:           06.15.2024
## Author:         Christophe 
#*======================================================================================================

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
list.files()
#*------------------------------------------------------------------------------------------------------
# Set the path to your directory folder called PhotoChain
directory_path <- "/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/"

# Set Working Directory
setwd(directory_path)

# Package
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(scales)
library(lubridate)

# Define the folder path
folder_path <- "input/hoboTemp/2025_02_18/"
# Get all file names in the folder
file_list <- list.files(folder_path, full.names = TRUE)
# Function to extract the 8-digit ID from the filename
extract_id <- function(file_name) {
  id_match <- str_extract(basename(file_name), "\\d{8}")
  return(id_match)
}
# Read files and create a combined dataframe
df_list <- lapply(file_list, function(file) {
  data <- read.csv2(file, header = TRUE, sep = ",")
  data$id <- extract_id(file)  # Add extracted ID as a new column
  return(data)
})

# Combine all dfs
final_df <- bind_rows(df_list)
dim(final_df)
# View the first few rows
head(final_df)

# check the nrow for each
id_counts <- final_df %>%
  group_by(id) %>%
  summarise(count = n())

# change name so its more efficient!
d <- final_df
# Change colum names
colnames(d) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint", "Started", "HostConnected", "EndOfFile", "ID")
str(d)
# Change chr columns to numeric
d$temperature <- as.numeric(d$temperature)
d$RH <- as.numeric(d$RH)
d$DewPoint <- as.numeric(d$DewPoint)

# Change date format to posixct (calendar time)
d$date.time <- as.POSIXct(d$date.time, format = "%m-%d-%Y %H:%M:%S")
# Add column with date only
d$Date <- format(d$date.time, format = "%d-%m-%Y")
# Add column with hour only 
d$Hour <- format(d$date.time, "%H")
# add column doy
d$Date <- as.Date(d$Date)  # Convert if necessary
d$DOY <- as.numeric(d$Date - as.Date("1970-01-01"))

d$DOY <- yday(d$Date)

# === === === === === === === === === === === === === === ===
## Check if I have temp data for each logger since the spring with the most up to date back up
# === === === === === === === === === === === === === === ===
# create empty data frame where the first date in the record
df <- data.frame(id = NA, firstmeasure = NA)
# Get unique IDs
unique_ids <- unique(d$ID)

# Loop through each unique ID and extract the earliest date
for (i in unique_ids) {
  first_date <- min(d$Date[d$ID == i], na.rm = TRUE)  
  df <- rbind(df, data.frame(id = i, firstmeasure = first_date))
}

# View result
print(df)

# Mean of each hour for each day
d.hourly.means <- aggregate(temperature ~ Date + Hour, data = d, FUN = mean)

# Change column hour to numeric 
d.hourly.means$hour <- as.numeric(d.hourly.means$hour)


# # Subset only for the hours where the sun is up
# d.hourly.means.sub <- subset(d.hourly.means, hour == 6:21)
# B1_5358.hourly.means.sub <- subset(B1_5358.hourly.means, hour == 6:21)
# B3_5910.hourly.means.sub <- subset(B3_5910.hourly.means, hour == 6:21)
# str(B3_5910.hourly.means.sub)
# 
# mean(d.hourly.means.sub$light.lux)
# mean(B3_5910.hourly.means.sub$light.lux)

