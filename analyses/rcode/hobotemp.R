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

# === === === === === === === === === === === === === === ===
#### Read the back up on 2024 6 7 ####
# === === === === === === === === === === === === === === ===
# Define the folder path
folder_path <- "input/hoboTemp/2024_06_07/"
# Get all file names in the folder
file_list <- list.files(folder_path, full.names = TRUE)
file_list <- file_list[file_list!=c("input/hoboTemp/2024_06_07//21218489 2024-06-07 17_35_36 PDT (Data PDT).csv", "input/hoboTemp/2024_06_07//21218486 2024-06-07 17_39_11 PDT (Data PDT).csv")]
# Function to extract the 8-digit ID from the filename
extract_id <- function(file_name) {
  id_match <- str_extract(basename(file_name), "\\d{8}")
  return(id_match)
}
# Read files and create a combined dataframe
dataframe <- lapply(file_list, function(file) {
  data <- read.csv2(file, header = TRUE, sep = ",")
  data$id <- extract_id(file)  # Add extracted ID as a new column
  return(data)
}) 
# Combine all dfs
d <- bind_rows(dataframe)
#select only 5 columns as it messes up the merge downstream
d <- d[,c(1:5, 8)]
# read 899 manually because it has a different format than the others
d899 <- read.csv2("input/hoboTemp/2024_06_07//21218489 2024-06-07 17_35_36 PDT (Data PDT).csv", header = TRUE, sep = ",")
d899$id <- "21218489"
dim(d899)
colnames(d899)
d899 <- d899[,c(1:5,10)]
# read 486
d486 <- read.csv2("input/hoboTemp/2024_06_07//21218486 2024-06-07 17_39_11 PDT (Data PDT).csv", header = TRUE, sep = ",")
d486$id <- "21218486"
colnames(d486)
d486 <- d486[,c(1:5,9)]
head(d486)
# change colnames
colnames(d899) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
colnames(d) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
colnames(d486) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
# Convert date.time to POSIXct in both datasets
d899$date.time2 <- as.POSIXct(d899$date.time, format = "%m-%d-%Y %H:%M:%S")
d486$date.time2 <- as.POSIXct(d486$date.time, format = "%m-%d-%Y %H:%M:%S") #### problem is here: i need to remove PDT or PST and figure out what date is the good one.
d$date.time2 <- as.POSIXct(d$date.time, format = "%m-%d-%Y %H:%M:%S")

# REUNITE
df <- rbind(d899,d,d486)
# Change chr columns to numeric
df$Temperature <- as.numeric(df$temperature)
df$RH <- as.numeric(df$RH)
df$DewPoint <- as.numeric(df$DewPoint)
# Change date format to posixct (calendar time)
df$date.time <- as.POSIXct(df$date.time, format = "%m-%d-%Y %H:%M:%S")
# Add column with date only
df$Date <- as.Date(df$date.time, format = "%d-%m-%Y")
# Add column with hour only 
df$Hour <- format(df$date.time, "%H")

# === === === === === === === === === === === === === === ===
#### Temperature Cool Spring according to 2 hobo loggers ####
# === === === === === === === === === === === === === === ===
# subset for the 2 loggers that were in the chambers
unique(df$ID) # will have to double check that these 2 are the good ones
# , 21218489"21479899"
# vec <- c("21479899", "21218489")
vec <- "21218486"
coolstempsub <- df[df$ID %in% vec,]
# change name so its more efficient!
cools <- coolstempsub
# Subset the dataframe for dates between 7 March 2024 and 24 May 2024
subset_cools <- cools[cools$Date >= as.Date("2024-03-10") & cools$Date <= as.Date("2024-05-19"), ]
str(cools)
str(subset_cools)
# quick plot to visualize the temperature
ggplot(subset_cools)+
  geom_smooth(aes(x=Date, y=Temperature), colour="blue")

# maybe i should aggregate for the min temperature
test <- aggregate(Temperature ~ Date, data = subset_cools, 
                             FUN = function(x) c(
                               Mean = mean(x, na.rm = TRUE), 
                               Min = min(x, na.rm = TRUE), 
                               Max = max(x, na.rm = TRUE)))
test <- do.call(data.frame, test)
colnames(test) <- c("Date", "Mean_Temp", "Min_Temp", "Max_Temp")

# ggplot
ggplot(test, aes(x = Date)) +
  # Add a ribbon for min and max temperatures
  geom_ribbon(aes(ymin = Min_Temp, ymax = Max_Temp), 
              fill = "lightblue", alpha = 0.5) +
  # Add a line for the mean temperature
  geom_line(aes(y = Mean_Temp), color = "blue", linewidth = 1) +
  # Add labels and title
  labs(x = "Date", y = "Temperature (°C)", 
       title = "Daily Temperature (Mean, Min, Max)") +
  # Customize the theme (optional)
  theme_minimal()
# === === === === === === === === === === === === === === ===
#### Temperature Warm Spring according to 2 hobo loggers ####
# === === === === === === === === === === === === === === ===
