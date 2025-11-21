# === === === === === === === === === === === === === === === === === === === 
## Hobo light loggers for the greenhouse
##  
##
## Project:        FUELINEX
## Date:           06.15.2024
## Author:         Christophe 
# === === === === === === === === === === === === === === === === === === === 

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
list.files()
#*------------------------------------------------------------------------------
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
# Change date format 
d$Date <- as.Date(d$date.time, format = "%d-%b-%Y")
# Add column with hour only 
d$Hour <- format(d$date.time, "%H")
# add year
d$Year <- format(d$date.time, "%Y")
# add column month
d$Month <- format(d$Date, "%m")
# add column julian
d$julian <- as.numeric(format(d$Date, "%j"))

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
# d.hourly.means$hour <- as.numeric(d.hourly.means$hour)

# === === === === === === === === === === === === === === ===
#### Read the back up on 2024 6 7 ####
# === === === === === === === === === === === === === === ===
# Define the folder path
folder_path <- "input/hoboTemp/2024_06_07/"
# Get all file names in the folder
file_list <- list.files(folder_path, full.names = TRUE)
# vec of files to exclude because of different formating
v <- file_list[c(2, 3)]
# exclude files stored in v
file_list <- file_list[!file_list %in% v]
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
# select only 5 columns as it messes up the merge downstream
d <- d[,c(1:5, 8)]
# read 899 manually because it has a different format than the others
d899 <- read.csv2("input/hoboTemp/2024_06_07//21218489 2024-06-07 17_35_36 PDT (Data PDT).csv", header = TRUE, sep = ",")
d899$id <- "21218489"
d899 <- d899[,c(1:5,10)]
# read 486
d486 <- read.csv2("input/hoboTemp/2024_06_07//21218486 2024-06-07 17_39_11 PDT (Data PDT).csv", header = TRUE, sep = ",")
d486$id <- "21218486"
d486 <- d486[,c(1:5,9)]
# change colnames
colnames(d899) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
colnames(d) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
colnames(d486) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
# REUNITE
df <- bind_rows(d899, d, d486)
# Change chr columns to numeric
df$Temperature <- as.numeric(df$temperature)
df$RH <- as.numeric(df$RH)
df$DewPoint <- as.numeric(df$DewPoint)
# Change date format to posixct (calendar time)
df$date.time <- as.POSIXct(df$date.time, format = "%m-%d-%Y %H:%M:%S")
# Change date format 
df$Date <- as.Date(df$date.time, format = "%d-%b-%Y")
# Add column with hour only 
df$Hour <- format(df$date.time, "%H")
# add year
df$Year <- format(df$date.time, "%Y")
# add column month
df$Month <- format(df$Date, "%m")
# add column julian
df$julian <- as.numeric(format(df$Date, "%j"))
b7june <- df

# === === === === === === === === === === === === === === ===
##### Temperature Cool Spring according to 2 hobo loggers #####
# === === === === === === === === === === === === === === ===
# subset for the 2 loggers that were in the chambers
unique(b7june$ID) # will have to double check that these 2 are the good ones
# , 21218489"21479899"
# vec <- c("21479899", "21218489")
vec <- "21218486"
cspringtempsub <- b7june[b7june$ID %in% vec,]
# change name so its more efficient!
cspring <- cspringtempsub
# Subset the dataframe for dates between 7 March 2024 and 24 May 2024
subset_cspring <- cspring[cspring$Date >= as.Date("2024-03-10") & cspring$Date <= as.Date("2024-05-24"), ]
# maybe i should aggregate for the min temperature
cspringagr <- aggregate(Temperature ~ julian + Year, data = subset_cspring, 
                             FUN = function(x) c(
                               Mean = mean(x, na.rm = TRUE), 
                               Min = min(x, na.rm = TRUE), 
                               Max = max(x, na.rm = TRUE)))
cspringagr <- do.call(data.frame, cspringagr)
colnames(cspringagr) <- c("julian", "Year", "Mean_Temp", "Min_Temp", "Max_Temp")
# === === === === === === === === === === === === === === ===
#### Read the back up on 2024_10_30 #####
# === === === === === === === === === === === === === === ===
# Define the folder path
folder_path <- "input/hoboTemp/2024_10_30/"
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
  data$id <- extract_id(file) 
  return(data)
})
# Combine all dfs
final_df <- bind_rows(df_list)
# remove non necessary columns
final_df <- final_df[, c(1:5, 9)]
# change name so its more logic
wfall <- final_df
# Change colum names
colnames(wfall) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
# Change chr columns to numeric
wfall$Temperature <- as.numeric(wfall$temperature)
wfall$RH <- as.numeric(wfall$RH)
wfall$DewPoint <- as.numeric(wfall$DewPoint)
# Change date format to posixct (calendar time)
wfall$date.time <- as.POSIXct(wfall$date.time, format = "%m-%d-%Y %H:%M:%S")
# Change date format 
wfall$Date <- as.Date(wfall$date.time, format = "%d-%b-%Y")
# Add column with hour only 
wfall$Hour <- format(wfall$date.time, "%H")
# add year
wfall$Year <- format(wfall$date.time, "%Y")
# add column month
wfall$Month <- format(wfall$Date, "%m")
# add column julian
wfall$julian <- as.numeric(format(wfall$Date, "%j"))

# === === === === === === === === === === === === === === ===
##### Temperature Warm Fall according to 2 hobo loggers #####
# === === === === === === === === === === === === === === ===
# subset for the duration of warm fall treatment
subset_wfall<- wfall[wfall$Date >= as.Date("2024-09-10") & wfall$Date <= as.Date("2024-10-29"), ]
# maybe i should aggregate for the min temperature
wfallagr <- aggregate(Temperature ~ julian + Year, data = subset_wfall, 
                      FUN = function(x) c(
                        Mean = mean(x, na.rm = TRUE), 
                        Min = min(x, na.rm = TRUE), 
                        Max = max(x, na.rm = TRUE))) # for now its the absolute max and min instead of the meanmin and mean max


wfallagr <- do.call(data.frame, wfallagr)
# change colnames of aggregated table
colnames(wfallagr) <- c("julian", "Year", "Mean_Temp", "Min_Temp", "Max_Temp")

# === === === === === === === === === === === === === === ===
#### Read the back up on 2025_02_18 #####
# === === === === === === === === === === === === === === ===
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
  data$id <- extract_id(file) 
  return(data)
})
# Combine all dfs
final_df <- bind_rows(df_list)
# remove non necessary columns
final_df <- final_df[, c(1:5, 9)]
# change name so its more logic
ambient <- final_df
# Change colum names
colnames(ambient) <- c("nMeasurement", "date.time", "temperature", "RH", "DewPoint","ID")
# Change chr columns to numeric
ambient$Temperature <- as.numeric(ambient$temperature)
ambient$RH <- as.numeric(ambient$RH)
ambient$DewPoint <- as.numeric(ambient$DewPoint)
# Change date format to posixct (calendar time)
ambient$date.time <- as.POSIXct(ambient$date.time, format = "%m-%d-%Y %H:%M:%S")
# Change date format 
ambient$Date <- as.Date(ambient$date.time, format = "%d-%b-%Y")
# Add column with hour only 
ambient$Hour <- format(ambient$date.time, "%H")
# add year column only
ambient$Year <- format(ambient$date.time, "%Y")
# add column month
ambient$Month <- format(ambient$Date, "%m")
# add column julian
ambient$julian <- as.numeric(format(ambient$Date, "%j"))

# === === === === === === === === === === === === === === ===
#### Ambient temperature at totem field from  May to Feb 2025 - Hobo####
# === === === === === === === === === === === === === === ===
### might not have to do this since I have totem data

# === === === === === === === === === === === === === === ===
#### Ambient temperature at totem field from 20 June to Feb 2025 - Hobo ####
# === === === === === === === === === === === === === === ===
# subset for the duration of warm fall treatment
subset_ambient<- ambient[ambient$Date >= as.Date("2024-06-20") & ambient$Date <= as.Date("2025-02-18"), ]
# maybe i should aggregate for the min temperature
ambientagr <- aggregate(Temperature ~ julian + Year, data = subset_ambient, 
                      FUN = function(x) c(
                        Mean = mean(x, na.rm = TRUE), 
                        Min = min(x, na.rm = TRUE), 
                        Max = max(x, na.rm = TRUE)))
ambientagr <- do.call(data.frame, ambientagr)
# change colnames of aggregated table
colnames(ambientagr) <- c("julian", "Year", "Mean_Temp", "Min_Temp", "Max_Temp")

# === === === === === === === === === === === === === === ===
#### Ambient temperature at totem field from 20 June to Feb 2025 - Totem C Station ####
# === === === === === === === === === === === === === === ===
# Read the CSV file
totem <- read.csv("input/TotemField_30Years.csv", skip = 2, header = TRUE)
totem <- totem[c(2:nrow(totem)),]
# Display the first few rows of the data
str(totem)
totem$Max_Temp <- as.numeric(totem$Tair_max)
totem$Min_Temp <- as.numeric(totem$Tair_min)
totem$Mean_Temp <- rowMeans(totem[c("Max_Temp", "Min_Temp")], na.rm = TRUE)
# Change date format
totem$date <- as.Date(totem$Date, format = "%d-%b-%Y")
# Add column for year
totem$Year <- format(totem$date, format = "%Y")
# Add column for month
totem$month <- format(totem$date, format = "%m")
# Add column for Julian day
totem$julian <- as.numeric(format(totem$date, "%j"))

### subset for 2023 and change for now for 2024 until i get the updated data
t2023 <- subset(totem, Year == "2023")
t2023$Year <- "2024"

#### Create temperature df for each treatment combos ####
#standardize all formats of temp + date
head(cspringagr)
head(t2023)
# rearrange totem columns
t2023 <- t2023[,c("julian", "Year", "Mean_Temp", "Min_Temp", "Max_Temp")] 
head(t2023)
tail(cspringagr)
##### 1. CSpring+CFall #####
# Start with CSpring, the trees were in the chambers from March 8 to May 24
# subset totem for the period when the trees were put back outside
tbfr69 <- t2023[t2023$julian <= 69, ]
tafter145 <- t2023[t2023$julian >= 146 & ncol(t2023), ]
# bind with cspringagr
cscf <- rbind(tbfr69, cspringagr, tafter145)
cscf$Treatment <- "CSCF"
##### 2. CSpring+WFall #####
# these are trees that were in the chambers from March 8 to May 24 and all fall
tbfr69 <- t2023[t2023$julian <= 69, ]
tafter145bfr248 <- t2023[t2023$julian >= 146 & t2023$julian <= 253, ]
t2023aftr304 <- t2023[t2023$julian >= 304, ]
cswf <- rbind(tbfr69, cspringagr, tafter145bfr248, wfallagr, t2023aftr304)
cswf$Treatment <- "CSWF"
##### 3. WSpring+CFall #####
# these are trees that were at ambient temperature all year long
wscf <- t2023
wscf$Treatment <- "WSCF"
##### 4. WSpring+WFall #####
t2023bfr253 <- t2023[t2023$julian <= 253, ]
t2023aftr304 <- t2023[t2023$julian >= 304, ]
wswf <- rbind(t2023bfr253, wfallagr,t2023aftr304)
wswf$Treatment <- "WSWF"

##### BIND THEM! #####
alltreatments <- rbind(cscf, cswf, wscf, wswf)
alltreatments
ggplot(alltreatments, aes(x = julian)) +
  # Add a ribbon for min and max temperatures
  geom_ribbon(aes(ymin = Min_Temp, ymax = Max_Temp), 
              fill = "lightblue", alpha = 0.5) +
  # Add a line for the mean temperature
  geom_smooth(aes(y = Mean_Temp), color=Treatment, linewidth = 1) +
  # facet_wrap(~Treatment) +
  # Add labels and title
  labs(x = "Date", y = "Temperature (°C)", 
       title = "Daily Temperature (Mean, Min, Max)") +
  # Customize the theme (optional)
  theme_minimal()
#### Plots ####
##### Warm Spring #####

##### Cool Spring #####
ggplot(test, aes(x = Date)) +
  # Add a ribbon for min and max temperatures
  geom_ribbon(aes(ymin = Min_Temp, ymax = Max_Temp), 
              fill = "lightblue", alpha = 0.5) +
  # Add a line for the mean temperature
  geom_smooth(aes(y = Mean_Temp), color = "blue", linewidth = 1) +
  # Add labels and title
  labs(x = "Date", y = "Temperature (°C)", 
       title = "Daily Temperature (Mean, Min, Max)") +
  # Customize the theme (optional)
  theme_minimal()

##### Warm Fall #####
# ggplot
ggplot(wfallagr, aes(x=Date)) +
  # Add a ribbon for min and max temperatures
  geom_ribbon(aes(ymin = Min_Temp, ymax = Max_Temp), 
              fill = "lightblue", alpha = 0.5) +
  # Add a line for the mean temperature
  geom_smooth(aes(x=Date, y = Mean_Temp), color = "blue", linewidth = 1) +
  # Add labels and title
  labs(x = "Date", y = "Temperature (°C)", 
       title = "Daily Temperature (Mean, Min, Max)") +
  # Customize the theme (optional)
  theme_minimal()

##### Cool Fall #####

##### Ambient #####
ggplot(ambientagr, aes(x = Date)) +
  # Add a ribbon for min and max temperatures
  geom_ribbon(aes(ymin = Min_Temp, ymax = Max_Temp), 
              fill = "lightblue", alpha = 0.5) +
  # Add a line for the mean temperature
  geom_smooth(aes(y = Mean_Temp), color = "blue", linewidth = 1) +
  # Add labels and title
  labs(x = "Date", y = "Temperature (°C)", 
       title = "Daily Temperature (Mean, Min, Max)") +
  # Customize the theme (optional)
  theme_minimal()

