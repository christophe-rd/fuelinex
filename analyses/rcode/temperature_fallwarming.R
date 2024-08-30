## Started 14 August 2024
## By Christophe RD

## This code intends to take a look at the summer temperature so I can make a decision on what temperature should I set the climate chambers at.

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
list.files()

#*------------------------------------------------------------------------------------------------------

directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github/fuelinex/"

# Set Working Directory
setwd(directory_path)

# Package
library(ggplot2)
library(dplyr)
# Read totem field data from the past 30 years
climate_totem <- read.csv2("data/totem_field_climate/TotemField_30Years_cleaned.csv", header = TRUE, sep = ",")

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

cut <- mean_30years[c(183:365),]

ggplot(cut) +
  geom_line(aes(x=julian, y= meantemp))+
  geom_ribbon(aes(ymin = mintemp, ymax = maxtemp),
              alpha = 0.1, na.rm = TRUE) 

ggplot(data = mean_30years, aes(x = julian, y = meantemp)) +
  geom_ribbon(aes(ymin = mintemp, ymax = maxtemp),
              alpha = 0.1, na.rm = TRUE) + 
  geom_line(aes(x=julian, y= meantemp))


# Select from 214 to 244 (August)
# Mean
mean_aug <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 214 & julian <= 244) %>%
  summarise(Tair_mean = mean(Tair_mean, na.rm=TRUE)) 

# Min
min_aug <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 214 & julian <= 244) %>%
  summarise(Tair_min = min(Tair_min, na.rm=TRUE)) 

# Max
max_aug <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 214 & julian <= 244) %>%
  summarise(Tair_max = max(Tair_max, na.rm=TRUE)) 

# Select from 245 to 274 (September)
# Mean
mean_sept <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 245 & julian <= 274) %>%
  summarise(Tair_mean = mean(Tair_mean, na.rm=TRUE)) 

# Min
min_sept <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 245 & julian <= 274) %>%
  summarise(Tair_min = min(Tair_min, na.rm=TRUE)) 

# Max
max_sept <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 245 & julian <= 274) %>%
  summarise(Tair_max = max(Tair_max, na.rm=TRUE)) 

as.vector(max_sept)
mean(mean_aug$Tair_mean)
mean(mean_sept$Tair_mean)

mean(max_aug$Tair_max)
mean(max_sept$Tair_max)

mean(min_aug$Tair_min)
mean(min_sept$Tair_min)


head(climate_totem)

#### Weekly temperatures ####
##### Max temperature #####
max_sept_a <- climate_totem %>%
  group_by(year) %>%
  filter(julian >= 214 & julian <= 221) %>%
  summarise(Tair_max = max(Tair_max, na.rm=TRUE))
head(climate_totem)

# Tests
# subset for the first week of september (245 to 252)
suby <- subset(climate_totem, julian == c(245, 246, 247, 248, 249, 250, 251, 252))
cut <- climate_totem[1:730, c("Tair_max", "year", "julian")]
dput(cut)
str(climate_totem)


# Assuming your data frame is named `climate_totem`

# Initialize an empty list to store the means
means_per_year <- list()

# Get the unique years in the data
years <- unique(climate_totem$year)

# Loop over each year
for (i in years) { # i = 2005
  
  # Subset the data for the current year and the Julian days 245 to 252
  subset_data <- climate_totem[climate_totem$year == i & 
                                 climate_totem$julian >= 214 & 
                                 climate_totem$julian <= 221, ]
  
  # Calculate the mean of the Tair_max column for this subset
  mean_Tair_max <- mean(subset_data$Tair_max, na.rm = TRUE)
  
  # Store the mean in the list with the year as the name
  means_per_year[[as.character(i)]] <- mean_Tair_max
}

# Convert the list to a data frame (if needed)
means_df <- data.frame(Year = names(means_per_year), Mean_Tair_Max = unlist(means_per_year))

mean(means_df$Mean_Tair_Max)

# View the result
print(means_df)

mean(max_sept_a$Tair_max)
print(max_sept_a)
dput(max_sept_a)
as.vector(max_sept_a)
