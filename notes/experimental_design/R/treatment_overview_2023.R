#*======================================================================================================
## Climate Chambers temperature data - Drought 1
##  
##
## Project:         PhaenoFlex
## Datum:           06.15.2023
## Autor:           Christophe 
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

# TREATMENTS SCHEDULE===========================================
schedule <- read_xlsx("raw_data/treatments_schedule.xlsx")
str(schedule)
# convert to numeric
# Convert selected columns to numeric
cols_to_convert <- c("dro_1_start", "dro_1_end", "dro_1_length", "dro_2_start", "dro_2_length", "heatwave_start", "defol_1_start", "defol_2_start")
for (col in cols_to_convert) {
  schedule[[col]] <- as.numeric(schedule[[col]])
}


#subset for all species
schedule_prvi <- subset(schedule, id =="Prvi")
# change to numeric
str(schedule_prvi)
schedule_prvi$dro_2_start <- as.numeric(schedule_prvi$dro_2_start)
schedule_prvi$dro_2_end <- as.numeric(schedule_prvi$dro_2_end)
schedule_prvi$dro_3_start <- as.numeric(schedule_prvi$dro_3_start)
schedule_prvi$dro_3_end <- as.numeric(schedule_prvi$dro_3_end)
#read excel file
cchamber1<-read.csv("raw_data/CC 1 2023-06-21 16_57_52 PDT (Data PDT).csv")
photoperiods<-read.csv("raw_data/dro_1_photoperiod.csv")

# Change format of time columns
photoperiods$light_on <- str_pad(photoperiods$light_on, width = 8, pad = "0")
photoperiods$light_off <- str_pad(photoperiods$light_off, width = 8, pad = "0")
photoperiods$high_temp_start <- str_pad(photoperiods$light_on, width = 8, pad = "0")
photoperiods$high_temp_end <- str_pad(photoperiods$light_off, width = 8, pad = "0")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# AMBIANT TEMPERATURE --> TOTEM FIELD OF 2022===================================
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# totem2022<- read.table("raw_data/TotemData_2022.dat", skip = 5, header = TRUE, sep = ",") [, c(1:4)]
# head(totem2022)
# # Change colnames
# colnames(totem2022) <- c("date_time", "temp", "rh", "precipitation")
# 
# # Add column for the date only
# totem2022$DOY <- as.POSIXlt(totem2022$date_time)
# # Add column for year
# totem2022$year <- format(totem2022$DOY, format = "%Y")
# # Add column for month
# totem2022$month <- format(totem2022$DOY, format = "%m")
# # Add column for Julian day
# totem2022$julian <- as.numeric(format(totem2022$DOY, "%j"))
# ##### Daily temperature values #####
# # 
# daily_totem_2022 <- totem2022 %>%
#   group_by(julian) %>%
#   summarise(meantemp = mean(temp),
#             mintemp = min(temp),
#             maxtemp = max(temp))
# str(daily_totem_2022)

# update of january 2024
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# AMBIANT TEMPERATURE --> TOTEM FIELD for 30 YEARS===================================
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
climate_totem <- read.csv2("raw_data/TotemField_30Years_cleaned.csv", header = TRUE, sep = ",")
str(climate_totem)
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

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# DROUGHT 1,2,3 --> CC CHAMBER 1==============================================
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
cchamber1<-read.csv("raw_data/CC 1 2023-06-21 16_57_52 PDT (Data PDT).csv")
colnames(cchamber1)<- c("Number","Date.time.pdt","Temperature", 'RH', 'Dew.point')

cchamber1 <- cchamber1[, -c(6, 7, 8)]
head(cchamber1)

# Standardize date format
cc1_stan <- cchamber1
cc1_stan$datetime <- as.POSIXlt(strptime(cc1_stan$Date.time.pdt, format = "%m-%d-%Y %H:%M:%S"))
str(cc1_stan)

# Create column with days only
cc1_stan$DOY <- format(cc1_stan$datetime, format = "%Y-%m-%d") # Change order (year first), so they can be further transformed to julian
head(cc1_stan)
str(cc1_stan)
# Create column with hours only back in the original file
cchamber1$Time <- format(cc1_stan$datetime, format = "%H:%M:%S")
head(cchamber1)

# Convert DOY as Date
cchamber1$DOY <- as.Date(cc1_stan$DOY)
str(cc1_stan)
# Create a new column to julian back in the original file
cchamber1$julian <- format(cchamber1$DOY, "%j")
head(cchamber1)

# Convert Time column to decimal
cchamber1$Time_decimal <- sapply(strsplit(cchamber1$Time, ":"), function(x) {
  hours <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  seconds <- as.numeric(x[3])
  
  decimal_time <- (hours + minutes/60 + seconds/3600) / 24
  return(decimal_time)
})

# Subset values for drought one for Prvi which is the species that stayed the longest
dro1 <- subset(cchamber1, julian >= schedule_prvi$dro_1_start & julian <= schedule_prvi$dro_1_end)
dro2 <- subset(cchamber1, julian >= schedule_prvi$dro_2_start & julian <= schedule_prvi$dro_2_end)
str(schedule_prvi)
# Create daily mean of when temperatures were set at 20 celcius
mean_low <- dro1 %>%
  group_by(julian) %>%
  summarize(temp = mean(Temperature[Time >= "00:00:00" & Time <= "05:20:00"], na.rm=TRUE))
mean_low
# Create daily mean of when temperatures were set at 30 celcius
mean_high <- dro1 %>%
  group_by(julian) %>%
  summarize(temp = mean(Temperature[Time >= "05:20:00" & Time <= "21:45:00"], na.rm=TRUE))
mean_high
# Create an object that corresponds to the decimal value of the mid time of each 
# interval mentionned previously, where 0.104 is about 02:40:00 and 0.583 is about 2 pm 
low<-as.numeric(0.111)
high<-as.numeric(0.583)
mean_low$julian_decim <- as.numeric(mean_low$julian) + low
mean_high$julian_decim <- as.numeric (mean_high$julian) + high

# Merge the 2 tables by adding the rows from mean_low underneath mean_high
dro_1_forplot <- mean_high %>% add_row(
  julian = mean_low$julian,
  temp = mean_low$temp,
  julian_decim = mean_low$julian_decim)
# Arrange them from julian_decim column
dro_1_forplot <- dro_1_forplot %>% arrange(julian_decim)

##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==
##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==
##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==

# Create a dummy data frame for drought 2, that will be changed at the end of all treatments
dro_2_forplot <- dro_1_forplot [c(1:28),]
dro_2_forplot$julian_decim <- dro_2_forplot$julian_decim+36
# Idem for drought 3
dro_3_forplot <- dro_1_forplot[c(1:28),]
dro_3_forplot$julian_decim <- dro_3_forplot$julian_decim+75


##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==
# FOLLOWING WILL BE FOR TABLE DATA, NOT FOR GRAPH.
##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==

# Subset for an example day that will be displayed in the graph
dro1_cc <- subset(cchamber1, julian >="146" & julian <= "146")
temp_rise <- photoperiods$high_temp_start[photoperiods$id == "week1"]
temp_down <- photoperiods$high_temp_end[photoperiods$id == "week1"]
# Create a column of up and down, where up is temperature rise and down is temperature down
dro1_cc$timestatus <- ifelse(dro1_cc$Time >= temp_rise & dro1_cc$Time <= temp_down, "up", "down")
# Calculate the mean of temperature up
dro1_mean_up <- mean(dro1_cc$Temperature[dro1_cc$timestatus == "up"], na.rm = TRUE)
# Calculate the mean of temperature down
dro1_mean_down <- mean(dro1_cc$Temperature[dro1_cc$timestatus == "down"], na.rm = TRUE)
# Calculate the standard deviation of temperature up
dro1_sd_up <- sd(dro1_cc$Temperature[dro1_cc$timestatus == "up"], na.rm = TRUE)
# Calculate the standard deviation of temperature down
dro1_sd_down <- sd(dro1_cc$Temperature[dro1_cc$timestatus == "down"], na.rm = TRUE)
dro1_mean_up
dro1_mean_down
dro1_sd_up
dro1_sd_down

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# DORMANCY PERIOD --> CC WALK IN================================================
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Load csv
ccwalkin<-read.csv("raw_data/CC walk in 2023-06-27 09_35_13 PDT (Data PDT).csv")
# Change colnames
colnames(ccwalkin)<- c("Number","Date.time.pdt","Temperature", 'RH', 'Dew.point')
ccwalkin <- ccwalkin[,-c(6,7,8)]
head(ccwalkin)
# Standardize date format
ccwi_stan <- ccwalkin
ccwi_stan$datetime <- strptime(ccwi_stan$Date.time.pdt, format = "%m-%d-%Y %H:%M:%S")
head(ccwi_stan)
# Create column with days only
ccwi_stan$DOY <- format(ccwi_stan$datetime, format = "%Y-%m-%d") # Change order (year first), so they can be further transformed to julian
head(ccwi_stan)
# Create column with hours only back in original file
ccwalkin$Time <- format(ccwi_stan$datetime, format = "%H:%M:%S")
head(ccwi_stan)
# Convert DOY as Date
ccwalkin$DOY <- as.Date(ccwi_stan$DOY)
# Create new column to julian back in original file
ccwalkin$julian <- format(ccwalkin$DOY, "%j")
head(ccwalkin)

# Subset interval that plants were in extended dormancy period
dormancy <- subset(ccwalkin, julian >="077" & julian <= "121") 

# Should be further changed, since I don't have the date that they were put in there
# Create daily mean of when temperatures were set at lowest temp
dormancy_mean_low <- dormancy %>%
  group_by(julian) %>%
  summarize(temp = mean(Temperature[Time >= "00:00:00" & Time <= "05:20:00"], na.rm=TRUE))
dormancy_mean_low
# Create daily mean of when temperatures were set at highest temp
dormancy_mean_high <- dormancy %>%
  group_by(julian) %>%
  summarize(temp = mean(Temperature[Time >= "05:20:00" & Time <= "21:45:00"], na.rm=TRUE))
dormancy_mean_high
# Time identical to the period of drought treatments (See above)
dormancy_low<-as.numeric(0.111)
dormancy_high<-as.numeric(0.583)

dormancy_mean_low$julian_decim <- as.numeric(dormancy_mean_low$julian) + dormancy_low
dormancy_mean_high$julian_decim <- as.numeric (dormancy_mean_high$julian) + dormancy_high
dormancy_mean_high
dormancy_mean_low
# Merge the 2 tables by adding the rows from mean_low underneath mean_high
dormancy_forplot <- dormancy_mean_high %>% add_row(
  julian = dormancy_mean_low$julian,
  temp = dormancy_mean_low$temp,
  julian_decim = dormancy_mean_low$julian_decim)
# Arrange them from julian_decim column
dormancy_forplot <- dormancy_forplot %>% arrange(julian_decim)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# DEFOLIATION 1 --> ???=========================================================
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# HEAT WAVE --> CC WALK IN======================================================
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### Data already cleaned at Dormancy step

# Subset for an example day that will be displayed in the graph
heatwave_wi <- subset(ccwalkin, julian >= schedule_prvi$heatwave_start & julian <= 188) # modify when the end of the treatment will be known
# Create daily mean of when temperatures were set at lowest temp
heatwave_mean_low <- heatwave_wi %>%
  group_by(julian) %>%
  summarize(temp = mean(Temperature[Time >= "00:00:00" & Time <= "05:20:00"], na.rm=TRUE))
# Add dummy values temporarily just so it can be displayed in graph
heatwave_mean_low_added_dates <- heatwave_mean_low %>% add_row(
  julian = as.character(c(179:184)),
  temp= c(30.1, 29.9, 29.8, 29.7, 30.2, 29.7)
)
# Create daily mean of when temperatures were set at highest temp
heatwave_mean_high <- heatwave_wi %>%
  group_by(julian) %>%
  summarize(temp = mean(Temperature[Time >= "05:20:00" & Time <= "21:45:00"], na.rm=TRUE))
heatwave_mean_high_added_dates <- heatwave_mean_high %>% add_row(
  julian = as.character(c(179:184)),
  temp= c(35, 36, 34, 35.5, 36.1, 35.6)
)
# Time identical to the period of drought treatments (See above)
heat_low<-as.numeric(0.111)
heat_high<-as.numeric(0.583)

heatwave_mean_low_added_dates$julian_decim <- as.numeric(heatwave_mean_low_added_dates$julian) + heat_low
heatwave_mean_high_added_dates$julian_decim <- as.numeric (heatwave_mean_high_added_dates$julian) + heat_high
dormancy_mean_high
dormancy_mean_low
# Merge the 2 tables by adding the rows from mean_low underneath mean_high
heat_forplot <- heatwave_mean_high_added_dates %>% add_row(
  julian = heatwave_mean_low_added_dates$julian,
  temp = heatwave_mean_low_added_dates$temp,
  julian_decim = heatwave_mean_low_added_dates$julian_decim)
# Arrange them from julian_decim column
heat_forplot <- heat_forplot %>% arrange(julian_decim)

##### CREATE A DF FOR DAYLENGTH
daylength_df <- tibble(julian = c(1:365), daylength = daylength(49.32, c(1:365)))
head(daylength_df)
# recheck the data, it might be wrong

#*======================================================================================================
############ PLOT ##############
#*======================================================================================================


#### Merge daylength and temperature
# select columns from climatetotem
merged_temp_daylength <- merge(mean_30years, daylength_df[, c("daylength", "julian")], by = "julian", all = TRUE)
head(merged_temp_daylength)

str(daylength_df)
list(daylength_df)
# Add column MM-DD
# merged_temp_daylength$Date.Time <- as.POSIXlt(merged_temp_daylength$Date.Time)
# merged_temp_daylength$time_MM_DD <- format(merged_temp_daylength$Date.Time, "%m-%d")
# head(merged_temp_daylength)


### GRAPH ###
tempgraph <- ggplot(data = merged_temp_daylength, aes(x = julian, y = meantemp)) +
  geom_ribbon(aes(ymin = mintemp, ymax = maxtemp),
              alpha = 0.1, na.rm = TRUE) +
  # Add Day length line --> adjust min and max values as it's set right now from doy 100 to 273
  # It needs to be scaled to the temperature axis
  geom_line(aes(y = daylength*1.9), size = 0.5, colour = "purple") +
  # Add scales for each Y-axis
  scale_y_continuous(
    name = "Temperature (°C)",
    limits = c(0, NA),
    sec.axis = sec_axis(~./1.9, name = "Daylength (hours)")
  ) +
  # Add Dormancy 
  geom_line(data=dormancy_forplot, aes(julian_decim, temp), size=0.5, colour="blue") +
  geom_text(x=schedule_prvi$dormancy_start,
            y=dormancy_forplot$temp[dormancy_forplot$julian == schedule_prvi$dormancy_end][1],
            label = "Coolling chambers",
            hjust = 0, #adjust the position of text on the x axis
            vjust = 1.1, #adjust the position of text on the y axis
            size = 3) +
    # Add arrow for dormancy removal
  geom_segment(x=schedule_prvi$dormancy_end, 
               y=25, 
               xend=schedule_prvi$dormancy_end, 
               yend=(merged_temp_daylength$meantemp[merged_temp_daylength$julian == schedule_prvi$dormancy_end])+1, colour="black", #go extract the value of defol 1 start
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text(x = schedule_prvi$dormancy_end, 
            y = 13,
            label = "Transfer to ambient conditions",
            hjust = 0, 
            vjust = -0.5,
            angle = 90,
            size = 3) + # adjust the text position on the x axis
  # Add line for harvest period
geom_segment(
    x = schedule_prvi$harvest_start,
    y = 20,
    xend = schedule_prvi$harvest_end,
    yend = 20,
    lineend = "butt",
    colour = "black"  
  ) +
  # vertical lines at beginning of harvest line
  geom_segment(
    x = schedule_prvi$harvest_start,
    y = 19.5,
    xend = schedule_prvi$harvest_start,
    yend = 20.5 
  ) +
  # vertical lines at end of line harvest 
  geom_segment(
    x = schedule_prvi$harvest_end,
    y = 19.5,
    xend = schedule_prvi$harvest_end,
    yend = 20.5
  ) +
  # text for harvest period
  geom_text(x = schedule_prvi$harvest_start, 
            y = 21,
            label = "Harvest period",
            hjust = 0,
            vjust = 0.5,
            size = 3) + 
  # Add Drought 1
  geom_line(data=dro_1_forplot, aes(julian_decim, temp, colour="Drought 1"), size=0.5) +
  # Add Defol 1
  geom_segment(x=schedule_prvi$defol_1_start, 
               y=8, 
               xend=schedule_prvi$defol_1_start, 
               yend=(merged_temp_daylength$meantemp[merged_temp_daylength$julian == schedule_prvi$defol_1_start])-1, aes(colour="Defoliation 1"), #go extract the value of defol 1 start
               arrow = arrow(length = unit(0.02, "npc"))) +
  # Add Drought 2
  geom_line(data=dro_2_forplot, aes(julian_decim, temp, colour="Drought 2"), size=0.5)+
  # Add Heat Wave
  geom_line(data=heat_forplot, aes(julian_decim, temp, colour="Heat Wave"), size=0.5)+
  # Add Defol 2
  geom_segment(x=schedule_prvi$defol_2_start, 
               y=8, 
               xend=schedule_prvi$defol_2_start, 
               yend=(merged_temp_daylength$meantemp[merged_temp_daylength$julian == schedule_prvi$defol_2_start])-0.5, aes(colour="Defoliation 2"), #go extract the value of defol 2 start
               arrow = arrow(length = unit(0.02, "npc")))+
  # Add Drought 3
  geom_line(data=dro_3_forplot, aes(julian_decim, temp, colour="Drought 3"), size=0.5) +
  # Add Defol 3
  geom_segment(x=schedule_prvi$dro_3_start, 
               y=8, 
               xend=schedule_prvi$dro_3_start, 
               yend=(merged_temp_daylength$meantemp[merged_temp_daylength$julian == schedule_prvi$defol_3_start])-0.5, aes(colour="Defoliation 3"), #go extract the value of defol 2 start
               arrow = arrow(length = unit(0.02, "npc"))) +
  #Set titles
  ggtitle("") +
  scale_color_manual(name = "Treatments", values = c("Drought 1" = "#fbd808",
                                                 "Drought 2" = "#ff9005",
                                                 "Drought 3" = "#f9530b",
                                                 "Defoliation 1" = "#abc32f",
                                                 "Defoliation 2" = "#809c13",
                                                 "Defoliation 3" = "#607c3c",
                                                 "Heat Wave" = "#ff0000")) +
  # Add mean temperature of the 2022 Totem daily temperatures 
  geom_line()+
  labs(x = "Julian days (2023)", y = "Mean temperature (°C)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.8),
        legend.background = element_rect(fill="white", 
                                         linewidth =0.5, linetype="solid"),
        legend.text=element_text(size=9),
        legend.key.size = unit(0.6, "cm"),
        axis.ticks = element_line()) +
  # scale x axis with the breaks and sequence I want. Also delete the white gap
  scale_x_continuous(breaks = seq(0, 365, by = 30), limits = c(0, 365), expand = c(0, 0)) 
tempgraph
# Save graph in working directory
ggsave("output/tempgraph.pdf", tempgraph, dpi = 300, width = 9, height = 7)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
