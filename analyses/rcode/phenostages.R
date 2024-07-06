#=== === === === === === === === === === === === === === === === === === === ===
## Title:           Phenostages FUELINEX
##  
##
## Project:         FUELINEX 
## Date:            May 20, 2024
## Autor:           Christophe 
#=== === === === === === === === === === === === === === === === === === === ===
# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(readxl)
library(ggplot2)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder 
directory <-"/Users/christophe_rouleau-desrochers/Documents/github/fuelinex/"
setwd(directory)
list.files()

# Read data
phenostages <- read.csv2("data/phenological_monitoring/phenological_monitoring.csv", header = TRUE, sep = ",", check.names = FALSE)
head(phenostages)
str(phenostages)

# Function to convert all doy columns to numeric without having tons of warnings: 
#source: https://stackoverflow.com/questions/32846176/applying-as-numeric-only-to-elements-of-a-list-that-can-be-coerced-to-numeric-i
convert_to_numeric <- function(x) {
  as.numeric(ifelse(grepl("^[0-9]+$", x), x, NA))
}
# Convert to numeric
for (i in 7:ncol(phenostages)) {
  phenostages[, i] <- convert_to_numeric(phenostages[, i])
}

#### ACNE ####
acne <- subset(phenostages, genus =="acer")
head(acne)
# Select columns
acne.sel <- acne[, c(1, 3, 7:ncol(acne))]
head(acne.sel)
#Convert to long format
acne.long <- melt(setDT(acne.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
acne.mean <- aggregate(value ~ treatment + doy, data = acne.long, FUN = mean, na.omit =TRUE )
head(acne.mean)
# Quick plot
acne.mean.plot <- ggplot(acne.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("Acne total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
acne.mean.plot

# only for spring or fall
acne.sf <- acne.sel
head(acne.sf)
acne.sf$springtreatment <- ifelse(grepl('^CoolS', acne.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment spcified by >length(colnames(acne.sf)) is at second position and that all days are included 
acne.sf <- acne.sf[, c(1, 27, 3:12)]
head(acne.sf)
#Convert to long format
acne.long <- melt(setDT(acne.sf), id.vars = c("tree_ID","springtreatment"), variable.name = "doy")
head(acne.long)
# Mean of treatments + doy
acne.mean <- aggregate(value ~ springtreatment + doy, data = acne.long, FUN = mean, na.omit =TRUE )
# Standard deviation of treatments + doy
acne.sd <- aggregate(value ~ springtreatment + doy, data = acne.long, FUN = sd)
# Merge dfs mean and sd
acne.merged <- merge(acne.mean, acne.sd, by=c("springtreatment","doy"))
# change colnames
colnames(acne.merged) <- c("springtreatment", "doy", "mean", "sd")
head(acne.merged)

head(acne.long)
ggplot(acne.merged, aes (x=doy, y=mean)) + 
  geom_bar(stat = "identity", position="dodge")



#### QUMA ####
quma <- subset(phenostages, genus =="quercus")
head(quma)
str(quma)
# Select columns
quma.sel <- quma[, c(1, 3, 7:ncol(quma))]
head(quma.sel)
#Convert to long format
quma.long <- melt(setDT(quma.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
quma.mean <- aggregate(value ~ treatment + doy, data = quma.long, FUN = mean, na.omit =TRUE )
head(quma.mean)
# Quick plot
quma.mean.plot <- ggplot(quma.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("quma total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
quma.mean.plot

# only for spring or fall 
quma.sf <- quma.sel
head(quma.sf)
quma.sf$springtreatment <- ifelse(grepl('^CoolS', quma.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment spcified by >length(colnames(quma.sf)) is at second position and that all days are included 
quma.sf <- quma.sf[, c(1, 27, 3:24)]
head(quma.sf)
#Convert to long format
quma.long <- melt(setDT(quma.sf), id.vars = c("tree_ID","springtreatment"), variable.name = "doy")
head(quma.long)
# Mean of treatments + doy
quma.mean <- aggregate(value ~ springtreatment + doy, data = quma.long, FUN = mean, na.omit =TRUE )
# Min of treatments + doy
quma.min <- aggregate(value ~ springtreatment + doy, data = quma.long, FUN = min, na.omit =TRUE )
# Max of treatments + doy
quma.max <- aggregate(value ~ springtreatment + doy, data = quma.long, FUN = max, na.omit =TRUE )
# Standard deviation of treatments + doy
quma.sd <- aggregate(value ~ springtreatment + doy, data = quma.long, FUN = sd)
# Merge dfs mean and min and max
quma.merged <- merge(quma.mean, quma.min, by=c("springtreatment","doy"))

quma.merged <- merge(quma.merged, quma.max, by=c("springtreatment","doy"))
head(quma.merged)

# change colnames
colnames(quma.merged) <- c("springtreatment", "doy", "mean", "min", "max")
head(quma.merged)
# Nuage de points
quma.plot <- ggplot(quma.merged)+
  geom_point(aes(x=doy, y=mean, color=springtreatment)) +
  geom_ribbon(aes(ymin=min, ymax=max))
quma.plot
str(quma.merged)
tmp<-subset(quma.merged, springtreatment == "CoolS")

ggplot() +
  geom_point(data=tmp, aes(x = doy, y = mean))+
  geom_line(na.rm=TRUE) +
  geom_ribbon(aes(x = doy, ymin = min, ymax = max)) +
  theme_minimal()

head(tmp)
ggplot()+
  geom_line(data=tmp, aes(x=doy, y=mean))

#cool spring subset
