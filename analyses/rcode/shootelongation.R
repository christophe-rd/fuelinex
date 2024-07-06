#=== === === === === === === === === === === === === === === === === === === ===
## TITLE:           Shoot elongation
##  
##
## Project:         FUELINEX
## Date:            2024-06-25
## Autor:           Christophe 
#=== === === === === === === === === === === === === === === === === === === ===

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder  
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github/fuelinex/"

# Set Working Directory
setwd(directory_path)

# Load librairies
library(readxl)
library(ggplot2)
library(data.table)

# Read csv
shootelongation <- read.csv2("analyses/input/shoot_elongation.csv", header = TRUE, sep = ",", check.names = FALSE)
head(shootelongation)

# Convert all chr values that should be numeric, numeric
for (i in 7:length(colnames(shootelongation))) {
  shootelongation[,i]<-as.numeric(shootelongation[,i])
}

#=== === === === === === === === === === === === === === === === === === === ===
#### ACNE #####
#=== === === === === === === === === === === === === === === === === === === ===

# Subset for Acne
acne <- subset(shootelongation, genus == "acer")

# Select essential column 
acne.sel <- acne[, c(1, 3, 7,11, 13:18)]
head(acne.sel)


##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
acne.absolute <- acne.sel
acne.absolute[3:ncol(acne.absolute)] <- acne.absolute[3:ncol(acne.absolute)]-acne.absolute[,3]

# Rotate table ABSOLUTE
acne.long.absolute <- melt(setDT(acne.absolute), id.vars = c("tree_ID","treatment"), variable.name = "doy")
# Mean of treatments + doy
acne.long.absolute.mean <- aggregate(value ~ treatment + doy, data = acne.long.absolute, FUN = mean, na.omit =TRUE )
# Quick plot for ABSOLUTE
acne.long.absolute.mean.plot <- ggplot(acne.long.absolute.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("Acne absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
acne.long.absolute.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
acne.long.total <- melt(setDT(acne.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
acne.long.total.mean <- aggregate(value ~ treatment + doy, data = acne.long.total, FUN = mean, na.omit =TRUE )

# Quick plot for TOTAL
acne.long.total.mean.plot <- ggplot(acne.long.total.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("Acne total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
acne.long.total.mean.plot


#=== === === === === === === === === === === === === === === === === === === ===
#### BEPA #####
#=== === === === === === === === === === === === === === === === === === === ===

# Subset for bepa
bepa <- subset(shootelongation, genus == "betula")

# Select essential column 
bepa.sel <- bepa[, c(1, 3, 8, 11, 13:length(colnames(bepa)))]
str(bepa.sel)
##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
bepa.absolute <- bepa.sel
bepa.absolute[3:ncol(bepa.absolute)] <- bepa.absolute[3:ncol(bepa.absolute)]-bepa.absolute[,3]

# Rotate table ABSOLUTE
bepa.long.absolute <- melt(setDT(bepa.absolute), id.vars = c("tree_ID","treatment"), variable.name = "doy")
# Mean of treatments + doy
bepa.long.absolute.mean <- aggregate(value ~ treatment + doy, data = bepa.long.absolute, FUN = mean, na.omit =TRUE )

# Quick plot for ABSOLUTE
bepa.long.absolute.mean.plot <- ggplot(bepa.long.absolute.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("Bepa absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
bepa.long.absolute.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
bepa.long.total <- melt(setDT(bepa.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
bepa.long.total.mean <- aggregate(value ~ treatment + doy, data = bepa.long.total, FUN = mean, na.omit =TRUE )

# Quick plot for TOTAL
bepa.long.total.mean.plot <- ggplot(bepa.long.total.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("Bepa total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
bepa.long.total.mean.plot


#=== === === === === === === === === === === === === === === === === === === ===
#### PIST #####
#=== === === === === === === === === === === === === === === === === === === ===

# Subset for pist
pist <- subset(shootelongation, genus == "pinus")

# Select essential column 
pist.sel <- pist[, c(1, 3, 9, 11, 13:length(colnames(pist)))]
head(pist.sel)

##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
pist.absolute <- pist.sel
pist.absolute[3:ncol(pist.absolute)] <- pist.absolute[3:ncol(pist.absolute)]-pist.absolute[,3]

# Rotate table ABSOLUTE
pist.long.absolute <- melt(setDT(pist.absolute), id.vars = c("tree_ID","treatment"), variable.name = "doy")
# Mean of treatments + doy
pist.long.absolute.mean <- aggregate(value ~ treatment + doy, data = pist.long.absolute, FUN = mean, na.omit =TRUE )

# Quick plot for ABSOLUTE
pist.long.absolute.mean.plot <- ggplot(pist.long.absolute.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("pist absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
pist.long.absolute.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
pist.long.total <- melt(setDT(pist.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
pist.long.total.mean <- aggregate(value ~ treatment + doy, data = pist.long.total, FUN = mean, na.omit =TRUE )

# Quick plot for TOTAL
pist.long.total.mean.plot <- ggplot(pist.long.total.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("pist total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
pist.long.total.mean.plot


#=== === === === === === === === === === === === === === === === === === === ===
#### POBA #####
#=== === === === === === === === === === === === === === === === === === === ===
# Subset for poba
poba <- subset(shootelongation, genus == "populus")

# Select essential column 
poba.sel <- poba[, c(1, 3, 9, 11, 13:length(colnames(poba)))]

##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
poba.absolute <- poba.sel
poba.absolute[3:ncol(poba.absolute)] <- poba.absolute[3:ncol(poba.absolute)]-poba.absolute[,3]

# Rotate table ABSOLUTE
poba.long.absolute <- melt(setDT(poba.absolute), id.vars = c("tree_ID","treatment"), variable.name = "doy")
# Mean of treatments + doy
poba.long.absolute.mean <- aggregate(value ~ treatment + doy, data = poba.long.absolute, FUN = mean, na.omit =TRUE )

# Quick plot for ABSOLUTE
poba.long.absolute.mean.plot <- ggplot(poba.long.absolute.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("poba absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
poba.long.absolute.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
poba.long.total <- melt(setDT(poba.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
poba.long.total.mean <- aggregate(value ~ treatment + doy, data = poba.long.total, FUN = mean, na.omit =TRUE )

# Quick plot for TOTAL
poba.long.total.mean.plot <- ggplot(poba.long.total.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("poba total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
poba.long.total.mean.plot


#=== === === === === === === === === === === === === === === === === === === ===
#### PRVI #####
#=== === === === === === === === === === === === === === === === === === === ===

# Subset for prvi
prvi <- subset(shootelongation, genus == "prunus")

# Select essential column 
prvi.sel <- prvi[, c(1, 3, 8, 11, 13:length(colnames(prvi)))]
head(prvi.sel)
##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
prvi.absolute <- prvi.sel
prvi.absolute[3:ncol(prvi.absolute)] <- prvi.absolute[3:ncol(prvi.absolute)]-prvi.absolute[,3]

# Rotate table ABSOLUTE
prvi.long.absolute <- melt(setDT(prvi.absolute), id.vars = c("tree_ID","treatment"), variable.name = "doy")
# Mean of treatments + doy
prvi.long.absolute.mean <- aggregate(value ~ treatment + doy, data = prvi.long.absolute, FUN = mean, na.omit =TRUE )

# Quick plot for ABSOLUTE
prvi.long.absolute.mean.plot <- ggplot(prvi.long.absolute.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("prvi absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
prvi.long.absolute.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
prvi.long.total <- melt(setDT(prvi.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
prvi.long.total.mean <- aggregate(value ~ treatment + doy, data = prvi.long.total, FUN = mean, na.omit =TRUE )

# Quick plot for TOTAL
prvi.long.total.mean.plot <- ggplot(prvi.long.total.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("prvi total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
prvi.long.total.mean.plot


#=== === === === === === === === === === === === === === === === === === === ===
#### QUMA #####
#=== === === === === === === === === === === === === === === === === === === ===

# Subset for quma
quma <- subset(shootelongation, genus == "quercus")

# Select essential column 
quma.sel <- quma[, c(1, 3, 10, 12:length(colnames(quma)))]
head(quma.sel)

##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
quma.absolute <- quma.sel
quma.absolute[3:ncol(quma.absolute)] <- quma.absolute[3:ncol(quma.absolute)]-quma.absolute[,3]

# Rotate table ABSOLUTE
quma.long.absolute <- melt(setDT(quma.absolute), id.vars = c("tree_ID","treatment"), variable.name = "doy")
# Mean of treatments + doy
quma.long.absolute.mean <- aggregate(value ~ treatment + doy, data = quma.long.absolute, FUN = mean, na.omit =TRUE )

# Quick plot for ABSOLUTE
quma.long.absolute.mean.plot <- ggplot(quma.long.absolute.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("quma absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
quma.long.absolute.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
quma.long.total <- melt(setDT(quma.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
quma.long.total.mean <- aggregate(value ~ treatment + doy, data = quma.long.total, FUN = mean, na.omit =TRUE )

# Quick plot for TOTAL
quma.long.total.mean.plot <- ggplot(quma.long.total.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("quma total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
quma.long.total.mean.plot


#=== === === === === === === === === === === === === === === === === === === ===
#### SEGI #####
#=== === === === === === === === === === === === === === === === === === === ===

# Subset for segi
segi <- subset(shootelongation, genus == "sequoiadendron")

# Select essential column 
segi.sel <- segi[, c(1, 3, 10, 12, 13:length(colnames(segi)))]
head(segi.sel)


##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
segi.absolute <- segi.sel
segi.absolute[3:ncol(segi.absolute)] <- segi.absolute[3:ncol(segi.absolute)]-segi.absolute[,3]

# Rotate table ABSOLUTE
segi.long.absolute <- melt(setDT(segi.absolute), id.vars = c("tree_ID","treatment"), variable.name = "doy")
# Mean of treatments + doy
segi.long.absolute.mean <- aggregate(value ~ treatment + doy, data = segi.long.absolute, FUN = mean, na.omit =TRUE )

# Quick plot for ABSOLUTE
segi.long.absolute.mean.plot <- ggplot(segi.long.absolute.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("segi absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
segi.long.absolute.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
segi.long.total <- melt(setDT(segi.sel), id.vars = c("tree_ID","treatment"), variable.name = "doy")

# Mean of treatments + doy
segi.long.total.mean <- aggregate(value ~ treatment + doy, data = segi.long.total, FUN = mean, na.omit =TRUE )

# Quick plot for TOTAL
segi.long.total.mean.plot <- ggplot(segi.long.total.mean)+
  geom_point(aes (x=doy, y=value, color = treatment)) +
  labs(x="", y="")+
  ggtitle("segi total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
segi.long.total.mean.plot


#### AGREGATE BY WARM/COOL SPRING ####

##### ACNE #####
head(acne)
# Select springtreatment column and place it after id
acne.sf <- acne[, c(1, 3, 7,11, 13:length(colnames(acne)))]
# Add a column with only the spring treatment
acne.sf$springtreatment <- ifelse(grepl('^CoolS', acne.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment spcified by >length(colnames(acne.sf)) is at second position and that all days are included 
acne.sf <- acne.sf[, c(1, length(colnames(acne.sf)), (4:length(colnames(acne.sf))-1))]

##### Absolute shoot elongation ##### 
# Substract each column by the first measurement
acne.absolute.sf <- acne.sf
acne.absolute.sf[3:ncol(acne.absolute.sf)] <- acne.absolute.sf[3:ncol(acne.absolute.sf)]-acne.absolute.sf[,3]

# Rotate table ABSOLUTE
acne.long.absolute.sf <- melt(setDT(acne.absolute.sf), id.vars = c("tree_ID","springtreatment"), variable.name = "doy")
# Mean of treatments + doy
acne.long.absolute.sf.mean <- aggregate(value ~ springtreatment + doy, data = acne.long.absolute.sf, FUN = mean, na.omit =TRUE )

# Min of treatments + doy
acne.long.absolute.sf.min <- aggregate(value ~ springtreatment + doy, data = acne.long.absolute.sf, FUN = min, na.omit =TRUE )

# Max of treatments + doy
acne.long.absolute.sf.max <- aggregate(value ~ springtreatment + doy, data = acne.long.absolute.sf, FUN = max, na.omit =TRUE )

# sd 
acne.long.absolute.sf.sd <- aggregate(value ~ springtreatment + doy, data = acne.long.absolute.sf, FUN = sd)

#merge
acne.abs.merge <- merge(acne.long.absolute.sf.mean, acne.long.absolute.sf.sd, by=c("springtreatment","doy"))
# acne.abs.merge2 <- merge(acne.abs.merge, acne.long.absolute.sf.max, by=c("springtreatment","doy"))
colnames(acne.abs.merge) <- c("springtreatment", "doy", "mean", "sd")

# to numeric doy
acne.abs.merge2$doy <- as.numeric(as.character(acne.abs.merge2$doy))
str(acne.abs.merge2)
#subset cools 
acnecools <- subset(acne.abs.merge, springtreatment == "CoolS")
acnecools$doy <- as.numeric(as.character(acnecools$doy))
head(acnecools)
# Quick plot for absolute cools
acne.long.absolute.sf.mean.plot <- ggplot(acnecools)+
  geom_point(aes (x=doy, y=mean)) +
  geom_ribbon(aes(x=doy, ymin=mean-sd, ymax=mean+sd), alpha=0.2)
  labs(x="DOY", y="CM")+
  ggtitle("Acne absolute shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
acne.long.absolute.sf.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
acne.long.total.sf <- melt(setDT(acne.sf), id.vars = c("tree_ID","springtreatment"), variable.name = "doy")
# Mean of treatments + doy
acne.long.total.sf.mean <- aggregate(value ~ springtreatment + doy, data = acne.long.total.sf, FUN = mean, na.omit =TRUE )
#sd
acne.long.total.sf.sd <- aggregate(value ~ springtreatment + doy, data = acne.long.total.sf, FUN = sd )

#merge
acne.total.merge <- merge(acne.long.total.sf.mean, acne.long.total.sf.sd, by=c("springtreatment","doy"))
# acne.abs.merge2 <- merge(acne.abs.merge, acne.long.absolute.sf.max, by=c("springtreatment","doy"))
colnames(acne.total.merge) <- c("springtreatment", "doy", "mean", "sd")

# to numeric doy
acne.total.merge$doy <- as.numeric(as.character(acne.total.merge$doy))
str(acne.total.merge)
#subset cools 
# acnecools <- subset(acne.abs.merge, springtreatment == "CoolS")
# acnecools$doy <- as.numeric(as.character(acnecools$doy))
# head(acnecools)
# Quick plot for TOTAL
acne.total.merge.plot<-ggplot(acne.total.merge)+
  geom_point(aes (x=doy, y=mean, color = springtreatment)) +
  geom_ribbon(aes(x=doy, ymin=mean-sd, ymax=mean+sd, color=springtreatment), alpha=0.2)+
  labs(x="DOY", y="CM")+
  ggtitle("Acne total shoot elongation")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
acne.total.merge.plot



##### BEPA #####

# Select springtreatment column and place it after id
bepa.sf <- bepa[, c(1, 3, 8, 11, 13:length(colnames(bepa)))]
head(bepa.sf)
# Add a column with only the spring treatment
bepa.sf$springtreatment <- ifelse(grepl('^CoolS', bepa.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment specified by >length(colnames(bepa.sf)) is at second position and that all days are included 
bepa.sf <- bepa.sf[, c(1, length(colnames(bepa.sf)), (4:length(colnames(bepa.sf))-1))]
head(bepa.sf)
##### Absolute shoot elongation ##### 
# Subtract each column by the first measurement
bepa.absolute.sf <- bepa.sf
bepa.absolute.sf[3:ncol(bepa.absolute.sf)] <- bepa.absolute.sf[3:ncol(bepa.absolute.sf)] - bepa.absolute.sf[, 3]

# Rotate table ABSOLUTE
bepa.long.absolute.sf <- melt(setDT(bepa.absolute.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
bepa.long.absolute.sf.mean <- aggregate(value ~ springtreatment + doy, data = bepa.long.absolute.sf, FUN = mean, na.rm = TRUE)

# Quick plot for absolute
bepa.long.absolute.sf.mean.plot <- ggplot(bepa.long.absolute.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Bepa absolute shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
bepa.long.absolute.sf.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
bepa.long.total.sf <- melt(setDT(bepa.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
bepa.long.total.sf.mean <- aggregate(value ~ springtreatment + doy, data = bepa.long.total.sf, FUN = mean, na.rm = TRUE)

# Quick plot for TOTAL
bepa.long.total.sf.mean.plot <- ggplot(bepa.long.total.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Bepa total shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
bepa.long.total.sf.mean.plot


##### PIST #####
head(pist)
# Select springtreatment column and place it after id
pist.sf <- pist[, c(1, 3, 9, 11, 13:length(colnames(pist)))]
# Add a column with only the spring treatment
pist.sf$springtreatment <- ifelse(grepl('^CoolS', pist.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment specified by >length(colnames(pist.sf)) is at second position and that all days are included 
pist.sf <- pist.sf[, c(1, length(colnames(pist.sf)), (4:length(colnames(pist.sf))-1))]
str(pist.sf)
##### Absolute shoot elongation ##### 
# Subtract each column by the first measurement
pist.absolute.sf <- pist.sf
pist.absolute.sf[3:ncol(pist.absolute.sf)] <- pist.absolute.sf[3:ncol(pist.absolute.sf)] - pist.absolute.sf[, 3]

# Rotate table ABSOLUTE
pist.long.absolute.sf <- melt(setDT(pist.absolute.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
pist.long.absolute.sf.mean <- aggregate(value ~ springtreatment + doy, data = pist.long.absolute.sf, FUN = mean, na.rm = TRUE)

# Quick plot for absolute
pist.long.absolute.sf.mean.plot <- ggplot(pist.long.absolute.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Pist absolute shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
pist.long.absolute.sf.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
pist.long.total.sf <- melt(setDT(pist.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
pist.long.total.sf.mean <- aggregate(value ~ springtreatment + doy, data = pist.long.total.sf, FUN = mean, na.rm = TRUE)

# Quick plot for TOTAL
pist.long.total.sf.mean.plot <- ggplot(pist.long.total.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Pist total shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
pist.long.total.sf.mean.plot


##### POBA #####
head(poba)
# Select springtreatment column and place it after id
poba.sf <- poba[, c(1, 3, 9, 11, 13:length(colnames(poba)))]
# Add a column with only the spring treatment
poba.sf$springtreatment <- ifelse(grepl('^CoolS', poba.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment specified by >length(colnames(poba.sf)) is at second position and that all days are included 
poba.sf <- poba.sf[, c(1, length(colnames(poba.sf)), (4:length(colnames(poba.sf))-1))]

##### Absolute shoot elongation ##### 
# Subtract each column by the first measurement
poba.absolute.sf <- poba.sf
poba.absolute.sf[3:ncol(poba.absolute.sf)] <- poba.absolute.sf[3:ncol(poba.absolute.sf)] - poba.absolute.sf[, 3]

# Rotate table ABSOLUTE
poba.long.absolute.sf <- melt(setDT(poba.absolute.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
poba.long.absolute.sf.mean <- aggregate(value ~ springtreatment + doy, data = poba.long.absolute.sf, FUN = mean, na.rm = TRUE)

# Quick plot for absolute
poba.long.absolute.sf.mean.plot <- ggplot(poba.long.absolute.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Poba absolute shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
poba.long.absolute.sf.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
poba.long.total.sf <- melt(setDT(poba.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
poba.long.total.sf.mean <- aggregate(value ~ springtreatment + doy, data = poba.long.total.sf, FUN = mean, na.rm = TRUE)

# Quick plot for TOTAL
poba.long.total.sf.mean.plot <- ggplot(poba.long.total.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Poba total shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
poba.long.total.sf.mean.plot


##### prvi #####
head(prvi)
# Select springtreatment column and place it after id
prvi.sf <- prvi[, c(1, 3, 9, 11, 13:length(colnames(prvi)))]
# Add a column with only the spring treatment
prvi.sf$springtreatment <- ifelse(grepl('^CoolS', prvi.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment specified by >length(colnames(prvi.sf)) is at second position and that all days are included 
prvi.sf <- prvi.sf[, c(1, length(colnames(prvi.sf)), (4:length(colnames(prvi.sf))-1))]

##### Absolute shoot elongation ##### 
# Subtract each column by the first measurement
prvi.absolute.sf <- prvi.sf
head(prvi.sf)
prvi.absolute.sf[3:ncol(prvi.absolute.sf)] <- prvi.absolute.sf[3:ncol(prvi.absolute.sf)] - prvi.absolute.sf[, 3]

# Rotate table ABSOLUTE
prvi.long.absolute.sf <- melt(setDT(prvi.absolute.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
head(prvi.long.absolute.sf)
# Mean of treatments + doy
prvi.long.absolute.sf.mean <- aggregate(value ~ springtreatment + doy, data = prvi.long.absolute.sf, FUN = mean, na.rm = TRUE)

# Quick plot for absolute
prvi.long.absolute.sf.mean.plot <- ggplot(prvi.long.absolute.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("prvi absolute shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
prvi.long.absolute.sf.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
prvi.long.total.sf <- melt(setDT(prvi.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
prvi.long.total.sf.mean <- aggregate(value ~ springtreatment + doy, data = prvi.long.total.sf, FUN = mean, na.rm = TRUE)

# Quick plot for TOTAL
prvi.long.total.sf.mean.plot <- ggplot(prvi.long.total.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("prvi total shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
prvi.long.total.sf.mean.plot

##### QUMA #####
head(quma)
# Select springtreatment column and place it after id
quma.sf <- quma[, c(1, 3, 10, 12, 13:length(colnames(quma)))]
# Add a column with only the spring treatment
quma.sf$springtreatment <- ifelse(grepl('^CoolS', quma.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment specified by >length(colnames(quma.sf)) is at second position and that all days are included 
quma.sf <- quma.sf[, c(1, length(colnames(quma.sf)), (4:length(colnames(quma.sf))-1))]

##### Absolute shoot elongation ##### 
# Subtract each column by the first measurement
quma.absolute.sf <- quma.sf
quma.absolute.sf[3:ncol(quma.absolute.sf)] <- quma.absolute.sf[3:ncol(quma.absolute.sf)] - quma.absolute.sf[, 3]

# Rotate table ABSOLUTE
quma.long.absolute.sf <- melt(setDT(quma.absolute.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
quma.long.absolute.sf.mean <- aggregate(value ~ springtreatment + doy, data = quma.long.absolute.sf, FUN = mean, na.rm = TRUE)

# Quick plot for absolute
quma.long.absolute.sf.mean.plot <- ggplot(quma.long.absolute.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Quma absolute shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
quma.long.absolute.sf.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
quma.long.total.sf <- melt(setDT(quma.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
quma.long.total.sf.mean <- aggregate(value ~ springtreatment + doy, data = quma.long.total.sf, FUN = mean, na.rm = TRUE)

# Quick plot for TOTAL
quma.long.total.sf.mean.plot <- ggplot(quma.long.total.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Quma total shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
quma.long.total.sf.mean.plot

##### SEGI #####
head(segi) 
# Select springtreatment column and place it after id
segi.sf <- segi[, c(1, 3, 10, 12, 13:length(colnames(segi)))]
head(segi.sf)
# Add a column with only the spring treatment
segi.sf$springtreatment <- ifelse(grepl('^CoolS', segi.sf$treatment), 'CoolS', 'WarmS')
# Reorganize columns so spring treatment specified by >length(colnames(segi.sf)) is at second position and that all days are included 
segi.sf <- segi.sf[, c(1, length(colnames(segi.sf)), (4:length(colnames(segi.sf))-1))]

##### Absolute shoot elongation ##### 
# Subtract each column by the first measurement
segi.absolute.sf <- segi.sf
segi.absolute.sf[3:ncol(segi.absolute.sf)] <- segi.absolute.sf[3:ncol(segi.absolute.sf)] - segi.absolute.sf[, 3]

# Rotate table ABSOLUTE
segi.long.absolute.sf <- melt(setDT(segi.absolute.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
segi.long.absolute.sf.mean <- aggregate(value ~ springtreatment + doy, data = segi.long.absolute.sf, FUN = mean, na.rm = TRUE)

# Quick plot for absolute
segi.long.absolute.sf.mean.plot <- ggplot(segi.long.absolute.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Segi absolute shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
segi.long.absolute.sf.mean.plot

##### Total shoot elongation #####
# Rotate table TOTAL
segi.long.total.sf <- melt(setDT(segi.sf), id.vars = c("tree_ID", "springtreatment"), variable.name = "doy")
# Mean of treatments + doy
segi.long.total.sf.mean <- aggregate(value ~ springtreatment + doy, data = segi.long.total.sf, FUN = mean, na.rm = TRUE)

# Quick plot for TOTAL
segi.long.total.sf.mean.plot <- ggplot(segi.long.total.sf.mean) +
  geom_point(aes(x = doy, y = value, color = springtreatment)) +
  labs(x = "DOY", y = "CM") +
  ggtitle("Segi total shoot elongation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right")
segi.long.total.sf.mean.plot


