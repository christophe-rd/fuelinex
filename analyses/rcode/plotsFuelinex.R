# CRD
# 27 May 2025
# Goal is to start visualization for fuelinex

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# Set the path to your directory folder 
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

# run cleaning cleanall 
source("cleaning/cleanAllFuelinex.R")

### === === === === === === === === === === === === ###
#### Number of observations in 2024 for each Species X treatments ####
### === === === === === === === === === === === === ###
# 1) Add sep
phenostage24$Group <- paste(phenostage24$Species, phenostage24$Treatment, sep="__SEP__")

# 2) Keep only the unique (ID, Group, Phenostage) combinations
df_unique <- unique(phenostage24[, c("ID","Group","phenostageNum")])

# 3) Build a contingency table: rows = Group; cols = Phenostage
tab <- table(df_unique$Group, df_unique$phenostageNum)

# 4) Convert that table to a wide data.frame
wide <- as.data.frame.matrix(tab)
wide$Group <- rownames(wide)         # pull the Group keys back into a column

# 5) Split Group back into Species & Treatment
splits <- do.call(rbind, strsplit(wide$Group, "__SEP__", fixed=TRUE))
colnames(splits) <- c("Species","Treatment")

# 6) Assemble final data.frame
res <- data.frame(
  splits,
  # ensure columns 0–4 are in order:
  wide[, as.character(0:7)],
  row.names = NULL,
  check.names = FALSE
)

# 7) Tidy up column names
colnames(res)[3:10] <- paste0("Stage_", 0:7)

write.csv(res, "output/2024nobservations.csv", row.names = FALSE)

df_long <- res %>%
  pivot_longer(
    cols = starts_with("Stage_"),
    names_to  = "Stage",
    names_prefix = "Stage_",
    values_to = "Pct"
  ) %>%
  mutate(Stage = factor(Stage, levels = as.character(0:7)))

df_long <- subset(df_long, Species != "Segi")
heatmap <- ggplot(df_long, aes(x = Stage, y = Treatment, fill = Pct)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Number of replicates", direction = -1) +
  facet_wrap(~ Species, ncol = 2, scales = "free_y") +
  labs(
    title = "Number of replicates that with recorded phenostage X Species X Treatments",
    x     = "Stage",
    y     = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
heatmap
ggsave("figures/heatmap.pdf", plot = heatmap, width = 8, height = 6)
ggsave("figures/heatmap.jpeg", plot = heatmap)

### === === === === === === === === === === === === ###
#### Budburst and budset ####
### === === === === === === === === === === === === ###
# mean and sd for each phenophase X Species X Treatment
summary_stats <- aggregate(
  DOY ~ phenophaseText + Species + Treatment, 
  data = phenostage24, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

summary_stats$mean_DOY <- summary_stats$DOY[, "mean"]
summary_stats$sd_DOY <- summary_stats$DOY[, "sd"]

# for now, just take dormant and unroled
vec <- c("Leaf fully unfolded", "Bud is fully set")
suby <- summary_stats[summary_stats$phenophaseText %in% vec, ]
# and select only for growing season extension treatments
vectreat <- c("CoolS/CoolF", "WarmS/CoolF", "CoolS/WarmF", "WarmS/WarmF")
suby <- suby[suby$Treatment %in% vectreat, ]

# Create the plot
# Sample data for shoot elongation periods
# Plot
ggplot(suby, aes(x = mean_DOY, y = Treatment, color = phenophaseText)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbarh(aes(xmin = mean_DOY - sd_DOY, xmax = mean_DOY + sd_DOY), 
                 height = 0.2, alpha = 0.5, linewidth = 0.6) + 

  # Add text annotation for shoot elongation (aligned with species)
  # geom_text(data = shootperiods, aes(x = (start + end) / 2, y = 4.7, label = "Shoot Elongation"), 
  #           vjust = 2, hjust = 0.5, color = "black", size = 4) +
  facet_wrap(~Species, scales = "free_y") +  # Adjust y-axis scales if needed
  theme_minimal() +
  labs(
    x = "Day of Year",
    y = "Treatment",
    title = "Phenophase 2024",
    color = "Phenophase"
  ) +
  theme(
    axis.text.y = element_text(size = 10, face = "italic"),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

### === === === === === ###
#### Shoot elongation ####
### === === === === === ###
# get mean measurement per doy, species and treatement i.e. mean per replicate
mean_stats <- aggregate(adjustedshootElong ~ Species + Treatment + DOY, 
                        data = shoot2025, FUN = mean, na.rm = TRUE)
sd_stats <- aggregate(adjustedshootElong ~ Species + Treatment + DOY, 
                        data = shoot2025, FUN = sd, na.rm = TRUE)
colnames(sd_stats)

# merge mean and sd dfs by species, treatment and DOY
mean_stats$id <- paste0(mean_stats$Species, "_", mean_stats$Treatment, "_", mean_stats$DOY)
sd_stats$id <- paste0(sd_stats$Species, "_", sd_stats$Treatment, "_", sd_stats$DOY)
sd_stats2 <- sd_stats[, c(5,4)]
colnames(sd_stats2) <- c("id", "sd")
mergeforplot <- merge(mean_stats, sd_stats2, by = "id")

# temporarily remove the measurement from doy 171 for all species
head(mean_stats)
mean_stats <- subset(mean_stats, !DOY == 164) # 164 and 192
# select only the non nitro treatments for now

vec <- c("CoolS/CoolF", "CoolS/WarmF", "WarmS/WarmF", "WarmS/CoolF")
green_palette <- c("#006400", "#32CD32", "#66CDAA", "#ADFF2F", "orange", "lightblue")

nonitro <- subset(mean_stats, Treatment %in% vec)

#shoot elongation plot
shootelongation <- ggplot(mean_stats) +
  geom_line(aes(x = DOY, y = adjustedshootElong, color = Treatment, group = Treatment)) + 
  facet_wrap(~Species, scales = "free_y") +  
  theme_minimal() +
  labs(
    x = "Day of Year",
    y = "Adjusted Shoot Elongation",  # Updated y-axis label
    title = "Shoot elongation 2025",
    color = "Treatment"  # Updated legend title
  ) +
  scale_color_manual(values=green_palette)+
  theme_classic() + 
  theme(
    axis.text.y = element_text(size = 10, face = "italic"),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right" 
  )
shootelongation
ggsave("figures/shootElongationbySpp.pdf", shootelongation)

# let's try something else
test <- subset(shoot2025, Species != "Segi")

shootelong2025XSppXTreat <- ggplot(test) +
  geom_line(aes(x = DOY, y = shootElongation, group = ID, color = Species)) + 
  facet_wrap(Species~Treatment, scales = "free_y") +  
  theme_minimal() +
  labs(
    x = "Day of Year",
    y = "Adjusted Shoot Elongation", 
    title = "Shoot elongation 2025",
    color = ""  # Updated legend title
  ) +
  scale_color_manual(values=green_palette)+
  theme_classic()
shootelong2025XSppXTreat
# save!
ggsave("figures/shootelong2025XSppXTreat.pdf", shootelong2025XSppXTreat, width = 16, height = 12)

##### Plots when they stopped elongating #####
# 2024
shoot24forstop <- shoot2024[order(shoot2024$ID, shoot2024$DOY), ]
## split by replicate, look at the first delta < 0.2
stopelong24 <- do.call(rbind,
                       by(shoot24forstop, shoot24forstop$ID, function(df) {
                         inc<- c(NA, diff(df$shootElongation))
                         idx<- which(inc < 0.2)[1] 
                         if (length(idx)) df[idx, c("ID", "DOY")]
                       })
)
row.names(stopelong24) <- NULL
stopelong24$year <- as.character(2024)

# 2025
shoot25forstop <- shoot2025[order(shoot2025$ID, shoot2025$DOY), ]

## split by replicate, look at the first delta < 0.2
stopelong25 <- do.call(rbind,
                    by(shoot25forstop, shoot25forstop$ID, function(df) {
                      inc<- c(NA, diff(df$shootElongation))
                      idx<- which(inc < 0.2)[1] 
                      if (length(idx)) df[idx, c("ID", "DOY")]
                    })
)
row.names(stopelong25) <- NULL
stopelong25$year <- as.character(2025)

# bind 2024 and 2024
shootelongbinded <- rbind(stopelong24, stopelong25)

# readd columns
shootelongbinded$Species <- sub("^([^_]+)_.*", "\\1", shootelongbinded$ID)
shootelongbinded$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", shootelongbinded$ID)

# Append "_nitro" if "nitro" appears anywhere in the ID_DOY
shootelongbinded$Treatment <- ifelse(grepl("_nitro", shootelongbinded$ID), 
                                paste0(shootelongbinded$Treatment, "_nitro"), 
                                shootelongbinded$Treatment)


shootelongforplot <- shootelongbinded[!is.na(shootelongbinded$DOY),]
shootelongforplot$DOY <- as.numeric(shootelongforplot$DOY)

# select only species that stopped elongating as of 5 August 2025
sub <- subset(shootelongforplot, Species %in% c("Acne", "Prvi", "Pist", "Quma"))

# PLOT
shootelongstop <- ggplot(sub) +
  geom_point(aes(x = Treatment, y = DOY, color = year),
             position = position_jitter(width = 0.1), alpha = 0.2) +
  # Mean point per Species + Treatment + year
  stat_summary(aes(x = Treatment, y = DOY, group = interaction(Species, Treatment, year), 
                   color = year),
               fun = mean,
               geom = "point",
               
               size = 2) +
  # Error bar (SE) per Species + Treatment + year
  stat_summary(aes(x = Treatment, y = DOY, group = interaction(Species, Treatment, year), 
                   color = year),
    fun.data = mean_se,
    geom = "linerange"
  )+
  facet_wrap(~Species, ncol = 6, nrow = 7, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
shootelongstop
# ggsave
ggsave("figures/shootelongstop.pdf", shootelongstop, width = 12, height = 8)

### === === === ###
#### Senescence ####
### === === === ###

# --- --- --- --- --- #
##### GreenessLoss #####
# --- --- --- --- --- #
greenesslossnona <- leafdrop24[!is.na(leafdrop24$greenessLoss), 1:ncol(leafdrop24)-1]
str(greenesslossnona)

greenessloss <- ggplot(greenesslossnona, aes(x = treatment, y = greenessLoss, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2, color = "black", position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "diameter increment X treament X species",
       y = "diameter Increment (cm)",
       x = "Treatment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
greenessloss
ggsave("figures/diameterplotIncrement.jpg", width = 10, height = 6, units = "in", dpi = 300)


# --- --- --- --- --- #
##### Leaf Drop #####
# --- --- --- --- --- #
leafdrop24
# --- --- --- --- --- --- --- ---  #
##### Visual green leaf cover #####
# --- --- --- --- --- --- --- --- #
greenleafcover24
# --- --- --- --- --- --- --- --- --- #
##### Chlorophyll measurements #####
# --- --- --- --- --- --- --- --- --- #
chl24

### === === === === === === === === ### ###
#### Height and diameter measurements ####
### === === === === === === === === ### ###
mea <- read.csv2("output/cleanedMeasurements.csv")

meawide <- reshape(mea,
                   timevar = "year",
                   idvar = "tree_ID",
                   direction = "wide")
# reorganize columns
meawide <- meawide[, c("tree_ID",
                       "bloc.2024",
                       "treatment.2024",
                       "genus.2024",
                       "species.2024",
                       "Height.2024",
                       "Diameter.2024",
                       "Height.2025",
                       "Diameter.2025",
                       "notes.2024")]
# clean colnames
colnames(meawide) <- c("tree_ID",
                        "bloc",
                        "treatment",
                        "genus",
                        "species",
                        "Height2024",
                        "Diameter2024",
                        "Height2025",
                        "Diameter2025",
                        "notes")

# height and diameter increment
meawide$heighincrement <- meawide$Height2025-meawide$Height2024
meawide$diameterincrement <- meawide$Diameter2025-meawide$Diameter2024

# remove tree_ID that are -
meawide2 <- subset(meawide, heighincrement >= 0 & diameterincrement >= 0)

# remove segi for now
meawide3 <- subset(meawide2, genus != "sequoiadendron")

# HEIGHT
heightplot <- ggplot(meawide3, aes(x = treatment, y = heighincrement, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2, color = "black", position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "height increment X treament X species",
       y = "Height Increment (cm)",
       x = "Treatment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/heightIncrement.jpg", width = 10, height = 6, units = "in", dpi = 300)

# DIAMETER
diameterplot <- ggplot(meawide3, aes(x = treatment, y = diameterincrement, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2, color = "black", position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "diameter increment X treament X species",
       y = "diameter Increment (cm)",
       x = "Treatment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/diameterplotIncrement.jpg", width = 10, height = 6, units = "in", dpi = 300)

