# CRD
# 27 May 2025
# Goal is to start visualization for fuelinex

# Set the path to your directory folder 
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

# run cleaning cleanall 
source("cleaning/cleanAllFuelinex.R")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(ggdist)
library(colorspace)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Custom color palets
variouspallet6 <- c("#41afaa", "#466eb4", "#af4b91", "#00a0e1", "#e6a532", "#d7642c")
greenpallet <- c("#006400", "#32CD32", "#66CDAA", "#ADFF2F", "#008000", "#7CFC00")

### === === === === === === === === === === === === ###
#### Number of observations in 2024 for each Species X treatments ####
### === === === === === === === === === === === === ###
# 1) Add sep
phenostage24$Group <- paste(phenostage24$species, phenostage24$treatment, sep="__SEP__")

# 2) Keep only the unique (tree_ID, Group, Phenostage) combinations
df_unique <- unique(phenostage24[, c("tree_ID","Group","phenostageNum")])

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
  DOY ~ phenophaseText + species + treatment, 
  data = phenostage24, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

summary_stats$mean_DOY <- summary_stats$DOY[, "mean"]
summary_stats$sd_DOY <- summary_stats$DOY[, "sd"]

# for now, just take dormant and unroled
vec <- c("Leaf fully unfolded", "Bud is fully set")
suby <- subset(summary_stats, phenophaseText %in% vec)

# and select only for growing season extension treatments
vectreat <- c("CoolS/CoolF", "WarmS/CoolF", "CoolS/WarmF", "WarmS/WarmF")
suby <- subset(suby, treatment %in% vectreat)

# Create the plot
# Sample data for shoot elongation periods
# Plot
ggplot(suby, aes(x = mean_DOY, y = treatment, color = phenophaseText)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbarh(aes(xmin = mean_DOY - sd_DOY, xmax = mean_DOY + sd_DOY), 
                 height = 0.2, alpha = 0.5, linewidth = 0.6) + 

  # Add text annotation for shoot elongation (aligned with species)
  # geom_text(data = shootperiods, aes(x = (start + end) / 2, y = 4.7, label = "Shoot Elongation"), 
  #           vjust = 2, hjust = 0.5, color = "black", size = 4) +
  facet_wrap(~species, scales = "free_y") +  # Adjust y-axis scales if needed
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
mean_stats <- aggregate(adjustedshootElong ~ species + treatment + DOY, 
                        data = shoot2025, FUN = mean, na.rm = TRUE)
sd_stats <- aggregate(adjustedshootElong ~ species + treatment + DOY, 
                        data = shoot2025, FUN = sd, na.rm = TRUE)
colnames(sd_stats)

# merge mean and sd dfs by species, treatment and DOY
mean_stats$id <- paste0(mean_stats$Species, "_", mean_stats$treatment, "_", mean_stats$DOY)
sd_stats$id <- paste0(sd_stats$Species, "_", sd_stats$treatment, "_", sd_stats$DOY)
sd_stats2 <- sd_stats[, c(5,4)]
colnames(sd_stats2) <- c("id", "sd")
mergeforplot <- merge(mean_stats, sd_stats2, by = "id")

# temporarily remove the measurement from doy 171 for all species
head(mean_stats)
mean_stats <- subset(mean_stats, !DOY == 164) # 164 and 192
# select only the non nitro treatments for now

vec <- c("CoolS/CoolF", "CoolS/WarmF", "WarmS/WarmF", "WarmS/CoolF")

nonitro <- subset(mean_stats, treatment %in% vec)

#shoot elongation plot
shootelongation <- ggplot(mean_stats) +
  geom_line(aes(x = DOY, y = adjustedshootElong, color = treatment, group = treatment)) + 
  facet_wrap(~species, scales = "free_y") +  
  theme_minimal() +
  labs(
    x = "Day of Year",
    y = "Adjusted Shoot Elongation",  # Updated y-axis label
    title = "Shoot elongation 2025",
    color = "Treatment"  # Updated legend title
  ) +
  scale_color_manual(values=greenpallet)+
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
test <- subset(shoot2025, genus != "sequoiadendron")

shootelong2025XSppXTreat <- ggplot(test) +
  geom_line(aes(x = DOY, y = shootElongation, group = tree_ID, color = species)) + 
  facet_wrap(species~treatment, scales = "free_y") +  
  theme_minimal() +
  labs(
    x = "Day of Year",
    y = "Adjusted Shoot Elongation", 
    title = "Shoot elongation 2025",
    color = ""  # Updated legend title
  ) +
  scale_color_manual(values=greenpallet)+
  theme_classic()
shootelong2025XSppXTreat
# save!
ggsave("figures/shootelong2025XSppXTreat.pdf", shootelong2025XSppXTreat, width = 16, height = 12)

##### Plots when they stopped elongating #####
# 2024
shoot24forstop <- shoot2024[order(shoot2024$tree_ID, shoot2024$DOY), ]
## split by replicate, look at the first delta < 0.2
stopelong24 <- do.call(rbind,
                       by(shoot24forstop, shoot24forstop$tree_ID, function(df) {
                         inc<- c(NA, diff(df$shootElongation))
                         idx<- which(inc < 0.2)[1] 
                         if (length(idx)) df[idx, c("tree_ID", "DOY")]
                       })
)
row.names(stopelong24) <- NULL
stopelong24$year <- as.character(2024)

# 2025
shoot25forstop <- shoot2025[order(shoot2025$tree_ID, shoot2025$DOY), ]

## split by replicate, look at the first delta < 0.2
stopelong25 <- do.call(rbind,
                    by(shoot25forstop, shoot25forstop$tree_ID, function(df) {
                      inc<- c(NA, diff(df$shootElongation))
                      idx<- which(inc < 0.2)[1] 
                      if (length(idx)) df[idx, c("tree_ID", "DOY")]
                    })
)
row.names(stopelong25) <- NULL
stopelong25$year <- as.character(2025)

# bind 2024 and 2024
shootelongbinded <- rbind(stopelong24, stopelong25)

# readd columns
shootelongbinded$Species <- sub("^([^_]+)_.*", "\\1", shootelongbinded$tree_ID)
shootelongbinded$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", shootelongbinded$tree_ID)

# Append "_nitro" if "nitro" appears anywhere in the ID_DOY
shootelongbinded$Treatment <- ifelse(grepl("_nitro", shootelongbinded$tree_ID), 
                                paste0(shootelongbinded$Treatment, "_nitro"), 
                                shootelongbinded$Treatment)


shootelongforplot <- shootelongbinded[!is.na(shootelongbinded$DOY),]
shootelongforplot$DOY <- as.numeric(shootelongforplot$DOY)

# PLOT
cols2 <- c("#00858a", "#e46e00")

subforplot <- subset(shootelongforplot, Species != "Segi")

shootelongstop <- ggplot(subforplot) +
  geom_point(aes(x = Treatment, y = DOY, color = year),
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0), 
             alpha = 0.2) +
  stat_summary(aes(x = Treatment, y = DOY, group = interaction(Species, Treatment, year), 
                   color = year),
               fun = mean,
               geom = "point",
               size = 2,
               position = position_dodge(width = 0.8)) +
  stat_summary(aes(x = Treatment, y = DOY, group = interaction(Species, Treatment, year), 
                   color = year),
               fun.data = mean_se,
               geom = "linerange",
               position = position_dodge(width = 0.8)
  )+
  scale_color_manual(values = cols2) +  
  facet_wrap(~Species, ncol = 3, nrow = 2, scales = "free_y") +
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
greenesslossnona24 <- leafdrop24[!is.na(leafdrop24$greenessLoss), 1:ncol(leafdrop24)-1]
str(greenesslossnona24)

greenessloss24_plot <- ggplot(greenesslossnona24, aes(x = treatment, y = greenessLoss, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "linerange", width = 0.2, color = "black", position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Time of greeness loss",
       y = "DOY",
       x = "Treatment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
greenessloss24_plot
ggsave("figures/greenessloss24.jpg", greenessloss24_plot, width = 10, height = 6, units = "in", dpi = 300)


# --- --- --- --- --- #
##### Leaf Drop #####
# --- --- --- --- --- #
leafdrop24nona <- leafdrop24[!is.na(leafdrop24$leafDrop), ]
str(leafdrop24nona)

leafdrop24_plot <- ggplot(leafdrop24nona, aes(x = treatment, y = leafDrop, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, 
               color = "black", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "linerange", width = 0.2, 
               color = "black", position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Time of leaf drop (no leaves left on tree)",
       y = "DOY",
       x = "Treatment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
leafdrop24_plot
ggsave("figures/leafdrop24.jpg", leafdrop24_plot, width = 10, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- ---  #
##### Visual green leaf cover #####
# --- --- --- --- --- --- --- --- #
greenleafcover24

greenleafcover24_plot <- ggplot(greenleafcover24) +
  geom_smooth(aes(x = DOY, y = greenCovPer, color = treatment)) + 
  facet_wrap(~species, scales = "free_y") +  
  theme_minimal() +
  labs(
    x = "DOY",
    y = "Remaining greeenleaf cover (%)", 
    title = "Greenleaf cover 2024",
    color = "Treatments"  
  ) +
  scale_color_manual(values=greenpallet)+
  theme_classic()
greenleafcover24_plot
ggsave("figures/greenleafcover24.jpg", greenleafcover24_plot, width = 10, height = 6, units = "in", dpi = 300)
# --- --- --- --- --- --- --- --- --- #
##### Chlorophyll measurements #####
# --- --- --- --- --- --- --- --- --- #
# until the conversion factor from ccm to minolta is done, I'll filter out all measurements from ccm
chl24sub <- subset(chl24, DOY >269)

# aggregate by measurement for means
fun <- aggregate(chlValue ~ tree_ID + species + treatment + DOY + measurement,
          data = chl24sub,
          FUN = mean,
          na.rm = TRUE)

chl24_plot <- ggplot(fun) +
  geom_smooth(aes(x = DOY, y = chlValue, color = treatment)) + 
  facet_wrap(~species, scales = "free_y") +  
  theme_minimal() +
  labs(
    x = "DOY",
    y = "chlValue", 
    title = "Chlorophyll measurements 2024",
    color = ""  # Updated legend title
  ) +
  scale_color_manual(values=greenpallet)+
  theme_classic()
chl24_plot


ggplot(chl24sub, aes(x = DOY, y = chlValue, group = tree_ID, color = treatment)) +
  geom_line(alpha = 0.2) +  # faint lines for individuals
  stat_summary(aes(group = interaction(species, treatment)), 
               fun = mean, geom = "line", size = 1.2) +
  facet_wrap(~ species) +
  scale_color_manual(values = variouspallet6) 

subby <- subset(fun, species == "acer_negundo")
ggplot(subby, aes(x = DOY, y = chlValue,  color = tree_ID)) +
  geom_line(alpha = 0.2) +  # faint lines for individuals
  stat_summary(aes(group = interaction(species, treatment)), 
               fun = mean, geom = "line", size = 1.2) +
  facet_wrap(~ species)
  # scale_color_manual(values = variouspallet6) 
dens(chl24sub$chlValue)

ggplot(chl24sub, aes(x = DOY, y = chlValue, color = treatment)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, aes(fill = treatment)) +
  facet_wrap(~ species) +
  scale_color_manual(values = variouspallet6) 


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
variouspallet6 <- c("#41afaa", "#466eb4", "#00a0e1", "#e6a532", "#d7642c", "#af4b91")

# --- ---- ----- ------------- ------------- ------------- ------------- 
# mean with line range sd
heightplot <- ggplot(meawide3, aes(x = treatment, y = heighincrement, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "linerange", color = "black") +
  scale_color_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Height increment X treatment X species",
       y = "Height Increment (cm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heightplot

ggsave("figures/heightIncrement.jpg", heightplot, width = 10, height = 6, units = "in", dpi = 300)

# violin
heightviolin <- ggplot(meawide3, aes(x = treatment, y = heighincrement, color = treatment)) +
  geom_violin(aes(fill = treatment), trim = FALSE, width = 0.8, alpha = 0.3) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "crossbar", shape = 18, size = 0.3, color = "black", position = position_dodge(width = 0.5)) +
  scale_color_manual(values = variouspallet6) +  
  scale_fill_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Height increment X treatment X species",
       y = "Height Increment (cm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heightviolin
ggsave("figures/heightviolin.jpeg", heightviolin, width = 10, height = 6, units = "in", dpi = 300)

# halfeye plot
heighthalfeye <- ggplot(meawide3, aes(x = treatment, y = heighincrement, color = treatment, fill = treatment)) +
  geom_point(position = position_jitter(width = 0.1), size = 1, alpha = 0.6) +
  ggdist::stat_halfeye(
    justification = -0.1, 
    .width = 0, 
    point_interval = ggdist::mean_qi
  ) +
  scale_color_manual(values = variouspallet6) +  
  scale_fill_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heighthalfeye

ggsave("figures/heighthalfeye.jpg", heighthalfeye, width = 10, height = 6, units = "in", dpi = 300)

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

