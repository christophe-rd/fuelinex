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
ggsave("figures/empiricalData_plotsheatmap.pdf", plot = heatmap, width = 8, height = 6)

### === === === === === === === === === === === === ###
#### Budburst and budset ####
### === === === === === === === === === === === === ###
# mean and sd for each phenophase X Species X Treatment
budburstToSet_stats <- aggregate(
  DOY ~ phenophaseText + species + treatment, 
  data = phenostage24, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

pheno24 <- aggregate(DOY ~ 
                       phenophaseText + phenostageNum + tree_ID + treatment + species, 
                     phenostage24, FUN = min)

lo24 <- subset(pheno24, species != "sequoiadendron_giganteum")



pdf("figures/empiricalData_plots/phenophases.pdf", width = 14, height = 6)

treatlevels <- unique(pheno24$treatment)

for(ph in  unique(pheno24$phenophaseText[order(pheno24$phenostageNum)])) {
  sub <- pheno24[pheno24$phenophaseText == ph, ]
  
  par(mfrow = c(1, 7), mar = c(8, 4, 3, 1), oma = c(0, 0, 2, 0))
  
  for(sp in unique(sub$species)) {
    subspp <- sub[sub$species == sp, ]
    subspp$treat_num <- as.integer(factor(subspp$treatment, levels = treatlevels))
    
    stats <- lapply(1:length(treatlevels), function(i) {
      x <- subspp$DOY[subspp$treat_num == i]
      q <- quantile(x, c(0.05, 0.25, 0.75, 0.95))
      c(mean = mean(x), p5 = q[1], p25 = q[2], p75 = q[3], p95 = q[4])
    })
    stats <- do.call(rbind, stats)
    colnames(stats) <- c("mean", "p5", "p25", "p75", "p95")
    
    plot(NULL,
         xlim = c(0.5, length(treatlevels) + 0.5),
         ylim = c(min(subspp$DOY, na.rm =TRUE) - 2, 
                  max(subspp$DOY, na.rm = TRUE) + 2),
         xaxt = "n", xlab = "", ylab = "DOY", main = sp)
    # segments(1:nrow(stats), stats[, "p5"],
    #          1:nrow(stats), stats[, "p95"],
    #          col = variouspallet6, lwd = 0.5)
    segments(1:nrow(stats), stats[, "p25"],
             1:nrow(stats), stats[, "p75"],
             col = variouspallet6, lwd = 1)
    points(1:nrow(stats), stats[, "mean"],
           pch = 16, cex = 1.5, col = variouspallet6)
    points(subspp$treat_num + runif(nrow(subspp), -0.15, 0.15), subspp$DOY,
           pch = 16, cex = 1,
           col = adjustcolor(variouspallet6[subspp$treat_num], alpha.f = 0.4))
    axis(1, at = 1:length(treatlevels), labels = treatlevels, las = 2)
  }
  mtext(ph, outer = TRUE, line = 0.5, cex = 1.2)
}

dev.off()
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
ggsave("figures/empiricalData_plots/2024shootElongationbySpp.pdf", shootelongation)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### All individuals shoot elongation adjusted #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
cols <- c("#a40000", "#16317d", "#007e2f", "#ffcd12", "#b86092", "#721b3e","#00b7a7") 

# 2024
pdf("figures/empiricalData_plots/shootelong2024XSppXTreat.pdf", 
    width = 12, height = 8)

spplevels <- unique(shoot2024$species)
treatlevels <- unique(shoot2024$treatment)

shoot2024$DOY <- as.numeric(shoot2024$DOY)
shoot2024$adjustedShootElong <- as.numeric(shoot2024$adjustedShootElong)

for(sp in spplevels) { # sp = "acer_negundo"
  subspp <- shoot2024[shoot2024$species == sp, ]
  
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
  
  for(tr in treatlevels) { # tr = "CoolS/CoolF"
    subtr <- subspp[subspp$treatment == tr, ]
    
    plot(NULL,
         xlim = range(subspp$DOY, na.rm = TRUE),
         ylim = range(subspp$adjustedShootElong, na.rm = TRUE),
         xlab = "Day of Year", ylab = "Adjusted Shoot Elongation",
         main = tr)
    for(id in unique(subtr$tree_ID)) {
      subtree <- subtr[subtr$tree_ID == id, ]
      lines(subtree$DOY, subtree$adjustedShootElong,
            col = cols[which(spplevels == sp)])
    }
  }
  mtext(sp, outer = TRUE, line = 0.5, cex = 1.2)
}

dev.off()

# 2025
pdf("figures/empiricalData_plots/shootelong2025XSppXTreat.pdf", 
    width = 12, height = 8)

spplevels <- unique(shoot2025$species)
treatlevels <- unique(shoot2025$treatment)

for(sp in spplevels) {
  subspp <- shoot2025[shoot2025$species == sp, ]
  
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
  
  for(tr in treatlevels) {
    subtr <- subspp[subspp$treatment == tr, ]
    
    plot(NULL,
         xlim = range(subspp$DOY, na.rm = TRUE),
         ylim = range(subspp$adjustedshootElong, na.rm = TRUE),
         xlab = "Day of Year", ylab = "Adjusted Shoot Elongation",
         main = tr)
    for(id in unique(subtr$tree_ID)) {
      subtree <- subtr[subtr$tree_ID == id, ]
      lines(subtree$DOY, subtree$adjustedshootElong,
            col = cols[which(spplevels == sp)])
    }
  }
  mtext(sp, outer = TRUE, line = 0.5, cex = 1.2)
}

dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
##### Plots when they stopped elongating #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
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
spplevels <- unique(subforplot$Species)
treatlevels <- unique(subforplot$Treatment)
yearlevels <- unique(subforplot$year)

pdf("figures/empiricalData_plots/shootelongstop.pdf", width = 9, height = 6)
par(mfrow = c(2, 3), mar = c(8, 4, 3, 1), oma = c(0, 0, 2, 0))

for(sp in spplevels) {
  subspp <- subforplot[subforplot$Species == sp, ]
  
  plot(NULL,
       xlim = c(0.5, length(treatlevels) + 0.5),
       ylim = range(subspp$DOY, na.rm = TRUE),
       xaxt = "n", xlab = "", ylab = "DOY", main = sp, frame = FALSE)
  
  for(tr in treatlevels) {
    xi <- which(treatlevels == tr)
    offset <- c(-0.15, 0.15)
    for(yi in 1:length(yearlevels)) {
      yr <- yearlevels[yi]
      x <- subspp$DOY[subspp$Treatment == tr & subspp$year == yr]
      col <- cols2[yi]
      # raw points
      points(rep(xi + offset[yi], length(x)) + runif(length(x), -0.05, 0.05),
             x, pch = 16, cex = 0.6, col = adjustcolor(col, alpha.f = 0.5))
      # mean and SE
      m <- mean(x, na.rm = TRUE)
      se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      points(xi + offset[yi], m, pch = 16, cex = 2, col = col)
      segments(xi + offset[yi], m - se, xi + offset[yi], m + se, col = col, lwd = 1.5)
    }
  }
  axis(1, at = 1:length(treatlevels), labels = treatlevels, las = 2)
  legend("topright", legend = yearlevels, pch = 16, col = cols2, bty = "n")
}
dev.off()
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
ggsave("figures/empiricalData_plots/greenessloss24.jpg", greenessloss24_plot, width = 10, height = 6, units = "in", dpi = 300)


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
ggsave("figures/empiricalData_plots/leafdrop24.jpg", leafdrop24_plot, width = 10, height = 6, units = "in", dpi = 300)

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
ggsave("figures/empiricalData_plots/greenleafcover24.jpg", greenleafcover24_plot, width = 10, height = 6, units = "in", dpi = 300)
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
# dens(chl24sub$chlValue)

ggplot(chl24sub, aes(x = DOY, y = chlValue, color = treatment)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, aes(fill = treatment)) +
  facet_wrap(~ species) +
  scale_color_manual(values = variouspallet6) 


### === === === === === === === === ### ###
#### Height and diameter measurements ####
### === === === === === === === === ### ###
mea <- read.csv("output/cleanedMeasurements.csv")

# reassess how 
meawide <- reshape(
  mea,
  timevar = "year",
  idvar = "tree_ID",
  direction = "wide"
)

meawide <- meawide[, c(
  "tree_ID",
  "bloc.2023",
  "treatment.2023",
  "genus.2023",
  "species.2023",
  "height.2023",
  "diameter.2023",
  "height.2024",
  "diameter.2024",
  "height.2025",
  "diameter.2025"
)]
meawide
colnames(meawide) <- c(
  "tree_ID",
  "bloc",
  "treatment",
  "genus",
  "species",
  "height2023",
  "diameter2023",
  "height2024",
  "diameter2024",
  "height2025",
  "diameter2025"
)

# height and diameter increment
meawide$heighincrement2024 <- meawide$height2024 - meawide$height2023
meawide$diameterincrement2024 <- meawide$diameter2024 - meawide$diameter2023

meawide$heighincrement2025 <- meawide$height2025 - meawide$height2024
meawide$diameterincrement2025 <- meawide$diameter2025 - meawide$diameter2024

# remove tree_ID that are 
meawide2 <- subset(meawide, heighincrement2024 <= 0 & 
                     diameterincrement2024 <= 0)
meawide2 <- subset(meawide, heighincrement2025 <= 0 & 
                     diameterincrement2025 <= 0)

# remove segi for now
meawide3 <- subset(meawide, genus != "sequoiadendron")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# mean with line range sd 2024
ggplot(meawide3, aes(x = treatment, y = heighincrement2024, 
                                   color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, 
               color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "linerange", color = "black") +
  scale_color_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Height increment 2025 X treatment X species",
       y = "Height Increment (cm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/empiricalData_plots/2024heightIncrement.jpg", 
       width = 10, height = 6, units = "in", dpi = 300)

# 2025
ggplot(meawide3, aes(x = treatment, y = heighincrement2025, 
                     color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, color = "black") +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1), geom = "linerange", color = "black") +
  scale_color_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Height increment 2025 X treatment X species",
       y = "Height Increment (cm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/empiricalData_plots/2025heightIncrement.jpg", 
       width = 10, height = 6, units = "in", dpi = 300)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Height 2024 Violin #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
ggplot(meawide3, aes(x = treatment, y = heighincrement2024, color = treatment)) +
  geom_violin(aes(fill = treatment), trim = FALSE, width = 0.8, alpha = 0.3) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "crossbar", shape = 18, size = 0.3, color = "black", position = position_dodge(width = 0.5)) +
  scale_color_manual(values = variouspallet6) +  
  scale_fill_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Height increment 2024 X treatment X species",
       y = "Height Increment (cm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/empiricalData_plots/2024heightviolin.jpeg", width = 10, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Height 2025 Violin #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
ggplot(meawide3, aes(x = treatment, y = heighincrement2025, color = treatment)) +
  geom_violin(aes(fill = treatment), trim = FALSE, width = 0.8, alpha = 0.3) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "crossbar", shape = 18, size = 0.3, color = "black", position = position_dodge(width = 0.5)) +
  scale_color_manual(values = variouspallet6) +  
  scale_fill_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Height increment 2025 X treatment X species",
       y = "Height Increment (cm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/empiricalData_plots/2025heightviolin.jpeg", width = 10, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Diameter 2024 Violin #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
ggplot(meawide3, aes(x = treatment, y = diameterincrement2024,
                     color = treatment)) +
  geom_violin(aes(fill = treatment), trim = FALSE, width = 0.8, alpha = 0.3) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "crossbar", shape = 18, size = 0.3, color = "black", position = position_dodge(width = 0.5)) +
  scale_color_manual(values = variouspallet6) +  
  scale_fill_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Diameter increment 2024 X treatment X species",
       y = "Diameter Increment (mm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/empiricalData_plots/2024diameterviolin.jpeg", width = 10, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Diameter 2025 Violin #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
ggplot(meawide3, aes(x = treatment, y = diameterincrement2025, color = treatment)) +
  geom_violin(aes(fill = treatment), trim = FALSE, width = 0.8, alpha = 0.3) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "crossbar", shape = 18, size = 0.3, color = "black", position = position_dodge(width = 0.5)) +
  scale_color_manual(values = variouspallet6) +  
  scale_fill_manual(values = variouspallet6) +  
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "Diameter increment 2025 X treatment X species",
       y = "Diameter Increment (cm)",
       x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/empiricalData_plots/2025diameterviolin.jpeg", width = 10, height = 6, units = "in", dpi = 300)


# DIAMETER
# 2024
diameterplot <- ggplot(meawide3, aes(x = treatment, y = diameterincrement2024, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2, color = "black", position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "diameter increment X treament X species",
       y = "diameter Increment (cm)",
       x = "Treatment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
diameterplot
ggsave("figures/empiricalData_plots/diameterplotIncrement.jpg", width = 10, height = 6, units = "in", dpi = 300)

# 2025
ggplot(meawide3, aes(x = treatment, y = diameterincrement2025, color = treatment)) +
  geom_point(position = position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2, color = "black", position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 3, nrow = 3, scales = "free_y") +
  labs(title = "diameter increment X treament X species",
       y = "diameter Increment (cm)",
       x = "Treatment") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/empiricalData_plots/diameterplotIncrement.jpg", width = 10, height = 6, units = "in", dpi = 300)

# Conceptual figure of phenophases #####
### Start with shoot2024. Didn't have time to finish but goal is to mimic fig 1 of korner2023

head(shoot2024)

budburstToSet_stats

sub <- subset(budburstToSet_stats, species == "acer_negundo")

# sub2 <- subset(sub, treatment == "CoolS/CoolF")
# ggplot(df) +
#   geom_segment(aes(x = start_day, xend = end_day,
#                    y = process, yend = process),
#                size = 8, color = "forestgreen") +
#   geom_segment(aes(x = 1, xend = 365,
#                    y = process, yend = process),
#                size = 8, color = "grey90") +
#   geom_text(aes(x = end_day + 10, y = process,
#                 label = paste(end_day - start_day, "d")),
#             hjust = 0, vjust = 0.5) +
#   scale_x_continuous(limits = c(0, 365), expand = c(0,0),
#                      breaks = seq(0, 365, 50)) +
#   labs(x = "Day of year", y = "") +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.y = element_text(size = 12),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank())

