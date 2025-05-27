# 27 May 2025
# CRD
# Goal is to start visualization for fuelinex


# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

## at some point I should add this function which selects rows after the same phenophase was recorded 3 times. Replace NumYs_in_Series with my phenophases
# d <- d[(d$NumYs_in_Series>=3),] 

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder 
# directory <-"/Users/christophe_rouleau-desrochers/github/fuelinex/"
# setwd(directory)
# list.files()

# Read data
# run cleaning phenostages first

### === === === === === === === === === === === === ###
#### Number of observations in 2024 for each Species X treatments ####
### === === === === === === === === === === === === ###
# 1) Add sep
d$Group <- paste(d$Species, filtered_df$Treatment, sep="__SEP__")

# 2) Keep only the unique (ID, Group, Phenostage) combinations
df_unique <- unique(d[, c("ID","Group","Phenostage")])

# 3) Build a contingency table: rows = Group; cols = Phenostage
tab <- table(df_unique$Group, df_unique$Phenostage)

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

write.csv(res, "analyses/output/2024nobservations.csv", row.names = FALSE)

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
ggsave("analyses/figures/heatmap.pdf", plot = heatmap, width = 8, height = 6)
ggsave("analyses/figures/heatmap.jpeg", plot = heatmap)

### === === === === === === === === === === === === ###
#### Shoot elongation X Budburst and budset ####
### === === === === === === === === === === === === ###
# mean and sd for each phenophase X Species X Treatment
summary_stats <- aggregate(
  DOY ~ phenophaseText + Species + Treatment, 
  data = d, 
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
w