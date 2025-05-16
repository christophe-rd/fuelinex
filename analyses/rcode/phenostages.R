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
# Load librair1ies
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

## at some point I should add this function which selects rows after the same phenophase was recorded 3 times. Replace NumYs_in_Series with my phenophases
# d <- d[(d$NumYs_in_Series>=3),] 

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder 
directory <-"/Users/christophe_rouleau-desrochers/github/fuelinex/"
setwd(directory)
list.files()

# Read data
phenostages <- read.csv2("analyses/input/2024BudburstToBudset.csv", header = TRUE, sep = ",", check.names = FALSE)
head(phenostages)
str(phenostages)

# change notes column name
colnames(phenostages)[6] <- "notes"

# first, standardize the note column
# clean the spaces and maybe later other stuff 
phenostages$Notes <- gsub("doy (\\d+)", "doy\\1", phenostages$notes)
# manual cleaning for format standardization
phenostages$Notes[which(phenostages$Notes == "116: stage 3 not 4")] <- "doy116: stage 3 not 4"
phenostages$Notes[which(phenostages$Notes == "116: stage 2 not 3")] <- "doy116: stage 2 not 3"
phenostages$Notes[which(phenostages$Notes == "116: stage 1 not 2")] <- "doy116: stage 1 not 2"
phenostages$Notes[which(phenostages$Notes == "probably dead. Doy137: main shoot dead, sprouting from bottom, no phenomonitoring")] <- "probably dead. doy137: main shoot dead, sprouting from bottom, no phenomonitoring"
phenostages$Notes[which(phenostages$Notes == "116: main stem 2, smaller branch 4; doy182: apical shoot almost dead")] <- "doy116: main stem 2, smaller branch 4; doy182: apical shoot almost dead"
phenostages$Notes[which(phenostages$Notes == "116: stage 3 not 4; doy207; one side of the bud is still green")] <- "doy116: stage 3 not 4; doy207: one side of the bud is still green"
phenostages$Notes[which(phenostages$Notes == "doy207: apical shoot ate doy221: apical shoot dead")] <- "doy207: apical shoot ate; doy221: apical shoot dead"
phenostages$Notes[which(phenostages$Notes == "doy262:bud bursting again")] <- "doy262:bud bursting again"
# add doyNA: when there is no doy associated with the note
phenostages$Notes[which(phenostages$Notes == "dead")] <- "doyNA: dead"
phenostages$Notes[which(phenostages$Notes == "probably dead. doy137: main shoot dead, sprouting from bottom, no phenomonitoring")] <- "doyNA: probably dead; doy137: main shoot dead, sprouting from bottom, no phenomonitoring"
phenostages$Notes[which(phenostages$Notes == "main branch dead")] <- "doyNA: main branch dead"
phenostages$Notes[which(phenostages$Notes == "main 2. lateral 4; doy199: side shoot observed")] <- "doyNA: main 2; lateral 4; doy199: side shoot observed"
phenostages$Notes[which(phenostages$Notes == "main maybe dead")] <- "doyNA: main maybe dead"
phenostages$Notes[which(phenostages$Notes == "terminal buds seem dead; lateral 3")] <- "doyNA: terminal buds seem dead. lateral 3"
phenostages$Notes[which(phenostages$Notes == "dead--dendrometer switch it?")] <- "doyNA: dead--dendrometer switch it?"
phenostages$Notes[which(phenostages$Notes == "main 2; lateral 4; doy199: side shoot observed")] <- "doyNA: main 2, lateral 4; doy199: side shoot observed"
phenostages$Notes[which(phenostages$Notes == "dead; side shoots sprouting")] <- "doyNA: dead. side shoots sprouting" 
phenostages$Notes[which(phenostages$Notes == "probably dead; doy207: ijbol it's missing")] <- "doyNA: probably dead; doy207: ijbol it's missing" 

#replace doyNA by doy000 so it's numerical
phenostages$Notes <- ifelse(phenostages$Notes == "" | is.na(phenostages$Notes), phenostages$Notes, gsub("doyNA", "doy000", phenostages$Notes))



extract_notes <- function(notes) {
  if (notes == "") {
    return(NULL)
  }
  # Split the notes by semicolon to handle multiple DOY entries
  note_parts <- strsplit(notes, ";")[[1]]
  # Extract DOY and note for each part
  doy_notes <- lapply(note_parts, function(part) {
    doy <- sub(".*doy(\\d+):.*", "\\1", part)
    note <- sub(".*doy\\d+: ", "", part)
    return(data.frame(DOY = doy, Note = note, stringsAsFactors = FALSE))
  })
  return(do.call(rbind, doy_notes))
}
# Apply the function to each row and combine results
notes_list <- lapply(phenostages$Notes, extract_notes)
# Handle NULL values in notes_list
notes_list <- lapply(notes_list, function(x) if (is.null(x)) data.frame(DOY = NA, Note = NA, stringsAsFactors = FALSE) else x)
# Combine into a single data frame
notes_df <- do.call(rbind, notes_list)
# Add tree_ID to the notes data frame
notes_df$tree_ID <- rep(phenostages$tree_ID, sapply(notes_list, function(x) nrow(x)))
# Pivot the data frame to wide format
wide_notes <- reshape(notes_df, idvar = "tree_ID", timevar = "DOY", direction = "wide")
# Clean up column names
colnames(wide_notes) <- gsub("Note\\.", "", colnames(wide_notes))
#remove na column
wide_notes <- wide_notes[,c(1:2,4:ncol(wide_notes))]

# Function to convert all doy columns to numeric without having tons of warnings: 
#source: https://stackoverflow.com/questions/32846176/applying-as-numeric-only-to-elements-of-a-list-that-can-be-coerced-to-numeric-i
convert_to_numeric <- function(x) {
  as.numeric(ifelse(grepl("^[0-9]+$", x), x, NA))
}
# # Convert to numeric
for (i in 7:ncol(phenostages)) {
  phenostages[, i] <- convert_to_numeric(phenostages[, i]) # at some point I should make sure I am not loosing any values
}

# convert to long format
phenolong <- phenostages %>%
  pivot_longer(
    cols = -c(tree_ID, bloc, treatment, genus, species, notes, Notes),
    names_to = "DOY",
    values_to = "Phenostage",
  ) %>%
  unite("ID_DOY", tree_ID, DOY, sep = "_") %>%  
  select(ID_DOY, Phenostage)                  

# convert notes wide df to long format
noteslong <- wide_notes %>%
  pivot_longer(
    cols = -tree_ID,  
    names_to = "DOY", 
    values_to = "Notes"  
  ) %>%
  unite("ID_DOY", tree_ID, DOY, sep = "_") %>%  
  select(ID_DOY, Notes) 

# merge phenostage+notes
phenoNotes <- merge(phenolong, noteslong, by = "ID_DOY", all.x = TRUE)
# separate again the doy and id
phenoNotes$ID <- sub("(_\\d+)$", "", phenoNotes$ID_DOY)  
phenoNotes$DOY <- sub(".*_(\\d+)$", "\\1", phenoNotes$ID_DOY) 
phenoNotes$Species <- sub("^([A-Za-z]+)_.*", "\\1", phenoNotes$ID_DOY)
phenoNotes$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", phenoNotes$ID_DOY)

# Append "_nitro" if "nitro" appears anywhere in the ID_DOY
phenoNotes$Treatment <- ifelse(grepl("_nitro", phenoNotes$ID_DOY), 
                               paste0(phenoNotes$Treatment, "_nitro"), 
                               phenoNotes$Treatment)
unique(phenoNotes$Treatment)

# Now, to avoid mixing up autumn and spring phenophases, I will change the phenophase 0 to 7 which corresponds to the last stage of budset.
phenoNotes$Phenostage[phenoNotes$DOY > 176 & phenoNotes$Phenostage == 0] <- 7
# phenostages textual description
phenophase_labels <- c(
  "Bud dormant", #0
  "Bud swollen", #1
  "Bud opened, leaves visible but curly", #2
  "Leaf out but not completely unfolded", #3
  "Leaf fully unfolded", #4
  "Bud start to set, still some green", #5
  "Bud changes color to red or green", #6
  "Bud is fully set" #7
)
phenoNotes$phenophaseText <- phenophase_labels[phenoNotes$Phenostage + 1]

phenoNOna <- phenoNotes[!is.na(phenoNotes$Phenostage),]

# Select for each replicate one value each
# Convert DOY to numeric
phenoNOna$DOY <- as.numeric(phenoNOna$DOY)
# Sort data by ID and DOY
phenoNOna <- phenoNOna[order(phenoNOna$ID, phenoNOna$DOY), ]
phenoNOna_filtered <- phenoNOna[!duplicated(phenoNOna[c("ID", "Phenostage")]), ]


phenoNOna_filtered$Group <- paste(phenoNOna_filtered$Species, phenoNOna_filtered$Treatment, sep="__SEP__")

# 2) Keep only the unique (ID, Group, Phenostage) combinations
df_unique <- unique(phenoNOna_filtered[, c("ID","Group","Phenostage")])

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

write.csv(res, "analyses/output/nobservations.csv", row.names = FALSE)

df_long <- pivot_longer(
  res,
  cols = starts_with("Stage_"),
  names_to = "Stage",
  names_prefix = "Stage_",
  values_to = "Count"
)


ggplot(df_long, aes(x = Stage, y = Treatment, fill = Count)) +
  geom_tile(color = "white") +
  facet_wrap(~ Species) +
  scale_alpha(range = c(0.1, 1)) +  # lower = more transparent, upper = more opaque
  labs(
    title = "Phenophase Heatmap",
    x = "Phenophase Stage",
    y = "Species",
    fill = "Stage",
    alpha = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary_stats <- aggregate(
  DOY ~ phenophaseText + Species + Treatment, 
  data = phenoNOna_filtered, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)
summary_stats$mean_DOY <- summary_stats$DOY[, "mean"]
summary_stats$sd_DOY <- summary_stats$DOY[, "sd"]
# summary_stats$DOY <- NULL  # Remove the old column

# for now, just take dormant and unroled
vec <- c("Leaf fully unfolded", "Bud is fully set")
suby <- summary_stats[summary_stats$phenophaseText %in% vec, ]
# and select only for growing season extension treatments
vectreat <- c("CoolS/CoolF", "WarmS/CoolF", "CoolS/WarmF", "WarmS/WarmF")
suby <- suby[suby$Treatment %in% vectreat, ]

#create small df for shoot elongation measurements
shootperiods <- data.frame(species=c("Acne", "Bepa", "Pist", "Poba", "Prvi", "Quma"), start = c(128,128,130,150,128,143), end=c(157,157,234,196,180,206))

# Create the plot
# Sample data for shoot elongation periods

# Plot

ggplot(suby, aes(x = mean_DOY, y = Treatment, color = phenophaseText)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_errorbarh(aes(xmin = mean_DOY - sd_DOY, xmax = mean_DOY + sd_DOY), 
                 height = 0.2, alpha = 0.5, linewidth = 0.6) + 
  # Add shoot elongation period lines (aligned with species)
  geom_segment(data = shootperiods, aes(x = start, xend = end, y = 4.5, yend = 4.5),
               color = "black", linewidth = 1, linetype = "dashed") +
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
##### right now there is a problem with quma because they flushed late so i need to distinguish between spring dormant and bud is completely set.









# Subset for species name
acne <- subset(phenostages, genus =="acer")
bepa <- subset(phenostages, genus =="betula")
poba <- subset(phenostages, genus =="populus")
prvi <- subset(phenostages, genus =="prunus")
pist <- subset(phenostages, genus =="pinus")
quma <- subset(phenostages, genus =="quercus")

#### ACNE ####
head(acne)
str(acne)
# Convert the data frame from wide to long format
# Convert the data frame from wide to long format

# View the result
head(long_df)
acnecut <- wide_notes[1:50, c(1,6)]
dput(acnecut)
# first clean the note column

unique(acnecut$Notes)



# View the r

acne_last_measurement<- acne[, "242"]
bepa_last_measurement<- bepa[, "242"]
prvi_last_measurement<- prvi[, "242"]
pist_last_measurement<- pist[, "242"]
quma_last_measurement<- quma[, "242"]
poba_last_measurement<- poba[, "242"]

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
