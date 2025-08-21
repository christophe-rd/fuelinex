# 20 May 2024
# CRD
# Goal is to clean phenology observations for 2024 from budburst to budset

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librair1ies
library(ggplot2)
library(tidyverse)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder 
# directory <-"/Users/christophe_rouleau-desrochers/github/fuelinex/"
# setwd(directory)
# list.files()

# Read data
phenostages <- read.csv2("input/2024BudburstToBudset.csv", header = TRUE, sep = ",", check.names = FALSE)
head(phenostages)
str(phenostages)

# change notes column name
colnames(phenostages)[6] <- "notes"

# first, standardize the note column
# clean the spaces and maybe later other stuff 
phenostages$Notes <- gsub("doy (\\d+)", "doy\\1", phenostages$notes)
# manual cleaning for format standardization
phenostages$Notes[which(phenostages$notes == "116: stage 3 not 4")] <- "doy116: stage 3 not 4"
phenostages$Notes[which(phenostages$notes == "116: stage 2 not 3")] <- "doy116: stage 2 not 3"
phenostages$Notes[which(phenostages$notes == "116: stage 1 not 2")] <- "doy116: stage 1 not 2"
phenostages$Notes[which(phenostages$notes == "probably dead. Doy137: main shoot dead, sprouting from bottom, no phenomonitoring")] <- "probably dead. doy137: main shoot dead, sprouting from bottom, no phenomonitoring"
phenostages$Notes[which(phenostages$notes == "116: main stem 2, smaller branch 4; doy182: apical shoot almost dead")] <- "doy116: main stem 2, smaller branch 4; doy182: apical shoot almost dead"
phenostages$Notes[which(phenostages$notes == "116: stage 3 not 4; doy 207; one side of the bud is still green")] <- "doy116: stage 3 not 4; doy207: one side of the bud is still green"
phenostages$Notes[which(phenostages$notes == "doy207: apical shoot ate doy221: apical shoot dead")] <- "doy207: apical shoot ate; doy221: apical shoot dead"
phenostages$Notes[which(phenostages$notes == "doy262:bud bursting again")] <- "doy262:bud bursting again"
# add doyNA: when there is no doy associated with the note
phenostages$Notes[which(phenostages$notes == "dead")] <- "doyNA: dead"
phenostages$Notes[which(phenostages$notes == "probably dead. doy137: main shoot dead, sprouting from bottom, no phenomonitoring")] <- "doyNA: probably dead; doy137: main shoot dead, sprouting from bottom, no phenomonitoring"
phenostages$Notes[which(phenostages$notes == "main branch dead")] <- "doyNA: main branch dead"
phenostages$Notes[which(phenostages$notes == "main 2. lateral 4; doy199: side shoot observed")] <- "doyNA: main 2; lateral 4; doy199: side shoot observed"
phenostages$Notes[which(phenostages$notes == "main maybe dead")] <- "doyNA: main maybe dead"
phenostages$Notes[which(phenostages$notes == "terminal buds seem dead; lateral 3")] <- "doyNA: terminal buds seem dead. lateral 3"
phenostages$Notes[which(phenostages$notes == "dead--dendrometer switch it?")] <- "doyNA: dead--dendrometer switch it?"
phenostages$Notes[which(phenostages$notes == "dead; side shoots sprouting")] <- "doyNA: dead. side shoots sprouting" 
phenostages$Notes[which(phenostages$notes == "probably dead; doy 207: ijbol it's missing")] <- "doyNA: probably dead" 
phenostages$Notes[which(phenostages$notes == "main 2; lateral 4; doy 199: side shoot observed")] <- "doy199: side shoot observed"

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
for (i in 7:ncol(phenostages)-1) {
  phenostages[, i] <- convert_to_numeric(phenostages[, i]) # at some point I should make sure I am not loosing any values
}

# Convert to long
idcols <- c("tree_ID", "bloc", "treatment", "genus", "species", "notes", "Notes")
# cols to select
measure_cols <- setdiff(names(phenostages), idcols)

# Reshape to long
phenolong <- reshape(
  phenostages,
  varying = measure_cols,
  v.names = "Phenostage",
  timevar = "DOY",
  times = measure_cols,
  idvar = "tree_ID",
  direction = "long"
)

# Create ID_DOY
phenolong$ID_DOY <- paste(phenolong$tree_ID, phenolong$DOY, sep = "_")

# select cols
phenolong <- phenolong[, c("ID_DOY", "tree_ID", "bloc", "treatment", "genus", "species", "DOY", "Phenostage")]

rownames(phenolong) <- NULL


# convert notes wide df to long format
idcol <- "tree_ID"
measure_cols <- setdiff(names(wide_notes), idcol)
# reshape to long format
long_notes <- reshape(
  wide_notes,
  varying = measure_cols,
  v.names = "Notes",
  timevar = "DOY",
  times = measure_cols,
  idvar = "tree_ID",
  direction = "long"
)
# ID_DOY 
long_notes$ID_DOY <- paste(long_notes$tree_ID, long_notes$DOY, sep = "_")
# select only ID_DOY and Notes
longnotes24 <- long_notes[, c("ID_DOY", "Notes")]
rownames(longnotes24) <- NULL

# merge phenostage+notes
phenoNotes <- merge(phenolong, longnotes24, by = "ID_DOY", all.x = TRUE)

# Now, to avoid mixing up autumn and spring phenophases, I will change the phenophase 0 to 7 which corresponds to the last stage of budset.
phenoNotes$Phenostage[phenoNotes$DOY > 176 & phenoNotes$Phenostage == 0] <- 7
# phenostages textual description
phenophase_labels <- c(
  "Bud dormant", #0
  "Bud swollen", #1
  "Bud opened, leaves visible but curly", #2
  "Leaf out but not completely unfolded", #3
  "Leaf fully unfolded", #4
  "Bud starts to set, still green", #5
  "Bud changes color to red or green", #6
  "Bud is fully set" #7
)

phenoNotes$phenophaseText <- NA

# validity indices i.e. when NA or if its not between 0 and 7
valid <- !is.na(phenoNotes$Phenostage) & phenoNotes$Phenostage %in% 0:7

# Assign where valid
phenoNotes$phenophaseText[valid] <- phenophase_labels[phenoNotes$Phenostage[valid] + 1] # matching phenophase to previous labels only when valid

# Removing rows with NAs
phenoNOna <- phenoNotes[!is.na(phenoNotes$Phenostage),]

# Select for each replicate one value each
# Convert DOY to numeric
phenoNOna$DOY <- as.numeric(phenoNOna$DOY)

# now function to select only 3 observations in a row for a single phase and for each ID
filter_phenostage <- function(df) {
  df <- df[order(df$DOY), ]  # order by DOY
  keep <- logical(nrow(df))  # initialize to FALSE
  current_phase <- NA
  count <- 0
  
  for (i in seq_len(nrow(df))) {
    phase <- df$Phenostage[i]
    
    if (is.na(phase)) {
      keep[i] <- TRUE  # keep NAs
      next
    }
    
    if (!is.na(current_phase) && phase == current_phase) {
      count <- count + 1
    } else {
      current_phase <- phase
      count <- 1
    }
    
    if (count <= 3) {
      keep[i] <- TRUE
    }
  }
  
  return(df[keep, ])
}
# apply function to df
d <- do.call(rbind, lapply(split(phenoNOna, phenoNOna$tree_ID), filter_phenostage))
rownames(d) <- NULL

# Change rows order and remove the ones I don't want anymore
d$phenostageNum <- as.numeric(as.character(d$Phenostage))

# add year column
d$Year <- 2024
d <- subset(d, select = c(
  "tree_ID", 
  "bloc", 
  "treatment",
  "genus",
  "species",
  "Year",
  "DOY",
  "phenostageNum",
  "phenophaseText", 
  "Notes"))

# rename 
phenostage24 <- d

write_csv2(d, "output/2024PhenostageCleaned.csv")

