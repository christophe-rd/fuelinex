# 22 July 2025
# CRD
# Goal is to clean shoot25 elongation measurements for 2025

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder  
directory_path <- "/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/"

# Set Working Directory
setwd(directory_path)

# Load librairies
library(ggplot2)
library(dplyr)
library(tidyr)

# Read csv
shoot25 <- read.csv2("input/2025shootElongation.csv", header = TRUE, sep = ",", check.names = FALSE)
colnames(shoot25)[6] <- "notes"

# start by doing some checks
for (i in 7:ncol(shoot25)) {
  # Only trim if the column is character (to avoid coercion issues)
  if (is.character(shoot25[[i]])) {
    shoot25[[i]] <- trimws(shoot25[[i]])
  }
}

#=== === === === === === === ===
#### Shoot elongation 2025  ####
#=== === === === === === === ===
# Convert all chr values that should be numeric
for (i in 7:length(colnames(shoot25))) {
  shoot25[,i] <- as.numeric(shoot25[,i])
}

# Check the notes column
unique(shoot25$notes)
#=== === === === === === === === === === === === === === === === === === === ===
# Clean the notes Column #
#=== === === === === === === === === === === === === === === === === === === ===
shoot25$Notes <- gsub("doy (\\d+)", "doy\\1", shoot25$notes)
# clean the spaces and maybe later other stuff
shoot25$Notes <- gsub("doy (\\d+)", "doy\\1", shoot25$notes)
# check which note doesn't have a doy associated with it and add doy NA for these cases
indices_to_fix <- which(!grepl("^doy\\d+:", shoot25$Notes) & shoot25$Notes != "")

# add doy Na in front of the following list
for (i in indices_to_fix) {
  shoot25$Notes[i] <- paste0("doy000: ", shoot25$Notes[i])
}

# manual cleaning for format standardization
shoot25$Notes[which(shoot25$Notes == "doy164:(discrepancy)")] <- "doy164: (discrepancy)"

shoot25$`149`[which(shoot25$tree_ID == "Acne_CoolS/CoolF_B3_R15")] <- 6.3

acne <- subset(shoot25, genus=="acer")
sub <- subset(acne, treatment == "CoolS/CoolF_nitro")
View(subset(acne, shootElongation<4))

# === === ===  === === ===  === === ===  === === ===  === === ===  === === ===
# for now i comment the note manipulation as I need to make sure they are clean first
extract_notes <- function(Notes) {
  if (Notes == "") {
    return(NULL)
  }
  # Split the Notes by semicolon to handle multiple DOY entries
  note_parts <- strsplit(Notes, ";")[[1]]
  # Extract DOY and note for each part
  doy_notes <- lapply(note_parts, function(part) {
    doy <- sub(".*doy(\\d+):.*", "\\1", part)
    note <- sub(".*doy\\d+: ", "", part)
    return(data.frame(DOY = doy, Note = note, stringsAsFactors = FALSE))
  })
  return(do.call(rbind, doy_notes))
}
# Apply the function to each row and combine results
notes_list <- lapply(shoot25$Notes, extract_notes)
# Handle NULL values in notes_list
notes_list <- lapply(notes_list, function(x) if (is.null(x)) data.frame(DOY = NA, Note = NA, stringsAsFactors = FALSE) else x)
# Combine into a single data frame
notes_df <- do.call(rbind, notes_list)
# Add tree_ID to the notes data frame
notes_df$tree_ID <- rep(shoot25$tree_ID, sapply(notes_list, function(x) nrow(x)))
# Pivot the data frame to wide format
wide_notes <- reshape(notes_df, idvar = "tree_ID", timevar = "DOY", direction = "wide")
# Clean up column names
colnames(wide_notes) <- gsub("Note\\.", "", colnames(wide_notes))
# remove na column
wide_notes <- wide_notes[,c(1:2,4:ncol(wide_notes))]

# convert to long format
cols <- c("tree_ID", "bloc", "treatment", "genus", "species", "notes", "Notes")
measure_cols <- setdiff(names(shoot25), cols)
# reshape from wide to long
long_df <- reshape(
  shoot25,
  varying = measure_cols,
  v.names = "shootElongation",
  timevar = "DOY",
  times = measure_cols,
  idvar = "tree_ID",
  direction = "long"
)
# create ID_DOY
long_df$ID_DOY <- paste(long_df$tree_ID, long_df$DOY, sep = "_")
# select the cols i want and remove anoying rownames
longshoot25 <- long_df[, c("ID_DOY", "shootElongation")]
rownames(longshoot25) <- NULL

### --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ###
vals <- longshoot25$shootElongation[!is.na(longshoot25$shootElongation)]
# Find entries that are NOT valid decimal numbers
invalid_vals <- unique(vals[!grepl("^[-]?[0-9]+(\\.[0-9]+)?$", vals)])
### --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ###

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
longnotes25 <- long_notes[, c("ID_DOY", "Notes")]
rownames(longnotes25) <- NULL

# merge phenostage+notes
longshoot25 <- merge(longshoot25, longnotes25, by = "ID_DOY", all.x = TRUE)

# separate again the doy and id
longshoot25$shootElongation <- as.numeric(longshoot25$shootElongation)
longshoot25$ID <- sub("(_\\d+)$", "", longshoot25$ID_DOY)  
longshoot25$DOY <- sub(".*_(\\d+)$", "\\1", longshoot25$ID_DOY) 
longshoot25$Species <- sub("^([A-Za-z]+)_.*", "\\1", longshoot25$ID_DOY)
longshoot25$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", longshoot25$ID_DOY)

# Append "_nitro" if "nitro" appears anywhere in the ID_DOY
longshoot25$Treatment <- ifelse(grepl("_nitro", longshoot25$ID_DOY), 
                              paste0(longshoot25$Treatment, "_nitro"), 
                              longshoot25$Treatment)

# remove na shoot25 elongation values
longshootnona25 <- longshoot25[!is.na(longshoot25$shootElongation),] 

# find the first doy for every replicate
first_doy <- aggregate(DOY ~ ID, data = longshootnona25, FUN = min)
first_values <- merge(longshootnona25, first_doy, by = c("ID", "DOY"))
first_values <- first_values[, c("ID", "shootElongation")]
colnames(first_values) <- c("ID", "First_shootElongation")

# merge back to have first measurement in df
mergedshoot25 <- merge(longshootnona25, first_values, by = "ID") 
mergedshoot25$adjustedshootElong <- mergedshoot25$shootElongation - mergedshoot25$First_shootElongation


# manual cleaning of outliers resulting of data collection mistake
run=FALSE
if(run){acne <- subset(shoot2025, Species=="Acne")
subset(acne, Treatment == "CoolS/CoolF_nitro")
View(subset(acne, shootElongation<4))
}
# rename file
shoot2025 <- mergedshoot25

