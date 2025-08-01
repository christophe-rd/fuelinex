# 26 June 2024
# CRD
# Goal is to clean shoot elongation measurements for 2024
# *updated on 22 July 2025 and deleted a bunch of stuff so the code can make it in the cleanall file

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder  
directory_path <- "/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/"

# Set Working Directory
setwd(directory_path)

# Load librairies
library(ggplot2)
library(dplyr)

# Read csv
shoot <- read.csv2("input/2024ShootElongation.csv", header = TRUE, sep = ",", check.names = FALSE)
colnames(shoot)[6] <- "notes"

#=== === === === === === === ===
#### Shoot elongation 2024  ####
#=== === === === === === === ===
# Convert all chr values that should be numeric
for (i in 7:length(colnames(shoot))) {
  shoot[,i] <- as.numeric(shoot[,i])
}

# Check the notes column
unique(shoot$Notes)
#=== === === === === === === === === === === === === === === === === === === ===
# Clean the notes Column #
#=== === === === === === === === === === === === === === === === === === === ===
shoot$Notes <- gsub("doy (\\d+)", "doy\\1", shoot$notes)
# clean the spaces and maybe later other stuff
shoot$Notes <- gsub("doy (\\d+)", "doy\\1", shoot$notes)
# check which note doesn't have a doy associated with it and add doy NA for these cases
indices_to_fix <- which(!grepl("^doy\\d+:", shoot$Notes) & shoot$Notes != "")

# add doy Na in front of the following list
for (i in indices_to_fix) {
  shoot$Notes[i] <- paste0("doy000: ", shoot$Notes[i])
}

# manual cleaning for format standardization
shoot$Notes[which(shoot$notes == "doy164:(discrepancy)")] <- "doy164: (discrepancy)"

# === === ===  === === ===  === === ===  === === ===  === === ===  === === ===
# for now i comment the note manipulation as I need to make sure they are clean first
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
notes_list <- lapply(shoot$Notes, extract_notes)
# Handle NULL values in notes_list
notes_list <- lapply(notes_list, function(x) if (is.null(x)) data.frame(DOY = NA, Note = NA, stringsAsFactors = FALSE) else x)
# Combine into a single data frame
notes_df <- do.call(rbind, notes_list)
# Add tree_ID to the notes data frame
notes_df$tree_ID <- rep(shoot$tree_ID, sapply(notes_list, function(x) nrow(x)))
# Pivot the data frame to wide format
wide_notes <- reshape(notes_df, idvar = "tree_ID", timevar = "DOY", direction = "wide")
# Clean up column names
colnames(wide_notes) <- gsub("Note\\.", "", colnames(wide_notes))
# remove na column
wide_notes <- wide_notes[,c(1:2,4:ncol(wide_notes))]

# convert to long format
longshoot <- shoot %>%
  pivot_longer(
    cols = -c(tree_ID, bloc, treatment, genus, species, notes, Notes),
    names_to = "DOY",
    values_to = "ShootElongation"
  ) %>%
  unite("ID_DOY", tree_ID, DOY, sep = "_") %>%  
  select(ID_DOY, ShootElongation)

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
longshoot <- merge(longshoot, noteslong, by = "ID_DOY", all.x = TRUE)

# separate again the doy and id
longshoot$ShootElongation <- as.numeric(longshoot$ShootElongation)
longshoot$ID <- sub("(_\\d+)$", "", longshoot$ID_DOY)  
longshoot$DOY <- sub(".*_(\\d+)$", "\\1", longshoot$ID_DOY) 
longshoot$Species <- sub("^([A-Za-z]+)_.*", "\\1", longshoot$ID_DOY)
longshoot$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", longshoot$ID_DOY)

# Append "_nitro" if "nitro" appears anywhere in the ID_DOY
longshoot$Treatment <- ifelse(grepl("_nitro", longshoot$ID_DOY), 
                               paste0(longshoot$Treatment, "_nitro"), 
                               longshoot$Treatment)

# remove na shoot elongation values
longshootnona <- longshoot[!is.na(longshoot$ShootElongation),] 

# find the first doy for every replicate
first_doy <- aggregate(DOY ~ ID, data = longshootnona, FUN = min)
first_values <- merge(longshootnona, first_doy, by = c("ID", "DOY"))
first_values <- first_values[, c("ID", "ShootElongation")]
colnames(first_values) <- c("ID", "First_ShootElongation")
# merge back to have first measurement in df
mergedshoot <- merge(longshootnona, first_values, by = "ID") 
mergedshoot$adjustedShootElong <- mergedshoot$ShootElongation - mergedshoot$First_ShootElongation

# rename file
shootelongation2024 <- mergedshoot

