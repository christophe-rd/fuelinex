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
shoot24 <- read.csv2("input/2024ShootElongation.csv", header = TRUE, sep = ",", check.names = FALSE)
colnames(shoot24)[6] <- "notes"

#=== === === === === === === ===
#### Shoot elongation 2024  ####
#=== === === === === === === ===
# Convert all chr values that should be numeric
for (i in 7:length(colnames(shoot24))) {
  shoot24[,i] <- as.numeric(shoot24[,i])
}

# Check the notes column
unique(shoot24$notes)
#=== === === === === === === === === === === === === === === === === === === ===
# Clean the notes Column #
#=== === === === === === === === === === === === === === === === === === === ===
shoot24$Notes <- shoot24$notes
shoot24$Notes <- gsub("doy (\\d+)", "doy\\1", shoot24$notes)
# clean the spaces and maybe later other stuff
shoot24$Notes <- gsub("doy (\\d+)", "doy\\1", shoot24$notes)
# manual cleaning
shoot24$Notes[which(shoot24$Notes == "dead; doy143 most branches dead, weakly resprouting at bottom")] <- "doy143: most branches dead, weakly resprouting at bottom"
shoot24$Notes[which(shoot24$Notes == "doy136 new shoot measured; previous measurement not relevant")] <- "doy136: new shoot measured. previous measurement not relevant"
shoot24$Notes[which(shoot24$notes == "doy164:(discrepancy)")] <- "doy164: (discrepancy)"
shoot24$Notes[which(shoot24$Notes == "doy192: apical shoot broken; doy199: new shoot measured, doy241: new shoot also died")] <- "doy192: apical shoot broken; doy199: new shoot measured; doy241: new shoot also died"
shoot24$Notes[which(shoot24$Notes == "didn't root; doy143 dead")] <- "doy143: dead"
shoot24$Notes[which(shoot24$notes == "doy164:(maybe last time typo)")] <- "doy164: (maybe last time typo)"
shoot24$Notes[which(shoot24$Notes == "doy171: apical shoot dead, doy192: new side shoot")] <- "doy171: apical shoot dead; doy192: new side shoot"
shoot24$Notes[which(shoot24$Notes == "not doing great. Took initial measure on side branch; doy143 apical bud is dead; doy164: (offshoot, from last branch point)")] <-"not doing great. Took initial measure on side branch; doy143: apical bud is dead; doy164: (offshoot, from last branch point)"
shoot24$Notes[which(shoot24$notes == "trunk is broken; measuring side branch" )] <- "trunk is broken. measuring side branch" 
shoot24$Notes[which(shoot24$Notes == "doy164: (taller offshoot) ; doy171: apical shoot dead")] <- "doy164: (taller offshoot); doy171: apical shoot dead"
shoot24$Notes[which(shoot24$notes == "probably dead. Doy150: not much hope anymore. No more shoot elong measures")] <- "probably dead; doy150: not much hope anymore. No more shoot elong measures"
shoot24$Notes[which(shoot24$notes == "doy185:probably dead")] <- "doy185: probably dead"
shoot24$Notes[which(shoot24$Notes == "doy206: red paint peeled off, still hanging on doy220: red paint peeled off")] <- "doy206: red paint peeled off but still hanging on; doy220: red paint peeled off"
shoot24$Notes[which(shoot24$notes == "initial measurement; looks dead but branches tender")] <-  "initial measurement. looks dead but branches tender"
shoot24$Notes[which(shoot24$notes == "initial measurement; looks dead but branches tender; doy157: dead")] <- "doy157: dead"
shoot24$Notes[which(shoot24$notes == "not doing great. Took initial measure on side branch; doy 143 apical bud is dead; doy164: (offshoot, from last branch point)")] <- "not doing great. Took initial measure on side branch; doy143: apical bud is dead; doy164: (offshoot, from last branch point)"
shoot24$Notes[which(shoot24$notes == "doy143 apical shoot probably dead; no measurement")] <- "doy143: apical shoot probably dead. no measurement"


# check which note doesn't have a doy associated with it and add doy NA for these cases
indices_to_fix <- which(!grepl("^doy\\d+:", shoot24$Notes) & shoot24$Notes != "")
# add doy Na in front of the following list
for (i in indices_to_fix) {
  shoot24$Notes[i] <- paste0("doy000: ", shoot24$Notes[i])
}

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
notes_list <- lapply(shoot24$Notes, extract_notes)
# Handle NULL values in notes_list
notes_list <- lapply(notes_list, function(x) if (is.null(x)) data.frame(DOY = NA, Note = NA, stringsAsFactors = FALSE) else x)
# Combine into a single data frame
notes_df <- do.call(rbind, notes_list)
# Add tree_ID to the notes data frame
notes_df$tree_ID <- rep(shoot24$tree_ID, sapply(notes_list, function(x) nrow(x)))
# Pivot the data frame to wide format
wide_notes <- reshape(notes_df, idvar = "tree_ID", timevar = "DOY", direction = "wide")
# Clean up column names
colnames(wide_notes) <- gsub("Note\\.", "", colnames(wide_notes))
# remove na column
wide_notes <- wide_notes[,c(1:2,4:ncol(wide_notes))]

# convert to long format
cols <- c("tree_ID", "bloc", "treatment", "genus", "species", "notes", "Notes")
measure_cols <- setdiff(names(shoot24), cols)
# reshape from wide to long
longdf24 <- reshape(
  shoot24,
  varying = measure_cols,
  v.names = "shootElongation",
  timevar = "DOY",
  times = measure_cols,
  idvar = "tree_ID",
  direction = "long"
)
# create ID_DOY
longdf24$ID_DOY <- paste(longdf24$tree_ID, longdf24$DOY, sep = "_")
# select the cols i want and remove anoying rownames
longdf24 <- longdf24[, c("ID_DOY", "shootElongation")]
rownames(longshoot24) <- NULL

### --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ###
vals <- longdf24$shootElongation[!is.na(longdf24$shootElongation)]
# Find entries that are NOT valid decimal numbers
invalid_vals <- unique(vals[!grepl("^[-]?[0-9]+(\\.[0-9]+)?$", vals)])
### --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ###

# convert notes wide df to long format
idcol <- "tree_ID"
measure_cols <- setdiff(names(wide_notes), idcol)
# reshape to long format
longnotes24 <- reshape(
  wide_notes,
  varying = measure_cols,
  v.names = "Notes",
  timevar = "DOY",
  times = measure_cols,
  idvar = "tree_ID",
  direction = "long"
)
# ID_DOY 
longnotes24$ID_DOY <- paste(longnotes24$tree_ID, longnotes24$DOY, sep = "_")
# select only ID_DOY and Notes
longnotes24 <- longnotes24[, c("ID_DOY", "Notes")]
rownames(longnotes24) <- NULL

# merge phenostage+notes
longshoot24 <- merge(longdf24, longnotes24, by = "ID_DOY", all.x = TRUE)

# separate again the doy and id
longshoot24$ShootElongation <- as.numeric(longshoot24$shootElongation)
longshoot24$ID <- sub("(_\\d+)$", "", longshoot24$ID_DOY)  
longshoot24$DOY <- sub(".*_(\\d+)$", "\\1", longshoot24$ID_DOY) 
longshoot24$Species <- sub("^([A-Za-z]+)_.*", "\\1", longshoot24$ID_DOY)
longshoot24$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", longshoot24$ID_DOY)

# Append "_nitro" if "nitro" appears anywhere in the ID_DOY
longshoot24$Treatment <- ifelse(grepl("_nitro", longshoot24$ID_DOY), 
                               paste0(longshoot24$Treatment, "_nitro"), 
                               longshoot24$Treatment)

# remove na shoot elongation values
longshootnona <- longshoot24[!is.na(longshoot24$ShootElongation),] 

# find the first doy for every replicate
first_doy <- aggregate(DOY ~ ID, data = longshootnona, FUN = min)
first_values <- merge(longshootnona, first_doy, by = c("ID", "DOY"))
first_values <- first_values[, c("ID", "ShootElongation")]
colnames(first_values) <- c("ID", "First_ShootElongation")

# merge back to have first measurement in df
mergedshoot <- merge(longshootnona, first_values, by = "ID") 
mergedshoot$adjustedShootElong <- mergedshoot$ShootElongation - mergedshoot$First_ShootElongation

# rename file
shoot2024 <- mergedshoot

