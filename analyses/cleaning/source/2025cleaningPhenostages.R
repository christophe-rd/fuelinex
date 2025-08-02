# 1 August 2025
# CRD
# Goal is to clean phenology observations for 2025 from budburst to budset

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librair1ies
library(ggplot2)
library(tidyverse)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Set the path to your directory folder 
directory <-"/Users/christophe_rouleau-desrochers/github/fuelinex/analyses"
setwd(directory)
list.files()

# Read data
phenostages <- read.csv2("input/2025BudburstToBudset.csv", header = TRUE, sep = ",", check.names = FALSE)
str(phenostages)

# change notes column name
colnames(phenostages)[6] <- "notes"

# first, standardize the note column
# clean the spaces and maybe later other stuff 
phenostages$Notes <- gsub("doy (\\d+)", "doy\\1", phenostages$notes)

# manual cleaning for format standardization
phenostages$Notes[which(phenostages$notes == "doy62: probably the replicate missing its tag. Pheno stage 1 on doy62. confirming on doy69 that this is the replicate with a missing tag.")] <- "doy62: probably the replicate missing its tag; doy62: Pheno stage 1; doy69: confirming that this is the replicate with a missing tag."
phenostages$Notes[which(phenostages$notes == "doy97:flowering bud emerging")] <- "doy97: flowering bud emerging"

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
unique(phenoNotes$Notes)

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
d <- do.call(rbind, lapply(split(phenoNOna, phenoNOna$ID), filter_phenostage))
rownames(d) <- NULL

# Change rows order and remove the ones I don't want anymore
d$phenostageNum <- d$Phenostage

# re-add bloc column
d$Bloc <- sub(".+_(B\\d+)_.*", "\\1", d$ID)
# add year column
d$Year <- 2025
d <- subset(d, select = c(
  "ID", 
  "Species", 
  "Treatment",
  "Bloc",
  "phenostageNum",
  "phenophaseText", 
  "DOY",
  "Year",
  "Notes"))

# rename 
phenostage25 <- d

write_csv2(d, "output/2025PhenostageCleaned.csv")

