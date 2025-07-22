# 27 May 2025
# CRD
# Goal is to clean senescence monitoring in 2024

# Load librairies
library(ggplot2)
library(dplyr)
library(tidyr)

# Set the path 
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/")

# Read csv
senescence <- read.csv2("input/2024Senescence.csv", header = TRUE, sep = ",", check.names = FALSE)

colnames(senescence)[6] <- "notes"

### === === === === ###
#### Clean notes ####
### === === === === ###
senescence$Notes <- senescence$notes
senescence$Notes[which(senescence$notes == "doy262: no green leaves; doy 276: last leaves fell")] <- "doy262: no green leaves; doy276: last leaves fell" 
senescence$Notes[which(senescence$notes == "doy262: no green leaves; doy 283: last leaves fell")] <- "doy262: no green leaves; doy283: last leaves fell" 
senescence$Notes[which(senescence$notes == "doy248:1 lateral branch dead; doy318: last leaves fell")] <- "doy248: one lateral branch dead; doy318: last leaves fell" 
senescence$Notes[which(senescence$notes == "doy248:barely dead measurement on only one leaf that fell after measure; doy255: no leaves")] <- "doy248: barely dead measurement on only one leaf that fell after measure; doy255: no leaves" 
senescence$Notes[which(senescence$notes == "doy155:POPLAR IN THE POT . I cut the stem. Roots still inside; doy290: last leaves fell")] <- "doy155: POPLAR IN THE POT . I cut the stem. Roots still inside; doy290: last leaves fell" 
senescence$Notes[which(senescence$notes == "doy276: no green leaves; doyna:last leaves fell")] <- "doy276: no green leaves; doyNA: last leaves fell" 
senescence$Notes[which(senescence$notes == "doy262:seam sick; doyna: last leaves fell")] <- "doy262: seems sick; doyNA: last leaves fell" 
ncol(senescence)
sub <- senescence[, c(1:5, 63)]

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
notes_list <- lapply(sub$Notes, extract_notes)

# Handle NULL values in notes_list
notes_list <- lapply(notes_list, function(x) if (is.null(x)) data.frame(DOY = NA, Note = NA, stringsAsFactors = FALSE) else x)
# Combine into a single data frame
notes_df <- do.call(rbind, notes_list)
notes_df$tree_ID <- rep(sub$tree_ID, sapply(notes_list, function(x) nrow(x)))

### === === === === === === === === === === === === ###
#### Get a column for leaf drop and greeness loss ####
### === === === === === === === === === === === === ###
# subset only for comments about loss of greeness or lost leaves
unique(notes_df$Note)
vec <- c("no green leaves", "last leaves fell", "cant measure chl", "no leaves")
subby <- subset(notes_df, Note %in% vec)
# Rename comments for ease of use
subby$Note[which(subby$Note == "no green leaves")] <- "greenessLoss" 
subby$Note[which(subby$Note == "cant measure chl")] <- "greenessLoss"
subby$Note[which(subby$Note == "last leaves fell")] <- "leafDrop" 
subby$Note[which(subby$Note == "no leaves")] <- "leafDrop" 

leafdrop <- subby %>%
  group_by(tree_ID, Note) %>%
  summarise(DOY = first(DOY), .groups = "drop") %>%
  pivot_wider(
    id_cols = tree_ID,
    names_from = Note,
    values_from = DOY
  )

### === === === === === === === === === ###
#### Organize chl and green leaf cover ####
### === === === === === === === === === ###

### === === === === === ###
##### Start with notes #####
### === === === === === ###
# get notes df and remove last leaves fell and no green leaves becasue they've been adressed separately
notes_df_upd <- subset(notes_df, !(Note %in% c("no green leaves", "last leaves fell")))

# remove rows that don't have notes
noteslong <- notes_df_upd[!is.na(notes_df_upd$DOY) & notes_df_upd$DOY != "", ]
# paste ID and DOY
noteslong$ID_DOY <- paste(noteslong$tree_ID, noteslong$DOY, sep = "_")
# keep only 2 col
noteslong <- noteslong[, c("ID_DOY", "Note")]

### === === === === === === === === === === === === === ###
##### Green leaf cover visual assessment in percentage #####
### === === === === === === === === === === === === === ###
# green visual assessment of green leaf cover
greencov <- senescence[, c(1:6, grep("_percent$", names(senescence)))]
names(greencov) <- gsub("_percent$", "", names(greencov))
# convert to long format
greencovlong  <- greencov %>%
  pivot_longer(
    cols = -c(tree_ID, bloc, treatment, genus, species, notes),
    names_to = "DOY",
    values_to = "greenCovPer",
  ) %>%
  unite("ID_DOY", tree_ID, DOY, sep = "_") %>%  
  select(ID_DOY, greenCovPer)                  

# add notes back in greencov
greencovlongwnotes <- merge(greencovlong, noteslong, by = "ID_DOY", all.x = TRUE)
# re-add cols
greencovlongwnotes$ID <- sub("(_\\d+)$", "", greencovlongwnotes$ID_DOY)  
greencovlongwnotes$DOY <- sub(".*_(\\d+)$", "\\1", greencovlongwnotes$ID_DOY) 
greencovlongwnotes$Species <- sub("^([A-Za-z]+)_.*", "\\1", greencovlongwnotes$ID_DOY)
greencovlongwnotes$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", greencovlongwnotes$ID_DOY)
greencovlongwnotes$Replicate <- sub(".*_R(\\d+)_.*", "\\1", greencovlongwnotes$ID_DOY)
# reorganize columns 
greencovlongwnotes <- greencovlongwnotes[,c("ID","Species", "Treatment", "Replicate", "Note", "DOY", "greenCovPer")]
# change to numeric
greencovlongwnotes$DOY <- as.numeric(greencovlongwnotes$DOY)
greencovlongwnotes$greenCovPer <- as.numeric(greencovlongwnotes$greenCovPer)
### === === === === === === === ###
##### Chlorophyll measurements #####
### === === === === === === === ###
chl <- senescence[, c(names(senescence)[1:6], grep("^[0-9]+_[1-3]$", names(senescence), value = TRUE))]
# convert chl measurements to long df. What this does is that it grabs each colname, gets the 3 digits that are the DOY, get the number after sep _ which is the measurement number and converts the df to long format
chlorolong <- chl %>%
  pivot_longer(
    cols = matches("^\\d{3}_\\d+$"),  # select only columns like 248_1, 255_2, etc.
    names_to = c("DOY", "measurement"),
    names_sep = "_",
    values_to = "chlValue"
  ) %>%
  mutate(
    ID_DOY = paste(tree_ID, DOY, sep = "_")
  ) %>%
  select(ID_DOY, measurement, chlValue)
# add notes back in chl 
chlorolongwnotes <- merge(chlorolong, noteslong, by = "ID_DOY", all.x = TRUE)

# re-add cols
chlorolongwnotes$ID <- sub("(_\\d+)$", "", chlorolongwnotes$ID_DOY)  
chlorolongwnotes$DOY <- sub(".*_(\\d+)$", "\\1", chlorolongwnotes$ID_DOY) 
chlorolongwnotes$Species <- sub("^([A-Za-z]+)_.*", "\\1", chlorolongwnotes$ID_DOY)
chlorolongwnotes$Treatment <- sub("^[^_]+_([^_]+)_B\\d+.*", "\\1", chlorolongwnotes$ID_DOY)
chlorolongwnotes$Replicate <- sub(".*_R(\\d+)_.*", "\\1", chlorolongwnotes$ID_DOY)
# reorganize columns 
chlorolongwnotes <- chlorolongwnotes[,c("ID","Species", "Treatment", "Replicate", "Note", "DOY", "measurement", "chlValue")]
# change to numeric
chlorolongwnotes$DOY <- as.numeric(chlorolongwnotes$DOY)
chlorolongwnotes$chlValue <- as.numeric(chlorolongwnotes$chlValue)
# rename df for something better
leafdrop24 <- leafdrop
greenleafcover24 <- greencovlongwnotes
chl24 <- chlorolongwnotes
