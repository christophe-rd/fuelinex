# 27 May 2025
# CRD
# Goal is to clean senescence monitoring in 2024

# Load librairies
library(ggplot2)
library(dplyr)
library(tidyr)

options(max.print = 150) 

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
senescence$Notes[which(senescence$notes == "doy276: no green leaves; doyna:last leaves fell")] <- "doy276: no green leaves" 
senescence$Notes[which(senescence$notes == "doy262:seam sick; doyna: last leaves fell")] <- "doy262: seems sick"
senescence$Notes[which(senescence$tree_ID == "Poba_WarmS/CoolF_B3_R15")] <- "doy000: three branches died because of herbivory"
senescence$Notes[which(senescence$notes == "dead")] <- "doy000: dead"

# clean data where it's necessary
senescence$`248_1`[grepl("doy248 measurement percent wrong", senescence$notes)]<- NA
senescence$`248_2`[grepl("doy248 measurement percent wrong", senescence$notes)]<- NA
senescence$`248_3`[grepl("doy248 measurement percent wrong", senescence$notes)]<- NA

senescence$`248_1`[which(grepl("doy255: previous measurement percent wrong", senescence$notes))] <- NA
senescence$`248_2`[which(grepl("doy255: previous measurement percent wrong", senescence$notes))] <- NA
senescence$`248_3`[which(grepl("doy255: previous measurement percent wrong", senescence$notes))] <- NA

senescence$`255_1`[grepl("doy255 wrong measure percent", senescence$notes)]<- NA
senescence$`255_2`[grepl("doy255 wrong measure percent", senescence$notes)]<- NA
senescence$`255_3`[grepl("doy255 wrong measure percent", senescence$notes)]<- NA

senescence$notes[which(grepl("previous measurement percent wrong", senescence$notes))]

sub <- senescence[, c(1:5, 63)]

extract_notes <- function(Notes) {
  if (Notes == "") {
    return(NULL)
  }
  # Split the notes by semicolon to handle multiple DOY entries
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
notes_list <- lapply(sub$Notes, extract_notes)

# Handle NULL values in notes_list
notes_list <- lapply(notes_list, function(x) if (is.null(x)) data.frame(DOY = NA, Note = NA, stringsAsFactors = FALSE) else x)
# Combine into a single data frame
notes_df <- do.call(rbind, notes_list)
notes_df$tree_ID <- rep(sub$tree_ID, sapply(notes_list, function(x) nrow(x)))

# change 000 to NA
notes_df$DOY[which(notes_df$DOY == "000")] <- NA

# readd columns by merge
notesmerged <- merge(notes_df, senescence[, c("tree_ID", "bloc", "treatment", "genus", "species")], by = "tree_ID", all.x = TRUE)

### === === === === === === === === === === === === ###
#### Get a column for leaf drop and greeness loss ####
### === === === === === === === === === === === === ###
# subset only for comments about loss of greeness or lost leaves
unique(notesmerged$Note)
vec <- c("no green leaves", "last leaves fell", "cant measure chl", "no leaves")
subby <- subset(notesmerged, Note %in% vec)
# Rename comments for ease of use
subby$Note[which(subby$Note == "no green leaves")] <- "greenessLoss" 
subby$Note[which(subby$Note == "cant measure chl")] <- "greenessLoss"
subby$Note[which(subby$Note == "last leaves fell")] <- "leafDrop" 
subby$Note[which(subby$Note == "no leaves")] <- "leafDrop" 

# reshape to get 3 cols
agg <- aggregate(DOY ~ tree_ID + Note + bloc + treatment + genus + species, data = subby, FUN = function(x) x[1])

leafdrop <- reshape(
  agg,
  idvar   = c("tree_ID", "bloc", "treatment", "genus", "species"),
  timevar = "Note",
  direction = "wide",
  v.names = "DOY"
)

## Rename DOY.greenessLoss -> greenessLoss, DOY.leafDrop -> leafDrop
names(leafdrop) <- sub("^DOY\\.", "", names(leafdrop))

## Keep only the columns you asked for (DOY now lives in the two note columns)
leafdrop <- leafdrop[, c("tree_ID", "bloc", "treatment", "genus", "species",
                 "greenessLoss", "leafDrop")]

names(leafdrop) <- sub("^DOY\\.", "", names(leafdrop))

leafdrop24 <- leafdrop

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
greencov <- senescence[, c(1:5, grep("_percent$", names(senescence)))]
names(greencov) <- gsub("_percent$", "", names(greencov))
          
# pivot (and remove old notes col)
vec <- c("tree_ID", "bloc", "treatment", "genus", "species", "Notes")
pivot_cols <- setdiff(names(greencov), vec)
greencovlong <- reshape(
  data = greencov,
  varying = pivot_cols,
  v.names = "greenCovPer",
  timevar = "DOY",
  times = pivot_cols,
  direction = "long"
)

rownames(greencovlong) <- NULL

# ADDITIONAL CLEANING 
# assuming it's a mistake that should have been entered in chl
greencovlong$greenCovPer[which(greencovlong$greenCovPer == "19.2")] <- NA
# assuming it's a typo that should've been 70
greencovlong$greenCovPer[which(greencovlong$greenCovPer == "79")] <- 70
greencovlong$greenCovPer[which(greencovlong$greenCovPer == "")] <- NA
greencovlong$greenCovPer[which(greencovlong$greenCovPer == "na")] <- NA

greencovlong$ID_DOY <- paste(greencovlong$tree_ID, greencovlong$DOY, sep="_")

# add notes back in greencov
greencovlongwnotes <- merge(greencovlong, noteslong, by = "ID_DOY", all.x = TRUE)

# change to numeric
greencovlongwnotes$DOY <- as.numeric(greencovlongwnotes$DOY)
greencovlongwnotes$greenCovPer <- as.numeric(greencovlongwnotes$greenCovPer)

greenleafcover24 <- greencovlongwnotes[,c("tree_ID","bloc", "treatment", "genus", "species", "DOY", "greenCovPer", "Note")]

### === === === === === === === ###
##### Chlorophyll measurements #####
### === === === === === === === ###
chl <- senescence[, c(names(senescence)[1:5], grep("^[0-9]+_[1-3]$", names(senescence), value = TRUE))]

# now pivot!
pivot_cols <- grep("^\\d{3}_\\d+$", names(chl), value = TRUE)

chlorolong <- reshape(
  chl,
  varying = pivot_cols,
  v.names = "chlValue",
  timevar = "DOY_measurement",
  times = pivot_cols,
  idvar = setdiff(names(chl), pivot_cols),
  direction = "long"
)

tmp <- do.call(rbind, strsplit(as.character(chlorolong$DOY_measurement), "_"))
chlorolong$DOY <- tmp[, 1]
chlorolong$measurement <- tmp[, 2]
rownames(chlorolong) <- NULL

chlorolong$ID_DOY <- paste(chlorolong$tree_ID, chlorolong$DOY, sep="_")

# add notes back in chl 
chlorolongwnotes <- merge(chlorolong, noteslong, by = "ID_DOY", all.x = TRUE)

# ADDITIONAL CLEANING
unique(chlorolongwnotes$chlValue)
chlorolongwnotes$chlValue[which(chlorolongwnotes$chlValue == "")] <- NA

# change to numeric
chlorolongwnotes$DOY <- as.numeric(chlorolongwnotes$DOY)
chlorolongwnotes$chlValue <- as.numeric(chlorolongwnotes$chlValue)

# reorganize columns 
chl24 <- chlorolongwnotes[,c("tree_ID","bloc", "treatment", "genus", "species", "DOY", "measurement", "chlValue", "Note")]

