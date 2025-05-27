## R cheat sheet with useful stuff in it.

### Look up differences between two data frames
setdiff(df1$replicates, df2$replicates)

### Add columns with groups of a specific column e.g. add each phenophase column:
phenos<-phenos%>%tidyr::spread(phase, doy)

### Count the number of time each id is present in a df
nbobsperID <- d %>% count(plantNickname)

### Extracting Characters from a specific position
cg18$species<-substr(cg18$Ind, 0,6) # here $species will return the characters from the position 0 to 6