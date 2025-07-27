## R cheat sheet with useful stuff in it.

# when converting to numeric 
# if I have 
# always use as.numeric(as.character()). It's a safety measure, otherwise if I convert to numeric as.numeric something bad will happen
### Look up differences between two data frames
setdiff(df1$replicates, df2$replicates)

### Add columns with groups of a specific column e.g. add each phenophase column:
phenos<-phenos%>%tidyr::spread(phase, doy)

### Count the number of time each id is present in a df
nbobsperID <- d %>% count(plantNickname)

### Extracting Characters from a specific position
cg18$species<-substr(cg18$Ind, 0,6) # here $species will return the characters from the position 0 to 6a