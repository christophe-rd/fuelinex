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

library(dslabs)
data(murders)
### Order by a specific column e.g. 
index <- order(murders$total) # this index returns a vector by number of murders
murders$state[index] # then by indexing this vector to the states, I have my states ordered in number of murders

#### Looks for entries in a vector and returns the entry to access them
index <- match(c("New York", "Florida", "Texas"), murders$state)

### %in%
x <- c("a", "b", "c", "d", "e", "f")
y <- c("a", "d", "f")
y %in% x

# get mean, sd, 5.5. and 94.5% intevals along with a cool hist (from rethinking package)
precis(d)

# aggregate by multiple cols
agg2 <- aggregate(d$lengthCM, by = list(d$name, d$Yearcor, d$sourceFolder), FUN = mean)

# subset columns
subset(df, select = c("col1", "col2"))

# calculate the mean of each row from one df indexed by a vector:
spp_means <- data.frame(
  "1" = rowMeans(fullintercept[, spp1vec]),
  "2" = rowMeans(fullintercept[, spp2vec]),
  "3" = rowMeans(fullintercept[, spp3vec]),
  "4" = rowMeans(fullintercept[, spp4vec])
)