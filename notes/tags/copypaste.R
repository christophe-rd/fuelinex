setwd("/Users/christophe_rouleau-desrochers/Library/Mobile Documents/com~apple~CloudDocs/UBC/fuelinex/planning/tags")
list.files()
library(readxl)
d<-read_excel("check_list_tags.xlsx")
d$new <- paste(d$Species...1, sep="_", d$Treatment, d$Bloc, d$Replicate)
d$nutrient <- paste(d$Species...1, sep="_", d$Treatment, d$Bloc, d$Replicate, d$`Nutrient addition`)

write.csv(d, "tablepasted.csv")
