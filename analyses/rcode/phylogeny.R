library(phytools)

setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/")
phy.plants<-read.tree("input/ALLMB.tre")

# Fuelinex
fu <- read.csv("input/2024BudburstToBudset.csv")
fu$latbicor <- sub("^(.)", "\\U\\1", fu$species, perl = TRUE)

# Treespotters
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
ts <- read.csv("output/cleanTS.csv")
ts$latbicor <- gsub(" ", "_", ts$latbi)


# Wildchrokie
setwd("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/")
wc <- read.csv("output/empiricalDataMAIN.csv")
wc$latbicor <- gsub(" ", "_", wc$latbi)



# === === ===
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses/")

# Fuelinex
fu_smallTree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% unique(fu$latbicor)))
smallnamesphy <- fu_smallTree$tip.label
fu_smallTree$root.edge <- 0
is.rooted(fu_smallTree)
fu_smallTree$node.label<-NULL

pdf("figures/phylogenies/phyloFuelinex.pdf", width = 6, height = 9)
mycol <- c("black", "red")
plot(fu_smallTree, cex = 1.5, tip.color = mycol, direction = "downwards")
dev.off()

# Treespotters
ts_smallTree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% unique(ts$latbicor)))
smallnamesphy <- ts_smallTree$tip.label
ts_smallTree$root.edge <- 0
is.rooted(ts_smallTree)
ts_smallTree$node.label<-NULL

pdf("figures/phylogenies/phyloTS.pdf", width = 7, height = 7)
mycol <- c("black", "red")
plot(ts_smallTree, cex = 1.5, tip.color = mycol, direction = "downwards")
dev.off()

# Wildchrokie
wc_smallTree <- keep.tip(phy.plants, which(phy.plants$tip.label %in% unique(wc$latbicor)))
smallnamesphy <- wc_smallTree$tip.label
wc_smallTree$root.edge <- 0
is.rooted(wc_smallTree)
wc_smallTree$node.label<-NULL

pdf("figures/phylogenies/phyloWC.pdf", width = 7, height = 7)
mycol <- c("black", "red")
plot(wc_smallTree, cex = 1.5, tip.color = mycol, direction = "downwards")
dev.off()
