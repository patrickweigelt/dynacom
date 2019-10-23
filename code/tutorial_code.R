### Clean workspace
rm(list=ls())




# load plots
plots <- read.csv("data/WAmazonian_Palms_transect.csv")


plot(plots$Longitude, plots$Latitude)



# load species per plot

specsbysites_long <- read.csv("data/WAmazonian_Palms_species.csv", stringsAsFactors = FALSE)


# simplify data: get rid of transect subunits and infraspecific taxon names
#specsbysites_long <- unique(specsbysites_long[,c("Transect", "Genus", "Species", "Growth_class")])  # don't take unique to keep abundance
specsbysites_long <- specsbysites_long[,c("Transect", "Genus", "Species", "Growth_class")]


species <- unique(specsbysites_long[,c("Genus", "Species")])
species$Species_full <- paste(species$Genus, species$Species, sep=" ")
nrow(species)

nrow(species[which(species$Species!="sp"),])
length(which((species$Species!="sp")))
sum(species$Species!="sp")


# load traits
traits <- read.table("data/PalmTraits_1.0.txt", stringsAsFactors = FALSE, sep= "\t", header = TRUE)

# check for missing species
species$Species_full[which(!species$Species_full %in% traits$SpecName & species$Species != "sp")]

# "Aiphanes truncata" is a synonym of Aiphanes horrida according to theplantlist.org
# "Geonoma longepedunculata" is spelled "Geonoma longipedunculata" in the traits table and this is correct according to theplantlist.org

specsbysites_long$Species[which(specsbysites_long$Genus == "Aiphanes" & specsbysites_long$Species == "truncata")] <- "horrida"
specsbysites_long$Species[which(specsbysites_long$Genus == "Geonoma" & specsbysites_long$Species == "longepedunculata")] <- "longipedunculata"

species <- unique(specsbysites_long[,c("Genus", "Species")])
species$Species_full <- paste(species$Genus, species$Species, sep=" ")
nrow(species)

species$Species_full[which(!species$Species_full %in% traits$SpecName & species$Species != "sp")]


traits <- traits[which(traits$SpecName %in% species$Species_full),]
nrow(traits)



# Species by sites
specsbysites_long$Species_full <- paste(specsbysites_long$Genus, specsbysites_long$Species, sep=" ")

specsbysites <- table(unique(specsbysites_long[,c("Transect", "Species_full")]))
specsbysites <- as.data.frame.matrix(specsbysites)

specsbysites_ab <- table(specsbysites_long[,c("Transect", "Species_full")])
specsbysites_ab <- as.data.frame.matrix(specsbysites_ab)


# species numbers per plot
rowSums(specsbysites)

# species numbers per plot
rowSums(specsbysites_ab)

par(mfrow=c(1,2))
hist(rowSums(specsbysites), xlab = "Species number")
hist(rowSums(specsbysites_ab), xlab = "Number of individuals")




# Hill numbers
# install.packages("iNEXT")
library(iNEXT)
# https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.html


#install.packages("remotes")
#remotes::install_github("MoBiodiv/mobr")
#library(mobr)




library(FD)
#http://www.imsbio.co.jp/RGM/R_rdfile?f=FD/man/FD-package.Rd&d=R_CC
#functional diversity


#calcluate functional diversity(
  
#calculate community weighted meandist(
  
  
 #do the same manually! with some apply function(
    
 
)
)

