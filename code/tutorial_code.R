##



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



