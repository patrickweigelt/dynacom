### Clean workspace
rm(list=ls())


# This is NOT a cookbook on how to do certain analyses but rather a showcase of two things:

# 1. There are basic R functions and specialized functions from various packages that allow you
# to combine data of various kinds (communities, site characteristics, traits, phylogenies) for analyses

# 2. There are a lot of dedicated R packages to calculate ecological metrics and do analyses


# The data for todays tutorial comes from 


# palm communities (plot data):
#Balslev, H., Kristiansen, S.M. & Muscarella, R. (2019) Palm community transects and soil properties in western Amazonia. Ecology, e02841.

# palm traits:
#Kissling, W.D., Balslev, H., Baker, W.J., Dransfield, J., Göldel, B., Lim, J.Y., Onstein, R.E. & Svenning, J.-C. (2019) PalmTraits 1.0, a species-level functional trait database of palms worldwide. Scientific Data, 6, 178.

# phylogeny
#Faurby, S., Eiserhardt, W.L., Baker, W.J. & Svenning, J.-C. (2016) An all-evidence species-level supertree for the palms (Arecaceae). Molecular Phylogenetics and Evolution, 100, 57-69.




# start a new RStudio project



# libraries

library(maps)
library(plyr)
library(dplyr)
library(psych)
library(vegan)
library(FactoMineR)
library(missMDA)

# load plot information
plots <- read.csv("data/WAmazonian_Palms_transect.csv")
str(plots)
range(plots$Latitude)
range(plots$Longitude)


map('world', xlim = c(-85,-47), ylim = c(-21,13), lforce="e")
points(plots$Longitude, plots$Latitude, pch=4)
map.axes()


# load soil data for plots
soil <- read.csv("data/WAmazonian_Palms_soil.csv")


### summarize soil data at the level of plots


# base R version

# apply a function to groups of values (here plots)
moisture <- tapply(soil$moist, soil$Transect, FUN = mean)
moisture <- tapply(soil$moist, soil$Transect, FUN = function(x) mean(x, na.rm=TRUE))
moisture <- tapply(soil$moist, soil$Transect, FUN = mean, na.rm=TRUE)

# apply a function to groups of values across a whole data.frame
soils_plots <- aggregate(soil, list(soil$Transect), FUN = function(x) mean(x, na.rm=TRUE))
tail(warnings(),1)
soils_plots_1 <- aggregate(soil[,c(3:29)], list(soil$Transect), FUN = function(x) mean(x, na.rm=TRUE))
soils_plots_2 <- aggregate(soil[,c(30:ncol(soil))], list(soil$Transect), FUN = function(x) paste(unique(x[!is.na(x)]), collapse=","))

soils_plots <- join(soils_plots_1,soils_plots_2)


### many packages that help with data manipulation (plyr, reshape2, data.table, purrrlyr)
### Most widely used nowadays: dplyr
# https://dplyr.tidyverse.org/
# Also see tutorial by Nathaly Guerrero-Ramirez

soil_grouped <- group_by(soil, Transect) ### group by plot
soils_plots <- dplyr::summarize(soil_grouped, mean_moist = mean(moist, na.rm = TRUE)) ### calculate mean values by grouping

# tidy version
soils_plots <- soil %>%
  group_by(Transect) %>% 
  dplyr::summarize(mean_moist = mean(moist, na.rm = TRUE)) 

View(soils_plots)


soils_plots_1 <- summarize_at(soil_grouped, .vars=c(2:28), .funs = mean, na.rm=TRUE)
soils_plots_2 <- summarize_at(soil_grouped, .vars=c(29:31), .funs = function(x) paste(unique(x[!is.na(x)]), collapse=","))
soils_plots <- join(soils_plots_1,soils_plots_2)

# Of course there are many other options than the arithmetic mean for aggregating data (median, sd, etc). In the case of pH
# (which is -log10(H+) it would be straightforward to backtransform before calulating the mean. This could be done beforehand
# on the entire ph columns or within the function for aggregating

# beforehand
soil$H <- 10^(-soil$pH_KCl) # backtransform
soil_grouped <- group_by(soil, Transect) ### group by plot
soils_pH <- dplyr::summarize(soil_grouped, mean_H = mean(H, na.rm = TRUE)) ### calculate mean values by grouping
soils_pH$pH_KCl <- -log10(soils_pH$mean_H) # transform to pH

# within function
soil_grouped <- group_by(soil, Transect) ### group by plot
soils_pH_2 <- dplyr::summarize(soil_grouped, mean_pH_KCl = -log10(mean(10^-pH_KCl, na.rm = TRUE))) ### apply function to values by grouping

plot(soils_pH$pH_KCl, soils_pH_2$mean_pH_KCl)
all(soils_pH$pH_KCl==soils_pH_2$mean_pH_KCl, na.rm=TRUE)

plot(soils_pH$pH_KCl, soils_plots$pH_KCl)
# I am not a soil biologist: Think about how to aggregate!


soils_plots$pH_KCl <- soils_pH$pH_KCl

# join main plot data and soil data
plots <- join(plots, soils_plots, type="left")
summary(plots)




### coorelation structure


# PCA
# remove Transects with two many NAs
plots_PCA <- plots[!(is.na(plots$pH_KCl) & is.na(plots$moist) & is.na(plots$ECEC)),]
# remove Variables that have more than 19% NAs
plots_PCA <- plots_PCA[,apply(plots_PCA, 2, function(x) sum(is.na(x))<=44) ] # keep all variables with 10 or less NAs

str(plots_PCA)
summary(plots_PCA)
names(plots_PCA)
#pairs.panels(plots_PCA[,unlist(lapply(plots_PCA, is.numeric))])
pairs.panels(plots_PCA[,c(15:28)])


pca<-PCA(plots_PCA[,c(4,7,15:28)],scale.unit = TRUE, graph=FALSE)

plot(pca,choix="var")


### Impute missing values
# https://youtu.be/YDbx2pk9xNY

# Estimate the number of dimensions for the Principal Component Analysis by cross-validation
number_dim <- estim_ncpPCA(plots_PCA[,c(4,7,15:28)], ncp.min=0, ncp.max=5, scale=TRUE, method.cv="kfold")
number_dim
# impute missing values
plots_PCA_imputed<-imputePCA(plots_PCA[,c(4,7,15:28)], ncp=number_dim$ncp)
plots_PCA_imputed<-plots_PCA_imputed$completeObs
plots_PCA_imputed[is.na(plots_PCA$pH_KCl),]

pca_imputed<-PCA(plots_PCA_imputed, scale.unit=TRUE, graph=FALSE) # keep 3 dimensions (85% of variation)
plot(pca_imputed,choix="var")


# make nicer plots

# extract axis scores and eigenvalues






# load species occurrances per plot
specsbysites_long <- read.csv("data/WAmazonian_Palms_species.csv", stringsAsFactors = FALSE)


# simplify data: get rid of transect subunits and infraspecific taxon names
specsbysites_long <- specsbysites_long[,c("Transect", "Genus", "Species", "Growth_class")]

# extract unique species names and create a combined species name (Genus epithet)
species <- unique(specsbysites_long[,c("Genus", "Species")])
species$Species_full <- paste(species$Genus, species$Species, sep=" ")
nrow(species)

nrow(species[which(species$Species!="sp"),])
length(which((species$Species!="sp")))
sum(species$Species!="sp")





### better move the species by sites matrix stuff here already and do nmds and diversity metrics then the traits and FD then (Phylogeny and PD?)





# load traits
traits <- read.table("data/PalmTraits_1.0.txt", stringsAsFactors = FALSE, sep= "\t", header = TRUE)

# check for species missing in traits data 
species$Species_full[which(!species$Species_full %in% traits$SpecName & species$Species != "sp")]

# "Aiphanes truncata" is a synonym of Aiphanes horrida according to theplantlist.org
# "Geonoma longepedunculata" is spelled "Geonoma longipedunculata" in the traits table and this is correct according to theplantlist.org

# change names
specsbysites_long$Species[which(specsbysites_long$Genus == "Aiphanes" & specsbysites_long$Species == "truncata")] <- "horrida"
specsbysites_long$Species[which(specsbysites_long$Genus == "Geonoma" & specsbysites_long$Species == "longepedunculata")] <- "longipedunculata"

# Get new unique species names
species <- unique(specsbysites_long[,c("Genus", "Species")])
species$Species_full <- paste(species$Genus, species$Species, sep=" ")
length(which(species$Species != "sp"))

species$Species_full[which(!species$Species_full %in% traits$SpecName & species$Species != "sp")]
# no missing species left

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





# nmds



# Hill numbers
# install.packages("iNEXT")
library(iNEXT)
# https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.html
?iNEXT
data(bird)
str(spider)

test <- iNEXT(t(specsbysites_ab))
test$DataInfo

#install.packages("remotes")
#remotes::install_github("MoBiodiv/mobr")
#library(mobr)




library(FD)
#http://www.imsbio.co.jp/RGM/R_rdfile?f=FD/man/FD-package.Rd&d=R_CC
#functional diversity


#calcluate functional diversity(
  
#calculate community weighted meandist(
  
#do the same manually! with some apply function(
    
 



