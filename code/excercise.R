### excercise: Community weighted means

head(traits_FD)
specsbysites_ab[1:5,1:5]

all(row.names(traits_FD)==names(specsbysites_ab))


# For one plot and trait:
sum(traits_FD[,"MaxStemHeight_m"] * specsbysites_ab["AB0081",], na.rm = TRUE)/sum(specsbysites_ab["AB0081",], na.rm = TRUE)


# now for all:

# loop: 
CMWs <- matrix(ncol = ncol(traits_FD), nrow = nrow(specsbysites_ab), dimnames=list(rownames(specsbysites_ab), colnames(traits_FD)))
for (i in 1:ncol(CMWs)){
  print(i)
  for (k in 1:nrow(CMWs)){
    CMWs[k,i] <- sum(traits_FD[,i] * specsbysites_ab[k,], na.rm = TRUE)/sum(specsbysites_ab[k,], na.rm = TRUE)
  }
}

# function approach
cmw <- function(x,y) {
  sum(x * y, na.rm = TRUE)/sum(y, na.rm = TRUE)
}


cmw(traits_FD[,"MaxStemHeight_m"],specsbysites_ab["AB0081",])


# apply the function
apply(traits_FD, 2, function(x) cmw(x, specsbysites_ab["AB0081",]))

palms_PD <- pd.query(palm_tree_pruned, specsbysites_ab, standardize = FALSE) 

CMWs_applied <- t(apply(specsbysites_ab, 1, function(x) apply(traits_FD, 2, function(y) cmw(y, x))))

# Tidy version
specsbysites_ab$site= row.names(specsbysites_ab)
traits_FD$species= row.names(traits_FD)

CWM_df <- gather(specsbysites_ab, species, abundance, -site) %>%
  inner_join(traits_FD, by='species') %>%
  gather(trait, value, -site, -species, -abundance) %>%
  group_by(site, trait) %>%
  summarise(CWM=weighted.mean(value, abundance))




