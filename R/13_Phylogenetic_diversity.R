################################################################################
######################## Phylogenetic Diversity ##############################
################################################################################


#Packages

library(cluster)
library(FD)
library(vegan)
library(SYNCSA)
library(taxize)


# Loading data

comm <- read.table(file = "data/raw/cestes/comm.csv", sep = ",", header = TRUE)
traits <- read.table(file = "data/raw/cestes/traits.csv", sep = ",", header = TRUE)


# Checking dataframes

head(comm)[,1:6]
head(traits)[,1:6]

# Changing the first column as rows names

rownames(comm)[1:6]
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

head(traits)[,1:6]
rownames(traits) <- traits$Sp
traits <- traits[, -1]
head(traits)


# Species richness

richness <- vegan::specnumber(comm)
shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)
#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?



class(gow) #different classes
class(gow2)

plot(gow, gow2, asp = 1) #same values


# Rao’s quadratic entropy calculations

tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)

# Calculating FD indices with package PD

#install.packages("FD")
library(FD)
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)



