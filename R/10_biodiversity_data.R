##########################################################################################
################################ Biodiversity databases ##################################
##########################################################################################

# Loading Packages

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
library(dplyr)

# Getting data

species <- "Myrsine coriacea"
occs <- occ_search(scientificName = species,
                   limit = 100000)
names(occs)

myrsine.data <- occs$data
colnames(myrsine.data)


#  Exporting dataset
dir.create("data/raw/", recursive = TRUE)
write.csv(myrsine.data,
          "data/raw/myrsine_data.csv",
          row.names = FALSE)

# Checking species taxonomy

sort(unique(myrsine.data$scientificName))

table(myrsine.data$taxonomicStatus)

table(myrsine.data$scientificName, myrsine.data$taxonomicStatus)

species.names <- unique(myrsine.data$scientificName)
dim(species.names)

tax.check <- TPL(species.names)

# creating new object w/ original and new names after TPL
new.tax <- data.frame(scientificName = species.names,
                      genus.new.TPL = tax.check$New.Genus,
                      species.new.TPL = tax.check$New.Species,
                      status.TPL = tax.check$Taxonomic.status,
                      scientificName.new.TPL = paste(tax.check$New.Genus,
                                                     tax.check$New.Species))
# now we are merging raw data and checked data
myrsine.new.tax <- merge(myrsine.data, new.tax, by = "scientificName")


# Exporting data after taxonomy check

dir.create("data/processed/", recursive = TRUE)
write.csv(myrsine.new.tax,
          "data/processed/data_taxonomy_check.csv",
          row.names = FALSE)

plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)+
map(, , , add = TRUE)


# How to create a map equal to this on tmap. How to add a shape to this points,
#Sf EQUAL A DATAFRAMES(DECIMAL LONGITUDE AND DECIMAL LATITUDE, HOW DO YOU GO ()
# explorar o tmap modecriar plots e mapas interativos
