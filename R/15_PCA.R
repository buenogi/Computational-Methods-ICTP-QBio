################################################################################
################################### PCA ########################################
################################################################################


library(vegan)

# Loading data

data("dune")
data("dune.env")

?decostand

chord_distance <- dist(decostand(dune, "norm"))

is(chord_distance)
norm <- decostand(dune, "norm")

# PCA

pca <- rda(norm)

plot(pca)

summary(pca)


# Let's do a PCA of the environmental matrix

names(dune.env)

pca_env <- rda(dune.env[, c("Al", "Moisture", "Manure")])

# A partir daqui está errado...


plot(pca_env, choice = c(2,3))

