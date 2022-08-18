################################################################################
############################ Multivariate analysis #############################
################################################################################

# Packages

library(vegan)
library(cluster)


#Loading Data

data(dune)
data(dune.env)
table(dune.env$Management)

# Cluester analysis of the dune vegetation

bray_distance <- vegdist(dune)

# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))
ba_cluster <- hclust(bray_distance, method = "average")
ca_cluster <- hclust(chord_distance, method = "average")


par(mfrow = c(5, 2))
plot(b_cluster)
plot(c_cluster)

par(mfrow = c(1, 2))
plot(ba_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(ca_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)


# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))
bb_cluster <- hclust(bray_distance, method = "centroid")
cb_cluster <- hclust(chord_distance, method = "centroid")


plot(bb_cluster)
plot(cb_cluster)
plot(bb_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(cb_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)

# Chord distance, euclidean distance normalized to 1.

chord_distance <- dist(decostand(dune, "norm"))
bc_cluster <- hclust(bray_distance, method = "single")
cc_cluster <- hclust(chord_distance, method = "single")


plot(bc_cluster)
plot(cc_cluster)
plot(bc_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(cc_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)


# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))
bd_cluster <- hclust(bray_distance, method = "complete")
cd_cluster <- hclust(chord_distance, method = "complete")


plot(bd_cluster)
plot(cd_cluster)
plot(bd_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(cd_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)

# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))
be_cluster <- hclust(bray_distance, method = "ward")
ce_cluster <- hclust(chord_distance, method = "ward")


plot(be_cluster)
plot(ce_cluster)
plot(be_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(ce_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
