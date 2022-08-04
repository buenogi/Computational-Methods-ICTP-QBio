################################################################################
################### Introduction  to biological diversity analyses##############
################################################################################


# Loading data
CESTES_comm <- read.csv(file = "data/raw/cestes/comm.csv")


# Species abundance
head(CESTES_comm)

# N species per plot

sort(colSums(CESTES_comm), decreasing = T)

# Most abundance species per plot

CESTES_comm_sites <- CESTES_comm$Sites
CESTES_comm_sites <- as.data.frame(CESTES_comm_sites)
CESTES_comm$Sites <- NULL
ifelse(CESTES_comm >=1 ,1,0)
CESTES_comm$richness <- rowSums(CESTES_comm)
richness <-  cbind(CESTES_comm_sites, CESTES_comm$richness)
max(richness$`CESTES_comm$richness`)

