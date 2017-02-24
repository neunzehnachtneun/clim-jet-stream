####################################################################################################
## source('~/Master_Thesis/Code/locate_jetstream_2d.r')


####
## DATEN EINLESEN ####
####

# source("a-read_era_ncdf.r")


####
## VARIABLEN PARAMETER PAKETE ####
####

library(parallel) # Paralleles Rechnen m Apply
n.cpu <- 2 # Anzahl der CPUs für parApply


####
## SUCHE DES MAXIMAS MITTELS APPLY ####
####

cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
model.max.u.discrete <- parApply(cl, u.era[,,2,], c(1,3), max)

##
model.max.lat.discrete <- matrix(NA, nrow = nrow(model.max.u.discrete), ncol = ncol(model.max.u.discrete))

for (i in 1:nrow(model.max.u.discrete)) {
  for (j in 1:ncol(model.max.u.discrete)) {
    model.max.lat.discrete[i,j] <- lat[which(model.max.u.discrete[i,j] == u.era[i,,2,j])]
  }
}






