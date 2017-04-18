## source('~/Master_Thesis/02-r-code-git/a-locate_jetstream_1polynomial_2d.r')
##
## ROUTINE ZUM AUFFINDEN DES JETSTREAMS AUF NORDHEMISPHÄRE ANHAND DES ZONALWINDES ####
####

library(parallel) # Paralleles Rechnen m Apply
library(pckg.cheb) # Least Squares Fit u Nullstellen d Ableitung
# install.packages("pckg.cheb_0.9.5.tar.gz", repos = NULL, type = "source")


find.jet.chebpoly <- function(array, axis, n.order = 8) {
  
  ####
  ## VARIABLEN UND PARAMETER ####
  ####

    
  ####
  ## LEAST SQUARES FIT                   ####
  ## CHEBYSHEV POLYNOME 8-TER ORDNUNG      #
  ## AN ZONAL WIND IN MERIDIONALER RICHTUNG #
  ####
  ####

  list.max <- apply(array, 1, cheb.find.max, x.axis = lat, n = n.order)
  list.max.len <- length(list.max)
  
  ## Maxima des Modells (Positionen und Werte)
  model.max.lat <- sapply(list.max, "[[", 1)
  model.max.u <- sapply(list.max, "[[", 2)
  n.max.max <- max(sapply(model.max.lat, length))
  model.max.lat <- sapply(model.max.lat, fun.fill, n = n.max.max)
  #model.max.lat <- apply(array(model.max.lat, c(n.max.max, list.max.dim)), c(1,3), t)
  model.max.u <- sapply(model.max.u, fun.fill, n = n.max.max)
  #model.max.u <- apply(array(model.max.u, c(n.max.max, list.max.nrow, list.max.ncol)), c(1,3), t)
  
  ## entscheidungsschema für pfj und stj
  ## Unterscheidung von polarem und subtropischem Jetstream
  ## Filterung der zwei stärksten Maxima
  model.max.2 <- apply(model.max.u, 2, diff.max)
  # Positionen als Indizes
  model.max.2.x <- sapply(model.max.2, "[[", 1) 
  # Umrechnung von Indizes zu Breitengraden
  model.max.2.lat <- matrix(NA,nrow(array), ncol = 2)
  for (i in 1:nrow(array)) {
    #print(model.max.lat[model.max.2.x[,i],i])
    model.max.2.lat[i,] <- model.max.lat[model.max.2.x[,i],i]
  }
  # Werte der Maxima (U-Wind)
  model.max.2.u <- sapply(model.max.2, "[[", 2) 
  
  # Annahmen: PFJ nördliches Maximum, STJ südliches Maximum
  PFJ.lat <- rep(NA, nrow(array)); 
  STJ.lat <- PFJ.lat; MaxJ.lat <- PFJ.lat
  PFJ.u <- PFJ.lat; STJ.u <- PFJ.lat; MaxJ.u <- PFJ.lat
  for (i in 1:192) {
    #print(i)
    PFJ.ind <- which.max(model.max.2.lat[i,])
    STJ.ind <- which.min(model.max.2.lat[i,])
    if (length(PFJ.ind) >= 1 | length(STJ.ind) >= 1) {
      PFJ.lat[i] <- model.max.2.lat[i,PFJ.ind]
      STJ.lat[i] <- model.max.2.lat[i,STJ.ind]
      PFJ.u[i] <- model.max.2.u[PFJ.ind,i]
      STJ.u[i] <- model.max.2.u[STJ.ind,i]
    } else {
      PFJ.lat[i] <- NA; STJ.lat[i] <- NA;
      PFJ.u[i] <- NA; STJ.u[i] <- NA
    }
    PFJ.ind <- NA; STJ.ind <- NA;
    # Position d stärksten Jets
    MaxJ.ind <- which.max(model.max.2.u[,i])
    if (length(MaxJ.ind) >= 1) {
      MaxJ.lat[i] <- model.max.2.lat[i, MaxJ.ind]
      MaxJ.u[i] <- model.max.2.u[MaxJ.ind, i]
    } else {
      MaxJ.lat[i] <- NA; MaxJ.u[i] <- NA;
    }
  }
  
  ## Löschen von temporär benötigten Daten
  # rm(list.max.dim, list.max.nrow, list.max.ncol)
  
  ## Übergabe von Variablen
  list.model.jet <- list("MaxJ.lat" = MaxJ.lat, "MaxJ.u" = MaxJ.u,
                         "PFJ.lat" = PFJ.lat, "PFJ.u" = PFJ.u,
                         "STJ.lat" = STJ.lat, "STJ.u" = STJ.u)
  return(list.model.jet)
}


