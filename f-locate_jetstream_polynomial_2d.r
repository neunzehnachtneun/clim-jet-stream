## source('~/Master_Thesis/02-r-code-git/a-locate_jetstream_1polynomial_2d.r')
##
## ROUTINE ZUM AUFFINDEN DES JETSTREAMS AUF NORDHEMISPHÄRE ANHAND DES ZONALWINDES ####
####



locate.jetstream.poly <- function(array, axis, n.order, n.cpu) {
  
  library(parallel) # Paralleles Rechnen m Apply
  library(pckg.cheb) # Least Squares Fit u Nullstellen d Ableitung
  # install.packages("pckg.cheb_0.9.5.tar.gz", repos = NULL, type = "source")
  
  # Hilfsfunktion zum Auffüllen von Vektoren
  fun.fill <- function(x, n) {
    while (length(x) < n) {
      x <- c(x, NA)
    }
    return(x)
  }

  
  ####
  ## VARIABLEN UND PARAMETER ####
  ####

    
  ####
  ## LEAST SQUARES FIT                   ####
  ## CHEBYSHEV POLYNOME 8-TER ORDNUNG      #
  ## AN ZONAL WIND IN MERIDIONALER RICHTUNG #
  ####

  cl <- makeCluster(getOption("cl.cores", n.cpu))
  list.max <- parApply(cl, array, c(1,3), cheb.find.max, x.axis = lat, n = n.order)
  stopCluster(cl)
  
  list.max.dim <- dim(list.max)
  list.max.nrow <- nrow(list.max)
  list.max.ncol <- ncol(list.max)
  
  
  ## Maxima des Modells (Positionen und Werte)
  model.max.lat <- sapply(list.max, "[[", 1)
  model.max.u <- sapply(list.max, "[[", 2)
  n.max.max <- max(sapply(model.max.lat, length))
  model.max.lat <- sapply(model.max.lat, fun.fill, n = n.max.max)
  model.max.lat <- apply(array(model.max.lat, c(n.max.max, list.max.dim)), c(1,3), t)
  model.max.u <- sapply(model.max.u, fun.fill, n = n.max.max)
  model.max.u <- apply(array(model.max.u, c(n.max.max, list.max.nrow, list.max.ncol)), c(1,3), t)
  
  
  ## Löschen von temporär benötigten Daten
  rm(list.max.dim, list.max.nrow, list.max.ncol)
  
  ## Übergabe von Variablen
  list.model.max <- list(model.max.lat = model.max.lat, model.max.u = model.max.u)
  return(list.model.max)
}


