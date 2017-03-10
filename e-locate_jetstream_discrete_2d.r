####################################################################################################
## source('~/Master_Thesis/Code/locate_jetstream_2d.r')


####
## DATEN EINLESEN ####
####



locate.jetstream.disc <- function(array, axis, n.cpu) {
  

  ## VARIABLEN PARAMETER PAKETE
  
  library(parallel) # Paralleles Rechnen m Apply

  
  ## SUCHE DES MAXIMAS MITTELS APPLY
  
  cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
  array.max.y <- parApply(cl, array, c(1,3), max)
  stopCluster(cl)
  
  ##
  array.max.x <- matrix(NA, nrow = nrow(array.max.y), ncol = ncol(array.max.y))
  
  for (i in 1:nrow(array.max.y)) {
    for (j in 1:ncol(array.max.y)) {
      array.max.x[i,j] <- if (!is.na(array.max.y[i,j])) axis[which(array.max.y[i,j] == array[i,,j])]
    }
  }
  
  # Vorbereiten der Liste für Übergabe
  list.max <- list(model.max.lat = array.max.x, model.max.u = array.max.y)
  return(list.max)
  
}



