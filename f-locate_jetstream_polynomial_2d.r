## source('~/Master_Thesis/02-r-code-git/a-locate_jetstream_1polynomial_2d.r')
##
## ROUTINE ZUM AUFFINDEN DES JETSTREAMS AUF NORDHEMISPHÄRE ANHAND DES ZONALWINDES ####
####


# ####
# ## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
# ####
# 
# library(parallel) # Paralleles Rechnen m Apply
# library(pckg.cheb) # Least Squares Fit u Nullstellen d Ableitung
# # options(unzip = 'internal')
# # install.packages("pckg.cheb_0.9.tar.gz", repos = NULL, type = "source")
# # install_github("sebaki/pckg.cheb", auth_token = "d789940fbff70c9eaa8baa41b7d77adef138eebd")
#  
# # Hilfsfunktion zum Auffüllen von Vektoren
# fun.fill <- function(x, n) {
#   while (length(x) < n) {
#     x <- c(x, NA)
#   }
#   return(x)
# }
# 
# 
# ####
# ## VARIABLEN UND PARAMETER ####
# ####
# 
# n.cpu <- 4 # Anzahl der CPUs für parApply
# n.order.lat <- 23 # Ordnung des Least-Square-Verfahrens für Fit über Breitengrad
# #n.order.lon <- 8 # Ordnung des Least-Square-Verfahrens für Fit über Längengrad
# 
# 
# ####
# ## LEAST SQUARES FIT                   ####
# ## CHEBYSHEV POLYNOME 23-TER ORDNUNG      #
# ## AN ZONAL WIND IN MERIDIONALER RICHTUNG #
# ####
# 
# # list.model.lat <- apply(u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
# cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
# list.model.lat <- parApply(cl, u.era[,,4,], c(1,3), cheb.fit.roots, x.axis = lat, n = n.order.lat, bc.harmonic = FALSE, roots.bound.l = 20, roots.bound.u = 80)
# stopCluster(cl)
# list.model.dim <- dim(list.model.lat)
# list.model.nrow <- nrow(list.model.lat)
# list.model.ncol <- ncol(list.model.lat)
# 
# ## Chebyshev-Koeffizienten
# cheb.coeff <- sapply(list.model.lat, "[[", 1)
# cheb.coeff <- apply(array(data = cheb.coeff, dim = c((n.order.lat + 1), list.model.dim)), c(1,3), t)
# 
# ## Gefiltertes Modell für Zonal-Wind
# model.u <- sapply(list.model.lat, "[[", 2)
# model.u <- apply(array(data = model.u, dim = c(lat.len, list.model.dim)), c(1,3), t)
# 
# ## Erste Ableitung des gefilterten Modells für Zonalwind
# model.u.deriv.1st <- sapply(list.model.lat, "[[", 3)
# model.u.deriv.1st <- apply(array(data = model.u.deriv.1st, dim = c(lat.len, list.model.dim)),  c(1,3), t)
# 
# ## Extrema des Modells (Positionen und Werte)
# model.extr.lat <- sapply(list.model.lat, "[[", 4)
# model.extr.u <- sapply(list.model.lat, "[[", 5)
# model.extr.deriv.2nd <- sapply(list.model.lat, "[[", 6)
# model.extr.lat <- sapply(model.extr.lat, fun.fill, n = 24)
# model.extr.lat <- apply(array(model.extr.lat, c(24, list.model.dim)), c(1,3), t)
# model.extr.u <- sapply(model.extr.u, fun.fill, n = 24)
# model.extr.u <- apply(array(model.extr.u, c(24, list.model.nrow, list.model.ncol)), c(1,3), t)
# model.extr.deriv.2nd <- sapply(model.extr.deriv.2nd, fun.fill, n = 24)
# model.extr.deriv.2nd <- apply(array(model.extr.deriv.2nd, c(24, list.model.dim)), c(1,3), t)
# 
# 
# ## Maxima des Modells (Positionen und Werte)
# model.max.u <- apply(model.extr.u, c(1,3), max, na.rm = TRUE)
# model.max.u[which(model.max.u == -Inf)] <- NA
# 
# model.max.lat <- array(rep(NA, 192*664), c(list.model.dim))
# for (i in 1:list.model.ncol) {
#   for (j in 1:list.model.nrow) {
#     max.lat <- model.extr.lat[j,which.max(model.extr.u[j,,i]),i]
#     if (length(max.lat) == 1) {
#       model.max.lat[j,i] <- max.lat
#     }
#   }
# }
# 
# 
# ## Löschen von temporär benötigten Daten
# rm(list.model.lat, list.model.dim, list.model.nrow, list.model.ncol)


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


