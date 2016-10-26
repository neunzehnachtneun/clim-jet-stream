## source('~/Master_Thesis/02-r-code-git/a-locate_jetstream_1polynomial_2d.r')
##
## ROUTINE ZUM AUFFINDEN DES JETSTREAMS AUF NORDHEMISPHÄRE ANHAND DES ZONALWINDES ####
####


####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

# Working Directory
setwd("~/Master_Thesis/02-r-code-git/")

# Einleseroutine
# source('a-read_era_ncdf.r')
# load(file = 'data.RData')

library(parallel) # Paralleles REchnen m Apply
library(pckg.cheb) # Least Squares Fit u Nullstellen d Ableitung
# options(unzip = 'internal')
# install.packages("pckg.cheb_0.9.tar.gz", repos = NULL, type = "source")
# install_github("sebaki/pckg.cheb", auth_token = "d789940fbff70c9eaa8baa41b7d77adef138eebd")
 
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

n.cpu <- 2#4 # Anzahl der CPUs für parApply
n.order.lat <- 31 # Ordnung des Least-Square-Verfahrens für Fit über Breitengrad
n.order.lon <- 8 # Ordnung des Least-Square-Verfahrens für Fit über Längengrad


####
## LEAST SQUARES FIT                   ####
## CHEBYSHEV POLYNOME 23-TER ORDNUNG      #
## AN ZONAL WIND IN MERIDIONALER RICHTUNG #
####

# list.model.lat <- apply(u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
list.model.lat <- parApply(cl, u.monmean[,,1:2], c(1,3), pckg.cheb:::cheb.fit.roots, x.axis = lat, n = n.order.lat, bc.harmonic = FALSE, roots.bound.l = 20, roots.bound.u = 80)
stopCluster(cl)
list.model.dim <- dim(list.model.lat)
list.model.nrow <- nrow(list.model.lat)
list.model.ncol <- ncol(list.model.lat)

## Chebyshev-Koeffizienten
cheb.coeff <- sapply(list.model.lat, "[[", 1)
cheb.coeff <- apply(array(data = cheb.coeff, dim = c((n.order.lat + 1), list.model.dim)), c(1,3), t)

## Gefiltertes Modell für Zonal-Wind
model.u <- sapply(list.model.lat, "[[", 2)
model.u <- apply(array(data = model.u, dim = c(n.lat, list.model.dim)), c(1,3), t)

## Erste Ableitung des gefilterten Modells für Zonalwind
model.u.deriv.1st <- sapply(list.model.lat, "[[", 3)
model.u.deriv.1st <- apply(array(data = model.u.deriv.1st, dim = c(n.lat, list.model.dim)),  c(1,3), t)

## Extrema des Modells (Positionen und Werte)
model.extr.lat <- sapply(list.model.lat, "[[", 4)
model.extr.u <- sapply(list.model.lat, "[[", 5)
model.extr.deriv.2nd <- sapply(list.model.lat, "[[", 6)
model.extr.lat <- sapply(model.extr.lat, fun.fill, n = 24)
model.extr.lat <- apply(array(model.extr.lat, c(24, list.model.dim)), c(1,3), t)
model.extr.u <- sapply(model.extr.u, fun.fill, n = 24)
model.extr.u <- apply(array(model.extr.u, c(24, dim.list[1], dim.list[2])), c(1,3), t)
model.extr.deriv.2nd <- sapply(model.extr.deriv.2nd, fun.fill, n = 24)
model.extr.deriv.2nd <- apply(array(model.extr.deriv.2nd, c(24, list.model.dim)), c(1,3), t)


## Maxima des Modells (Positionen und Werte)
model.max.u <- apply(model.extr.u, c(1,3), max, na.rm = TRUE)
model.max.lat <- array(rep(0, 192*664), c(dim.list))
for (i in 1:dim.list[2]) {
  for (j in 1:dim.list[1]) {
    model.max.lat[j,i] <- model.extr.lat[j, which(model.extr.u[j,,i] == model.max.u[j,i]), i]
  }
}
rm(list.model.lat, list.model.dim, list.model.nrow, list.model.ncol)


######################################################################
## LEAST SQUARES FIT
## CHEBYSHEV POLYNOME 8-TER ORDNUNG
## AN MERIDIONALE MAXIMA DES ZONALWINDS IN ZONALER RICHTUNG
######################################################################
##

#list.model.lon <- apply(model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = n.order.lon)
cl <- makeCluster(getOption("cl.cores", n.cpu))
list.model.lon <- parApply(cl, model.max.lat, 2, pckg.cheb:::cheb.fit.roots, x.axis = lon, n = n.order.lon, bc.harmonic = TRUE)
stopCluster(cl)

## Gefiltertes Modell für Maxima des Zonal-Wind in Zonalrichtung
#model.max.lon <- list.model.lon, "[[", 2)
#rm(list.model.lon)





######################################################################
######################################################################
save.image(file = "monthly.RData")



 

####################################################################################################
########## ableitung des drehimpulses ##############################################################
########## aus zonal wind ##########################################################################
####################################################################################################
### ref: m = 
### formel noch inkorrekt
### keine schleife benutzen
##
# m <- matrix(NA,n.lon,n.lat)
# for (i in 1:n.lon){
#   for (j in 1:n.lat){
#     m[i,j] <- u.era.t63.monmean[i,j,1]*cos(lat.era.t63[j]) + 1/86400*u.era.t63.monmean[i,j,1]**2*cos(lat.era.t63[j])**2
#   }
# }
# #m <- u.era.t63.monmean*cos(lat.era.t63)
# 




# ######################################################################
# ## LEAST SQUARES FIT ÜBER **SEQUENZEN** (l=8)
# ## CHEBYSHEV POLYNOME 3-TER ORDNUNG
# ## AN ZONAL-WIND IN MERIDIONALER RICHTUNG
# ######################################################################
# ##
# 
# # list.model.lat.seq <- apply(u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit.seq, x.axis = lat, n = n.order.lat.seq, l = len.seq)
# cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
# list.model.lat.seq <- parApply(cl, u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit.seq, x.axis = lat, n = n.order.lat.seq, l = len.seq)
# stopCluster(cl)
# dim.list <- dim(list.model.lat.seq)
# 
# ## Gefiltertes Modell für Zonal-Wind
# model.u.seq <- sapply(list.model.lat.seq, "[[", 1)
# model.u.seq <- apply(array(data = model.u.seq, dim = c(n.lat, dim.list[1], dim.list[2])),  c(1,3), t)
# 
# ## Erste Ableitung des gefilterten Modells für Zonalwind
# model.u.deriv.1st.seq <- sapply(list.model.lat.seq, "[[", 2)
# model.u.deriv.1st.seq <- apply(array(data = model.u.deriv.1st.seq, dim = c(n.lon, dim.list[1], dim.list[2])),  c(1,3), t)
# 
# ## Extrema des Modells (Positionen und Werte)
# model.extr.lat.seq <- sapply(list.model.lat.seq, "[[", 3)
# model.extr.lat.seq <- sapply(model.extr.lat.seq, fun.fill, n = 24)
# model.extr.lat.seq <- apply(array(model.extr.lat.seq, c(24, dim.list[1], dim.list[2])), c(1,3), t)
# model.extr.u.seq <- sapply(list.model.lat.seq, "[[", 4)
# model.extr.u.seq <- sapply(model.extr.u.seq, fun.fill, n = 24)
# model.extr.u.seq <- apply(array(model.extr.u.seq, c(24, dim.list[1], dim.list[2])), c(1,3), t)
# 
# ## Maxima des Modells (Positionen und Werte)
# model.max.u.seq <- apply(model.extr.u.seq, c(1,3), max, na.rm = TRUE)
# model.max.lat.seq <- array(rep(0, dim.list[1]*dim.list[2]), c(dim.list))
# for (i in 1:dim.list[2]) {
#   for (j in 1:dim.list[1]) {
#     model.max.lat.seq[j,i] <- model.extr.lat.seq[j, which(model.extr.u.seq[j,,i] == model.max.u.seq[j,i]), i]
#   }
# }
# rm(list.model.lat.seq, dim.list)
# 
# 
# ######################################################################
# ## LEAST SQUARES FIT 
# ## CHEBYSHEV POLYNOME 8-TER ORDNUNG
# ## AN MERIDIONALE MAXIMA DES ZONALWINDS IN ZONALER RICHTUNG
# ## ANGEWANDT AUF SEQUENZIERTES MODELL
# ######################################################################
# ##
# 
# #list.model.lon.seq <- apply(model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
# cl <- makeCluster(getOption("cl.cores", n.cpu))
# list.model.lon.seq <- parApply(cl, model.max.lat.seq, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = n.order.lon)
# stopCluster(cl)
# 
# ## Gefiltertes Modell für Maxima des Zonal-Wind in Zonalrichtung
# model.max.lon.seq <- sapply(list.model.lon.seq, "[[", 2)
# rm(list.model.lon.seq)

