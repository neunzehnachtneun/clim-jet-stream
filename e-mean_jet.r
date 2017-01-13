######################################################################
######################################################################
## ROUTINE ZUM EINLESEN UND VERARBEITEN DER ZEITLICHEN MITTELWERTE
## DES U- ,V- ,W-WINDFELDES AUS ERA-DATEN IM NCDF-FORMAT
## source('~/Master_Thesis/r-code-git/process_mean_uvw.r')
######################################################################
######################################################################


######################################################################
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE
######################################################################
##

library(ncdf4)
library(parallel)
library(pckg.cheb)


# setwd("~/01-Master-Thesis/02-r-code-git/")
# path <- "03-data-nc/"
# path <- "/home/skiefer/era/raw/"
file <- "1958-2015-e4ei-t63-uv-timmean.nc"  # Nordhemisphäre + Südhemisphäre



######################################################################
## EINLESEN DER DATEN
## ERA40 / ERA-INTERIM
## T63 - GRID - GAUSSIAN
## NORDHEMISPHÄRE & TROPEN
## 192 (lat) * 64 (lon)
######################################################################
##

nc <- nc_open(paste(path, file, sep = ""))
# print(nc)
u.mean <- ncvar_get(nc, "u") # U-Wind-Komponente
v.mean <- ncvar_get(nc, "v") # V-Wind-Komponente
# w.mean <- ncvar_get(nc, "var135") # W-Wind-Komponente

#lon <- ncvar_get(nc, "lon") # Längengrad
#lat <- ncvar_get(nc, "lat") # Breitengrad
#lev <- ncvar_get(nc, "level") # Drucklevel
# date.help <- ncvar_get(nc, "time")

nc_close(nc)
rm(nc)

# betrage des horizontalen windfeldes
# uv.mean <- sqrt( u.mean ** 2 + v.mean ** 2 )

#################################################
## ZONAL MEAN
u.zon.mean <- apply(u.mean[,,], c(2,3), mean)
u.zon.sd <- apply(u.mean[,,], c(2,3), sd)
v.zon.mean <- apply(v.mean[,,], c(2,3), mean)
v.zon.sd <- apply(v.mean[,,], c(2,3), sd)


## ZONAL-WIND U

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

n.cpu <- 4 # Anzahl der CPUs für parApply
n.order.lat <- 23 # Ordnung des Least-Square-Verfahrens für Fit über Breitengrad
#n.order.lon <- 8 # Ordnung des Least-Square-Verfahrens für Fit über Längengrad


####
## LEAST SQUARES FIT                   ####
## CHEBYSHEV POLYNOME 23-TER ORDNUNG      #
## AN ZONAL WIND IN MERIDIONALER RICHTUNG #
####

# list.model.lat <- apply(u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
list.model.lat <- parApply(cl, u.era[,,4,], c(1,3), cheb.fit.roots, x.axis = lat, n = n.order.lat, bc.harmonic = FALSE, roots.bound.l = 20, roots.bound.u = 80)
stopCluster(cl)
list.model.dim <- dim(list.model.lat)
list.model.nrow <- nrow(list.model.lat)
list.model.ncol <- ncol(list.model.lat)

## Chebyshev-Koeffizienten
cheb.coeff <- sapply(list.model.lat, "[[", 1)
cheb.coeff <- apply(array(data = cheb.coeff, dim = c((n.order.lat + 1), list.model.dim)), c(1,3), t)

## Gefiltertes Modell für Zonal-Wind
model.u <- sapply(list.model.lat, "[[", 2)
model.u <- apply(array(data = model.u, dim = c(lat.len, list.model.dim)), c(1,3), t)

## Erste Ableitung des gefilterten Modells für Zonalwind
model.u.deriv.1st <- sapply(list.model.lat, "[[", 3)
model.u.deriv.1st <- apply(array(data = model.u.deriv.1st, dim = c(lat.len, list.model.dim)),  c(1,3), t)

## Extrema des Modells (Positionen und Werte)
model.extr.lat <- sapply(list.model.lat, "[[", 4)
model.extr.u <- sapply(list.model.lat, "[[", 5)
model.extr.deriv.2nd <- sapply(list.model.lat, "[[", 6)
model.extr.lat <- sapply(model.extr.lat, fun.fill, n = 24)
model.extr.lat <- apply(array(model.extr.lat, c(24, list.model.dim)), c(1,3), t)
model.extr.u <- sapply(model.extr.u, fun.fill, n = 24)
model.extr.u <- apply(array(model.extr.u, c(24, list.model.nrow, list.model.ncol)), c(1,3), t)
model.extr.deriv.2nd <- sapply(model.extr.deriv.2nd, fun.fill, n = 24)
model.extr.deriv.2nd <- apply(array(model.extr.deriv.2nd, c(24, list.model.dim)), c(1,3), t)


## Maxima des Modells (Positionen und Werte)
model.max.u <- apply(model.extr.u, c(1,3), max, na.rm = TRUE)
model.max.u[which(model.max.u == -Inf)] <- NA

model.max.lat <- array(rep(NA, 192*664), c(list.model.dim))
for (i in 1:list.model.ncol) {
  for (j in 1:list.model.nrow) {
    max.lat <- model.extr.lat[j,which.max(model.extr.u[j,,i]),i]
    if (length(max.lat) == 1) {
      model.max.lat[j,i] <- max.lat
    }
  }
}


## Löschen von temporär benötigten Daten
rm(list.model.lat, list.model.dim, list.model.nrow, list.model.ncol)






