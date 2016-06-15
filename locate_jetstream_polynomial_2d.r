######################################################################
######################################################################
## ROUTINE ZUM EINLESEN VON ERA-DATEN (ZONAL-WIND) IM NCDF-FORMAT
## UND AUFFINDEN DES JETSTREAMS AUF NORDHEMISPHÄRE
## source('~/Master_Thesis/r-code-git/locate_jetstream_polynomial_2d.r')
######################################################################
######################################################################


######################################################################
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE
######################################################################
##

library(ncdf4)
library(rootSolve)
library(parallel)
library(chron)

# eigenes package für least squares fit mit chebyshev polynomen
# install.packages("pckg.cheb_0.2.tar.gz", repos = NULL, type = "source")
library(pckg.cheb)

setwd("~/Master_Thesis/r-code-git/")
path <- "data/"
file <- "era--t63_ua_monmean_300hpa_nh.nc"  # Nordhemisphäre
#file <- "era--t63_ua_monmean_300hpa_sh.nc"  # Südhemisphäre
#file <- "era--t63_ua_monmean_300hpa.nc"     # Globus


######################################################################
## KLEINE HILFSFUNKTIONEN
######################################################################
##

fun.fill <- function(x, n) {
  while (length(x) < n) {
    x <- c(x, NA)
  }
  return(x)
}


######################################################################
## EINLESEN DER DATEN
## ERA40 / ERA-INTERIM
## T63 - GRID
## NCDF4
######################################################################
##

nc <- nc_open(paste(path, file, sep = ""))
uwind.monmean <- ncvar_get(nc, "var131")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
lev <- ncvar_get(nc, "lev")
date.help <- ncvar_get(nc, "time")
dts = chron(dates. = date.help/24, origin. = c(month = 9,day = 1,year = 1957), format = "day mon year")
nc_close(nc)
rm(nc)


######################################################################
## NÜTZLICHE VARIABLEN AUS DATENSATZ
######################################################################
##

n.cpu <- 22
n.lat <- length(lat)
n.lon <- length(lon)

dts.month <- months(dts, abbreviate = TRUE)
dts.year <- years(dts)


## Zeitlich gemittelter Zonalwind
uwind.mean <- apply(uwind.monmean,c(1,2),mean)
uwind.std <- apply(uwind.monmean,c(1,2),sd)

## Meridional und zeitlich gemittelter Zonalwind
uwind.mon.mer.mean <- apply(uwind.monmean, 2, mean)
uwind.mon.mer.sd <- apply(uwind.mean, 2, sd)
plot(uwind.mon.mer.sd)

## Meridional gemittelter Zonalwind
uwind.monmean.mermean <- apply(uwind.monmean, c(2,3), mean)
apply(uwind.monmean, c(2,3), sd)


######################################################################
## LEAST SQUARES FIT 
## CHEBYSHEV POLYNOME 23-TER ORDNUNG
## AN ZONAL WIND IN MERIDIONALER RICHTUNG
######################################################################
##

# list.model.lat <- apply(uwind.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = 23)
cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
list.model.lat <- parApply(cl, uwind.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = 23)
stopCluster(cl)
dim.list <- dim(list.model.lat)

## Chebyshev-Koeffizienten
cheb.coeff <- sapply(list.model.lat, "[[", 1)
cheb.coeff <- apply(array(data = cheb.coeff, dim = c(dim(cheb.coeff)[1], dim.list[1], dim.list[2])),  c(1,3), t)

## Gefiltertes Modell für Zonal-Wind
model.uwind <- sapply(list.model.lat, "[[", 2)
model.uwind <- apply(array(data = model.uwind, dim = c(n.lat, dim.list[1], dim.list[2])),  c(1,3), t)

## Erste Ableitung des gefilterten Modells für Zonalwind
model.uwind.deriv.1st <- sapply(list.model.lat, "[[", 3)
model.uwind.deriv.1st <- apply(array(data = model.uwind.deriv.1st, dim = c(n.lat, dim.list[1], dim.list[2])),  c(1,3), t)

## Extrema des Modells (Positionen und Werte)
model.extr.lat <- sapply(list.model.lat, "[[", 4)
model.extr.uwind <- sapply(list.model.lat, "[[", 5)
model.extr.lat <- sapply(model.extr.lat, fun.fill, n = 24)
model.extr.lat <- apply(array(model.extr.lat, c(24, dim.list[1], dim.list[2])), c(1,3), t)
model.extr.uwind <- sapply(model.extr.uwind, fun.fill, n = 24)
model.extr.uwind <- apply(array(model.extr.uwind, c(24, dim.list[1], dim.list[2])), c(1,3), t)

## Maxima des Modells (Positionen und Werte)
model.max.uwind <- apply(model.extr.uwind, c(1,3), max, na.rm = TRUE)
model.max.lat <- array(rep(0, 192*664), c(dim.list))
for (i in 1:dim.list[2]) {
  for (j in 1:dim.list[1]) {
    model.max.lat[j,i] <- model.extr.lat[j, which(model.extr.uwind[j,,i] == model.max.uwind[j,i]), i]
  }
}
rm(list.model.lat, dim.list)


######################################################################
## LEAST SQUARES FIT 
## CHEBYSHEV POLYNOME 8-TER ORDNUNG
## AN MERIDIONALE MAXIMA DES ZONALWINDS IN ZONALER RICHTUNG
######################################################################
##

#list.model.lon <- apply(model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
cl <- makeCluster(getOption("cl.cores", n.cpu))
list.model.lon <- parApply(cl, model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
stopCluster(cl)

## Gefiltertes Modell für Maxima des Zonal-Wind in Zonalrichtung
model.max.lon <- sapply(list.model.lon, "[[", 2)
rm(list.model.lon)


######################################################################
## LEAST SQUARES FIT ÜBER **SEQUENZEN** (l=8)
## CHEBYSHEV POLYNOME 3-TER ORDNUNG
## AN ZONAL-WIND IN MERIDIONALER RICHTUNG
######################################################################
##

# list.model.lat.seq <- apply(uwind.monmean[,,], c(1,3), pckg.cheb:::cheb.fit.seq, x.axis = lat, n = 3, l = 8)
cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
list.model.lat.seq <- parApply(cl, uwind.monmean[,,], c(1,3), pckg.cheb:::cheb.fit.seq, x.axis = lat, n = 3, l = 8)
stopCluster(cl)
dim.list <- dim(list.model.lat.seq)

## Gefiltertes Modell für Zonal-Wind
model.uwind.seq <- sapply(list.model.lat.seq, "[[", 1)
model.uwind.seq <- apply(array(data = model.uwind.seq, dim = c(n.lat, dim.list[1], dim.list[2])),  c(1,3), t)

## Erste Ableitung des gefilterten Modells für Zonalwind
model.uwind.deriv.1st.seq <- sapply(list.model.lat.seq, "[[", 2)
model.uwind.deriv.1st.seq <- apply(array(data = model.uwind.deriv.1st.seq, dim = c(n.lon, dim.list[1], dim.list[2])),  c(1,3), t)

## Extrema des Modells (Positionen und Werte)
model.extr.lat.seq <- sapply(list.model.lat.seq, "[[", 3)
model.extr.uwind.seq <- sapply(list.model.lat.seq, "[[", 4)
model.extr.lat.seq <- sapply(model.extr.lat.seq, fun.fill, n = 24)
model.extr.lat.seq <- apply(array(model.extr.lat.seq, c(24, dim.list[1], dim.list[2])), c(1,3), t)
model.extr.uwind.seq <- sapply(model.extr.uwind.seq, fun.fill, n = 24)
model.extr.uwind.seq <- apply(array(model.extr.uwind.seq, c(24, dim.list[1], dim.list[2])), c(1,3), t)

## Maxima des Modells (Positionen und Werte)
model.max.uwind.seq <- apply(model.extr.uwind.seq, c(1,3), max, na.rm = TRUE)
model.max.lat.seq <- array(rep(0, dim.list[1]*dim.list[2]), c(dim.list))
for (i in 1:dim.list[2]) {
  for (j in 1:dim.list[1]) {
    model.max.lat.seq[j,i] <- model.extr.lat.seq[j, which(model.extr.uwind.seq[j,,i] == model.max.uwind.seq[j,i]), i]
  }
}
rm(list.model.lat.seq, dim.list)


######################################################################
## LEAST SQUARES FIT 
## CHEBYSHEV POLYNOME 8-TER ORDNUNG
## AN MERIDIONALE MAXIMA DES ZONALWINDS IN ZONALER RICHTUNG
## ANGEWANDT AUF SEQUENZIERTES MODELL
######################################################################
##

#list.model.lon.seq <- apply(model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
cl <- makeCluster(getOption("cl.cores", n.cpu))
list.model.lon.seq <- parApply(cl, model.max.lat.seq, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
stopCluster(cl)

## Gefiltertes Modell für Maxima des Zonal-Wind in Zonalrichtung
model.max.lon.seq <- sapply(list.model.lon.seq, "[[", 2)
rm(list.model.lon.seq)


######################################################################
## FEHLERGRÖẞEN
## MSE
## RMSE
######################################################################
##

residuals.cheb <- uwind.monmean - model.uwind
residuals.cheb.seq <- uwind.monmean - model.uwind.seq
mse <- sum(residuals.cheb ** 2) / length(residuals.cheb)
mse.seq <- sum(residuals.cheb.seq **2) / length(residuals.cheb.seq)
rmse <- sqrt(sum(residuals.cheb ** 2) / length(residuals.cheb))
rmse.seq <- sqrt(sum(residuals.cheb.seq **2) / length(residuals.cheb.seq))
## rmse = 0.4079846  ## mse = 
## rmse = 0.2911683  ## mse = 0.08477901


######################################################################
######################################################################
save.image()










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
#     m[i,j] <- uwind.era.t63.monmean[i,j,1]*cos(lat.era.t63[j]) + 1/86400*uwind.era.t63.monmean[i,j,1]**2*cos(lat.era.t63[j])**2
#   }
# }
# #m <- uwind.era.t63.monmean*cos(lat.era.t63)
# 

