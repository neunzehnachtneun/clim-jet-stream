######################################################################
######################################################################
## ROUTINE ZUM EINLESEN VON ERA-DATEN (ZONAL-WIND) IM NCDF-FORMAT
## UND AUFFINDEN DES JETSTREAMS AUF NORDHEMISPHÄRE
## source('~/Master_Thesis/r-code-git/locate_jetstream_polynomial_2d.r')
######################################################################
######################################################################


library(ncdf)
library(ncdf4)
library(rootSolve)
library(parallel)
library(chron)
# eigenes package für least squares fit mit chebyshev polynomen
# install.packages("pckg.cheb_0.1.tar.gz", repos = NULL, type = "source")
library(pckg.cheb)


path <- "data/"
file <- "era--t63_ua_monmean_300hpa_nh.nc"  # Nordhemisphäre
#file <- "era--t63_ua_monmean_300hpa_sh.nc"  # Südhemisphäre
#file <- "era--t63_ua_monmean_300hpa.nc"     # Globus


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
dts = chron(dates. = time.help/24, origin. = c(month = 9,day = 1,year = 1957), format = "day mon year")
nc_close(nc)
rm(nc)


######################################################################
## NÜTZLICHE VARIABLEN AUS DATENSATZ
######################################################################
##

n.lat <- length(lat)
n.lat.diff <- n.lat - 1
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

# model.list <- apply(uwind.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = 23)
cl <- makeCluster(getOption("cl.cores", 2)) ## Variante für paralleles Rechnen
model.list <- parApply(cl, uwind.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = 23)
stopCluster(cl)

## Chebyshev-Koeffiziente
cheb.coeff <- sapply(model.list, "[[", 1)
cheb.coeff <- apply(array(data = cheb.coeff, dim = c(dim(cheb.coeff)[1], 192, 664)),  c(1,3), t)

## Gefiltertes Modell für Zonal-Wind
model.uwind <- sapply(model.list, "[[", 2)
model.uwind <- apply(array(data = model.uwind, dim = c(dim(model.uwind)[1], 192, 664)),  c(1,3), t)

## Erste Ableitung des gefilterten Modells für Zonalwind
model.uwind.deriv.1st <- sapply(model.list, "[[", 3)
model.uwind.deriv.1st <- apply(array(data = model.uwind.deriv.1st, dim = c(dim(model.uwind.deriv.1st)[1], 192, 664)),  c(1,3), t)

## Extrema des Modells (Positionen und Werte)
model.extr.lat <- sapply(model.list, "[[", 4)
model.extr.uwind <- sapply(model.list, "[[", 5)

fun.fill <- function(x, n) {
  while (length(x) < n) {
    x <- c(x, NA)
  }
  return(x)
}

model.extr.lat <- sapply(model.extr.lat, fun.fill, n = 24)
model.extr.lat <- apply(array(model.extr.lat, c(dim(model.extr.lat)[1], 192, 664)), c(1,3), t)
model.extr.uwind <- sapply(model.extr.uwind, fun.fill, n = 24)
model.extr.uwind <- apply(array(model.extr.uwind, c(dim(model.extr.uwind)[1], 192, 664)), c(1,3), t)


## Maxima des Modells (Positionen und Werte)
model.max.uwind <- apply(model.extr.uwind, c(1,3), max, na.rm = TRUE)
model.max.lat <- array(rep(0, 192*664), c(192, 664))
for (i in 1:664) {
  for (j in 1:192) {
    model.max.lat[j,i] <- model.extr.lat[j, which(model.extr.uwind[j,,i] == model.max.uwind[j,i]), i]
  }
}
rm(model.list)


######################################################################
## LEAST SQUARES FIT 
## CHEBYSHEV POLYNOME 8-TER ORDNUNG
## AN MERIDIONALE MAXIMA DES ZONALWINDS IN ZONALER RICHTUNG
######################################################################
##

#model.max.lon <- apply(model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
cl <- makeCluster(getOption("cl.cores", 2))
model.list <- parApply(cl, model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
stopCluster(cl)

## Gefiltertes Modell für Maxima des Zonal-Wind in Zonalrichtung
model.max.lon <- sapply(model.list, "[[", 2)
rm(model.list)


######################################################################
## LEAST SQUARES FIT ÜBER **SEQUENZEN** (l=8)
## CHEBYSHEV POLYNOME 3-TER ORDNUNG
## AN ZONAL-WIND IN MERIDIONALER RICHTUNG
######################################################################
##
pckg.cheb:::cheb.fit.seq(uwind.monmean[1,,1], lat, 3, 8)



######################################################################
## FEHLERGRÖẞEN
## MSE
## RMSE
######################################################################
##

residuals.cheb <- uwind.monmean - model.uwind
mse <- sum(residuals.cheb ** 2) / length(residuals.cheb)
rmse <- sqrt(sum(residuals.cheb ** 2) / length(residuals.cheb))
## rmse = 0.4079846  ## mse = ???
## rmse = 0.2911683  ## mse = 0.08477901













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

