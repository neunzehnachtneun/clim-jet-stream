######################################################################
######################################################################
## source('~/Master_Thesis/r-code-git/visualize.r')

library(fields)
library(clim.pact)

#load(".RData")

## Festlegen der Parameter
lo <- 1 # index längengrad
la <- 1 # index breitengrad
ti <- 1 # index zeit


######################################################################
## VERGLEICHSPLOT
## ERA-DATEN VS MODELL VS ABLEITUNG D. MODELLS
######################################################################
##

plot(lat, uwind.monmean[lo,,ti], type = "l")
lines(lat, model.uwind[lo,,ti], col = "dark red")
lines(lat, model.uwind.deriv.1st[lo,,ti], col = "light blue")


######################################################################
## IMAGE PLOT
## ZONAL WIND NORDHEMISPHÄRE IM ZEITLICHEN VERLAUF
######################################################################
##

image.plot(lon, lat, uwind.monmean[,,ti])
addland(col = "grey50",lwd = 1)


######################################################################
## DIFFERENZ IMAGE PLOT
## ZONAL WIND MINUS GEFILTERTES MODELL
######################################################################
##

image.plot(lon, lat, uwind.monmean[,,ti] - model.uwind[,,ti])
addland(col = "grey50",lwd = 1)


######################################################################
######################################################################
## VERGLEICHSPLOT
## DATEN VS MODELL VS EXTREMA
######################################################################
##

plot(lat, uwind.monmean[lo,,ti])
points(model.extr.lat[lo,,ti], model.extr.uwind[lo,,ti], pch = 20)
lines(lat, model.uwind[lo,,ti], lty = 3)

######################################################################
## IMAGE PLOT
## ZONAL WIND UND GEFITTETE MAXIMA
######################################################################
##

image.plot(lon, lat, uwind.monmean[,,ti])
lines(lon, model.max.lon[,ti])




