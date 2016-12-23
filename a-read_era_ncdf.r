## source('~/01-Master-Thesis/02-r-code-git/a-read_era_ncdf.r')
##
## ROUTINE ZUM EINLESEN VON ERA-DATEN (ZONAL-WIND) IM NCDF-FORMAT ####
####
####

####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

library(ncdf4)
library(chron)

####
## EINLESEN DER DATEN   ####
## ERA40 / ERA-INTERIM     #
## T63 - GRID - GAUSSIAN   #
## NORDHEMISPHÄRE & TROPEN #
## 192 (lat) * 48 (lon)    #
####

nc <- nc_open(paste(path, filename, sep = ""))
u.era <- ncvar_get(nc, "u") # U-Wind-Komponente
v.era <- ncvar_get(nc, "v") # V-Wind-Komponente

lon <- ncvar_get(nc, "lon") # Längengrad
lat <- ncvar_get(nc, "lat") # Breitengrad
lev <- ncvar_get(nc, "level") # Drucklevel
date.help <- ncvar_get(nc, "time")

nc_close(nc)
rm(nc)

####
## BERECHNEN ABHÄNGIGER GRÖẞEN ####
## MITTELWERTE, ABWEICHUNGEN ETC. #
## BETRAG DER WINDGESCHWINDIGKEIT #
####

## Räumliche Auflösung
lat.len <- length(lat)
lon.len <- length(lon)

## Zeitliche Auflösung
dts = as.POSIXct(date.help*3600, origin = '1900-01-01 00:00', tz = 'UTC')
dts.month <- months(dts, abbreviate = TRUE)
dts.year <- years(dts)

## Betrag der Windgeschwindigkeit
uv.era <- sqrt( u.era ** 2 + v.era ** 2 )

