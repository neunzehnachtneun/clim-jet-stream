## source('~/Master_Thesis/02-r-code-git/a-read_era_ncdf.r')
##
## ROUTINE ZUM EINLESEN VON ERA-DATEN (ZONAL-WIND) IM NCDF-FORMAT ####
####
####

####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

library(ncdf4)
library(chron)

setwd("~/01-Master-Thesis/02-r-code-git/")
path <- "03-data-nc/"
# path <- "/home/skiefer/era/raw/"
file <- "1958-2015-e4ei-t63-nh-uv-monmean.nc"  # Nordhemisphäre

####
## EINLESEN DER DATEN   ####
## ERA40 / ERA-INTERIM     #
## T63 - GRID - GAUSSIAN   #
## NORDHEMISPHÄRE & TROPEN #
## 192 (lat) * 64 (lon)    #
####

nc <- nc_open(paste(path, file, sep = ""))
# print(nc)
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
date.help. <- date.help - min(date.help)
dts = chron(dates. = date.help./24, origin. = c(month = 1,day = 15, year = 1958))
dts.month <- months(dts, abbreviate = TRUE)
dts.year <- years(dts)

## Zeitlich gemittelter Zonalwind
# u.mean <- apply(u.monmean,c(1,2),mean)
# u.std <- apply(u.monmean,c(1,2),sd)

## Meridional und zeitlich gemittelter Zonalwind
# u.mon.mer.mean <- apply(u.monmean, 2, mean)
# u.mon.mer.sd <- apply(u.mean, 2, sd)

## Meridional gemittelter Zonalwind
# u.monmean.mermean <- apply(u.monmean, c(2,3), mean)
# u.monmean.mersd <- apply(u.monmean, c(2,3), sd)

## Betrag der Windgeschwindigkeit
uv.era <- sqrt( u.era ** 2 + v.era ** 2 )


####
## ÜBERGABE D VARIABLEN ####
####

# save.image(file = 'data.RData')


read.uv <- function(path, file) {
  
}
