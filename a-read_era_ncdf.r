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

setwd("~/Master_Thesis/02-r-code-git/")
path <- "03-data-nc/"
# path <- "/home/skiefer/era/raw/"
file <- "era-t63-1957-2016.nh-trop-inv.nc"  # Nordhemisphäre + Tropen

####
## EINLESEN DER DATEN   ####
## ERA40 / ERA-INTERIM     #
## T63 - GRID - GAUSSIAN   #
## NORDHEMISPHÄRE & TROPEN #
## 192 (lat) * 64 (lon)    #
####

nc <- nc_open(paste(path, file, sep = ""))
# print(nc)
u.era <- ncvar_get(nc, "var131") # U-Wind-Komponente
v.era <- ncvar_get(nc, "var132") # V-Wind-Komponente
# w.monmean <- ncvar_get(nc, "var135") # W-Wind-Komponente
# z.monmean <- ncvar_get(nc, "var129") # Geopotenzial
# t.monmean <- ncvar_get(nc, "var130") # Temperatur
# d.monmean <- ncvar_get(nc, "var155") # Divergenz

lon <- ncvar_get(nc, "lon") # Längengrad
lat <- ncvar_get(nc, "lat") # Breitengrad
lev <- ncvar_get(nc, "lev") # Drucklevel
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
dts = chron(dates. = date.help/24, origin. = c(month = 9,day = 1,year = 1957), format = "day mon year")
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

save.image(file = 'data.RData')

