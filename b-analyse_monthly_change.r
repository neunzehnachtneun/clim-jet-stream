source("b-analyse_monthly_change.r")
## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####

## EINLESEN DER DATEN ####
##
require(ncdf4)
nc <- nc_open("04-data-nc/b-1957-2016-e4ei-t63-uv-nh-monmean.nc")
u <- ncvar_get(nc, "u") # U-Wind-Komponente
v <- ncvar_get(nc, "v") # V-Wind-Komponente

lon <- ncvar_get(nc, "lon") # Längengrad
lat <- ncvar_get(nc, "lat") # Breitengrad
lev <- ncvar_get(nc, "level") # Drucklevel
date.help <- ncvar_get(nc, "time")

nc_close(nc)
rm(nc)

## ZEITLICHE AUFLÖSUNG
## 
library(chron)
dts = as.POSIXct(date.help*3600, origin = '1900-01-01 00:00', tz = 'UTC')
dts.month <- months(dts, abbreviate = TRUE)
dts.year <- years(dts)

## VERARBEITEN DER DATEN ####
## 
source("e-jetstream_detection_schemes.r")
source("n-help-functions.r")

## METHODE 1: find.jet.maximum ####
## 
find.jet.maximum(mat = u[,,1,1], axis = lat)

## METHODE 2: find.jet.chebpoly
##
find.jet.chebpoly(array = u[,,1,1], axis = lat)

## METHODE 3: find.jet.dijkstra.2d
## 
dts.month[1]
find.jet.dijkstra.2d(u = u[,,1,1], v = v[,,1,1], lon = lon, lat = lat, jet = "STJ", season = "warm")


