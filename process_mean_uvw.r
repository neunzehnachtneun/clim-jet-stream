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
library(chron)

library(fields)
library(clim.pact)

setwd("~/Master_Thesis/02-r-code-git/")
path <- "data/"
# path <- "/home/skiefer/era/raw/"
file <- "era-t63-mean_uvw_13z.nh-trop-inv.nc"  # Nordhemisphäre + Südhemisphäre



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
u.mean <- ncvar_get(nc, "var131") # U-Wind-Komponente
v.mean <- ncvar_get(nc, "var132") # V-Wind-Komponente
w.mean <- ncvar_get(nc, "var135") # W-Wind-Komponente

lon <- ncvar_get(nc, "lon") # Längengrad
lat <- ncvar_get(nc, "lat") # Breitengrad
lev <- ncvar_get(nc, "lev") # Drucklevel
date.help <- ncvar_get(nc, "time")

nc_close(nc)
rm(nc)

uvw.mean <- sqrt( u.mean ** 2 + v.mean **2 + w.mean ** 2 )
##
## ZONAL MEAN
u.zon.mean <- apply(u.mean[,,], c(2,3), mean)
v.zon.mean <- apply(v.mean[,,], c(2,3), mean)
uvw.zon.mean <- apply(uvw.mean[,,], c(2,3), mean)

## ZONAL-WIND U
contour(lat, 1:13, u.zon.mean, xlab = "Breitengrad in (deg)", ylab = "Druckniveaus in (hPa)", axes = FALSE)
title("Zonal gemittelter Zonal-Wind in (m/s)")
axis(1, at = seq(-90,90,10), labels = TRUE)
axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))

## MERIDIONAL-WIND V
image.plot(lat, 1:13, v.zon.mean, xlab = "Breitengrad in (deg)", ylab = "Druckniveaus in (hPa)", axes = FALSE)
title("Zonal gemittelter Meridional-Wind in (m/s)")
axis(1, at = seq(-90,90,10), labels = TRUE)
axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))

## BETRAG DES WINDFELDES
image.plot(lat, 1:13, uvw.zon.mean, xlab = "Breitengrad in (deg)", ylab = "Druckniveaus in (hPa)", axes = FALSE)
title("Zonal gemittelter Wind in (m/s)")
axis(1, at = seq(-90,90,10), labels = TRUE)
axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))

##
## MERIDIONAL MEAN
u.mer.mean <- apply(u.mean[,,], c(1,3), mean)
v.mer.mean <- apply(v.mean[,,], c(1,3), mean)
uvw.mer.mean <- apply(uvw.mean[,,], c(1,3), mean)

## ZONAL-WIND U
image.plot(lon, 1:13, u.mer.mean, xlab = "Längengrad in (deg)", ylab = "Druckniveaus in (hPa)", axes = FALSE)
title("Meridional gemittelter Zonal-Wind in (m/s)")
axis(1, at = seq(0,360,40), labels = TRUE)
axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))

## MERIDIONAL-WIND V
contour(lon, 1:13, v.mer.mean, xlab = "Längengrad in (deg)", ylab = "Druckniveaus in (hPa)", axes = FALSE)
title("Meridional gemittelter Meridional-Wind in (m/s)")
axis(1, at = seq(0,360,40), labels = TRUE)
axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))

## BETRAG DES WINDFELDES
image.plot(lon, 1:13, uvw.mer.mean, xlab = "Längengrad in (deg)", ylab = "Druckniveaus in (hPa)", axes = FALSE)
title("Meridional gemittelter Wind in (m/s)")
axis(1, at = seq(0,360,40), labels = TRUE)
axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))
