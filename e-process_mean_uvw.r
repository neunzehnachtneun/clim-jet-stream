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


#################################################
## ZONAL MEAN
u.zon.mean <- apply(u.mean[,,], c(2,3), mean)
u.zon.sd <- apply(u.mean[,,], c(2,3), sd)
v.zon.mean <- apply(v.mean[,,], c(2,3), mean)
uvw.zon.mean <- apply(uvw.mean[,,], c(2,3), mean)

## ZONAL-WIND U
filled.contour(lat, 1:13, u.zon.mean, 
               xlab = "Breitengrad in (deg)", 
               ylab = "Druckniveaus in (hPa)",
               main = "Zonal gemittelter Zonal-Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(-90, 90, 10))
                 axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))
                 contour(lat, 1:13, u.zon.mean, nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
                 
               },
               color.palette = tim.colors, nlevels = 16
)


## MERIDIONAL-WIND V
filled.contour(lat, 1:13, v.zon.mean, 
               xlab = "Breitengrad in (deg)", 
               ylab = "Druckniveaus in (hPa)",
               main = "Zonal gemittelter Meridional-Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(-90, 90, 10))
                 axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))
                 contour(lat, 1:13, v.zon.mean, nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
               },
               color.palette = tim.colors, nlevels = 16
)

## BETRAG DES WINDFELDES
filled.contour(lat, 1:13, uvw.zon.mean, 
               xlab = "Breitengrad in (deg)", 
               ylab = "Druckniveaus in (hPa)",
               main = "Zonal gemittelter Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(-90, 90, 10))
                 axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))
                 contour(lat, 1:13, uvw.zon.mean, nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
               },
               color.palette = tim.colors, nlevels = 16
)



#################################################
## MERIDIONAL MEAN
u.mer.mean <- apply(u.mean[,,], c(1,3), mean)
u.mer.mean <- apply(u.mean[,,], c(1,3), mean)

v.mer.mean <- apply(v.mean[,,], c(1,3), mean)
uvw.mer.mean <- apply(uvw.mean[,,], c(1,3), mean)

## ZONAL-WIND U
filled.contour(lon, 1:13, u.mer.mean, 
               xlab = "Längengrad in (deg)", 
               ylab = "Druckniveaus in (hPa)",
               main = "Meridional gemittelter Zonal-Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(0, 360, 40))
                 axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))
                 contour(lon, 1:13, u.mer.mean, nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
               },
               color.palette = tim.colors, nlevels = 16
)

## MERIDIONAL-WIND V
filled.contour(lon, 1:13, v.mer.mean, 
               xlab = "Längengrad in (deg)", 
               ylab = "Druckniveaus in (hPa)",
               main = "Meridional gemittelter Meridional-Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(0, 360, 40))
                 axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))
                 contour(lon, 1:13, v.mer.mean, nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
               },
               color.palette = tim.colors, nlevels = 16
)

## BETRAG DES WINDFELDES
filled.contour(lon, 1:13, uvw.mer.mean, 
               xlab = "Längengrad in (deg)", 
               ylab = "Druckniveaus in (hPa)",
               main = "Meridional gemittelter Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(0, 360, 40))
                 axis(2, at = seq(1,13,2), labels = c('1000', '850', '700', '500',  '300',  '200', '100'))
                 contour(lon, 1:13, uvw.mer.mean, nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
               },
               color.palette = tim.colors, nlevels = 16
)






#################################################
##
filled.contour(lon, lat, u.mean[,,9], 
               xlab = "Längengrad in (deg)", 
               ylab = "Breitengrad in (deg)",
               main = "Meridional gemittelter Zonal-Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(0, 360, 40))
                 axis(2, at = seq(-90, 90, 20))
                 contour(lon, lat, u.mean[,,9], nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
               },
               color.palette = tim.colors, nlevels = 16
)


filled.contour(lon, lat, u.mean[,(which(lat < 25) : which(lat > 75)),9], 
               xlab = "Längengrad in (deg)", 
               ylab = "Breitengrad in (deg)",
               main = "Meridional gemittelter Zonal-Wind in (m/s)",
               plot.axes = {
                 axis(1, at = seq(0, 360, 40))
                 axis(2, at = seq(-90, 90, 20))
                 contour(lon, lat, u.mean[,,9], nlevels = 16,
                         drawlabels = TRUE, axes = FALSE, 
                         frame.plot = FALSE, add = TRUE,
                         col = "grey0", lty = 3, lwd = 1)
               },
               color.palette = tim.colors, nlevels = 16
)

image.plot(lon, lat[(which(lat > 25 & lat < 85))], u.mean[,(which(lat > 25 & lat < 85)),9])







