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



setwd("~/01-Master-Thesis/02-r-code-git/")
path <- "03-data-nc/"
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

lon <- ncvar_get(nc, "lon") # Längengrad
lat <- ncvar_get(nc, "lat") # Breitengrad
lev <- ncvar_get(nc, "level") # Drucklevel
# date.help <- ncvar_get(nc, "time")

nc_close(nc)
rm(nc)

# betrage des horizontalen windfeldes
uv.mean <- sqrt( u.mean ** 2 + v.mean ** 2 )

#################################################
## ZONAL MEAN
u.zon.mean <- apply(u.mean[,,], c(2,3), mean)
u.zon.sd <- apply(u.mean[,,], c(2,3), sd)
v.zon.mean <- apply(v.mean[,,], c(2,3), mean)
v.zon.sd <- apply(v.mean[,,], c(2,3), sd)

uv.zon.mean <- apply(uv.mean[,,], c(2,3), mean)

## ZONAL-WIND U

pdf(file = "05-visu-praes/vschnitt_meridianmittel.pdf", width = 16, height = 9, family = "serif")
image.plot(lat, lev, uv.zon.mean[96:1, 13:1], col = rev(brewer.pal(11, "RdYlBu")),
           xlab = "Breitengrad (in deg)",
           ylab = "Drucklevel (in hPa)",
           main = "Zeitlich und meridional gemittelter Zonal-Wind in (in m/s)",
           axes = F)
contour(lat, lev, u.zon.mean[96:1, 13:1], nlevels = 16, add = TRUE)
axis(1, seq(-90, 90, 30), rev(seq(-90, 90, 30)))
axis(2, lev, rev(lev))
box()
dev.off()

pdf(file = "05-visu-praes/200hpa_u.pdf", width = 16, height = 9, family = "serif")
image.plot(lon, lat, u.mean[,,3], col = rev(brewer.pal(11, "RdYlBu")), 
           xlab = "Längenengrad (in deg)",
           ylab = "Breitengrad (in deg)",
           main = "Zeitlich gemittelter Zonal-Wind in (in m/s)",
           axes = F)
contour(lon, lat, u.mean[,,3], nlevels = 16, add = TRUE)
addland()
axis(1, seq(0, 360, 40))
axis(2, seq(-90, 90, 30))
box()
dev.off()

pdf(file = "05-visu-praes/200hpa_v.pdf", width = 16, height = 9, family = "serif")
image.plot(lon, lat, v.mean[,,3], col = rev(brewer.pal(11, "RdYlBu")), 
           xlab = "Längenengrad (in deg)",
           ylab = "Breitengrad (in deg)",
           main = "Zeitlich gemittelter Meridional-Wind in (in m/s)",
           axes = F)
contour(lon, lat, v.mean[,,3], nlevels = 16, add = TRUE)
addland()
axis(1, seq(0, 360, 40))
axis(2, seq(-90, 90, 30))
box()
dev.off()

rm(list = ls())
