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

library(raster)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(egg)


setwd("~/Master_Thesis/02-r-code-git/")
path <- "03-data-nc/"
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
w.zon.mean <- apply(w.mean[,,], c(2,3), mean)
uvw.zon.mean <- apply(uvw.mean[,,], c(2,3), mean)

## ZONAL-WIND U

gg.data.raw <- u.zon.mean
breaks <- c(-Inf,seq(-5,35, 5),Inf)
gg.data.hovm <- melt(gg.data.raw)
gg.data.hovm$color <- as.character(cut(gg.data.hovm$value, breaks = breaks, labels = FALSE))
col.pal <- brewer.pal(11, "RdYlBu")
names(col.pal) <- as.character(1:10)

ggplot(gg.data.hovm, aes(x = lat[Var1], y = Var2, z = value, fill = color)) + 
  geom_raster() + scale_fill_manual(values = col.pal, guide_colourbar(title = "Zonal wind speed", reverse = FALSE)) +
  geom_contour(data = gg.data.hovm, aes(x = lat[Var1], y = Var2, z = value, fill = value), binwidth = 5, color = "gray0") +
  xlab("Latitude") + ylab("Pressure level") +
  scale_x_continuous(breaks = seq(-90, 90, 30)) +
  scale_y_continuous(breaks = lev)+
  coord_fixed(3)


## MERIDIONAL-WIND V

gg.data.raw <- v.zon.mean
breaks <- c(-Inf,seq(-2,2,0.5),Inf)
gg.data.hovm <- melt(gg.data.raw)
gg.data.hovm$color <- as.character(cut(gg.data.hovm$value, breaks = breaks, labels = FALSE))
col.pal <- brewer.pal(11, "RdYlBu")
names(col.pal) <- as.character(1:10)

ggplot(gg.data.hovm, aes(x = lat[Var1], y = Var2, z = value, fill = color)) + 
  geom_raster() + scale_fill_manual(values = col.pal, guide_colourbar(title = "Mer wind speed", reverse = FALSE)) +
  geom_contour(data = gg.data.hovm, aes(x = lat[Var1], y = Var2, z = value, fill = value), binwidth = 5, color = "gray0") +
  xlab("Latitude") + ylab("Pressure level") +
  scale_x_continuous(breaks = seq(-90, 90, 30)) +
  scale_y_continuous(breaks = lev)+
  coord_fixed(3)

## vertical wind speed

gg.data.raw <- w.zon.mean
breaks <- c(-Inf,0,Inf)
gg.data.hovm <- melt(gg.data.raw)
gg.data.hovm$color <- as.character(cut(gg.data.hovm$value, breaks = breaks, labels = FALSE))
col.pal <- brewer.pal(3, "RdYlBu")
names(col.pal) <- as.character(1:3)

ggplot(gg.data.hovm, aes(x = lat[Var1], y = Var2, z = value, fill = color)) + 
  geom_raster() + scale_fill_manual(values = col.pal, guide_colourbar(title = "Mer wind speed")) +
  geom_contour(data = gg.data.hovm, aes(x = lat[Var1], y = Var2, z = value, fill = value), binwidth = 5, color = "gray0") +
  xlab("Latitude") + ylab("Pressure level") +
  scale_x_continuous(breaks = seq(-90, 90, 30)) +
  scale_y_continuous(breaks = lev)+
  coord_fixed(3)


#################################################
## MERIDIONAL MEAN
u.mer.mean <- apply(u.mean[,,], c(1,3), mean)
u.mer.mean <- apply(u.mean[,,], c(1,3), mean)

v.mer.mean <- apply(v.mean[,,], c(1,3), mean)
uvw.mer.mean <- apply(uvw.mean[,,], c(1,3), mean)

## ZONAL-WIND U


## MERIDIONAL-WIND V


## BETRAG DES WINDFELDES





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







