######################################################################
######################################################################
## ROUTINE ZUM EINLESEN VON ERA-DATEN (ZONAL-WIND) IM NCDF-FORMAT
## UND AUFFINDEN DES JETSTREAMS AUF NORDHEMISPHÄRE
## source('~/Master_Thesis/02-r-code-git/locate_jetstream_polynomial_2d.r')
######################################################################
######################################################################


######################################################################
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE
######################################################################
##

library(ncdf4)
library(parallel)
library(chron)

# eigenes package für least squares fit mit chebyshev polynomen
# install.packages("pckg.cheb_0.9.tar.gz", repos = NULL, type = "source")
library(pckg.cheb)

setwd("~/Master_Thesis/02-r-code-git/")
path <- "data/"
# path <- "/home/skiefer/era/raw/"
file <- "era-t63-1957-2016.nh-trop-inv.nc"  # Nordhemisphäre + Tropen


######################################################################
## KLEINE HILFSFUNKTIONEN
######################################################################
##

fun.fill <- function(x, n) {
  while (length(x) < n) {
    x <- c(x, NA)
  }
  return(x)
}


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
u.monmean <- ncvar_get(nc, "var131") # U-Wind-Komponente
v.monmean <- ncvar_get(nc, "var132") # V-Wind-Komponente
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

uv.monmean <- sqrt( u.monmean ** 2 + v.monmean **2 )

######################################################################
## VARIABLEN UND PARAMETER
######################################################################
##

n.cpu <- 24 # Anzahl der CPUs für parApply
n.order.lat <- 31 # Ordnung des Least-Square-Verfahrens für Fit über Breitengrad
n.order.lon <- 8 # Ordnung des Least-Square-Verfahrens für Fit über Längengrad

## Räumliche Auflösung
n.lat <- length(lat)
n.lon <- length(lon)

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



######################################################################
## LEAST SQUARES FIT 
## CHEBYSHEV POLYNOME 23-TER ORDNUNG
## AN ZONAL WIND IN MERIDIONALER RICHTUNG
######################################################################
##

# list.model.lat <- apply(u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
list.model.lat <- parApply(cl, u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit.roots, x.axis = lat, n = n.order.lat, bc.harmonic = FALSE, roots.bound.l = 20, roots.bound.u = 80)
stopCluster(cl)
dim.list <- dim(list.model.lat)

## Chebyshev-Koeffizienten
cheb.coeff <- sapply(list.model.lat, "[[", 1)
cheb.coeff <- apply(array(data = cheb.coeff, dim = c((n.order.lat + 1), dim.list[1], dim.list[2])) , c(1,3), t)

## Gefiltertes Modell für Zonal-Wind
model.u <- sapply(list.model.lat, "[[", 2)
model.u <- apply(array(data = model.u, dim = c(n.lat, dim.list[1], dim.list[2])),  c(1,3), t)

## Erste Ableitung des gefilterten Modells für Zonalwind
model.u.deriv.1st <- sapply(list.model.lat, "[[", 3)
model.u.deriv.1st <- apply(array(data = model.u.deriv.1st, dim = c(n.lat, dim.list[1], dim.list[2])),  c(1,3), t)

## Extrema des Modells (Positionen und Werte)
model.extr.lat <- sapply(list.model.lat, "[[", 4)
model.extr.u <- sapply(list.model.lat, "[[", 5)
model.extr.lat <- sapply(model.extr.lat, fun.fill, n = 24)
model.extr.lat <- apply(array(model.extr.lat, c(24, dim.list[1], dim.list[2])), c(1,3), t)
model.extr.u <- sapply(model.extr.u, fun.fill, n = 24)
model.extr.u <- apply(array(model.extr.u, c(24, dim.list[1], dim.list[2])), c(1,3), t)

## Maxima des Modells (Positionen und Werte)
model.max.u <- apply(model.extr.u, c(1,3), max, na.rm = TRUE)
model.max.lat <- array(rep(0, 192*664), c(dim.list))
for (i in 1:dim.list[2]) {
  for (j in 1:dim.list[1]) {
    model.max.lat[j,i] <- model.extr.lat[j, which(model.extr.u[j,,i] == model.max.u[j,i]), i]
  }
}
rm(list.model.lat, dim.list)


######################################################################
## LEAST SQUARES FIT 
## CHEBYSHEV POLYNOME 8-TER ORDNUNG
## AN MERIDIONALE MAXIMA DES ZONALWINDS IN ZONALER RICHTUNG
######################################################################
##

#list.model.lon <- apply(model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = n.order.lon)
cl <- makeCluster(getOption("cl.cores", n.cpu))
list.model.lon <- parApply(cl, model.max.lat, 2, pckg.cheb:::cheb.fit.roots, x.axis = lon, n = n.order.lon, bc.harmonic = TRUE)
stopCluster(cl)

## Gefiltertes Modell für Maxima des Zonal-Wind in Zonalrichtung
#model.max.lon <- list.model.lon, "[[", 2)
#rm(list.model.lon)


######################################################################
## FEHLERGRÖẞEN
## MSE
## RMSE
######################################################################
##

residuals.cheb <- u.monmean - model.u
#residuals.cheb.seq <- u.monmean - model.u.seq
mse <- sum(residuals.cheb ** 2) / length(residuals.cheb)
#mse.seq <- sum(residuals.cheb.seq **2) / length(residuals.cheb.seq)
rmse <- sqrt(sum(residuals.cheb ** 2) / length(residuals.cheb))
#rmse.seq <- sqrt(sum(residuals.cheb.seq **2) / length(residuals.cheb.seq))

## rmse.seq = 0.4079846  ## mse.seq = 0.1664514
## rmse     = 0.2911683  ## mse     = 0.08477901


######################################################################
######################################################################
save.image()



######################################################################
## Berechnung von Mean und Sd
## über fünf Jahre & saisonal
######################################################################
# dts.year.mn <- seq(1960, 2010, 5)
# 
# ind.mam <- which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")
# ind.jja <- which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")
# ind.son <- which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")
# ind.djf <- which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")
# 
# ## Mittelwerte global
# u.seas.mam.mean <- array( NA , dim = c(n.lon, n.lat, 11))
# u.seas.mam.sd <- array( NA , dim = c(n.lon, n.lat, 11))
# u.seas.jja.mean <- array( NA , dim = c(n.lon, n.lat, 11))
# u.seas.jja.sd <- array( NA , dim = c(n.lon, n.lat, 11))
# u.seas.son.mean <- array( NA , dim = c(n.lon, n.lat, 11))
# u.seas.son.sd <- array( NA , dim = c(n.lon, n.lat, 11))
# u.seas.djf.mean <- array( NA , dim = c(n.lon, n.lat, 11))
# u.seas.djf.sd <- array( NA , dim = c(n.lon, n.lat, 11))
# 
# ## Mittelwerte meridional *???*
# u.mer.seas.mam.mean <- array( NA , dim = c(n.lat, 11))
# u.mer.seas.mam.sd <- array( NA , dim = c(n.lat, 11))
# u.mer.seas.jja.mean <- array( NA , dim = c(n.lat, 11))
# u.mer.seas.jja.sd <- array( NA , dim = c(n.lat, 11))
# u.mer.seas.son.mean <- array( NA , dim = c(n.lat, 11))
# u.mer.seas.son.sd <- array( NA , dim = c(n.lat, 11))
# u.mer.seas.djf.mean <- array( NA , dim = c(n.lat, 11))
# u.mer.seas.djf.sd <- array( NA , dim = c(n.lat, 11))
# 
# for (i in seq(1, 11)) {
#   print(i)
#   yr.i <- dts.year.mn[i]
#   ind.yr <- which(dts.year ==  yr.i | dts.year == (yr.i + 1) | dts.year == (yr.i + 2) | dts.year == (yr.i + 3) | dts.year == (yr.i + 4) )
#   ## Mar Apr May
#   ind.mam.yr <- intersect(ind.yr, ind.mam)
#   u.seas.mam.mean[,,i] <- apply(u.monmean[,, ind.mam.yr], c(1,2), mean)
#   u.seas.mam.sd[,,i] <- apply(u.monmean[,, ind.mam.yr], c(1,2), sd)
#   u.mer.seas.mam.mean[,i] <- apply(u.monmean[,, ind.mam.yr], 2, mean)
#   u.mer.seas.mam.sd[,i] <- apply(u.monmean[,, ind.mam.yr], 2, sd)
#   ## Jun Jul Aug
#   ind.jja.yr <- intersect(ind.yr, ind.jja)
#   u.seas.jja.mean[,,i] <- apply(u.monmean[,, ind.jja.yr], c(1,2), mean)
#   u.seas.jja.sd[,,i] <- apply(u.monmean[,, ind.jja.yr], c(1,2), sd)
#   u.mer.seas.jja.mean[,i] <- apply(u.monmean[,, ind.jja.yr], 2, mean)
#   u.mer.seas.jja.sd[,i] <- apply(u.monmean[,, ind.jja.yr], 2, sd)
#   ## Sep Oct Nov
#   ind.son.yr <- intersect(ind.yr, ind.son)
#   u.seas.son.mean[,,i] <- apply(u.monmean[,, ind.son.yr], c(1,2), mean)
#   u.seas.son.sd[,,i] <- apply(u.monmean[,, ind.son.yr], c(1,2), sd)
#   u.mer.seas.son.mean[,i] <- apply(u.monmean[,, ind.son.yr], 2, mean)
#   u.mer.seas.son.sd[,i] <- apply(u.monmean[,, ind.son.yr], 2, sd)
#   ## Dec Jan Feb
#   ind.djf.yr <- intersect(ind.yr, ind.djf)
#   u.seas.djf.mean[,,i] <- apply(u.monmean[,, ind.djf.yr], c(1,2), mean)
#   u.seas.djf.sd[,,i] <- apply(u.monmean[,, ind.djf.yr], c(1,2), sd)
#   u.mer.seas.djf.mean[,i] <- apply(u.monmean[,, ind.djf.yr], 2, mean)
#   u.mer.seas.djf.sd[,i] <- apply(u.monmean[,, ind.djf.yr], 2, sd)
#   ## Löschen von Übergangsvariablen
#   rm(yr.i, ind.yr, ind.mam.yr, ind.jja.yr, ind.son.yr, ind.djf.yr, i)
# }
# 
# max(u.seas.mam.mean, u.seas.jja.mean, u.seas.son.mean, u.seas.djf.mean)
# min(u.seas.mam.mean, u.seas.jja.mean, u.seas.son.mean, u.seas.djf.mean)
# range(u.seas.mam.mean, u.seas.jja.mean, u.seas.son.mean, u.seas.djf.mean)
# 
# max(u.seas.mam.mean)
# min(u.seas.mam.mean)
# range(u.seas.mam.mean)
# 
# max(u.seas.jja.mean)
# min(u.seas.jja.mean)
# range(u.seas.jja.mean)
# 
# max(u.seas.son.mean)
# min(u.seas.son.mean)
# range(u.seas.son.mean)
# 
# max(u.seas.djf.mean)
# min(u.seas.djf.mean)
# range(u.seas.djf.mean)
# 
# 
# image.plot(lon, lat, u.mean+u.std)
# contour(lon, lat, u.std[,,1], add=TRUE)
# addland(col= "grey50", lwd = 1)
# 
# cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
# list.lat.m.sd <- parApply(cl, (u.mean[,] - u.std[,]), 1, pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
# list.lat.mn <- parApply(cl, u.mean[,], 1, pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
# list.lat.p.sd <- parApply(cl, (u.mean[,] + u.std[,]), 1, pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
# stopCluster(cl)
# 
# model.extr.lat.m.sd <- sapply(list.lat.m.sd, "[[", 4)
# model.extr.lat.mn <- sapply(list.lat.mn, "[[", 4)
# model.extr.lat.p.sd <- sapply(list.lat.m.sd, "[[", 4)
# 
# model.extr.u.m.sd <- sapply(list.lat.m.sd, "[[", 5)
# model.extr.u.mn <- sapply(list.lat.mn, "[[", 5)
# model.extr.u.p.sd <- sapply(list.lat.p.sd, "[[", 5)
# 
# model.extr.lat.m.sd <- sapply(model.extr.lat.m.sd, fun.fill, n = 16)
# model.extr.lat.mn <- sapply(model.extr.lat.mn, fun.fill, n = 16)
# model.extr.lat.p.sd <- sapply(model.extr.lat.p.sd, fun.fill, n = 16)
# 
# model.extr.u.m.sd <- sapply(model.extr.u.m.sd, fun.fill, n = 16)
# model.extr.u.mn <- sapply(model.extr.u.mn, fun.fill, n = 16)
# model.extr.u.p.sd <- sapply(model.extr.u.p.sd, fun.fill, n = 16)
# 
# model.max.u.m.sd <- apply(model.extr.u.m.sd, 2, max, na.rm = TRUE)
# model.max.u.mn <- apply(model.extr.u.mn, 2, max, na.rm = TRUE)
# model.max.u.p.sd <- apply(model.extr.u.p.sd, 2, max, na.rm = TRUE)
# 
# model.max.lat.m.sd <- array(rep(0, 192))
# model.max.lat.mn <- array(rep(0, 192))
# model.max.lat.p.sd <- array(rep(0, 192))
# 
# for (i in 1:192) {
#   model.max.lat.m.sd[i] <- model.extr.lat.m.sd[which(model.extr.u.m.sd[,i] == model.max.u.m.sd[i]), i]
#   model.max.lat.mn[i] <- model.extr.lat.mn[which(model.extr.u.mn[,i] == model.max.u.mn[i]), i]
#   model.max.lat.p.sd[i] <- model.extr.lat.p.sd[which(model.extr.u.p.sd[,i] == model.max.u.p.sd[i]), i]
# }
# 
# list.lon.m.sd <- pckg.cheb:::cheb.fit(d = model.max.lat.m.sd, x.axis = lon, n = 11)
# list.lon.mn <- pckg.cheb:::cheb.fit(d = model.max.lat.mn, x.axis = lon, n = 11)
# list.lon.p.sd <- pckg.cheb:::cheb.fit(d = model.max.lat.p.sd, x.axis = lon, n = 11)
# 
# model.max.lon.m.sd <- list.lon.m.sd[[2]]
# model.max.lon.mn <- list.lon.mn[[2]]
# model.max.lon.p.sd <- list.lon.p.sd[[2]]
# 
# 
# lines(lon, model.max.lon.m.sd)
# lines(lon, model.max.lon.mn)
# lines(lon, model.max.lon.p.sd)




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
#     m[i,j] <- u.era.t63.monmean[i,j,1]*cos(lat.era.t63[j]) + 1/86400*u.era.t63.monmean[i,j,1]**2*cos(lat.era.t63[j])**2
#   }
# }
# #m <- u.era.t63.monmean*cos(lat.era.t63)
# 




# ######################################################################
# ## LEAST SQUARES FIT ÜBER **SEQUENZEN** (l=8)
# ## CHEBYSHEV POLYNOME 3-TER ORDNUNG
# ## AN ZONAL-WIND IN MERIDIONALER RICHTUNG
# ######################################################################
# ##
# 
# # list.model.lat.seq <- apply(u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit.seq, x.axis = lat, n = n.order.lat.seq, l = len.seq)
# cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
# list.model.lat.seq <- parApply(cl, u.monmean[,,], c(1,3), pckg.cheb:::cheb.fit.seq, x.axis = lat, n = n.order.lat.seq, l = len.seq)
# stopCluster(cl)
# dim.list <- dim(list.model.lat.seq)
# 
# ## Gefiltertes Modell für Zonal-Wind
# model.u.seq <- sapply(list.model.lat.seq, "[[", 1)
# model.u.seq <- apply(array(data = model.u.seq, dim = c(n.lat, dim.list[1], dim.list[2])),  c(1,3), t)
# 
# ## Erste Ableitung des gefilterten Modells für Zonalwind
# model.u.deriv.1st.seq <- sapply(list.model.lat.seq, "[[", 2)
# model.u.deriv.1st.seq <- apply(array(data = model.u.deriv.1st.seq, dim = c(n.lon, dim.list[1], dim.list[2])),  c(1,3), t)
# 
# ## Extrema des Modells (Positionen und Werte)
# model.extr.lat.seq <- sapply(list.model.lat.seq, "[[", 3)
# model.extr.lat.seq <- sapply(model.extr.lat.seq, fun.fill, n = 24)
# model.extr.lat.seq <- apply(array(model.extr.lat.seq, c(24, dim.list[1], dim.list[2])), c(1,3), t)
# model.extr.u.seq <- sapply(list.model.lat.seq, "[[", 4)
# model.extr.u.seq <- sapply(model.extr.u.seq, fun.fill, n = 24)
# model.extr.u.seq <- apply(array(model.extr.u.seq, c(24, dim.list[1], dim.list[2])), c(1,3), t)
# 
# ## Maxima des Modells (Positionen und Werte)
# model.max.u.seq <- apply(model.extr.u.seq, c(1,3), max, na.rm = TRUE)
# model.max.lat.seq <- array(rep(0, dim.list[1]*dim.list[2]), c(dim.list))
# for (i in 1:dim.list[2]) {
#   for (j in 1:dim.list[1]) {
#     model.max.lat.seq[j,i] <- model.extr.lat.seq[j, which(model.extr.u.seq[j,,i] == model.max.u.seq[j,i]), i]
#   }
# }
# rm(list.model.lat.seq, dim.list)
# 
# 
# ######################################################################
# ## LEAST SQUARES FIT 
# ## CHEBYSHEV POLYNOME 8-TER ORDNUNG
# ## AN MERIDIONALE MAXIMA DES ZONALWINDS IN ZONALER RICHTUNG
# ## ANGEWANDT AUF SEQUENZIERTES MODELL
# ######################################################################
# ##
# 
# #list.model.lon.seq <- apply(model.max.lat, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = 8)
# cl <- makeCluster(getOption("cl.cores", n.cpu))
# list.model.lon.seq <- parApply(cl, model.max.lat.seq, 2, pckg.cheb:::cheb.fit, x.axis = lon, n = n.order.lon)
# stopCluster(cl)
# 
# ## Gefiltertes Modell für Maxima des Zonal-Wind in Zonalrichtung
# model.max.lon.seq <- sapply(list.model.lon.seq, "[[", 2)
# rm(list.model.lon.seq)

