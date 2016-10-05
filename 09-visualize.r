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
pdf(file = "test.pdf")
for (ti in 1:2) {
  print(ti)
  for (lon in 1:192) {
    for (lat in 1:48) {
    }
  }
}
dev.off()


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

pdf(file = "model-uni.pdf", family="serif")
for (ti in 1:664) {
  print(ti)
  for (lo in 1:192) {
    
    plot(lat, uwind.monmean[lo,,ti], xlab = "Breitengrad", ylab = "Zonalwind in #", ylim=c(-25, 85), axes = FALSE)
    points(model.extr.lat[lo,,ti], model.extr.uwind[lo,,ti], pch = 20)
    lines(lat, model.uwind[lo,,ti], lty = 3)
    title(paste(dts.month[ti], dts.year[ti], "//", "Lon =", lon[lo], "deg", sep = " "))
    axis(1, at = seq(0,90,10), labels = TRUE)
    axis(2, at = seq(-25, 85, 10), labels = TRUE)
  }
}
dev.off()


pdf(file = "model-seq.pdf", family="serif")
for (ti in 1:2) {
  print(ti)
  for (lo in 1:192) {
    
    plot(lat, uwind.monmean[lo,,ti], xlab = "Breitengrad", ylab = "Zonalwind in #", ylim=c(-25, 85), axes = FALSE)
    points(model.extr.lat.seq[lo,,ti], model.extr.uwind.seq[lo,,ti], pch = 20)
    lines(lat, model.uwind.seq[lo,,ti], lty = 3)
    title(paste(dts.month[ti], dts.year[ti], "//", "Lon =", lon[lo], "deg", sep = " "))
    axis(1, at = seq(0,90,10), labels = TRUE)
    axis(2, at = seq(-25, 85, 10), labels = TRUE)
  }
}
dev.off()

plot(lat, uwind.monmean[lo,,ti], axes = FALSE)
points(model.extr.lat[lo,,ti], model.extr.uwind[lo,,ti], pch = 20)
lines(lat, model.uwind[lo,,ti], lty = 3)
title(paste(dts.month[ti], dts.year[ti], "//", "Lon =", lon[lo], sep = " "))
axis(1, at = seq(0,90,10), labels = TRUE)
axis(2, at = seq(-25, 85, 10), labels = TRUE)



plot(lat, uwind.monmean[lo,,ti], axes = FALSE)
points(model.extr.lat.seq[lo,,ti], model.extr.uwind.seq[lo,,ti], pch = 20)
lines(lat, model.uwind.seq[lo,,ti], lty = 3)
title(paste(dts.month[ti], dts.year[ti], "//", "Lon =", lon[lo], sep = " "))
axis(1, at = seq(0,90,10), labels = TRUE)
axis(2, at = seq(-25, 85, 10), labels = TRUE)

######################################################################
## IMAGE PLOT
## ZONAL WIND UND GEFITTETE MAXIMA
######################################################################
##


pdf(file = "test.pdf", family="serif")
for (ti in 1:664) {
  print(ti)
  image.plot(lon, lat, uwind.monmean[,,ti], xlab = "Längengrad", ylab = "Breitengrad", zlim = c(-20,80), axes = F)
  title(paste(time.era[ti]))
  axis(1, at = seq(0, 360, 40), labels = TRUE)
  axis(2, at = seq(0,90,10), labels = TRUE)
  addland(col = "grey50",lwd = 1)
  lines(lon, model.max.lon[,ti])
}
dev.off()



pdf(file = "test.pdf", family="serif")
for (ti in 1:664) {
  print(ti)
  contour(lon, lat, uwind.monmean[,,ti], xlab = "Längengrad", ylab = "Breitengrad", zlim = c(-20,80))
  title(paste(time.era[ti]))
  axis(1, at = seq(0, 360, 40), labels = TRUE)
  axis(2, at = seq(0,90,10), labels = TRUE)
  addland(col = "grey50",lwd = 1)
  lines(lon, model.max.lon[,ti])
}
dev.off()


######################################################################
## IMAGE PLOT
## SAISONALE MEANS UND STANDARDABWEICHUNG
######################################################################
##

## MAR APR MAY
pdf(file = "1-mam.pdf", family="serif", width = 30, height = 10)
for (i in 1:11) {
  image.plot(lon, lat, u.seas.mam.mean[,,i], xlab = "Längengrad", ylab = "Breitengrad", zlim = c(-15,70), axes = F)
  title(paste("mean & sd //", "Mar Apr May //", dts.year.mn[i],"-", dts.year.mn[i] + 4, sep = " "))
  axis(1, at = seq(0, 360, 30), labels = TRUE)
  axis(2, at = seq(-30,90,15), labels = TRUE)
  # addland(col = "grey50",lwd = 1)
  ## Sd # Mar Apr May
  contour(lon, lat, u.seas.mam.sd[,,i], add = TRUE)
}
dev.off()

## JUN JUL AUG
pdf(file = "2-jja.pdf", family="serif")
for (i in 1:11) {
  image.plot(lon, lat, u.seas.jja.mean[,,i], xlab = "Längengrad", ylab = "Breitengrad", zlim = c(-15,70), axes = F)
  title(paste("mean & sd //", "Jun Jul Aug //", dts.year.mn[i],"-", dts.year.mn[i] + 4, sep = " "))
  axis(1, at = seq(0, 360, 30), labels = TRUE)
  axis(2, at = seq(-30,90,15), labels = TRUE)
  # addland(col = "grey50",lwd = 1)
  ## Sd # Mar Apr May
  contour(lon, lat, u.seas.jja.sd[,,i], add = TRUE)
}
dev.off()

## SEP OCT NOV
pdf(file = "3-son.pdf", family="serif")
for (i in 1:11) {
  image.plot(lon, lat, u.seas.son.mean[,,i], xlab = "Längengrad", ylab = "Breitengrad", zlim = c(-15,70), axes = F)
  title(paste("mean & sd //", "Sep Oct Nov //", dts.year.mn[i],"-", dts.year.mn[i] + 4, sep = " "))
  axis(1, at = seq(0, 360, 30), labels = TRUE)
  axis(2, at = seq(-30,90,15), labels = TRUE)
  # addland(col = "grey50",lwd = 1)
  ## Sd # Mar Apr May
  contour(lon, lat, u.seas.son.sd[,,i], add = TRUE)
}
dev.off()

## DEC JAN FEB
pdf(file = "4-djf.pdf", family="serif")
for (i in 1:11) {
  image.plot(lon, lat, u.seas.djf.mean[,,i], xlab = "Längengrad", ylab = "Breitengrad", zlim = c(-15,70), axes = F)
  title(paste("mean & sd //", "Dec Jan Feb //", dts.year.mn[i],"-", dts.year.mn[i] + 4, sep = " "))
  axis(1, at = seq(0, 360, 30), labels = TRUE)
  axis(2, at = seq(-30,90,15), labels = TRUE)
  # addland(col = "grey50",lwd = 1)
  ## Sd # Mar Apr May
  contour(lon, lat, u.seas.djf.sd[,,i], add = TRUE)
}
dev.off()