######################################################################
## Berechnung von Mean und Sd
## über fünf Jahre & saisonal
######################################################################

load(file = "monthly.RData")


dts.year.mn <- seq(1960, 2010, 5)
n.time <- length(dts.year.mn)
seas <- "djf" # jja son djf
if (seas == "mam") {
  ind.seas <- which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")
} else if (seas == "jja") {
  ind.seas <- which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")
} else if (seas == "son") {
  ind.seas <- which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")
} else if (seas == "djf") {
  ind.seas <- which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")
}

## Mittelwerte global
## Vorbereiten der Matrizen
u.seas.mean <- array(NA , dim = c(n.lon, n.lat, n.time))
u.seas.sd <- array(NA , dim = c(n.lon, n.lat, n.time))
model.max.u.mean <- array(NA , dim = c(n.lon, n.time))
model.max.u.sd <- array(NA , dim = c(n.lon, n.time))
model.max.lat.mean <- array(NA , dim = c(n.lon, n.time))
model.max.lat.sd <- array(NA , dim = c(n.lon, n.time))

for (i in seq(1, 11)) {
  print(i)
  yr.i <- dts.year.mn[i]
  ind.yr <- which(dts.year ==  yr.i | dts.year == (yr.i + 1) | dts.year == (yr.i + 2) | dts.year == (yr.i + 3) | dts.year == (yr.i + 4) )
  ind.seas.yr <- intersect(ind.yr, ind.seas)
  u.seas.mean[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), mean)
  u.seas.sd[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), sd)
  model.max.u.mean[,i] <- apply(model.max.u[, ind.seas.yr], 1, mean)
  model.max.u.sd[,i] <- apply(model.max.u[, ind.seas.yr], 1, sd)
  model.max.lat.mean[,i] <- apply(model.max.lat[, ind.seas.yr], 1, mean)
  model.max.lat.sd[,i] <- apply(model.max.lat[, ind.seas.yr], 1, sd)
}

max(u.seas.mean)
min(u.seas.mean)
range(u.seas.mean)


image.plot(lon, dts.year.mn, model.max.lat.mean)
contour(lon, dts.year.mn, model.max.lat.sd, add = TRUE)








