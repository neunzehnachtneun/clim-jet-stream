######################################################################
## Berechnung von Mean und Sd
## über fünf Jahre & saisonal
######################################################################

load(file = "monthly.RData")


dts.year.mn <- seq(1960, 2013)
n.time <- length(dts.year.mn)
seas <- "jja" # jja son djf
for (seas in (c("mam", "jja", "son", "djf"))) {}
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
model.max.u.seas.mean <- array(NA , dim = c(n.lon, n.time))
model.max.u.seas.sd <- array(NA , dim = c(n.lon, n.time))
model.max.lat.seas.mean <- array(NA , dim = c(n.lon, n.time))
model.max.lat.seas.sd <- array(NA , dim = c(n.lon, n.time))

for (i in seq(1, n.time)) {
  print(i)
  yr.i <- dts.year.mn[i]
  ind.yr <- which(dts.year == (yr.i - 2) | dts.year == (yr.i - 1) | dts.year ==  yr.i | dts.year == (yr.i + 1) | dts.year == (yr.i + 2))
  ind.seas.yr <- intersect(ind.yr, ind.seas)
#  print(ind.seas.yr)
  u.seas.mean[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), mean)
  u.seas.sd[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), sd)
  model.max.u.seas.mean[,i] <- apply(model.max.u[, ind.seas.yr], 1, mean)
  model.max.u.seas.sd[,i] <- apply(model.max.u[, ind.seas.yr], 1, sd)
  model.max.lat.seas.mean[,i] <- apply(model.max.lat[, ind.seas.yr], 1, mean)
  model.max.lat.seas.sd[,i] <- apply(model.max.lat[, ind.seas.yr], 1, sd)
  
  
}

#max(u.seas.mean)
#min(u.seas.mean)
#range(u.seas.mean)





## Vorbereiten der Daten für ggplot2
gg.data.hovm <- melt(model.max.lat.seas.mean)

## Plotroutine
gg.hovm <- ggplot(data = gg.data.hovm, mapping = aes(x = lon[Var1], y = dts.year.mn[Var2], fill = value))
gg.hovm + geom_raster() + scale_fill_gradientn(
  colours = rev(tim.colors(14)), 
  limits = c(20, 70),
  guide_colourbar(title = "Breitengrad",
                  reverse = FALSE)) + 
  xlab("Längengrad") + ylab("Jahr") +
  scale_x_continuous(breaks = seq(0, 360, 30)) +
  scale_y_continuous(breaks = seq(1960, 2010, 10))


tst <- melt(apply(model.max.lat.seas.mean, 2, mean))
ggplot(data = tst, mapping = aes(x = "", y = Var2, fill = value))
  
  
  