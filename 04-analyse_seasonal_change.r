######################################################################
## Berechnung von Mean und Sd
## über fünf Jahre & saisonal
######################################################################

load(file = "monthly.RData")
dts.year.mn <- seq(1960, 2010, 5)

ind.mam <- which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")
ind.jja <- which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")
ind.son <- which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")
ind.djf <- which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")

## Mittelwerte global
u.seas.mam.mean <- array( NA , dim = c(n.lon, n.lat, 11))
u.seas.mam.sd <- array( NA , dim = c(n.lon, n.lat, 11))
u.seas.jja.mean <- array( NA , dim = c(n.lon, n.lat, 11))
u.seas.jja.sd <- array( NA , dim = c(n.lon, n.lat, 11))
u.seas.son.mean <- array( NA , dim = c(n.lon, n.lat, 11))
u.seas.son.sd <- array( NA , dim = c(n.lon, n.lat, 11))
u.seas.djf.mean <- array( NA , dim = c(n.lon, n.lat, 11))
u.seas.djf.sd <- array( NA , dim = c(n.lon, n.lat, 11))

## Mittelwerte meridional *???*
u.mer.seas.mam.mean <- array( NA , dim = c(n.lat, 11))
u.mer.seas.mam.sd <- array( NA , dim = c(n.lat, 11))
u.mer.seas.jja.mean <- array( NA , dim = c(n.lat, 11))
u.mer.seas.jja.sd <- array( NA , dim = c(n.lat, 11))
u.mer.seas.son.mean <- array( NA , dim = c(n.lat, 11))
u.mer.seas.son.sd <- array( NA , dim = c(n.lat, 11))
u.mer.seas.djf.mean <- array( NA , dim = c(n.lat, 11))
u.mer.seas.djf.sd <- array( NA , dim = c(n.lat, 11))

for (i in seq(1, 11)) {
  print(i)
  yr.i <- dts.year.mn[i]
  ind.yr <- which(dts.year ==  yr.i | dts.year == (yr.i + 1) | dts.year == (yr.i + 2) | dts.year == (yr.i + 3) | dts.year == (yr.i + 4) )
  ## Mar Apr May
  ind.mam.yr <- intersect(ind.yr, ind.mam)
  u.seas.mam.mean[,,i] <- apply(u.monmean[,, ind.mam.yr], c(1,2), mean)
  u.seas.mam.sd[,,i] <- apply(u.monmean[,, ind.mam.yr], c(1,2), sd)
  u.mer.seas.mam.mean[,i] <- apply(u.monmean[,, ind.mam.yr], 2, mean)
  u.mer.seas.mam.sd[,i] <- apply(u.monmean[,, ind.mam.yr], 2, sd)
  ## Jun Jul Aug
  ind.jja.yr <- intersect(ind.yr, ind.jja)
  u.seas.jja.mean[,,i] <- apply(u.monmean[,, ind.jja.yr], c(1,2), mean)
  u.seas.jja.sd[,,i] <- apply(u.monmean[,, ind.jja.yr], c(1,2), sd)
  u.mer.seas.jja.mean[,i] <- apply(u.monmean[,, ind.jja.yr], 2, mean)
  u.mer.seas.jja.sd[,i] <- apply(u.monmean[,, ind.jja.yr], 2, sd)
  ## Sep Oct Nov
  ind.son.yr <- intersect(ind.yr, ind.son)
  u.seas.son.mean[,,i] <- apply(u.monmean[,, ind.son.yr], c(1,2), mean)
  u.seas.son.sd[,,i] <- apply(u.monmean[,, ind.son.yr], c(1,2), sd)
  u.mer.seas.son.mean[,i] <- apply(u.monmean[,, ind.son.yr], 2, mean)
  u.mer.seas.son.sd[,i] <- apply(u.monmean[,, ind.son.yr], 2, sd)
  ## Dec Jan Feb
  ind.djf.yr <- intersect(ind.yr, ind.djf)
  u.seas.djf.mean[,,i] <- apply(u.monmean[,, ind.djf.yr], c(1,2), mean)
  u.seas.djf.sd[,,i] <- apply(u.monmean[,, ind.djf.yr], c(1,2), sd)
  u.mer.seas.djf.mean[,i] <- apply(u.monmean[,, ind.djf.yr], 2, mean)
  u.mer.seas.djf.sd[,i] <- apply(u.monmean[,, ind.djf.yr], 2, sd)
  ## Löschen von Übergangsvariablen
  rm(yr.i, ind.yr, ind.mam.yr, ind.jja.yr, ind.son.yr, ind.djf.yr, i)
}

max(u.seas.mam.mean, u.seas.jja.mean, u.seas.son.mean, u.seas.djf.mean)
min(u.seas.mam.mean, u.seas.jja.mean, u.seas.son.mean, u.seas.djf.mean)
range(u.seas.mam.mean, u.seas.jja.mean, u.seas.son.mean, u.seas.djf.mean)

max(u.seas.mam.mean)
min(u.seas.mam.mean)
range(u.seas.mam.mean)

max(u.seas.jja.mean)
min(u.seas.jja.mean)
range(u.seas.jja.mean)

max(u.seas.son.mean)
min(u.seas.son.mean)
range(u.seas.son.mean)

max(u.seas.djf.mean)
min(u.seas.djf.mean)
range(u.seas.djf.mean)

a <- apply(u.mer.seas.djf.mean, 1, mean)
plot(u.mer.seas.djf.mean[,1]-a)
















###################################
###################################
###################################
image.plot(lon, lat, u.mean+u.std)
contour(lon, lat, u.std[,,1], add=TRUE)
addland(col= "grey50", lwd = 1)

cl <- makeCluster(getOption("cl.cores", n.cpu)) ## Variante für paralleles Rechnen
list.lat.m.sd <- parApply(cl, (u.mean[,] - u.std[,]), 1, pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
list.lat.mn <- parApply(cl, u.mean[,], 1, pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
list.lat.p.sd <- parApply(cl, (u.mean[,] + u.std[,]), 1, pckg.cheb:::cheb.fit, x.axis = lat, n = n.order.lat)
stopCluster(cl)

model.extr.lat.m.sd <- sapply(list.lat.m.sd, "[[", 4)
model.extr.lat.mn <- sapply(list.lat.mn, "[[", 4)
model.extr.lat.p.sd <- sapply(list.lat.m.sd, "[[", 4)

model.extr.u.m.sd <- sapply(list.lat.m.sd, "[[", 5)
model.extr.u.mn <- sapply(list.lat.mn, "[[", 5)
model.extr.u.p.sd <- sapply(list.lat.p.sd, "[[", 5)

model.extr.lat.m.sd <- sapply(model.extr.lat.m.sd, fun.fill, n = 16)
model.extr.lat.mn <- sapply(model.extr.lat.mn, fun.fill, n = 16)
model.extr.lat.p.sd <- sapply(model.extr.lat.p.sd, fun.fill, n = 16)

model.extr.u.m.sd <- sapply(model.extr.u.m.sd, fun.fill, n = 16)
model.extr.u.mn <- sapply(model.extr.u.mn, fun.fill, n = 16)
model.extr.u.p.sd <- sapply(model.extr.u.p.sd, fun.fill, n = 16)

model.max.u.m.sd <- apply(model.extr.u.m.sd, 2, max, na.rm = TRUE)
model.max.u.mn <- apply(model.extr.u.mn, 2, max, na.rm = TRUE)
model.max.u.p.sd <- apply(model.extr.u.p.sd, 2, max, na.rm = TRUE)

model.max.lat.m.sd <- array(rep(0, 192))
model.max.lat.mn <- array(rep(0, 192))
model.max.lat.p.sd <- array(rep(0, 192))

for (i in 1:192) {
  model.max.lat.m.sd[i] <- model.extr.lat.m.sd[which(model.extr.u.m.sd[,i] == model.max.u.m.sd[i]), i]
  model.max.lat.mn[i] <- model.extr.lat.mn[which(model.extr.u.mn[,i] == model.max.u.mn[i]), i]
  model.max.lat.p.sd[i] <- model.extr.lat.p.sd[which(model.extr.u.p.sd[,i] == model.max.u.p.sd[i]), i]
}

list.lon.m.sd <- pckg.cheb:::cheb.fit(d = model.max.lat.m.sd, x.axis = lon, n = 11)
list.lon.mn <- pckg.cheb:::cheb.fit(d = model.max.lat.mn, x.axis = lon, n = 11)
list.lon.p.sd <- pckg.cheb:::cheb.fit(d = model.max.lat.p.sd, x.axis = lon, n = 11)

model.max.lon.m.sd <- list.lon.m.sd[[2]]
model.max.lon.mn <- list.lon.mn[[2]]
model.max.lon.p.sd <- list.lon.p.sd[[2]]


lines(lon, model.max.lon.m.sd)
lines(lon, model.max.lon.mn)
lines(lon, model.max.lon.p.sd)
