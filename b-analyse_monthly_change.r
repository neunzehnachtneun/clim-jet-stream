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

# Unterscheidung von warmen & kalten Monaten
dts.cld.wrm <- rep(NA, length.out = length(dts))
dts.cld.wrm[which(dts.month == "Nov" | dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb" | dts.month == "Mar" | dts.month == "Apr")] <- "cold"
dts.cld.wrm[which(dts.month == "May" | dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug" | dts.month == "Sep" | dts.month == "Oct")] <- "warm"

# Unterscheidung von Jahreszeiten
dts.season <- rep(NA, length.out = length(dts))
dts.season[which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")] <- "DJF"
dts.season[which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")] <- "MAM"
dts.season[which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")] <- "JJA"
dts.season[which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")] <- "SON"

## VERARBEITEN DER DATEN ####
## 
source("e-jetstream_detection_schemes.r")
source("n-help-functions.r")
# cluster für paralleles Prozessieren
cl <- makeCluster(getOption("cl.cores", 2))

####
## SCHLEIFE ÜBER DRUCK LEVEL ####
####
for (p.lvl in 3:6) {
  print(lev[p.lvl])
}; p.lvl <- 3
## ZEITSCHRITTE
t.stp <- 1; print(paste(dts.month[t.stp], dts.year[t.stp]))
#

## METHODE 0: find.jet.maximum ####
## 
#m0 <- find.jet.maximum(matrix = u[,,p.lvl, t.stp], axis = lat)

## METHODE 1: find.jet.chebpoly
##
m1.t <- find.jets.chebpoly(matrix = u[,, p.lvl, t.stp], axis = lat, n.order = 8)
# plot(m2$MaxJ.lat - m1$MaxJ.lat)
# print(mean(m2$MaxJ.lat - m1$MaxJ.lat, na.rm = TRUE))
m1 <- clusterApply(cl, u[,,p.lvl,], fun = find.jets.chebpoly, c(1,3), axis = lat)
stopCluster(cl)
## METHODE 2: find.jet.dijkstra.2d
## 
m2 <- NULL
for (i in length(dts)) {
  m2[i] <- find.jets.dijkstra.2d(u = u[,,1,1], v = v[,,1,1], lon = lon, lat = lat, season = dts.cld.wrm[t.stp])
}



