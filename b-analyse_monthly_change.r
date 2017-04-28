## source("b-analyse_monthly_change.r")
## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####

## EINLESEN DER DATEN ####
##
library(ncdf4)
nc <- nc_open("04-data-nc/b-1957-2016-e4ei-t63-uv-nh-monmean.nc")
u <- ncvar_get(nc, "u") # U-Wind-Komponente
v <- ncvar_get(nc, "v") # V-Wind-Komponente
lon <- ncvar_get(nc, "lon") # Längengrad
lat <- ncvar_get(nc, "lat") # Breitengrad
lev <- ncvar_get(nc, "level") # Drucklevel
date.help <- ncvar_get(nc, "time")
nc_close(nc); rm(nc)

## RÄUMLICHE AUFLÖSUNG
##
n.lon <- length(lon) ; n.lat = length(lat)

## ZEITLICHE AUFLÖSUNG
## 
library(chron)
dts = as.POSIXct(date.help*3600, origin = '1900-01-01 00:00', tz = 'UTC')
dts.month <- months(dts, abbreviate = TRUE); dts.year <- years(dts); 

# Unterscheidung von warmen & kalten Monaten
dts.cld.wrm <- rep(NA, length.out = length(dts))
dts.cld.wrm[which(dts.month == "Nov" | dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb" | dts.month == "Mar" | dts.month == "Apr")] <- "cold"
dts.cld.wrm[which(dts.month == "May" | dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug" | dts.month == "Sep" | dts.month == "Oct")] <- "warm"

# Unterscheidung von Jahreszeiten
dts.season <- rep(NA, length.out = length(dts))
dts.season[which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")] <- "djf"
dts.season[which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")] <- "mam"
dts.season[which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")] <- "jja"
dts.season[which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")] <- "son"


## VERARBEITEN DER DATEN ####
## 
source("e-jetstream_detection_schemes.r")
source("n-help-functions.r")


####
## SCHLEIFE ÜBER DRUCK LEVEL ####
####
for (p.lvl in 3:6) {
  print(lev[p.lvl])
}; p.lvl <- 4

## ZEITSCHRITTE
t.stp <- 1; print(paste(dts.month[t.stp], dts.year[t.stp]))
#

## METHODE 0: find.jet.maximum ####
## 
# m0 <- find.jet.maximum(matrix = u[,,p.lvl, t.stp], axis = lat)
# apply(X = u[,,p.lvl,1:2], MARGIN = 3, FUN = find.jet.maximum, axis = lat)


## METHODE 1: find.jet.chebpoly ####
##
library(parallel)
cl.fork <- makeCluster(24, type = "FORK")
# m1.t <- find.jets.chebpoly(matrix = u[,, p.lvl, t.stp], axis = lat, n.order = 8)
m1 <- parApply(cl.fork, X = u[,,p.lvl,], MARGIN = 3, FUN = find.jets.chebpoly, axis = lat)
stopCluster(cl.fork)



## METHODE 2: find.jet.dijkstra.2d ####
## 
library(foreach); library(doParallel)
cl.psock <- makeCluster(16, type = "PSOCK")
registerDoParallel(cl.psock)
m2 <- foreach(t.stp = 1:length(dts), .packages = "igraph") %dopar% {
  find.jets.dijkstra.2d(u = u[,, p.lvl, t.stp], 
                        v = v[,, p.lvl, t.stp], 
                        lon = lon, lat = lat, 
                        season = dts.cld.wrm[t.stp])}
stopCluster(cl.psock)


## ZWISCHENSPEICHERN DER WERTE UND ERNEUTES LADEN DES DATENSATZES ####
# Speichern
save.image()
# Laden
load("170427-b.RData")
ls()

# Nachladen der Packages
library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);


## FORMATIEREN DES DATENSATZES ####
## 
library(dplyr); library(reshape2)

## Jetdatensatz M1 & M2
# m1.STJ.lon  <- sapply(m1, "[[", "STJ.lon")
m1.STJ.lat  <- sapply(m1, "[[", "STJ.lat"); colnames(m1.STJ.lat) <- dts; rownames(m1.STJ.lat) <- lon
m1.STJ.u    <- sapply(m1, "[[", "STJ.u")
# m1.STJ.v    <- sapply(m1, "[[", "STJ.v")
m2.STJ.lon  <- sapply(m2, "[[", "STJ.lon")
m2.STJ.lat  <- sapply(m2, "[[", "STJ.lat")
m2.STJ.u    <- sapply(m2, "[[", "STJ.u")
m2.STJ.v    <- sapply(m2, "[[", "STJ.v")

# m1.PFJ.lon  <- sapply(m1, "[[", "PfJ.lon")
m1.PFJ.lat  <- sapply(m1, "[[", "PFJ.lat")
m1.PFJ.u    <- sapply(m1, "[[", "PFJ.u")
# m1.PFJ.v    <- sapply(m1, "[[", "PFJ.v")
m2.PFJ.lon  <- sapply(m2, "[[", "PfJ.lon")
m2.PFJ.lat  <- sapply(m2, "[[", "PFJ.lat")
m2.PFJ.u    <- sapply(m2, "[[", "PFJ.u")
m2.PFJ.v    <- sapply(m2, "[[", "PFJ.v")

# Melten des Jet-Datensatzes
data.jets <- melt(m1.STJ.lat,varnames = c("lon", "dts"),value.name = "STJ.lat.m1")
# Einfügen der Jahreszahlen, Monate, Jahreszeiten
data.jets$dts <- rep(dts, each = n.lon); data.jets$year <- rep(dts.year, each = n.lon)
data.jets$month <- rep(dts.month, each = n.lon); data.jets$season <- rep(dts.season, each = n.lon)
# Subtropenjet
data.jets$STJ.u.m1    <- melt(m1.STJ.u)[,3]
data.jets$STJ.lat.m2  <- melt(m2.STJ.lat)[,3]
data.jets$STJ.u.m2    <- melt(m2.STJ.u)[,3]
data.jets$STJ.v.m2    <- melt(m2.STJ.v)[,3]
# Polarjet
data.jets$PFJ.lat.m1  <- melt(m1.PFJ.lat)[,3]
data.jets$PFJ.u.m1    <- melt(m1.PFJ.u)[,3]
data.jets$PFJ.lat.m2  <- melt(m2.PFJ.lat)[,3]
data.jets$PFJ.u.m2    <- melt(m2.PFJ.u)[,3]
data.jets$PFJ.v.m2    <- melt(m2.PFJ.v)[,3]
# Umsortieren der Spalten des Datensatzes
data.jets <- data.jets[,c("dts", "year", "month", "season", "lon", "STJ.lat.m1", "STJ.lat.m2", "STJ.u.m1","STJ.u.m2", "STJ.v.m2", "PFJ.lat.m1", "PFJ.lat.m2", "PFJ.u.m1", "PFJ.u.m2", "PFJ.v.m2")]
# # Test, ob Datensatz vernünftig aussieht
# colnames(data.jets)
# head(data.jets)

## U-V-Datensatz
# Melten
data.uv <- melt(u, varnames = c("lon", "lat", "p.lvl", "t.stp"), value.name = "u")
data.uv$v <- melt(data = v, value.name = "v")[5]

## VISUALISIEREN DER DATEN ####
## 
library(ggplot2)

tst <- 
  ggplot(data = data.jets, mapping = aes(x = lon, y = dts)) + 
  geom_tile(mapping = aes(fill = m1.STJ.lat)) +
  scale_y_date(date_breaks = "5 years", date_labels = "%y")




