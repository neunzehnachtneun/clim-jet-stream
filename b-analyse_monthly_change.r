## source("b-analyse_monthly_change.r")
## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####
setwd("~/01-Master-Thesis/02-code-git/")

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

## Beschreibung der Matrizen
dimnames(u) <- list(lon, lat, lev, dts)
dimnames(v) <- list(lon, lat, lev, dts)

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
save.image("170503-a.RData")
# Laden
load("170503-a.RData")
ls()
# Nachladen der Packages
library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## FORMATIEREN DER DATENSATZE UND LÖSCHEN ÜBERFLÜSSIGER VARIABLEN ####
## 
library(plyr); library(dplyr); library(reshape2)

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
m2.PFJ.lon  <- sapply(m2, "[[", "PFJ.lon")
m2.PFJ.lat  <- sapply(m2, "[[", "PFJ.lat")
m2.PFJ.u    <- sapply(m2, "[[", "PFJ.u")
m2.PFJ.v    <- sapply(m2, "[[", "PFJ.v")


## UMRECHNEN DER DATEN VON PAZIFIKZENTRISCHEM AUF EUROPAZENTRISCHES GITTER ####
## 
lon.hlp <- lon - 180
# lon.hlp <- c( lon[97:192] - 360, lon[1:96] )
lon[1]; lon.hlp[97];
# cut.lon <- which(lon.hlp == lon[1])
u <- u[c(97:192,1:96),,,]; dimnames(u) <- list(lon.hlp, lat, lev, dts)
v <- v[c(97:192,1:96),,,]; dimnames(v) <- list(lon.hlp, lat, lev, dts)
m1.STJ.lat  <- m1.STJ.lat[c(97:192,1:96),]; rownames(m1.STJ.lat) <- lon.hlp
m1.STJ.u    <- m1.STJ.u[c(97:192,1:96),]
m2.STJ.lon  <- m2.STJ.lon[c(97:192,1:96),]
m2.STJ.lat  <- m2.STJ.lat[c(97:192,1:96),]
m2.STJ.u    <- m2.STJ.u[c(97:192,1:96),]
m2.STJ.v    <- m2.STJ.v[c(97:192,1:96),]
m1.PFJ.lat  <- m1.PFJ.lat[c(97:192,1:96),]
m1.PFJ.u    <- m1.PFJ.u[c(97:192,1:96),]
m2.PFJ.lon  <- m2.PFJ.lon[c(97:192,1:96),]
m2.PFJ.lat  <- m2.PFJ.lat[c(97:192,1:96),]
m2.PFJ.u    <- m2.PFJ.u[c(97:192,1:96),]
m2.PFJ.v    <- m2.PFJ.v[c(97:192,1:96),]


## MELTEN DES DATENSATZES (RESHAPE2::MELT) FÜR GGPLOT2 ####
## 
data.jets <- melt(m1.STJ.lat,varnames = c("lon", "dts"),value.name = "STJ.lat.m1")
# Einfügen der Jahreszahlen, Monate, Jahreszeiten
data.jets$dts <- rep(dts, each = n.lon); data.jets$year <- rep(dts.year, each = n.lon)
data.jets$month <- rep(dts.month, each = n.lon); data.jets$season <- rep(dts.season, each = n.lon)
# Subtropenjet
data.jets$STJ.u.m1    <- melt(m1.STJ.u)$value
data.jets$STJ.lat.m2  <- melt(m2.STJ.lat)$value
data.jets$STJ.u.m2    <- melt(m2.STJ.u)$value
data.jets$STJ.v.m2    <- melt(m2.STJ.v)$value
# Polarjet
data.jets$PFJ.lat.m1  <- melt(m1.PFJ.lat)$value
data.jets$PFJ.u.m1    <- melt(m1.PFJ.u)$value
data.jets$PFJ.lat.m2  <- melt(m2.PFJ.lat)$value
data.jets$PFJ.u.m2    <- melt(m2.PFJ.u)$value
data.jets$PFJ.v.m2    <- melt(m2.PFJ.v)$value
# Umsortieren der Spalten des Datensatzes
data.jets <- data.jets[,c("dts", "year", "month", "season", "lon", "STJ.lat.m1", "STJ.lat.m2", "STJ.u.m1","STJ.u.m2", "STJ.v.m2", "PFJ.lat.m1", "PFJ.lat.m2", "PFJ.u.m1", "PFJ.u.m2", "PFJ.v.m2")]
# colnames(data.jets)
# head(data.jets)

## U-V-Datensatz
# Melten
data.uv <- melt(u, varnames = c("lon", "lat", "p.lvl", "t.stp"), value.name = "u")
data.uv$v <- melt(data = v)$value
data.uv$uv <- sqrt( data.uv$u ** 2 + data.uv$v ** 2 )

## Löschen überflüssiger Variablen
rm(u, v, m1, m2, 
   m1.STJ.lat, m1.STJ.u, m1.PFJ.lat, m1.PFJ.u, 
   m2.STJ.lon, m2.STJ.lat, m2.STJ.u, m2.STJ.v, 
   m2.PFJ.lon, m2.PFJ.lat, m2.PFJ.u, m2.PFJ.v,
   cl.fork, cl.psock)

## ZWISCHENSPEICHERN DER WERTE UND ERNEUTES LADEN DES DATENSATZES ####
# Speichern
save.image("170503-b.RData")
# Laden
load("170503-b.RData")
ls()
# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## VISUALISIEREN DER DATEN ####
## 
library(ggplot2)
library(RColorBrewer)
library(ggsci)
# library(maps)

# Initiieren einer passenden Weltkarte
map_nh <- map_data("world")

# Plot der Nordhemisphäre // Untersuchungsgebiet
ggp.nh <- 
  ggplot() + geom_polygon(data = map_nh, mapping = aes(x = long, y = lat, group = group), fill = "gray50") +
  scale_y_continuous(name = "Breitengrad", breaks = c(0, 30, 60, 90)) +
  coord_fixed(xlim = c(-180,180), ylim = c(0,90))
# Plot der Nordhemisphäre auf Mercator-Projektion
ggp.nh.merc <- 
  ggplot() + geom_polygon(data = map_nh, mapping = aes(x = long, y = lat, group = group), fill = "gray50") +
  scale_y_continuous(name = "Breitengrad", breaks = c(0, 30, 60, 90)) +
  coord_map(xlim = c(-180,180), ylim = c(0,90))

# Plot des zonalen Windfeldes und Chebyshev-Jets
ggp.u.m1 <- 
  ggplot(data = data.uv[which(data.uv$t.stp == dts[t.stp] & data.uv$p.lvl == lev[p.lvl]),], 
         mapping = aes(x = lon, y = lat, fill = u)) +
  ggtitle("bla") +
  geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
  geom_point(mapping = aes(x = lon , y = PFJ.lat.m1, fill = NULL),
             data = data.jets[which(data.jets$dts == dts[t.stp]),],
             shape = 24, fill = "black", size = 1) +
  geom_point(mapping = aes(x = lon , y = STJ.lat.m1, fill = NULL),
             data = data.jets[which(data.jets$dts == dts[t.stp]),],
             shape = 25, fill = "black", size = 1) +
  scale_x_continuous(name = "Längengrad", 
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad", 
                     breaks = c(0, 30, 60, 90)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               data = map_nh, fill = "gray50", alpha = 0.3) +
  coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic() 
  
# Plot des Betrags des horizontalen Windfeldes und Dijkstra-Jets
ggp.uv.m2 <- 
  ggplot(data = data.uv[which(data.uv$t.stp == dts[t.stp] & data.uv$p.lvl == lev[p.lvl]),], 
         mapping = aes(x = lon, y = lat, fill = uv)) +
  geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
  geom_point(mapping = aes(x = lon , y = PFJ.lat.m2, fill = NULL),
             data = data.jets[which(data.jets$dts == dts[t.stp]),],
             shape = 24, fill = "black", size = 1) +
  geom_point(mapping = aes(x = lon , y = STJ.lat.m2, fill = NULL),
             data = data.jets[which(data.jets$dts == dts[t.stp]),],
             shape = 25, fill = "black", size = 1) +
  scale_x_continuous(name = "Längengrad", 
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad", 
                     breaks = c(0, 30, 60, 90)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               data = map_nh, fill = "gray50", alpha = 0.3) +
  coord_fixed(xlim = c(-180,180), ylim = c(0,90))



ggplot(data = data.jets, mapping = aes(x = lon, y = dts)) + 
  geom_tile(mapping = aes(fill = STJ.lat.m1)) +
  scale_y_datetime(date_breaks = "5 years", date_labels = "%Y") +
  scale_fill_distiller(palette = 'RdYlBu')

ggplot(data = data.jets, mapping = aes(x = lon, y = dts)) + 
  geom_raster(mapping = aes(fill = STJ.lat.m1)) +
  scale_y_datetime(date_breaks = "5 years", date_labels = "%Y") +
  scale_fill_distiller(palette = 'RdYlBu')


ggsave()

## ENDE ENDE ENDE ####