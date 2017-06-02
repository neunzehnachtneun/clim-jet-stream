## source("b-analyse_monthly_change.r")
## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####

## WORKING DIRECTORY & FIRST THINGS FIRST ####
## 
setwd("~/01-Master-Thesis/02-code-git/")
# getwd()
n.cluster <- 12

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

## UMRECHNEN DER DATEN VON PAZIFIKZENTRISCHEM AUF EUROPAZENTRISCHES GITTER ####
##
lon.hlp <- lon - 180
# lon.hlp <- c( lon[97:192] - 360, lon[1:96] )
lon[1]; lon.hlp[97];
# cut.lon <- which(lon.hlp == lon[1])
u <- u[c(97:192,1:96),,,]; dimnames(u) <- list(lon.hlp, lat, lev, dts)
v <- v[c(97:192,1:96),,,]; dimnames(v) <- list(lon.hlp, lat, lev, dts)
lon <- lon.hlp
# m1.STJ.lat  <- m1.STJ.lat[c(97:192,1:96),]; rownames(m1.STJ.lat) <- lon.hlp
# m1.STJ.u    <- m1.STJ.u[c(97:192,1:96),]
# m2.STJ.lon  <- m2.STJ.lon[c(97:192,1:96),]
# m2.STJ.lat  <- m2.STJ.lat[c(97:192,1:96),]
# m2.STJ.u    <- m2.STJ.u[c(97:192,1:96),]
# m2.STJ.v    <- m2.STJ.v[c(97:192,1:96),]
# m1.PFJ.lat  <- m1.PFJ.lat[c(97:192,1:96),]
# m1.PFJ.u    <- m1.PFJ.u[c(97:192,1:96),]
# m2.PFJ.lon  <- m2.PFJ.lon[c(97:192,1:96),]
# m2.PFJ.lat  <- m2.PFJ.lat[c(97:192,1:96),]
# m2.PFJ.u    <- m2.PFJ.u[c(97:192,1:96),]
# m2.PFJ.v    <- m2.PFJ.v[c(97:192,1:96),]


## LADEN NÖTIGER HILFSFUNKTIONEN ####
## 
source("d-jetstream_detection_schemes.r")
source("f-help-functions.r")


## SCHLEIFE ÜBER DRUCK LEVEL ####
##
# for (p.lvl in 3:6) {
#   print(lev[p.lvl])
# }
p.lvl <- 4

## ZEITSCHRITTE
t.stp <- 1; # 
print(paste(dts.month[t.stp], dts.year[t.stp]))


## METHODE 0: find.jet.maximum.2d ####
## 
library(parallel)
cl.fork.1 <- makeCluster(n.cluster, type = "FORK")
# find.jet.maximum.2d(matrix = u[,,p.lvl, t.stp], axis = lat)
m0 <- parApply(cl.fork.1, X = u[,,p.lvl,], MARGIN = 3, FUN = find.jet.maximum.2d, axis = lat)
stopCluster(cl.fork.1)

## METHODE 1a: find.jet.chebpoly.all.2d ####
## 
cl.fork.2 <- makeCluster(n.cluster, type = "FORK")
m1a <- parApply(cl.fork.2, X = u[,,p.lvl,], MARGIN = 3, FUN = find.jets.chebpoly.all.2d, axis = lat)
stopCluster(cl.fork.2)

## METHODE 1bc: find.jet.chebpoly.max.2d ####
##
cl.fork.3 <- makeCluster(n.cluster, type = "FORK")
m1bc <- parApply(cl.fork.3, X = u[,,p.lvl,], MARGIN = 3, FUN = find.jets.chebpoly.max.2d, axis = lat)
stopCluster(cl.fork.3)

## METHODE 1d: find.jet.chebpoly.fit.2d ####
##
library(foreach); library(doParallel)
cl.fork.4 <- makeCluster(n.cluster, type = "FORK")
registerDoParallel(cl.fork.4)
m1d <- foreach(t.stp = 1:length(dts)) %dopar% {
  find.jets.chebpoly.fit.2d(matrix.u = u[,,p.lvl,t.stp], 
                            matrix.v = v[,,p.lvl,t.stp],
                            axis.x = lon, axis.y = lat)}
stopCluster(cl.fork.4)


## METHODE 1e: find.jets.chebpoly.sect.2d ####
## 
cl.fork.5 <- makeCluster(n.cluster, type = "FORK")
registerDoParallel(cl.fork.5)
m1e <- foreach(t.stp = 1:length(dts)) %dopar% {
  find.jets.chebpoly.sect.2d(matrix.u = u[,,p.lvl,t.stp],
                             axis.x = lon, axis.y = lat)}
stopCluster(cl.fork.5)

## METHODE 2: find.jet.dijkstra.2d ####
## 
cl.psock.1 <- makeCluster(n.cluster, type = "PSOCK")
registerDoParallel(cl.psock.1)
m2 <- foreach(t.stp = 1:length(dts), .packages = "igraph") %dopar% {
  find.jets.dijkstra.2d(u = u[,, p.lvl, t.stp], 
                        v = v[,, p.lvl, t.stp], 
                        lon = lon, lat = lat, 
                        season = dts.cld.wrm[t.stp]) }
stopCluster(cl.psock.1)


## ZWISCHENSPEICHERN DER WERTE UND ERNEUTES LADEN DES DATENSATZES ####
# Speichern
save.image("stp-a.RData")
# Laden
load("stp-a.RData")
ls()
# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## FORMATIEREN DER DATENSATZE UND LÖSCHEN ÜBERFLÜSSIGER VARIABLEN ####
## 
library(plyr); library(dplyr); library(reshape2)

## Jetdatensatz M1a M1b & M2
# Maximaljet M0
m0.J.lat    <- sapply(m0, "[[", "MaxJ.lat"); colnames(m0.J.lat) <- dts; rownames(m0.J.lat) <- lon
m0.J.u      <- sapply(m0, "[[", "MaxJ.u")
# Subtropenjet
m1a.STJ.lat <- sapply(m1a, "[[", "STJ.lat")
m1a.STJ.u   <- sapply(m1a, "[[", "STJ.u")
m1b.STJ.lat <- sapply(m1b, "[[", "STJ.lat")
m1b.STJ.u   <- sapply(m1b, "[[", "STJ.u")
m1b.STJ.v   <- sapply(m1b, "[[", "STJ.v")
m1c.STJ.lat <- sapply(m1c, "[[", "STJ.lat")
m1c.STJ.u   <- sapply(m1c, "[[", "STJ.u")
m2.STJ.lon  <- sapply(m2, "[[", "STJ.lon")
m2.STJ.lat  <- sapply(m2, "[[", "STJ.lat")
m2.STJ.u    <- sapply(m2, "[[", "STJ.u")
m2.STJ.v    <- sapply(m2, "[[", "STJ.v")
# Polarfrontjet
m1a.PFJ.lat  <- sapply(m1a, "[[", "PFJ.lat")
m1a.PFJ.u    <- sapply(m1a, "[[", "PFJ.u")
m1b.PFJ.lat  <- sapply(m1b, "[[", "PFJ.lat")
m1b.PFJ.u    <- sapply(m1b, "[[", "PFJ.u")
m1b.PFJ.v    <- sapply(m1b, "[[", "PFJ.v")
m1c.PFJ.lat <- sapply(m1c, "[[", "PFJ.lat")
m1c.PFJ.u   <- sapply(m1c, "[[", "PFJ.u")
m2.PFJ.lon  <- sapply(m2, "[[", "PFJ.lon")
m2.PFJ.lat  <- sapply(m2, "[[", "PFJ.lat")
m2.PFJ.u    <- sapply(m2, "[[", "PFJ.u")
m2.PFJ.v    <- sapply(m2, "[[", "PFJ.v")

## MELTEN DES DATENSATZES (RESHAPE2::MELT) FÜR GGPLOT2 ####
## 
# Maximaljet
df.jets.month <- melt(m0.J.lat,varnames = c("lon", "dts"),value.name = "J.lat.m0")
# Einfügen der Jahreszahlen, Monate, Jahreszeiten
df.jets.month$dts <- rep(dts, each = n.lon); df.jets.month$year <- rep(dts.year, each = n.lon)
df.jets.month$month <- rep(dts.month, each = n.lon); df.jets.month$season <- rep(dts.season, each = n.lon)
df.jets.month$J.u.m0 <- melt(m0.J.u)$value
# Subtropenjet
df.jets.month$STJ.lat.m1a  <- melt(m1a.STJ.lat)$value
df.jets.month$STJ.u.m1a    <- melt(m1a.STJ.u)$value
df.jets.month$STJ.lat.m1b  <- melt(m1b.STJ.lat)$value
df.jets.month$STJ.u.m1b    <- melt(m1b.STJ.u)$value
df.jets.month$STJ.v.m1b    <- melt(m1b.STJ.v)$value
df.jets.month$STJ.lat.m1c  <- melt(m1c.STJ.lat)$value
df.jets.month$STJ.u.m1c    <- melt(m1c.STJ.u)$value
df.jets.month$STJ.lat.m2   <- melt(m2.STJ.lat)$value
df.jets.month$STJ.u.m2     <- melt(m2.STJ.u)$value
df.jets.month$STJ.v.m2     <- melt(m2.STJ.v)$value
# Polarfrontjet
df.jets.month$PFJ.lat.m1a  <- melt(m1a.PFJ.lat)$value
df.jets.month$PFJ.u.m1a    <- melt(m1a.PFJ.u)$value
df.jets.month$PFJ.lat.m1b  <- melt(m1b.PFJ.lat)$value
df.jets.month$PFJ.u.m1b    <- melt(m1b.PFJ.u)$value
df.jets.month$PFJ.v.m1b    <- melt(m1b.PFJ.v)$value
df.jets.month$PFJ.lat.m1c  <- melt(m1c.PFJ.lat)$value
df.jets.month$PFJ.u.m1c    <- melt(m1c.PFJ.u)$value
df.jets.month$PFJ.lat.m2   <- melt(m2.PFJ.lat)$value
df.jets.month$PFJ.u.m2     <- melt(m2.PFJ.u)$value
df.jets.month$PFJ.v.m2     <- melt(m2.PFJ.v)$value
# Umsortieren der Spalten des Datensatzes
df.jets.month <- df.jets.month[,c("dts", "year", "month", "season", "lon", "J.lat.m0", "J.u.m0", 
                          "STJ.lat.m1a", "STJ.lat.m1b", "STJ.lat.m1c", "STJ.lat.m2", 
                          "STJ.u.m1a", "STJ.u.m1b", "STJ.v.m1b", "STJ.u.m1c", 
                          "STJ.u.m2", "STJ.v.m2", 
                          "PFJ.lat.m1a", "PFJ.lat.m1b", "PFJ.lat.m1c", "PFJ.lat.m2", 
                          "PFJ.u.m1a", "PFJ.u.m1b", "PFJ.v.m1b", "PFJ.u.m1c",
                          "PFJ.u.m2", "PFJ.v.m2")]
# colnames(df.jets.month)
# head(df.jets.month)

## U-V-Datensatz
# Melten
df.uv <- melt(u[,,p.lvl,], varnames = c("lon", "lat", "t.stp"), value.name = "u")
df.uv$v <- melt(data = v[,,p.lvl,])$value
df.uv$uv <- sqrt( df.uv$u ** 2 + df.uv$v ** 2 )

## Löschen überflüssiger Variablen
rm(u, v, m0, m1a, m1b, m1c, m2, 
   m0.J.lat, m0.J.u,
   m1a.STJ.lat, m1a.STJ.u, m1a.PFJ.lat, m1a.PFJ.u, 
   m1b.STJ.lat, m1b.STJ.u, m1b.STJ.v,
   m1b.PFJ.lat, m1b.PFJ.u, m1b.PFJ.v,
   m1c.STJ.lat, m1c.STJ.u, m1c.PFJ.lat, m1c.PFJ.u,
   m2.STJ.lon, m2.STJ.lat, m2.STJ.u, m2.STJ.v, 
   m2.PFJ.lon, m2.PFJ.lat, m2.PFJ.u, m2.PFJ.v,
   cl.fork.1, cl.fork.2, cl.fork.3, cl.fork.4, cl.psock.1)


## SAISONALES GLEITENDES MITTEL ÜBER FÜNF JAHRE ####
##

# Festlegen des Untersuchungszeitraums
year.start <- 1960; year.end <- 2009; n.seas <- 4
n.years <- year.end - year.start + 1
# Definieren des Dataframes
length.df <- n.years * n.seas * n.lon # Länge
df.jets.season <- data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
                               rep(c('djf','mam','jja','son'), each = n.lon),
                               rep(lon),
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA)
colnames(df.jets.season) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:27])

# Schleife über alle Jahre, Jahreszeiten und Längengradabschnitte
for (i.stp in seq(from = 1, to = dim(df.jets.season)[1])) {
  # print(i.stp)
  # which(df.jets.month$year >= df.jets.season[t.stp,]$Year - 2 &
  #         df.jets.month$year <= df.jets.season[t.stp,]$Year +2 &
  #         df.jets.month$season == df.jets.season[t.stp,]$Season &
  #         df.jets.month$lon == df.jets.season[t.stp,]$Longitude)
  df.jets.season[i.stp, 4:25] <- 
    apply(X = df.jets.month[which(
      df.jets.month$year >= df.jets.season[i.stp,]$Year - 2 &
        df.jets.month$year <= df.jets.season[i.stp,]$Year + 2 &
        df.jets.month$season == df.jets.season[i.stp,]$Season &
        df.jets.month$lon == df.jets.season[i.stp,]$Longitude), 6:27],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}


## ZWISCHENSPEICHERN DER WERTE UND ERNEUTES LADEN DES DATENSATZES ####
# Speichern
save.image("stp-b.RData")
# Laden
load("stp-b.RData")
ls()
# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## VISUALISIEREN DER DATEN MIT GGPLOT2() ####
##
library(ggplot2)
# library(RColorBrewer)
library(ggsci)
# library(maps)
library(gridExtra)
library(egg)

## VISUALISIEREN DER BESTIMMTEN JETPOSITIONEN ####
##

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

for (t.stp in round(seq(1,length(dts), length.out = 6))) {
  print(t.stp)

  # Plot des zonalen Windfeldes und der Position des maximalen Jets
  ggp.u.m0 <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = J.lat.m0, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.3) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + 
    theme_classic(base_family = "Droid Serif")

  # Plot des zonalen Windfeldes und Chebyshev-Jets
  ggp.u.m1a <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m1a, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m1a, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 25, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.3) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic()

  ggp.u.m1b <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp] ),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m1b, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m1b, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 25, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.3) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic()

  ggp.u.m1c <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp] ),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m1c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1, na.rm = TRUE) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m1c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 25, fill = "black", size = 1, na.rm = TRUE) +
    geom_hline(yintercept = c(20, 45, 85)) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.3) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic()

  # Plot des Betrags des horizontalen Windfeldes und Dijkstra-Jets
  ggp.uv.m2 <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m2, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m2, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 25, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.3) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic()


  ggp.jets <- grid.arrange(ggp.u.m0, ggp.u.m1a, ggp.u.m1b, ggp.u.m1c, ggp.uv.m2, ncol = 1)
  ggsave(filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], ".pdf"),
       plot = ggp.jets,device = pdf, path = "05-visu-pdf/", width = 210, height = 297, units = "mm")
}


## VISUALISIERUNG DER HOVMÖLLER-DIAGRAMME ** PFJ ** DJF ####
## POLARFRONT JETSTREAM
## DECEMBER JANUARY FEBRUARY
# Positionen Breitengrad Chebyshev
hovm.pfj.djf.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.pfj.djf.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.pfj.djf.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.pfj.djf.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.pfj.djf.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.pfj.djf.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                 mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()

## HOVMÖLLER-DIAGRAMME ** PFJ ** MAM ####
## POLARFRONT JETSTREAM
## MARCH APRIL MAY
# Positionen Breitengrad Chebyshev
hovm.pfj.mam.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.pfj.mam.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.pfj.mam.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.pfj.mam.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.pfj.mam.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.pfj.mam.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                 mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()

## HOVMÖLLER-DIAGRAMME ** PFJ ** JJA ####
## POLARFRONT JETSTREAM
## JUNE JULY AUGUST
# Positionen Breitengrad Chebyshev
hovm.pfj.jja.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.pfj.jja.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.pfj.jja.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.pfj.jja.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.pfj.jja.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.pfj.jja.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                 mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()

## HOVMÖLLER-DIAGRAMME ** PFJ ** SON ####
## POLARFRONT JETSTREAM
## SEPTEMBER OCTOBER NOVEMBER
# Positionen Breitengrad Chebyshev
hovm.pfj.djf.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.pfj.djf.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.pfj.djf.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.pfj.djf.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.pfj.djf.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.pfj.djf.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                 mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()

## HOVMÖLLER-DIAGRAMME ** STJ ** DJF ####
## SUBTROPISCHER JETSTREAM
## DECEMBER JANUARY FEBRUARY
# Positionen Breitengrad Chebyshev
hovm.stj.djf.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.djf.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.djf.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.djf.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.djf.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.djf.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()

## HOVMÖLLER-DIAGRAMME ** STJ ** MAM ####
## SUBTROPISCHER JETSTREAM
## MARCH APRIL MAY
# Positionen Breitengrad Chebyshev
hovm.stj.mam.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.mam.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.mam.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.mam.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.mam.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.mam.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "mam"),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()

## HOVMÖLLER-DIAGRAMME ** STJ ** JJA ####
## SUBTROPISCHER JETSTREAM
## JUNE JULY AUGUST
# Positionen Breitengrad Chebyshev
hovm.stj.jja.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.jja.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.jja.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.jja.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.jja.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.jja.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "jja"),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()

## HOVMÖLLER-DIAGRAMME ** STJ ** SON ####
## SUBTROPISCHER JETSTREAM
## SEPTEMBER OCTOBER NOVEMBER
hovm.stj.djf.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.djf.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.djf.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.djf.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.djf.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.djf.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == "djf"),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()


## ENDE ENDE ENDE ####
