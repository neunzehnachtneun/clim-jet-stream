## source("b-analyse_monthly_change.r")
## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####

## WORKING DIRECTORY & FIRST THINGS FIRST ####
## 
setwd("~/01-Master-Thesis/02-code-git/")
# getwd()
n.cluster <- 16

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


## SETZEN DES ZU UNTERSUCHENDEN DRUCK LEVELS ####
##
# for (p.lvl in 3:6) {
#   print(lev[p.lvl])
# }
p.lvl <- 4

## ZEITSCHRITTE
t.stp <- 1; # 
print(paste(dts.month[t.stp], dts.year[t.stp]))


## METHODE 1: find.jet.maximum.2d ####
## 
library(parallel)
cl.fork.1 <- makeCluster(n.cluster, type = "FORK")
# find.jet.maximum.2d(matrix = u[,,p.lvl, t.stp], axis = lat)
m1 <- parApply(cl.fork.1, X = u[,,p.lvl,], MARGIN = 3, FUN = find.jet.maximum.2d, axis = lat)
stopCluster(cl.fork.1); rm(cl.fork.1)


## METHODE 2: find.jet.chebpoly.fit.2d ####
##
library(foreach); library(doParallel)
cl.fork.2 <- makeCluster(n.cluster, type = "FORK")
registerDoParallel(cl.fork.2)
m2 <- foreach(t.stp = 1:length(dts)) %dopar% {
  find.jets.chebpoly.2d(matrix.u = u[,,p.lvl,t.stp], 
                        matrix.v = v[,,p.lvl,t.stp],
                        axis.x = lon, axis.y = lat)}
stopCluster(cl.fork.2); rm(cl.fork.2)


## METHODE 3: find.jet.dijkstra.2d ####
## 
cl.psock.1 <- makeCluster(n.cluster, type = "PSOCK")
registerDoParallel(cl.psock.1)
m3 <- foreach(t.stp = 1:length(dts), .packages = "igraph") %dopar% {
  find.jets.dijkstra.2d(u = u[,, p.lvl, t.stp], 
                        v = v[,, p.lvl, t.stp], 
                        lon = lon, lat = lat, 
                        season = dts.cld.wrm[t.stp]) }
stopCluster(cl.psock.1); rm(cl.psock.1)


## ZWISCHENSPEICHERN DER WERTE UND ERNEUTES LADEN DES DATENSATZES ####
# Speichern
save.image("stp-a.RData")
# Löschen des Workspaces
rm(list = ls())
# Laden
load("stp-a.RData")
ls()
# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## FORMATIEREN DER DATENSATZE UND LÖSCHEN ÜBERFLÜSSIGER VARIABLEN ####
## 
library(plyr); library(dplyr); library(reshape2)

## Lokales Maximum M0
# Maximaljet
m0.J.lat    <- sapply(m0, "[[", "MaxJ.lat"); colnames(m0.J.lat) <- dts; rownames(m0.J.lat) <- lon
m0.J.u      <- sapply(m0, "[[", "MaxJ.u")

## Chebyshev-Methodik M1
# alle gefundenen Maxima/Jets
m1a.J.lat <- sapply(m1a, "[[", "all.max.lat")
m1a.J.u <- sapply(m1a, "[[", "all.max.u")
# maximaler Chebyshev-Jet
m1b.J.lat <- sapply(m1bc, "[[", "MaxJ.lat")
m1b.J.u   <- sapply(m1bc, "[[", "MaxJ.u")
# Maximum und zweitstärkstes Maximum
m1c.STJ.lat <- sapply(m1bc, "[[", "STJ.lat")  # Suptropenjet
m1c.STJ.u   <- sapply(m1bc, "[[", "STJ.u")
m1c.PFJ.lat <- sapply(m1bc, "[[", "PFJ.lat")  # Polarfrontjet
m1c.PFJ.u   <- sapply(m1bc, "[[", "PFJ.u")
# Fit durch m1c
m1d.STJ.lat <- sapply(m1d, "[[", "STJ.lat")   # Subtropenjet
m1d.STJ.u   <- sapply(m1d, "[[", "STJ.u")
m1d.STJ.v   <- sapply(m1d, "[[", "STJ.v")
m1d.PFJ.lat <- sapply(m1d, "[[", "PFJ.lat")   # Polarfrontjet
m1d.PFJ.u   <- sapply(m1d, "[[", "PFJ.u")
m1d.PFJ.v   <- sapply(m1d, "[[", "PFJ.v")
# Sektorielle Suche nach stärkstem Maximum
m1e.STJ.lat <- sapply(m1e, "[[", "STJ.lat")   # Subtropenjet
m1e.STJ.u   <- sapply(m1e, "[[", "STJ.u")
m1e.PFJ.lat <- sapply(m1e, "[[", "PFJ.lat")   # Subtropenjet
m1e.PFJ.u   <- sapply(m1e, "[[", "PFJ.u")

## Dijkstra-Methodik M2
m2.STJ.lon  <- sapply(m2, "[[", "STJ.lon")    # Subtropenjet
m2.STJ.lat  <- sapply(m2, "[[", "STJ.lat")
m2.STJ.u    <- sapply(m2, "[[", "STJ.u")
m2.STJ.v    <- sapply(m2, "[[", "STJ.v")
m2.PFJ.lon  <- sapply(m2, "[[", "PFJ.lon")    # Polarfrontjet
m2.PFJ.lat  <- sapply(m2, "[[", "PFJ.lat")
m2.PFJ.u    <- sapply(m2, "[[", "PFJ.u")
m2.PFJ.v    <- sapply(m2, "[[", "PFJ.v")

## MELTEN DES DATENSATZES (RESHAPE2::MELT) FÜR GGPLOT2 ####
## 
# Maximaljet
df.jets.month              <- melt(m0.J.lat,varnames = c("lon", "dts"),value.name = "J.lat.m0")
# Einfügen der Jahreszahlen, Monate, Jahreszeiten
df.jets.month$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
df.jets.month$J.u.m0       <- melt(m0.J.u)$value

# alle auffindbaren Chebyshev-Jets
df.jets.month$J.lat.m1a.a  <- melt(m1a.J.lat[seq(from = 1, by = 4, length.out = n.lon)])$value
df.jets.month$J.lat.m1a.b  <- melt(m1a.J.lat[seq(from = 2, by = 4, length.out = n.lon)])$value
df.jets.month$J.lat.m1a.c  <- melt(m1a.J.lat[seq(from = 3, by = 4, length.out = n.lon)])$value
df.jets.month$J.lat.m1a.d  <- melt(m1a.J.lat[seq(from = 4, by = 4, length.out = n.lon)])$value
df.jets.month$J.u.m1a.a    <- melt(m1a.J.u[seq(from = 1, by = 4, length.out = n.lon)])$value
df.jets.month$J.u.m1a.b    <- melt(m1a.J.u[seq(from = 2, by = 4, length.out = n.lon)])$value
df.jets.month$J.u.m1a.c    <- melt(m1a.J.u[seq(from = 3, by = 4, length.out = n.lon)])$value
df.jets.month$J.u.m1a.d    <- melt(m1a.J.u[seq(from = 4, by = 4, length.out = n.lon)])$value

# Maximaljet Chebyshev
df.jets.month$J.lat.m1b    <- melt(m1b.J.lat)$value
df.jets.month$J.u.m1b      <- melt(m1b.J.u)$value

# Maximum und zweitstärkstes Maximum
df.jets.month$STJ.lat.m1c  <- melt(m1c.STJ.lat)$value # Subtropenjet
df.jets.month$STJ.u.m1c    <- melt(m1c.STJ.u)$value
df.jets.month$PFJ.lat.m1c  <- melt(m1c.PFJ.lat)$value # Polarfrontjet
df.jets.month$PFJ.u.m1c    <- melt(m1c.PFJ.u)$value
# Fit durch zwei stärkste Maxima
df.jets.month$STJ.lat.m1d  <- melt(m1d.STJ.lat)$value # Subtropenjet
df.jets.month$STJ.u.m1d    <- melt(m1d.STJ.u)$value
df.jets.month$STJ.v.m1d    <- melt(m1d.STJ.v)$value
df.jets.month$PFJ.lat.m1d  <- melt(m1d.PFJ.lat)$value # Polarfrontjet
df.jets.month$PFJ.u.m1d    <- melt(m1d.PFJ.u)$value
df.jets.month$PFJ.v.m1d    <- melt(m1d.PFJ.v)$value
# Sektorielle Suche nach Maxima
df.jets.month$STJ.lat.m1e  <- melt(m1e.STJ.lat)$value # Subtropenjet
df.jets.month$STJ.u.m1e    <- melt(m1e.STJ.u)$value
df.jets.month$PFJ.lat.m1e  <- melt(m1e.PFJ.lat)$value # Polarfrontjet
df.jets.month$PFJ.u.m1e    <- melt(m1e.PFJ.u)$value
# Dijkstra-Methodik
df.jets.month$STJ.lat.m2   <- melt(m2.STJ.lat)$value  # Subtropenjet
df.jets.month$STJ.u.m2     <- melt(m2.STJ.u)$value
df.jets.month$STJ.v.m2     <- melt(m2.STJ.v)$value
df.jets.month$PFJ.lat.m2   <- melt(m2.PFJ.lat)$value  # Polarfrontjet
df.jets.month$PFJ.u.m2     <- melt(m2.PFJ.u)$value
df.jets.month$PFJ.v.m2     <- melt(m2.PFJ.v)$value

# Umsortieren der Spalten des Datensatzes
df.jets.month <- df.jets.month[,c("dts", "year", "month", "season", "lon", 
                                  "J.lat.m0", "J.u.m0", "J.lat.m1b", "J.u.m1b",
                                  "J.lat.m1a.a", "J.lat.m1a.b", "J.lat.m1a.c", "J.lat.m1a.d",
                                  "J.u.m1a.a", "J.u.m1a.b", "J.u.m1a.c", "J.u.m1a.d",
                                  "STJ.lat.m1c", "STJ.u.m1c",   
                                  "STJ.lat.m1d", "STJ.u.m1d", "STJ.v.m1d",
                                  "STJ.lat.m1e", "STJ.u.m1e",
                                  "STJ.lat.m2",  "STJ.u.m2",  "STJ.v.m2",
                                  "PFJ.lat.m1c", "PFJ.u.m1c",   
                                  "PFJ.lat.m1d", "PFJ.u.m1d", "PFJ.v.m1d",
                                  "PFJ.lat.m1e", "PFJ.u.m1e",
                                  "PFJ.lat.m2",  "PFJ.u.m2",  "PFJ.v.m2"
                                  )]


# colnames(df.jets.month)
# head(df.jets.month)

## U-V-Datensatz
# Melten
df.uv <- melt(u[,,p.lvl,], varnames = c("lon", "lat", "t.stp"), value.name = "u")
df.uv$v <- melt(data = v[,,p.lvl,])$value
df.uv$uv <- sqrt( df.uv$u ** 2 + df.uv$v ** 2 )

## Löschen überflüssiger Variablen
rm(u, v, m0, m1a, m1bc, m1d, m1e, m2, 
   m0.J.lat, m0.J.u, 
   m1a.J.lat, m1a.J.u, 
   m1b.J.lat, m1b.J.u,
   m1c.STJ.lat, m1c.STJ.u, m1c.PFJ.lat, m1c.PFJ.u,
   m1d.STJ.lat, m1d.STJ.u, m1d.STJ.v,
   m1d.PFJ.lat, m1d.PFJ.u, m1d.PFJ.v,
   m1e.STJ.lat, m1e.STJ.u, m1e.PFJ.lat, m1e.PFJ.u,
   m2.STJ.lon, m2.STJ.lat, m2.STJ.u, m2.STJ.v, 
   m2.PFJ.lon, m2.PFJ.lat, m2.PFJ.u, m2.PFJ.v,
   cl.fork.1, cl.fork.2, cl.fork.3, cl.fork.4, cl.fork.5, cl.psock.1)


## SAISONALES GLEITENDES MITTEL ÜBER FÜNF JAHRE ####
##

# Festlegen des Untersuchungszeitraums
year.start <- 1960; year.end <- 2009; n.seas <- 4
n.years <- year.end - year.start + 1
# Definieren des Dataframes
length.df <- n.years * n.seas * n.lon # Länge
df.jets.season <- 
  data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
             rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:37])
# Schleife über alle Jahre, Jahreszeiten und Längengradabschnitte
for (i.stp in seq(from = 1, to = dim(df.jets.season)[1])) {
  df.jets.season[i.stp, 4:35] <- 
    apply(X = df.jets.month[which(
      df.jets.month$year >= df.jets.season[i.stp,]$Year - 2 &
        df.jets.month$year <= df.jets.season[i.stp,]$Year + 2 &
        df.jets.month$season == df.jets.season[i.stp,]$Season &
        df.jets.month$lon == df.jets.season[i.stp,]$Longitude), 6:37],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}

## SAISONALES MITTEL ÜBER ALLE JAHRE ####
## 

df.jets.season.mean <- 
  data.frame(rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.mean) <- c('Season', 'Longitude', colnames(df.jets.month)[6:37])

for (i.stp in seq(from = 1, to = dim(df.jets.season.mean)[1])) {
  # print(i.stp)
  df.jets.season.mean[i.stp, 3:34] <- 
    apply(X = df.jets.month[which(
        df.jets.month$season == df.jets.season.mean[i.stp,]$Season &
        df.jets.month$lon == df.jets.season.mean[i.stp,]$Longitude), 6:37],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}

## SAISONALES GLEITENDES MITTEL ABZGL DES ZEITLICHEN MITTELS ####
## 

df.jets.season.rel <-
  data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
             rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.rel) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:37])
for (i.stp in 1:length(unique(df.jets.season$Year))) {
  y.stp <- unique(df.jets.season$Year)[i.stp]
  df.jets.season.rel[which(df.jets.season.rel$Year == y.stp), 4:35] <-
    df.jets.season[which(df.jets.season$Year == y.stp), 4:35] - df.jets.season.mean[,3:34]
}

## ZEITLICHE MITTEL DER JET-POSITIONEN ####
##

## Zeitliches Mitteln für jeden Meridionalschritt
# Definieren des Dataframes
length.df <- n.lon # Länge
df.jets.tim.mean <- data.frame(rep(lon),
                             NA, NA, NA, NA, NA, NA, NA, NA, 
                             NA, NA, NA, NA, NA, NA, NA, NA, 
                             NA, NA, NA, NA, NA, NA, NA, NA, 
                             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.tim.mean) <- c('Longitude', colnames(df.jets.month)[6:37])
# Schleife über alle Zeitschritte
for (i.stp in seq(from = 1, to = dim(df.jets.tim.mean)[1])) {
  # print(i.stp)
  df.jets.tim.mean[i.stp, 2:33] <- 
    apply(X = df.jets.month[which(
      df.jets.month$lon == df.jets.season[i.stp,]$Longitude), 6:37],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}


## Zeitliches und meridionales Mitteln
# Definieren des Dataframes
df.jets.tim.mer.mean <- 
  data.frame(NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA, 
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.tim.mer.mean) <- colnames(df.jets.month)[6:37]
# Mitteln der Jet-Positionen und -Geschwindigkeiten
df.jets.tim.mer.mean <- apply(X = df.jets.month[, 6:37],
                      MARGIN = 2, FUN = mean, na.rm = TRUE)


## ZWISCHENSPEICHERN DER WERTE UND ERNEUTES LADEN DES DATENSATZES ####
# Speichern
save.image("stp-b.RData")
# Löschen des Workspaces
rm(list = ls())
# Laden
load("stp-b.RData")
ls()
# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## VISUALISIERUNG GGPLOT2() ####
##
## Nötige Pakete
library(ggplot2) # Grundpaket
# library(RColorBrewer)
library(ggsci) # Farbskala
# library(maps)
# library(gridExtra)
# library(egg)

## Schriftarten
library(extrafont)
fonts()
fonttable()
loadfonts(device = "postscript")

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
  
  # Plot des zonalen Windfeldes und der Position aller gefundenen Chebyshev-Jets
  ggp.nh.m1a <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = J.lat.m1a.a, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m1a.b, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m1a.c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m1a.d, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position aller gefundenen Chebyshev-Maxima", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")

  
  # Plot des zonalen Windfeldes und der Position des maximalen Jets sowie des maximalen Chebyshev-Jets
  ggp.nh.m0.m1b <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = J.lat.m0, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 16, fill = "black", size = 1, show.legend = TRUE) +
    geom_point(mapping = aes(x = lon , y = J.lat.m1b, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 18, fill = "black", size = 1.5, show.legend = TRUE) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position der meridionalen Zonalwindmaxima und des absoluten Chebyshev-Maximums", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  # Plot des zonalen Windfeldes und der zwei stärksten Chebyshev-Maxima im Bereich [20,85]
  ggp.nh.m1c <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m1c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m1c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 25, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position der zwei stärksten Chebyshev-Maxima", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  # # Plot des zonalen Windfeldes und dem Fit durch die zwei stärksten Chebyshev-Maxima
  # ggp.nh.m1d <-
  #   ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
  #          mapping = aes(x = lon, y = lat, fill = u)) +
  #   geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
  #   geom_point(mapping = aes(x = lon , y = PFJ.lat.m1d, fill = NULL),
  #              data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
  #              shape = 24, fill = "black", size = 1) +
  #   geom_point(mapping = aes(x = lon , y = STJ.lat.m1d, fill = NULL),
  #              data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
  #              shape = 25, fill = "black", size = 1) +
  #   scale_x_continuous(name = "Längengrad",
  #                      breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  #   scale_y_continuous(name = "Breitengrad",
  #                      breaks = c(0, 30, 60, 90)) +
  #   geom_polygon(mapping = aes(x = long, y = lat, group = group),
  #                data = map_nh, fill = "gray50", alpha = 0.35) +
  #   ggtitle("Zonales Windfeld und Position des Fits an die zwei stärksten Chebyshev-Maxima", 
  #           subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
  #   coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  # # Plot des zonalen Windfeldes und der zwei gefundenen sektoriellen Chebyshev-Maxima
  # ggp.nh.m1e <-
  #   ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
  #          mapping = aes(x = lon, y = lat, fill = u)) +
  #   geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
  #   geom_point(mapping = aes(x = lon , y = PFJ.lat.m1e, fill = NULL),
  #              data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
  #              shape = 24, fill = "black", size = 1) +
  #   geom_point(mapping = aes(x = lon , y = STJ.lat.m1e, fill = NULL),
  #              data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
  #              shape = 25, fill = "black", size = 1) +
  #   geom_hline(yintercept = c(20, 45, 85)) +
  #   scale_x_continuous(name = "Längengrad",
  #                      breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  #   scale_y_continuous(name = "Breitengrad",
  #                      breaks = c(0, 30, 60, 90)) +
  #   geom_polygon(mapping = aes(x = long, y = lat, group = group),
  #                data = map_nh, fill = "gray50", alpha = 0.35) +
  #   ggtitle("Zonales Windfeld und Position der lokalen Chebyshev-Maxima in [20,45] und [45,85]", 
  #           subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
  #   coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  # Plot des Betrags des horizontalen Windfeldes und Dijkstra-Jets
  ggp.nh.m2 <-
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
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Betrag des horizontalen Windfeldes und Position des Dijkstra-Jets", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  ## Speichern der Plots als pdfs
  # ggp.jets <- grid.arrange(ggp.u.m0, ggp.u.m1a, ggp.u.m1b, ggp.u.m1c, ggp.uv.m2, ncol = 1)
  ggsave(filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m0-m1b.pdf"),
         plot = ggp.nh.m0.m1b, device = pdf, path = "05-visu-pdf/",
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m1a.pdf"),
         plot = ggp.nh.m1a, device = pdf, path = "05-visu-pdf/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m1c.pdf"),
         plot = ggp.nh.m1c, device = pdf, path = "05-visu-pdf/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m2.pdf"),
         plot = ggp.nh.m2, device = pdf, path = "05-visu-pdf/", 
         dpi = 600, width = 297, height = 210, units = "mm")
}


## VISUALISIERUNG DER HOVMÖLLER-DIAGRAMME ** PFJ ####
## POLARFRONT JETSTREAM

# Schleife über Jahreszeiten
for (i.ssn in c("djf", "mam", "jja", "son")) {
  print(i.ssn)
  
  # Positionen Breitengrad Chebyshev
  hovm.pfj.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m1c)) +
    geom_tile() + scale_fill_gsea()
  # Positionen Breitengrad Dijkstra
  hovm.pfj.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season = i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.pfj.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m1c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.pfj.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.pfj.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Betrag Windstärke Dijkstra
  hovm.pfj.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                               mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2 * PFJ.v.m2)) +
    geom_tile() + scale_fill_gsea()
  
  ## RELATIV ZU ZONALEM MITTEL
  # Positionen Breitengrad Chebyshev
  hovm.pfj.lat.m1.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m1c)) +
    geom_tile() + scale_fill_gsea()
  # Positionen Breitengrad Dijkstra
  hovm.pfj.lat.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.pfj.u.m1.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m1c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.pfj.u.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.pfj.v.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Betrag Windstärke Dijkstra
  hovm.pfj.uv.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                               mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2 * PFJ.v.m2)) +
    geom_tile() + scale_fill_gsea()
}
## HOVMÖLLER-DIAGRAMME ** STJ ####
## SUBTROPISCHER JETSTREAM

# Schleife über Jahreszeiten
for (i.ssn in c("djf", "mam", "jja", "son")) {
  print(i.ssn)

# Positionen Breitengrad Chebyshev
hovm.stj.lat.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.u.m1.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.uv.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()


## RELATIV ZU ZONALEM MITTEL
# Positionen Breitengrad Chebyshev
hovm.stj.lat.m1.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m1c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.lat.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.u.m1.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m1c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.u.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.v.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.uv.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2 * PFJ.v.m2)) +
  geom_tile() + scale_fill_gsea()
}

## ENDE ENDE ENDE ####
