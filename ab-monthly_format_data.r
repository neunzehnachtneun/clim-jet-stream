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
nc <- nc_open("04-data-nc/b-1957-2016-e4ei-t63-zuv-nh-monmean.nc")
u <- ncvar_get(nc, "u") # U-Wind-Komponente
v <- ncvar_get(nc, "v") # V-Wind-Komponente
z <- ncvar_get(nc, "z") # Geopotenzielle Höhe
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
dimnames(z) <- list(lon, lat, lev, dts)

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
z <- z[c(97:192,1:96),,,]; dimnames(z) <- list(lon.hlp, lat, lev, dts)
lon <- lon.hlp


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
                        matrix.z = z[,,p.lvl,t.stp],
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

## FORMATIEREN DER DATENSATZE ####
## 
library(plyr); library(dplyr); library(reshape2)

## Globales Maximum M1
# Maximaljet
m1.J.lat    <- sapply(m1, "[[", "MaxJ.lat"); colnames(m1.J.lat) <- dts; rownames(m1.J.lat) <- lon
m1.J.u      <- sapply(m1, "[[", "MaxJ.u")

## Chebyshev-Methodik M2
# alle gefundenen Maxima/Jets
m2a.J.lat <- sapply(m2, "[[", "all.max.lat")
#m2a.J.u <- sapply(m2, "[[", "all.max.u")
# maximaler Chebyshev-Jet
m2b.J.lat <- sapply(m2, "[[", "MaxJ.lat")
#m2b.J.u   <- sapply(m2, "[[", "MaxJ.u")
# Maximum und zweitstärkstes Maximum in Sektor
m2c.STJ.lat <- sapply(m2, "[[", "STJ.lat")  # Suptropenjet
m2c.STJ.u   <- sapply(m2, "[[", "STJ.u")
m2c.STJ.v   <- sapply(m2, "[[", "STJ.v")
m2c.STJ.z   <- sapply(m2, "[[", "STJ.z")
m2c.PFJ.lat <- sapply(m2, "[[", "PFJ.lat")  # Polarfrontjet
m2c.PFJ.u   <- sapply(m2, "[[", "PFJ.u")
m2c.PFJ.v   <- sapply(m2, "[[", "PFJ.v")
m2c.PFJ.z   <- sapply(m2, "[[", "PFJ.z")

## Dijkstra-Methodik M3
# m3.STJ.lon  <- sapply(m2, "[[", "STJ.lon")    # Subtropenjet
m3.STJ.lat  <- sapply(m3, "[[", "STJ.lat")
m3.STJ.u    <- sapply(m3, "[[", "STJ.u")
m3.STJ.v    <- sapply(m3, "[[", "STJ.v")
# m3.PFJ.lon  <- sapply(m2, "[[", "PFJ.lon")    # Polarfrontjet
m3.PFJ.lat  <- sapply(m3, "[[", "PFJ.lat")
m3.PFJ.u    <- sapply(m3, "[[", "PFJ.u")
m3.PFJ.v    <- sapply(m3, "[[", "PFJ.v")


## MELTEN DES DATENSATZES (RESHAPE2::MELT) FÜR GGPLOT2 ####
## 
# Maximaljet
df.jets.month              <- melt(m1.J.lat,varnames = c("lon", "dts"),value.name = "J.lat.m1")
# Einfügen der Jahreszahlen, Monate, Jahreszeiten
df.jets.month$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# df.jets.month$J.u.m1       <- melt(m1.J.u)$value

# Umsortieren der Spalten des Datensatzes
df.jets.month <- df.jets.month[,c("dts", "year", "month", "season", "lon", "J.lat.m1")]

# Maximaljet Chebyshev
df.jets.month$J.lat.m2b    <- melt(m2b.J.lat)$value
# df.jets.month$J.u.m2b      <- melt(m2b.J.u)$value

# alle auffindbaren Chebyshev-Jets
df.jets.month$J.lat.m2a.a  <- melt(m2a.J.lat[seq(from = 1, by = 4, length.out = n.lon)])$value
df.jets.month$J.lat.m2a.b  <- melt(m2a.J.lat[seq(from = 2, by = 4, length.out = n.lon)])$value
df.jets.month$J.lat.m2a.c  <- melt(m2a.J.lat[seq(from = 3, by = 4, length.out = n.lon)])$value
df.jets.month$J.lat.m2a.d  <- melt(m2a.J.lat[seq(from = 4, by = 4, length.out = n.lon)])$value
# df.jets.month$J.u.m2a.a    <- melt(m2a.J.u[seq(from = 1, by = 4, length.out = n.lon)])$value
# df.jets.month$J.u.m2a.b    <- melt(m2a.J.u[seq(from = 2, by = 4, length.out = n.lon)])$value
# df.jets.month$J.u.m2a.c    <- melt(m2a.J.u[seq(from = 3, by = 4, length.out = n.lon)])$value
# df.jets.month$J.u.m2a.d    <- melt(m2a.J.u[seq(from = 4, by = 4, length.out = n.lon)])$value

# Maximum und zweitstärkstes Maximum
df.jets.month$STJ.lat.m2c  <- melt(m2c.STJ.lat)$value # Subtropenjet
df.jets.month$STJ.u.m2c    <- melt(m2c.STJ.u)$value
df.jets.month$STJ.v.m2c    <- melt(m2c.STJ.v)$value
df.jets.month$STJ.z.m2c    <- melt(m2c.STJ.v)$value
df.jets.month$PFJ.lat.m2c  <- melt(m2c.PFJ.lat)$value # Polarfrontjet
df.jets.month$PFJ.u.m2c    <- melt(m2c.PFJ.u)$value
df.jets.month$PFJ.v.m2c    <- melt(m2c.PFJ.v)$value
df.jets.month$PFJ.z.m2c    <- melt(m2c.PFJ.v)$value

# Dijkstra-Methodik
df.jets.month$STJ.lat.m3   <- melt(m3.STJ.lat)$value  # Subtropenjet
df.jets.month$STJ.u.m3     <- melt(m3.STJ.u)$value
df.jets.month$STJ.v.m3     <- melt(m3.STJ.v)$value
df.jets.month$PFJ.lat.m3   <- melt(m3.PFJ.lat)$value  # Polarfrontjet
df.jets.month$PFJ.u.m3     <- melt(m3.PFJ.u)$value
df.jets.month$PFJ.v.m3     <- melt(m3.PFJ.v)$value


# colnames(df.jets.month)
# head(df.jets.month)

## U-V-Datensatz
# Melten
df.uv <- melt(u[,,p.lvl,], varnames = c("lon", "lat", "t.stp"), value.name = "u")
df.uv$v <- melt(data = v[,,p.lvl,])$value
df.uv$uv <- sqrt( df.uv$u ** 2 + df.uv$v ** 2 )
df.uv$z <- melt(data = z[,,p.lvl,])$value


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
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:25])
# Schleife über alle Jahre, Jahreszeiten und Längengradabschnitte
for (i.stp in seq(from = 1, to = dim(df.jets.season)[1])) {
  df.jets.season[i.stp, 4:23] <- 
    apply(X = df.jets.month[which(
      df.jets.month$year >= df.jets.season[i.stp,]$Year - 2 &
        df.jets.month$year <= df.jets.season[i.stp,]$Year + 2 &
        df.jets.month$season == df.jets.season[i.stp,]$Season &
        df.jets.month$lon == df.jets.season[i.stp,]$Longitude), 6:25],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}

## SAISONALES MITTEL ÜBER ALLE JAHRE ####
## 

df.jets.season.mean <- 
  data.frame(rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.mean) <- c('Season', 'Longitude', colnames(df.jets.month)[6:25])

for (i.stp in seq(from = 1, to = dim(df.jets.season.mean)[1])) {
  # print(i.stp)
  df.jets.season.mean[i.stp, 3:22] <- 
    apply(X = df.jets.month[which(
        df.jets.month$season == df.jets.season.mean[i.stp,]$Season &
        df.jets.month$lon == df.jets.season.mean[i.stp,]$Longitude), 6:25],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}

## SAISONALES GLEITENDES MITTEL ABZGL DES ZEITLICHEN MITTELS ####
## 

df.jets.season.rel <-
  data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
             rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.rel) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:25])

for (i.stp in 1:length(unique(df.jets.season$Year))) {
  y.stp <- unique(df.jets.season$Year)[i.stp]
  df.jets.season.rel[which(df.jets.season.rel$Year == y.stp), 4:23] <-
    df.jets.season[which(df.jets.season$Year == y.stp), 4:23] - df.jets.season.mean[,3:22]
}

## ZEITLICHE MITTEL DER JET-POSITIONEN ####
##

## Zeitliches Mitteln für jeden Meridionalschritt
# Definieren des Dataframes
length.df <- n.lon # Länge
df.jets.tim.mean <- data.frame(rep(lon),
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.tim.mean) <- c('Longitude', colnames(df.jets.month)[6:25])

# Schleife über alle Zeitschritte
for (i.stp in seq(from = 1, to = dim(df.jets.tim.mean)[1])) {
  # print(i.stp)
  df.jets.tim.mean[i.stp, 2:21] <- 
    apply(X = df.jets.month[which(
      df.jets.month$lon == df.jets.season[i.stp,]$Longitude), 6:25],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}


## Zeitliches und meridionales Mitteln
# Definieren des Dataframes
df.jets.tim.mer.mean <- 
  data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.tim.mer.mean) <- colnames(df.jets.month)[6:25]

# Mitteln der Jet-Positionen und -Geschwindigkeiten
df.jets.tim.mer.mean <- apply(X = df.jets.month[, 6:25],
                      MARGIN = 2, FUN = mean, na.rm = TRUE)


## LÖSCHEN UNNÖTIGER VARIABLEN ####
## 

# ls()
rm(date.help, diff.max, find.jet.dijkstra.2d, find.jet.maximum.2d, find.jets.chebpoly.2d, find.jets.dijkstra.2d, fun.fill, len.na, length.df, lon.hlp,  m1, m1.J.lat, m1.J.u, m2, m2a.J.lat, m2a.J.u, m2b.J.lat, m2b.J.u, m2c.PFJ.lat, m2c.PFJ.u, m2c.PFJ.v, m2c.PFJ.z, m2c.STJ.lat, m2c.STJ.u, m2c.STJ.v, m2c.STJ.z, m3, m3.PFJ.lat, m3.PFJ.u , m3.PFJ.v, m3.STJ.lat, m3.STJ.u, m3.STJ.v,n.cluster,norm.vec, u, v, y.stp, t.stp, year.end, year.start, z)
# ls()

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
  ggp.nh.m2a <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.a, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.b, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.d, fill = NULL),
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
  ggp.nh.m1.m2b <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = J.lat.m1, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 16, fill = "black", size = 1, show.legend = TRUE) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2b, fill = NULL),
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
  ggp.nh.m2c <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m2c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m2c, fill = NULL),
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
  
  # Plot des Betrags des horizontalen Windfeldes und Dijkstra-Jets
  ggp.nh.m3 <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m3, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m3, fill = NULL),
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
  hovm.pfj.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Positionen Breitengrad Dijkstra
  hovm.pfj.lat.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.pfj.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.pfj.u.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.pfj.v.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Betrag Windstärke Dijkstra
  hovm.pfj.uv.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                               mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m3 * PFJ.v.m3)) +
    geom_tile() + scale_fill_gsea()
  
  ## RELATIV ZU ZONALEM MITTEL
  # Positionen Breitengrad Chebyshev
  hovm.pfj.lat.m1.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Positionen Breitengrad Dijkstra
  hovm.pfj.lat.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.pfj.u.m1.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.pfj.u.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.pfj.v.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Betrag Windstärke Dijkstra
  hovm.pfj.uv.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                               mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m3 * PFJ.v.m3)) +
    geom_tile() + scale_fill_gsea()
}
## HOVMÖLLER-DIAGRAMME ** STJ ####
## SUBTROPISCHER JETSTREAM

# Schleife über Jahreszeiten
for (i.ssn in c("djf", "mam", "jja", "son")) {
  print(i.ssn)

# Positionen Breitengrad Chebyshev
hovm.stj.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.lat.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m3)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.u.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m3)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.v.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m3)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.uv.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m3 * PFJ.v.m3)) +
  geom_tile() + scale_fill_gsea()


## RELATIV ZU ZONALEM MITTEL
# Positionen Breitengrad Chebyshev
hovm.stj.lat.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2c)) +
  geom_tile() + scale_fill_gsea()
# Positionen Breitengrad Dijkstra
hovm.stj.lat.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                  mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m3)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Chebyshev
hovm.stj.u.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2c)) +
  geom_tile() + scale_fill_gsea()
# Intensität Zonalwind Dijkstra
hovm.stj.u.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.u.m3)) +
  geom_tile() + scale_fill_gsea()
# Intensität Meridionalwind Dijkstra
hovm.stj.v.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.v.m3)) +
  geom_tile() + scale_fill_gsea()
# Intensität Betrag Windstärke Dijkstra
hovm.stj.uv.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                 mapping = aes(x = Longitude, y = Year, fill = STJ.u.m3 * PFJ.v.m3)) +
  geom_tile() + scale_fill_gsea()
}

## ENDE ENDE ENDE ####
