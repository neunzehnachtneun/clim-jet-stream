## source("ab-monthly_format_data.r")
## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####

## WORKING DIRECTORY & FIRST THINGS FIRST ####
## 
setwd("~/01-Master-Thesis/02-code-git/")
# getwd()


## LADEN DES DATENSATZES ####
# Löschen des Workspaces
rm(list = ls())
# Laden
load("stp-a.RData")
ls()
# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## Laden des Sea Ice Index Datensatzes ####
##
sea.ice.index <- read.csv("04-data-nc/d-sea-ice-index-arctic-noaa.csv")


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


## MELTEN DER DATENSÄTZE (RESHAPE2::MELT) FÜR GGPLOT2 ####
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
# df.jets.month <- df.jets.month[,c("dts", "year", "month", "season", "lon", "J.lat.m1", "J.u.m1")]

# Maximaljet Chebyshev
df.jets.month$J.lat.m2b    <- melt(m2b.J.lat)$value
# df.jets.month$J.u.m2b      <- melt(m2b.J.u)$value

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


## Alle Cheb-Jets
# alle auffindbaren Chebyshev-Jets
df.jets.cheb.all              <- melt(m2a.J.lat[seq(from = 1, by = 4, length.out = n.lon*length(dts))],
                                      value.name = "J.lat.m2a.a")

df.jets.cheb.all$dts          <- df.jets.month$dts
df.jets.cheb.all$year         <- df.jets.month$year
df.jets.cheb.all$month        <- df.jets.month$month
df.jets.cheb.all$lon          <- df.jets.month$lon
df.jets.cheb.all              <- df.jets.cheb.all[,c("dts", "year", "month", "lon", "J.lat.m2a.a")]

df.jets.cheb.all $J.lat.m2a.b <- melt(m2a.J.lat[seq(from = 2, by = 4, length.out = n.lon*length(dts))])$value
df.jets.cheb.all $J.lat.m2a.c <- melt(m2a.J.lat[seq(from = 3, by = 4, length.out = n.lon*length(dts))])$value
df.jets.cheb.all $J.lat.m2a.d <- melt(m2a.J.lat[seq(from = 4, by = 4, length.out = n.lon*length(dts))])$value




## U-V-Datensatz
# Melten
df.uv <- melt(u[,,p.lvl,], varnames = c("lon", "lat", "t.stp"), value.name = "u")
df.uv$v <- melt(data = v[,,p.lvl,])$value
df.uv$uv <- sqrt( df.uv$u ** 2 + df.uv$v ** 2 )
df.uv$z <- melt(data = z[,,p.lvl,])$value



## EINFLUSS DER OROGRAPHIE AUF ZONALWIND ####

## Orographie
source("e-get-orography.r")
df.zh <- get.orography.df()


# 
# ## Coriolis-Parameter
# # f <- 2 * 0.7272 * 10 ** -4 * lat 
# 
# u.max <- 2 * 0.7272 * 10 ** -4 * df.jets.month$STJ.lat.m2c / 
#   (df.jets.month$STJ.z.m2c) - get.orography.ll(df.zh = df.zh, lon = df.jets.month$lon, lat = df.jets.month$STJ.lat.m2c))
# 
# get.orography.ll(df.zh = df.zh, lon = df.jets.month$lon, lat = df.jets.month$STJ.lat.m2c)
# 
# get.orography.ll(df.zh = df.zh, lon = lon[1], lat = lat[1])
# #df.jets.month$u.max <- 
#  




## SAISONALES MITTEL ÜBER ALLE JAHRE ####
## 

df.jets.season.mean <-
  data.frame(rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.mean) <- c('Season', 'Longitude', colnames(df.jets.month)[6:21])

for (i.stp in seq(from = 1, to = dim(df.jets.season.mean)[1])) {
#  print(i.stp)
  df.jets.season.mean[i.stp, 3:18] <-
    apply(X = df.jets.month[which(
      df.jets.month$season == df.jets.season.mean[i.stp,]$Season &
        df.jets.month$lon == df.jets.season.mean[i.stp,]$Longitude), 6:21],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}


## SAISONALES JÄHRLICHES MITTEL ####
##

# Festlegen des Untersuchungszeitraums
year.start <- 1958; year.end <- 2015; n.seas <- 4
n.years <- year.end - year.start + 1
# Definieren des Dataframes
length.df <- n.years * n.seas * n.lon # Länge
df.jets.season <- 
  data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
             rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:21])
# Schleife über alle Jahre, Jahreszeiten und Längengradabschnitte
for (i.stp in seq(from = 1, to = dim(df.jets.season)[1])) {
  df.jets.season[i.stp, 4:19] <- 
    apply(X = df.jets.month[which(
        df.jets.month$season == df.jets.season[i.stp,]$Season &
        df.jets.month$lon == df.jets.season[i.stp,]$Longitude), 6:21],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}


## SAISONALES JÄHRLICHES MITTEL ABZGL DES ZEITLICHEN MITTELS ####
## 

df.jets.season.rel <-
  data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
             rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.rel) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:21])

for (i.stp in 1:length(unique(df.jets.season$Year))) {
  y.stp <- unique(df.jets.season$Year)[i.stp]
  print(y.stp)
  df.jets.season.rel[which(df.jets.season.rel$Year == y.stp), 4:19] <-
    df.jets.season[which(df.jets.season$Year == y.stp), 4:19] - df.jets.season.mean[,3:18]
}


## MERIDIONALES MITTEL ####
## 

df.jets.season.mer <-
  data.frame(rep(seq(year.start, year.end), each = n.seas ),
             rep(c('djf','mam','jja','son')),
             NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.mer) <- c('Year', 'Season', colnames(df.jets.month)[6:21])

for (i.stp in seq(from = 1, to = dim(df.jets.season.mer)[1])) {
  print(i.stp)
  df.jets.season.mer[i.stp, 3:18] <- 
    apply(X = df.jets.month[which(
      df.jets.month$year == df.jets.season.mer[i.stp,]$Year &
        df.jets.month$season == df.jets.season.mer[i.stp,]$Season), 6:21],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}


## SAISONALES GLEITENDES MITTEL ÜBER FÜNF JAHRE ####
##

# Festlegen des Untersuchungszeitraums
year.start <- 1960; year.end <- 2009; n.seas <- 4
n.years <- year.end - year.start + 1
# Definieren des Dataframes
length.df <- n.years * n.seas * n.lon # Länge
df.jets.season.rn <- 
  data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
             rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.rn) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:21])
# Schleife über alle Jahre, Jahreszeiten und Längengradabschnitte
for (i.stp in seq(from = 1, to = dim(df.jets.season.rn)[1])) {
  df.jets.season.rn[i.stp, 4:19] <- 
    apply(X = df.jets.month[which(
      df.jets.month$year == df.jets.season.rn[i.stp,]$Year &
        df.jets.month$year >= df.jets.season.rn[i.stp,]$Year - 2 &
        df.jets.month$year <= df.jets.season.rn[i.stp,]$Year + 2 &
        df.jets.month$season == df.jets.season.rn[i.stp,]$Season &
        df.jets.month$lon == df.jets.season.rn[i.stp,]$Longitude), 6:21],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}

#### SAISONALES GLEITENDES MITTEL ABZGL DES ZEITLICHEN MITTELS ####
## 

df.jets.season.rn.rel <-
  data.frame(rep(seq(year.start, year.end), each = n.seas * n.lon),
             rep(c('djf','mam','jja','son'), each = n.lon),
             rep(lon),
             NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.rn.rel) <- c('Year', 'Season', 'Longitude', colnames(df.jets.month)[6:21])

for (i.stp in 1:length(unique(df.jets.season.rn.rel$Year))) {
  y.stp <- unique(df.jets.season.rn.rel$Year)[i.stp]
  df.jets.season.rn.rel[which(df.jets.season.rn.rel$Year == y.stp), 4:19] <-
    df.jets.season.rn[which(df.jets.season.rn.rel$Year == y.stp), 4:19] - df.jets.season.mean[,3:18]
}


## MERIDIONALES GLEITENDES MITTEL ####
## 

df.jets.season.mer.rn <-
  data.frame(rep(seq(year.start, year.end), each = n.seas ),
             rep(c('djf','mam','jja','son')),
             NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.season.mer.rn) <- c('Year', 'Season', colnames(df.jets.month)[6:21])

for (i.stp in seq(from = 1, to = dim(df.jets.season.mer.rn)[1])) {
#  print(i.stp)
  df.jets.season.mer.rn[i.stp, 3:18] <- 
    apply(X = df.jets.month[which(
      df.jets.month$year == df.jets.season.mer.rn[i.stp,]$Year &
        df.jets.month$year >= df.jets.season.mer.rn[i.stp,]$Year - 2 &
        df.jets.month$year <= df.jets.season.mer.rn[i.stp,]$Year + 2 &
        df.jets.month$season == df.jets.season.mer.rn[i.stp,]$Season), 6:21],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}




## ZEITLICHE MITTEL DER JET-POSITIONEN ####
##

## Zeitliches Mitteln für jeden Meridionalschritt
# Definieren des Dataframes
length.df <- n.lon # Länge
df.jets.tim.mean <- data.frame(rep(lon),
                             NA, NA, NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.tim.mean) <- c('Longitude', colnames(df.jets.month)[6:21])

# Schleife über alle Zeitschritte
for (i.stp in seq(from = 1, to = dim(df.jets.tim.mean)[1])) {
  # print(i.stp)
  df.jets.tim.mean[i.stp, 2:17] <- 
    apply(X = df.jets.month[which(
      df.jets.month$lon == df.jets.season[i.stp,]$Longitude), 6:21],
      MARGIN = 2, FUN = mean, na.rm = TRUE)
}


## Zeitliches und meridionales Mitteln
# Definieren des Dataframes
df.jets.tim.mer.mean <- 
  data.frame(NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA)
colnames(df.jets.tim.mer.mean) <- colnames(df.jets.month)[6:21]

# Mitteln der Jet-Positionen und -Geschwindigkeiten
df.jets.tim.mer.mean[1,] <- apply(X = df.jets.month[, 6:21],
                      MARGIN = 2, FUN = mean, na.rm = TRUE)


## LÖSCHEN UNNÖTIGER VARIABLEN ####
## 
# ls()
# 
setdiff(ls(), list("u", "v", "z"))


setdiff(ls(), list("df.jets.cheb.all", "df.jets.month", 
                   "df.jets.tim.mean", "df.jets.tim.mer.mean", "df.jets.season.mean", 
                   "df.jets.season", "df.jets.season.rel", "df.jets.season.mer", 
                   "df.jets.season.rn", "df.jets.season.rel.rn", "df.jets.season.mer.rn",
                   "df.uv", "df.zh",
                   "dts", "dts.cld.wrm", "dts.month", "dts.season", "dts.year", 
                   "get.orography.df", "get.orography.ll", "
                   i.stp", "lat", "lev", "lon", "n.lat", "n.lon", "n.seas", "n.years", "p.lvl"))


rm(list = setdiff(ls(), list("df.jets.cheb.all", "df.jets.month", 
                             "df.jets.tim.mean", "df.jets.tim.mer.mean", "df.jets.season.mean", 
                             "df.jets.season", "df.jets.season.rel", "df.jets.season.mer", 
                             "df.jets.season.rn", "df.jets.season.rel.rn", "df.jets.season.mer.rn",
                             "df.uv", "df.zh",
                             "dts", "dts.cld.wrm", "dts.month", "dts.season", "dts.year", 
                             "get.orography.df", "get.orography.ll", 
                             "i.stp", "lat", "lev", "lon", "n.lat", "n.lon", "n.seas", "n.years", "p.lvl")))


ls()


## ZWISCHENSPEICHERN DER WERTE DES DATENSATZES ####
# Speichern
save.image("stp-b.RData")


## ENDE ENDE ENDE ####