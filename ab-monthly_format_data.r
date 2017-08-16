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
m1.J.lat    <- sapply(m1, "[[", "MaxJ.lat"); colnames(m1.J.lat)  <- dts; rownames(m1.J.lat) <- lon;
# maximaler Chebyshev-Jet
m2b.J.lat <- sapply(m2, "[[", "MaxJ.lat");   colnames(m2b.J.lat) <- dts; rownames(m2b.J.lat) <- lon;

## Chebyshev

## Polarfrontjet
m2c.PFJ.lat <- sapply(m2, "[[", "PFJ.lat"); colnames(m2c.PFJ.lat) <- dts; rownames(m2c.PFJ.lat) <- lon;
m2c.PFJ.u   <- sapply(m2, "[[", "PFJ.u");   colnames(m2c.PFJ.u)   <- dts; rownames(m2c.PFJ.u)   <- lon;
m2c.PFJ.v   <- sapply(m2, "[[", "PFJ.v");   colnames(m2c.PFJ.v)   <- dts; rownames(m2c.PFJ.v)   <- lon;
m2c.PFJ.z   <- sapply(m2, "[[", "PFJ.z");   colnames(m2c.PFJ.z)   <- dts; rownames(m2c.PFJ.z)   <- lon;
## Subtropenjet
m2c.STJ.lat <- sapply(m2, "[[", "STJ.lat"); colnames(m2c.STJ.lat) <- dts; rownames(m2c.STJ.lat) <- lon;
m2c.STJ.u   <- sapply(m2, "[[", "STJ.u");   colnames(m2c.STJ.u)   <- dts; rownames(m2c.STJ.u)   <- lon;
m2c.STJ.v   <- sapply(m2, "[[", "STJ.v");   colnames(m2c.STJ.v)   <- dts; rownames(m2c.STJ.v)   <- lon;
m2c.STJ.z   <- sapply(m2, "[[", "STJ.z");   colnames(m2c.STJ.z)   <- dts; rownames(m2c.STJ.z)   <- lon;
## Single-Jetstream
m2c.SJS.lat <- sapply(m2, "[[", "SJS.lat"); colnames(m2c.SJS.lat) <- dts; rownames(m2c.SJS.lat) <- lon;
m2c.SJS.u   <- sapply(m2, "[[", "SJS.u");   colnames(m2c.SJS.u)   <- dts; rownames(m2c.SJS.u)   <- lon;
m2c.SJS.v   <- sapply(m2, "[[", "SJS.v");   colnames(m2c.SJS.v)   <- dts; rownames(m2c.SJS.v)   <- lon;
m2c.SJS.z   <- sapply(m2, "[[", "SJS.z");   colnames(m2c.SJS.z)   <- dts; rownames(m2c.SJS.z)   <- lon;

## Dijkstra
## Polarfrontjet
m3.PFJ.lat  <- sapply(m3, "[[", "PFJ.lat"); colnames(m3.PFJ.lat) <- dts; rownames(m3.PFJ.lat) <- lon;
m3.PFJ.u    <- sapply(m3, "[[", "PFJ.u");   colnames(m3.PFJ.u)   <- dts; rownames(m3.PFJ.u)   <- lon;
m3.PFJ.v    <- sapply(m3, "[[", "PFJ.v");   colnames(m3.PFJ.v)   <- dts; rownames(m3.PFJ.v)   <- lon;
## Subtropenjet
m3.STJ.lat  <- sapply(m3, "[[", "STJ.lat"); colnames(m3.STJ.lat) <- dts; rownames(m3.STJ.lat) <- lon;
m3.STJ.u    <- sapply(m3, "[[", "STJ.u");   colnames(m3.STJ.u)   <- dts; rownames(m3.STJ.u)   <- lon;
m3.STJ.v    <- sapply(m3, "[[", "STJ.v");   colnames(m3.STJ.v)   <- dts; rownames(m3.STJ.v)   <- lon;
## Single-Jetstream
m3.SJS.lat  <- sapply(m3, "[[", "SJS.lat"); colnames(m3.SJS.lat) <- dts; rownames(m3.SJS.lat) <- lon;
m3.SJS.u    <- sapply(m3, "[[", "SJS.u");   colnames(m3.SJS.u)   <- dts; rownames(m3.SJS.u)   <- lon;
m3.SJS.v    <- sapply(m3, "[[", "SJS.v");   colnames(m3.SJS.v)   <- dts; rownames(m3.SJS.v)   <- lon;

## MELTEN DER DATENSÄTZE (RESHAPE2::MELT) FÜR GGPLOT2 ####
## Maximaljet max()
df.jets.month.m1.mj <- melt(m1.J.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m1.mj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m1.mj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m1.mj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m1.mj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m1.mj <- df.jets.month.m1.mj[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m1.mj$u  <- NA
df.jets.month.m1.mj$v  <- NA
df.jets.month.m1.mj$uv <- NA
df.jets.month.m1.mj$z  <- NA
df.jets.month.m1.mj$type <- "MJ-max"
## Maximaljet Chebyshev
df.jets.month.m2.mj <- melt(m2b.J.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.mj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.mj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.mj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.mj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.mj <- df.jets.month.m2.mj[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.mj$u  <- NA
df.jets.month.m2.mj$v  <- NA
df.jets.month.m2.mj$uv <- NA
df.jets.month.m2.mj$z  <- NA
df.jets.month.m2.mj$type <- "MJ-Chebyshev"
## PFJ || Chebyshev- Methode 
df.jets.month.m2.pfj <- melt(m2c.PFJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.pfj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.pfj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.pfj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.pfj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.pfj <- df.jets.month.m2.pfj[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.pfj$u  <- melt(m2c.PFJ.u)$value
df.jets.month.m2.pfj$v  <- melt(m2c.PFJ.v)$value
df.jets.month.m2.pfj$uv <- sqrt(df.jets.month.m2.pfj$u**2 + df.jets.month.m2.pfj$v**2)
df.jets.month.m2.pfj$z  <- melt(m2c.PFJ.z)$value
df.jets.month.m2.pfj$type <- "PFJ-Chebyshev"
## STJ || Chebyshev- Methode 
df.jets.month.m2.stj <- melt(m2c.STJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.stj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.stj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.stj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.stj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.stj <- df.jets.month.m2.stj[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.stj$u  <- melt(m2c.STJ.u)$value
df.jets.month.m2.stj$v  <- melt(m2c.STJ.v)$value
df.jets.month.m2.stj$uv <- sqrt(df.jets.month.m2.stj$u**2 + df.jets.month.m2.stj$v**2)
df.jets.month.m2.stj$z  <- melt(m2c.STJ.z)$value
df.jets.month.m2.stj$type <- "STJ-Chebyshev"
## SJS || Chebyshev- Methode 
df.jets.month.m2.sjs <- melt(m2c.SJS.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.sjs$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.sjs$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.sjs$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.sjs$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.sjs <- df.jets.month.m2.sjs[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.sjs$u  <- melt(m2c.SJS.u)$value
df.jets.month.m2.sjs$v  <- melt(m2c.SJS.v)$value
df.jets.month.m2.sjs$uv <- sqrt(df.jets.month.m2.sjs$u**2 + df.jets.month.m2.sjs$v**2)
df.jets.month.m2.sjs$z  <- melt(m2c.SJS.z)$value
df.jets.month.m2.sjs$type <- "SJS-Chebyshev"
## PFJ || Dijkstra- Methode 
df.jets.month.m3.pfj <- melt(m3.PFJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m3.pfj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m3.pfj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m3.pfj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m3.pfj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m3.pfj <- df.jets.month.m3.pfj[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m3.pfj$u  <- melt(m3.PFJ.u)$value
df.jets.month.m3.pfj$v  <- melt(m3.PFJ.v)$value
df.jets.month.m3.pfj$uv <- sqrt(df.jets.month.m3.pfj$u**2 + df.jets.month.m3.pfj$v**2)
df.jets.month.m3.pfj$z  <- NA
df.jets.month.m3.pfj$type <- "PFJ-Dijkstra"
## STJ || Dijkstra- Methode 
df.jets.month.m3.stj <- melt(m3.STJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m3.stj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m3.stj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m3.stj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m3.stj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m3.stj <- df.jets.month.m3.stj[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m3.stj$u  <- melt(m3.STJ.u)$value
df.jets.month.m3.stj$v  <- melt(m3.STJ.v)$value
df.jets.month.m3.stj$uv <- sqrt(df.jets.month.m3.stj$u**2 + df.jets.month.m3.stj$v**2)
df.jets.month.m3.stj$z  <- NA
df.jets.month.m3.stj$type <- "STJ-Dijkstra"
## SJS || Dijkstra- Methode 
df.jets.month.m3.sjs <- melt(m3.SJS.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m3.sjs$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m3.sjs$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m3.sjs$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m3.sjs$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
# Umsortieren der Spalten des Datensatzes
df.jets.month.m3.sjs <- df.jets.month.m3.sjs[,c("dts", "year", "month", "season", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m3.sjs$u  <- melt(m3.SJS.u)$value
df.jets.month.m3.sjs$v  <- melt(m3.SJS.v)$value
df.jets.month.m3.sjs$uv <- sqrt(df.jets.month.m3.sjs$u**2 + df.jets.month.m3.sjs$v**2)
df.jets.month.m3.sjs$z  <- NA
df.jets.month.m3.sjs$type <- "SJS-Dijkstra"

## MERGEN DER DATAFRAMES ####
## 
df.jets.month <- merge(df.jets.month.m1.mj, df.jets.month.m2.mj, all = TRUE)
df.jets.month <- merge(df.jets.month, df.jets.month.m2.pfj, all = TRUE)
df.jets.month <- merge(df.jets.month, df.jets.month.m2.stj, all = TRUE)
df.jets.month <- merge(df.jets.month, df.jets.month.m2.sjs, all = TRUE)
df.jets.month <- merge(df.jets.month, df.jets.month.m3.pfj, all = TRUE)
df.jets.month <- merge(df.jets.month, df.jets.month.m3.stj, all = TRUE)
df.jets.month <- merge(df.jets.month, df.jets.month.m3.sjs, all = TRUE)

rm(df.jets.month.m1.mj, df.jets.month.m2.mj, 
   df.jets.month.m2.pfj, df.jets.month.m2.sjs, df.jets.month.m2.stj,
   df.jets.month.m3.pfj, df.jets.month.m3.sjs, df.jets.month.m3.stj)

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



## SAISONALES JÄHRLICHES MITTEL ####
##



## SAISONALES JÄHRLICHES MITTEL ABZGL DES ZEITLICHEN MITTELS ####
## 



## MERIDIONALES MITTEL ####
## 



## SAISONALES GLEITENDES MITTEL ÜBER FÜNF JAHRE ####
##



#### SAISONALES GLEITENDES MITTEL ABZGL DES ZEITLICHEN MITTELS ####
## 



## MERIDIONALES GLEITENDES MITTEL ####
## 



## ZEITLICHE MITTEL DER JET-POSITIONEN ####
##



## LÖSCHEN UNNÖTIGER VARIABLEN ####
## 

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
# load("stp-b.RData")

## ENDE ENDE ENDE ####
