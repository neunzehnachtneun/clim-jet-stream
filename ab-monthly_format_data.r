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

## LADEN DES SEA ICE DATENSATZTES ####
##
library(lubridate)

df.sea.ice <- read.csv("04-data-nc-csv/d-sea-ice-index-arctic-noaa.csv")

df.sea.ice$dts <- as.Date(paste0(df.sea.ice$year,"-",df.sea.ice$mo,"-1"))
df.sea.ice$year <- year(df.sea.ice$dts)
df.sea.ice$month <- as.character(month(df.sea.ice$dts, label = TRUE))

names(df.sea.ice)
df.sea.ice <- df.sea.ice[c(-1, -3, -4, -5, -8)]
names(df.sea.ice)

## FORMATIEREN DER DATENSATZE ####
## 
# library(plyr); 
library(dplyr); library(reshape2)

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
df.jets.month.m1.mj <- melt(m1.J.lat, varnames = c("lon", "dts"),  value.name = "lat" )
df.jets.month.m1.mj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m1.mj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m1.mj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m1.mj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m1.mj$method <- "Max"
df.jets.month.m1.mj$class <- "MJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m1.mj <- df.jets.month.m1.mj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m1.mj$u  <- NA
df.jets.month.m1.mj$v  <- NA
df.jets.month.m1.mj$uv <- NA
df.jets.month.m1.mj$z  <- NA


## Maximaljet Chebyshev
df.jets.month.m2.mj <- melt(m2b.J.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.mj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.mj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.mj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.mj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m2.mj$method <- "Chebyshev"
df.jets.month.m2.mj$class <- "MJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.mj <- df.jets.month.m2.mj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.mj$u  <- NA
df.jets.month.m2.mj$v  <- NA
df.jets.month.m2.mj$uv <- NA
df.jets.month.m2.mj$z  <- NA

## PFJ || Chebyshev- Methode 
df.jets.month.m2.pfj <- melt(m2c.PFJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.pfj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.pfj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.pfj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.pfj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m2.pfj$method <- "Chebyshev"
df.jets.month.m2.pfj$class <- "PFJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.pfj <- df.jets.month.m2.pfj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.pfj$u  <- melt(m2c.PFJ.u)$value
df.jets.month.m2.pfj$v  <- melt(m2c.PFJ.v)$value
df.jets.month.m2.pfj$uv <- sqrt(df.jets.month.m2.pfj$u**2 + df.jets.month.m2.pfj$v**2)
df.jets.month.m2.pfj$z  <- melt(m2c.PFJ.z)$value


## STJ || Chebyshev- Methode 
df.jets.month.m2.stj <- melt(m2c.STJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.stj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.stj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.stj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.stj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m2.stj$method <- "Chebyshev"
df.jets.month.m2.stj$class <- "STJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.stj <- df.jets.month.m2.stj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.stj$u  <- melt(m2c.STJ.u)$value
df.jets.month.m2.stj$v  <- melt(m2c.STJ.v)$value
df.jets.month.m2.stj$uv <- sqrt(df.jets.month.m2.stj$u**2 + df.jets.month.m2.stj$v**2)
df.jets.month.m2.stj$z  <- melt(m2c.STJ.z)$value


## SJS || Chebyshev- Methode 
df.jets.month.m2.sjs <- melt(m2c.SJS.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m2.sjs$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m2.sjs$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m2.sjs$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m2.sjs$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m2.sjs$method <- "Chebyshev"
df.jets.month.m2.sjs$class <- "SJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m2.sjs <- df.jets.month.m2.sjs[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m2.sjs$u  <- melt(m2c.SJS.u)$value
df.jets.month.m2.sjs$v  <- melt(m2c.SJS.v)$value
df.jets.month.m2.sjs$uv <- sqrt(df.jets.month.m2.sjs$u**2 + df.jets.month.m2.sjs$v**2)
df.jets.month.m2.sjs$z  <- melt(m2c.SJS.z)$value


## PFJ || Dijkstra- Methode 
df.jets.month.m3.pfj <- melt(m3.PFJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m3.pfj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m3.pfj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m3.pfj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m3.pfj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m3.pfj$method <- "Dijkstra"
df.jets.month.m3.pfj$class <- "PFJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m3.pfj <- df.jets.month.m3.pfj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m3.pfj$u  <- melt(m3.PFJ.u)$value
df.jets.month.m3.pfj$v  <- melt(m3.PFJ.v)$value
df.jets.month.m3.pfj$uv <- sqrt(df.jets.month.m3.pfj$u**2 + df.jets.month.m3.pfj$v**2)
df.jets.month.m3.pfj$z  <- NA


## STJ || Dijkstra- Methode 
df.jets.month.m3.stj <- melt(m3.STJ.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m3.stj$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m3.stj$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m3.stj$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m3.stj$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m3.stj$method <- "Dijkstra"
df.jets.month.m3.stj$class <- "STJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m3.stj <- df.jets.month.m3.stj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m3.stj$u  <- melt(m3.STJ.u)$value
df.jets.month.m3.stj$v  <- melt(m3.STJ.v)$value
df.jets.month.m3.stj$uv <- sqrt(df.jets.month.m3.stj$u**2 + df.jets.month.m3.stj$v**2)
df.jets.month.m3.stj$z  <- NA


## SJS || Dijkstra- Methode 
df.jets.month.m3.sjs <- melt(m3.SJS.lat, varnames = c("lon", "dts"),value.name = "lat" )
df.jets.month.m3.sjs$dts          <- rep(dts, each = n.lon)          # Datum
df.jets.month.m3.sjs$year         <- rep(dts.year, each = n.lon)     # Jahr
df.jets.month.m3.sjs$month        <- rep(dts.month, each = n.lon)    # Monat
df.jets.month.m3.sjs$season       <- rep(dts.season, each = n.lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df.jets.month.m3.sjs$method <- "Dijkstra"
df.jets.month.m3.sjs$class <- "SJ"
# Umsortieren der Spalten des Datensatzes
df.jets.month.m3.sjs <- df.jets.month.m3.sjs[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df.jets.month.m3.sjs$u  <- melt(m3.SJS.u)$value
df.jets.month.m3.sjs$v  <- melt(m3.SJS.v)$value
df.jets.month.m3.sjs$uv <- sqrt(df.jets.month.m3.sjs$u**2 + df.jets.month.m3.sjs$v**2)
df.jets.month.m3.sjs$z  <- NA



## MERGEN DER DATAFRAMES ####
## 
## Mergen der einzelnen Dataframes der unterschiedlichen Methoden
df.jets.month <- rbind(df.jets.month.m1.mj, df.jets.month.m2.mj,
                       df.jets.month.m2.pfj, df.jets.month.m2.stj, df.jets.month.m2.sjs,
                       df.jets.month.m3.pfj, df.jets.month.m3.stj, df.jets.month.m3.sjs)
# Sortieren nach Datum und Längengrad
df.jets.month <- df.jets.month[order(df.jets.month$dts, df.jets.month$lon),]
# Umformatieren der year-Datenreihe als numeric 
df.jets.month$year <- as.numeric(as.character(df.jets.month$year))


## Mergen des Jet-Dataframes mit dem Seeeis-Dataframe
df.jets.month <- merge(df.jets.month, df.sea.ice, 
                       by.x = c("year", "month"), 
                       by.y = c("year", "month"), 
                       all.x = TRUE)
# Sortieren nach Datum und Längengrad
df.jets.month <- df.jets.month[order(df.jets.month$dts, df.jets.month$lon),]


## U-V-Datensatz ####
# Melten
df.uv <- melt(u[,,p.lvl,], varnames = c("lon", "lat", "dts"), value.name = "u")
df.uv$dts <- rep(dts, each = n.lon*n.lat)
df.uv$v <- melt(data = v[,,p.lvl,])$value
df.uv$uv <- sqrt( df.uv$u ** 2 + df.uv$v ** 2 )
df.uv$z <- melt(data = z[,,p.lvl,])$value


## DATAFRAMES ALS TIBBLES ####
## 
tb.jets.month <- as_tibble(df.jets.month)
tb.uv <- as_tibble(df.uv)

tb.jets.month$method <- factor(tb.jets.month$method, levels = c("Max", "Chebyshev", "Dijkstra"), 
                               labels = c("Maximum", "Chebyshev", "Dijkstra"))
tb.jets.month$class <- factor(tb.jets.month$class, levels = c("MJ", "PFJ", "SJ", "STJ"), 
                              labels = c("MJ", "PFJ", "SJ", "STJ"))

## LÖSCHEN UNNÖTIGER VARIABLEN ####
## 

rm(list = setdiff(ls(), list("tb.jets.month", "tb.uv", 
                             "dts", "dts.cld.wrm", "dts.month", "dts.season", "dts.year", 
                             "get.orography.df", "get.orography.ll", 
                             "i.stp", "lat", "lev", "lon", "n.lat", "n.lon", "n.seas", "n.years", "p.lvl")))


ls()


## ZWISCHENSPEICHERN DER WERTE DES DATENSATZES ####
# Speichern
save.image("stp-b.RData")
# load("stp-b.RData")


## ENDE ENDE ENDE ####
