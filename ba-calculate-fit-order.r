## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####

## WORKING DIRECTORY & FIRST THINGS FIRST ####
## 
setwd("~/01-Master-Thesis/02-code-git/")
# getwd()
n.cluster <- 8

## EINLESEN DER DATEN ####
##
library(ncdf4)
nc <- nc_open("04-data-nc-csv/1957-2016-e4ei-t63-monmean-zuv-nh.nc")
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
#library(chron)
library(lubridate)
dts = as.POSIXct(date.help*3600, origin = '1900-01-01 00:00', tz = 'UTC')
dts.month <- as.character(month(dts, label = TRUE)); 
dts.year <- year(dts); 

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


## Schleife über Ordnungen von 8 - 32
## 
library(dplyr)
i.order <- seq(from = 8, to = 32)
df.order <- data.frame(order = i.order, rmse = NA,  dist = NA)
tb.order <- as_tibble(df.order)


library(parallel);
library(foreach); library(doParallel)

for (ii in seq_along(i.order)) {
  print(i.order[ii])

  cl.fork.1 <- makeCluster(n.cluster, type = "FORK")
  m1 <- parApply(cl.fork.1, X = u[,,p.lvl,], MARGIN = 3, FUN = find.jet.maximum.2d, axis = lat)
  stopCluster(cl.fork.1); rm(cl.fork.1)
  
  cl.fork.2 <- makeCluster(n.cluster, type = "FORK")
  registerDoParallel(cl.fork.2)
  m2 <- foreach(t.stp = seq_along(dts)) %dopar% {
    find.jets.chebpoly.2d(matrix.u = u[,,p.lvl,t.stp], 
                          matrix.v = v[,,p.lvl,t.stp],
                          matrix.z = z[,,p.lvl,t.stp],
                          axis.x = lon, axis.y = lat, n.order = i.order[ii])}
  stopCluster(cl.fork.2); rm(cl.fork.2)
  
  
  
  
  ## Globales Maximum
  # Maximaljet
  m1.J.lat    <- sapply(m1, "[[", "MaxJ.lat"); colnames(m1.J.lat)  <- dts; rownames(m1.J.lat) <- lon;
  # maximaler Chebyshev-Jet
  m2b.J.lat <- sapply(m2, "[[", "MaxJ.lat");   colnames(m2b.J.lat) <- dts; rownames(m2b.J.lat) <- lon;
  
  ## Polarfrontjet
  m2c.PFJ.lat <- sapply(m2, "[[", "PFJ.lat"); colnames(m2c.PFJ.lat) <- dts; rownames(m2c.PFJ.lat) <- lon;
  ## Subtropenjet
  m2c.STJ.lat <- sapply(m2, "[[", "STJ.lat"); colnames(m2c.STJ.lat) <- dts; rownames(m2c.STJ.lat) <- lon;
  
  ##
  rmse <- sqrt(mean((m1.J.lat - m2b.J.lat)**2, na.rm = TRUE))
  dist <- sqrt(mean((m2c.STJ.lat - m2c.PFJ.lat)**2, na.rm = TRUE))
  
  tb.order$rmse[ii] <- rmse
  tb.order$dist[ii] <- dist
  print(tb.order)
  write.table(tb.order, file = "b-set_order_fit.csv")
}


## ZWISCHENSPEICHERN DER WERTE DES DATENSATZES ####
# Speichern
# save.image("fit-order.RData")
