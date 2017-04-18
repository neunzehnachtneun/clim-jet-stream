## source('~/01-Master-Thesis/02-code-git/a-analyse_seasonal_change.r')
## 
## BERECHNUNG VON GLEITENDEM MITTEL U SD ####
## ÜBER FÜNF JAHRE U SAISONAL              ## 
####
####

####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

setwd("~/01-Master-Thesis/02-code-git/")

## routinen zum einlesen und plotten
source("d-read_era_ncdf.r")
source("e-locate_jetstream_discrete_2d.r")
source("f-locate_jetstream_polynomial_2d.r")
source("j-shortest-path-dijkstra.r")
source("m-plots-master.r")


## hilfsfunktion zur bestimmung der länge eines vektors ohne berücksichtigung von NAs
len.na <- function(x) {
  len <- sum(!is.na(x))
  return(len)
}

## hilfsfunktion zur bestimmung der zwei größten werte eines vektors
diff.max <- function(x) {
  xx <- x
  # auffinden der zwei größten werte
  xx <- if (length(which(!is.na(xx)))) xx[!is.na(xx)] else xx
  y.max.1 <- max(xx)
  xx <- if (length(xx) == 1) NA else if (length(which(!is.na(xx)))) xx[which(xx < max(xx))] else xx
  y.max.2 <- max(xx)
  # positionen der zwei größten werte
  x.max.1 <- if (!is.na(y.max.1)) which(x == y.max.1) else NA
  x.max.2 <- if (!is.na(y.max.2)) which(x == y.max.2) else NA
  return(list(max.x = c(x.max.1, x.max.2), max.y = c(y.max.1, y.max.2)))
}


path <- "04-data-nc/"
list.files(path = paste0(getwd(), path))
filename <- "a-1957-2016-e4ei-t63-uv-nh-timmean.nc"

list.era <- read.era.nc(path = path, file = filename)
u.era <- list.era$u.era; v.era <- list.era$v.era; uv.era <- list.era$uv.era;
lon <- list.era$lon; lat <- list.era$lat; lev <- list.era$lev;
dts <- list.era$dts; dts.month <- list.era$dts.month; dts.year <- list.era$dts.year;
rm(list.era)

PFJ.cold <- find.jet.dijkstra.2d(u, v, lon, lat, jet = "PFJ", season = "cold")


image.plot(lon, lat, sqrt(u ** 2 + v ** 2))
points(PFJ.cold$lon.jet, PFJ.cold$lat.jet)

