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
source("m-plots-master.r")
source("f-locate_jetstream_polynomial_2d.r")

## hilfsfunktion zur bestimmung der länge eines vektors ohne berücksichtigung von NAs
len.na <- function(x) {
  len <- sum(!is.na(x))
  return(len)
}


####
## LOOP OVER DIFFERENT PRESSURE LEVELS
####
pl <- 2:6
print(lev[pl])
for (i in pl) {
  print(lev[i])
}

path <- "04-data-nc/"
filename <- "c-1957-2016-e4ei-t63-uv-nh-seasmean.nc"

list.era <- read.era.nc(path = path, file = filename)
u.era <- list.era$u.era; v.era <- list.era$v.era; uv.era <- list.era$uv.era;
lon <- list.era$lon; lat <- list.era$lat; lev <- list.era$lev;
dts <- list.era$dts; dts.month <- list.era$dts.month; dts.year <- list.era$dts.year;
rm(list.era)

pl <- 1
list.max <- locate.jetstream.poly(array = u.era[,,pl,], axis = lat, n.order = 8, n.cpu = 2)
model.max.lat <- list.max$model.max.lat; model.max.u <- list.max$model.max.u;
rm(list.max)


## Trennung der Daten nach Jahreszeiten

model.max.lat.mam <- model.max.lat[,, which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
model.max.lat.jja <- model.max.lat[,, which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
model.max.lat.son <- model.max.lat[,, which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
model.max.lat.djf <- model.max.lat[,, which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]

model.max.u.mam <- model.max.u[,, which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
model.max.u.jja <- model.max.u[,, which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
model.max.u.son <- model.max.u[,, which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
model.max.u.djf <- model.max.u[,, which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]

dts.mam <- dts[which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
dts.jja <- dts[which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
dts.son <- dts[which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
dts.djf <- dts[which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]


## anzahl der maxima

n.model.max.mam <- apply(model.max.lat.mam, c(1, 3), len.na)
n.model.max.jja <- apply(model.max.lat.jja, c(1, 3), len.na)
n.model.max.son <- apply(model.max.lat.son, c(1, 3), len.na)
n.model.max.djf <- apply(model.max.lat.djf, c(1, 3), len.na)


####
## DATENVISUALISIERUNG ####
####

### 
colbreaks <- seq(min(model.max.lat, na.rm = TRUE), max(model.max.lat, na.rm = TRUE), length.out = 12)
plt.image(lon, dts, model.max.lat, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.mam, model.max.lat.mam, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.jja, model.max.lat.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.son, model.max.lat.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.djf, model.max.lat.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")

###
colbreaks <- seq(min(model.max.u, na.rm = TRUE), max(model.max.u, na.rm = TRUE), length.out = 12)
plt.image(lon, dts, model.max.u, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.mam, model.max.u.mam, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.jja, model.max.u.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.son, model.max.u.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
plt.image(lon, dts.djf, model.max.u.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")



