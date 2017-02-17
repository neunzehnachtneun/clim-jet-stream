## source('~/Master_Thesis/02-r-code-git/d-analyse_seasonal_change.r')
## 
## BERECHNUNG VON GLEITENDEM MITTEL U SD ####
## ÜBER FÜNF JAHRE U SAISONAL              ## 
####
####

####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

setwd("~/01-Master-Thesis/02-r-code-git/")
path <- "03-data-nc/"

####
## PLOTROUTINEN ####
####

source("m-plots-master.r")


####
## SEASONALLY YEARLY MEANS
####
####

filename <- "d-1957-2016-e4ei-t63-uv-nh-seasmean-runmean.nc"

source("a-read_era_ncdf.r")
source("b-locate_jetstream_1polynomial_2d.r")


####
## DATENAUFBEREITUNG ####
####


model.max.lat.mam <- model.max.lat[, which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
model.max.lat.jja <- model.max.lat[, which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
model.max.lat.son <- model.max.lat[, which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
model.max.lat.djf <- model.max.lat[, which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]

model.max.u.mam <- model.max.u[, which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
model.max.u.jja <- model.max.u[, which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
model.max.u.son <- model.max.u[, which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
model.max.u.djf <- model.max.u[, which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]

dts.mam <- dts[which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
dts.jja <- dts[which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
dts.son <- dts[which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
dts.djf <- dts[which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]

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




####
## RUNNING MEAN OVER FIVE YEARS ####
## (STEP +/- TWO YEARS)
####

dts.year.mn <- seq(1960, 2013)
n.time <- length(dts.year.mn)
for (seas in (c("mam", "jja", "son", "djf"))) {
  if (seas == "mam") {
    ind.seas <- which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")
  } else if (seas == "jja") {
    ind.seas <- which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")
  } else if (seas == "son") {
    ind.seas <- which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")
  } else if (seas == "djf") {
    ind.seas <- which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")
  }
  
  ## Mittelwerte global
  ## Vorbereiten der Matrizen
  u.seas.mean <- array(NA , dim = c(n.lon, n.lat, n.time))
  u.seas.sd <- array(NA , dim = c(n.lon, n.lat, n.time))
  model.max.u.seas.mean <- array(NA , dim = c(n.lon, n.time))
  model.max.u.seas.sd <- array(NA , dim = c(n.lon, n.time))
  model.max.lat.seas.mean <- array(NA , dim = c(n.lon, n.time))
  model.max.lat.seas.sd <- array(NA , dim = c(n.lon, n.time))
  
  for (i in seq(1, n.time)) {
    print(i)
    yr.i <- dts.year.mn[i]
    ind.yr <- which(dts.year == (yr.i - 2) | dts.year == (yr.i - 1) | dts.year ==  yr.i | dts.year == (yr.i + 1) | dts.year == (yr.i + 2))
    ind.seas.yr <- intersect(ind.yr, ind.seas)
    #  print(ind.seas.yr)
    u.seas.mean[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), mean)
    u.seas.sd[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), sd)
    model.max.u.seas.mean[,i] <- apply(model.max.u[, ind.seas.yr], 1, mean)
    model.max.u.seas.sd[,i] <- apply(model.max.u[, ind.seas.yr], 1, sd)
    model.max.lat.seas.mean[,i] <- apply(model.max.lat[, ind.seas.yr], 1, mean)
    model.max.lat.seas.sd[,i] <- apply(model.max.lat[, ind.seas.yr], 1, sd)
    
    
  }
}


