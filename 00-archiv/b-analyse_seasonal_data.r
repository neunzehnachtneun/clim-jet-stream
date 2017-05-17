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
filename <- "c-1957-2016-e4ei-t63-uv-nh-seasmean.nc"

list.era <- read.era.nc(path = path, file = filename)
u.era <- list.era$u.era; v.era <- list.era$v.era; uv.era <- list.era$uv.era;
lon <- list.era$lon; lat <- list.era$lat; lev <- list.era$lev;
dts <- list.era$dts; dts.month <- list.era$dts.month; dts.year <- list.era$dts.year;
rm(list.era)


####
## LOOP OVER DIFFERENT PRESSURE LEVELS
####
for (pl in 2:6) {
  print(lev[pl])

  
  ####
  ## METHODE 2: ZONALE MAXIMA DES U-WINDFELDS ####
  ####
  list.max.disc <- locate.jetstream.disc(array = u.era[,,pl,], axis = lat, n.cpu = 5)
  model.max.lat.disc <- list.max.disc$model.max.lat; model.max.u.disc <- list.max.disc$model.max.u;
  rm(list.max.disc)
  
  
  
  ####
  ## METHODE 2: CHEBYSHEV-LEAST-SQUARES-FIT + MAXIMA ####
  ####
  
  list.max.poly <- locate.jetstream.poly(array = u.era[,,pl,], axis = lat, n.order = 8, n.cpu = 5)
  model.max.lat.poly <- list.max.poly$model.max.lat; model.max.u.poly <- list.max.poly$model.max.u;
  rm(list.max.poly)
  
  
  ## Anzahl der Maxima
  n.model.max.poly <- apply(model.max.lat.poly, c(1, 3), len.na)
  
  ## Unterscheidung von polarem und subtropischem Jetstream
  ## Filterung der zwei stärksten Maxima
  model.max.2 <- apply(model.max.u.poly, c(1,3), diff.max)
  # Positionen
  model.max.2.x <- sapply(model.max.2, "[[", 1) 
  model.max.2.x <- array(model.max.2.x, c(nrow(model.max.2.x), dim(model.max.2)))
  # Werte
  model.max.2.u <- sapply(model.max.2, "[[", 2) 
  model.max.2.u <- array(model.max.2.u, c(nrow(model.max.2.u), dim(model.max.2)))
  # Umrechnung der Index-Positionen zu Breitengraden
  model.max.2.lat <- array(NA, dim = c(nrow(model.max.2.x), dim(model.max.2)))
  for (j in 1:237) { for (i in 1:192) {
    model.max.2.lat[,i,j] <- model.max.lat.poly[i,model.max.2.x[,i,j],j]
  }}
  
  # Unterscheidung zwischen Situationen mit einem und mehreren Jet
  # Bei mehreren Jets werden die zwei stärksten Maxima als Polar- und Subtropen-Jet geführt.
  jet.lat.1 <- matrix(nrow = 192,ncol = 237)
  jet.lat.p <- matrix(nrow = 192,ncol = 237); jet.lat.st <- matrix(nrow = 192,ncol = 237)
  for (j in 1:237) { for (i in 1:192) {
    if (n.model.max.poly[i,j] == 1) {
      jet.lat.1[i,j] <- model.max.2.lat[1,i,j]
    } else if (n.model.max.poly[i,j] > 1) {
      jet.lat.p[i,j] <- max(model.max.2.lat[,i,j])
      jet.lat.st[i,j] <- min(model.max.2.lat[,i,j])
    }
  }}
  
  
  ## Trennung der Daten nach Jahreszeiten
  # Datum / Zeitpunkt
  dts.mam <- dts[which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
  dts.jja <- dts[which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
  dts.son <- dts[which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
  dts.djf <- dts[which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  # Methode: diskrete Maxima-Suche
  # Position d Jets 
  model.max.lat.disc.mam <- model.max.lat.disc[, which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
  model.max.lat.disc.jja <- model.max.lat.disc[, which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
  model.max.lat.disc.son <- model.max.lat.disc[, which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
  model.max.lat.disc.djf <- model.max.lat.disc[, which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  # Werte d Jets 
  model.max.u.disc.mam <- model.max.u.disc[, which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
  model.max.u.disc.jja <- model.max.u.disc[, which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
  model.max.u.disc.son <- model.max.u.disc[, which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
  model.max.u.disc.djf <- model.max.u.disc[, which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  # Methode: Maximalstellensuche über Ableitung des Least-Squares-Fit
  # Anzahl d Jets
  n.model.max.poly.mam <- n.model.max.poly[,which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  n.model.max.poly.jja <- n.model.max.poly[,which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
  n.model.max.poly.son <- n.model.max.poly[,which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
  n.model.max.poly.djf <- n.model.max.poly[,which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  # Position d Jets bei Situationnen mit einem Jet
  jet.lat.1.mam <- jet.lat.1[,which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
  jet.lat.1.jja <- jet.lat.1[,which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
  jet.lat.1.son <- jet.lat.1[,which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
  jet.lat.1.djf <- jet.lat.1[,which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  # Positionen d Polar Jets
  jet.lat.p.mam <- jet.lat.p[,which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
  jet.lat.p.jja <- jet.lat.p[,which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
  jet.lat.p.son <- jet.lat.p[,which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
  jet.lat.p.djf <- jet.lat.p[,which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  # Positionen d Subtropen-Jets
  jet.lat.st.mam <- jet.lat.st[,which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")]
  jet.lat.st.jja <- jet.lat.st[,which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")]
  jet.lat.st.son <- jet.lat.st[,which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")]
  jet.lat.st.djf <- jet.lat.st[,which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")]
  
  
  
  ####
  ## DATENVISUALISIERUNG ####
  ####
  
  ## Jet aus max-Routine (diskret)
  pdf(file = paste0("05-visu-pdf/",lev[pl],"-hPa-discrete.pdf"))
  colbreaks <- seq(min(model.max.lat.disc, na.rm = TRUE), max(model.max.lat.disc, na.rm = TRUE), length.out = 12)
  plt.image(lon, dts, model.max.lat.disc, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.mam, model.max.lat.disc.mam, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.jja, model.max.lat.disc.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.son, model.max.lat.disc.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.djf, model.max.lat.disc.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  
  colbreaks <- seq(min(model.max.u.disc, na.rm = TRUE), max(model.max.u.disc, na.rm = TRUE), length.out = 12)
  plt.image(lon, dts, model.max.u.disc, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.mam, model.max.u.disc.mam, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.jja, model.max.u.disc.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.son, model.max.u.disc.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.djf, model.max.u.disc.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  dev.off()
  ## Jet aus Least-Squares
  ## Anzahl d. gefitteten Jets 
  pdf(file = paste0("05-visu-pdf/",lev[pl],"-hPa-polynomial.pdf"))
  colbreaks <- seq(-0.5, max(n.model.max.poly, na.rm = TRUE) + 0.5, length.out = max(n.model.max.poly, na.rm = TRUE) + 2)
  plt.image(lon, dts, n.model.max.poly, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.mam, n.model.max.poly.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.jja, n.model.max.poly.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.son, n.model.max.poly.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.djf, n.model.max.poly.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  
  ## Single-Jet
  colbreaks <- seq(min(jet.lat.1, na.rm = TRUE), max(jet.lat.1, na.rm = TRUE), length.out = 12)
  plt.image(lon, dts, jet.lat.1, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.mam, jet.lat.1.mam, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.jja, jet.lat.1.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.son, jet.lat.1.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.djf, jet.lat.1.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  
  ## Polar-Jet
  colbreaks <- seq(min(jet.lat.p, na.rm = TRUE), max(jet.lat.p, na.rm = TRUE), length.out = 12)
  plt.image(lon, dts, jet.lat.p, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.mam, jet.lat.p.mam, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.jja, jet.lat.p.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.son, jet.lat.p.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.djf, jet.lat.p.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  
  ## Subtropen-Jet
  colbreaks <- seq(min(jet.lat.st, na.rm = TRUE), max(jet.lat.st, na.rm = TRUE), length.out = 12)
  plt.image(lon, dts, jet.lat.st, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.mam, jet.lat.st.mam, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.jja, jet.lat.st.jja, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.son, jet.lat.st.son, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  plt.image(lon, dts.djf, jet.lat.st.djf, colbreaks, y.dts = TRUE, label.x = "Längengrad", label.y = "Jahr")
  dev.off()
}