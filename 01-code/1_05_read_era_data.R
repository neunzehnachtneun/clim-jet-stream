#' ==========================================================================================================
#' EINLESEROUTINE FÜR NCDF-DATEN DER ERA40- UND ERA-INTERIM-DATENSÄTZE                                     ==
#' EINGELESEN WERDEN DER ZONALWIND U, DER MERIDIONALWIND V, DAS GEOPOTENZIAL Z SOWIE                       ==
#' DIE ACHSEN DES DATENSATZES, ALSO LÄNGEN- UND BREITENGRAD UND ZEITACHSE                                  ==
#' DIE ROHDATEN LIEGEN AUF EINEM PAZIFIKZENTRISCHEN GITTER VOR,                                            ==
#' DIESES WIRD AUF EIN EUROPAZENTRISCHES TRANSFORMIERT                                                     ==
#' ==========================================================================================================


#' Einlesen der Daten mit ncdf4-Routinen
nc <- nc_open("04-data-nc-csv/1957-2016-e4ei-t63-monmean-zuv-nh.nc")
u <- ncvar_get(nc, "u") ## U-Wind-Komponente
v <- ncvar_get(nc, "v") # V-Wind-Komponente
z <- ncvar_get(nc, "z") # Geopotenzielle Höhe
lon <- ncvar_get(nc, "lon") # Längengrad
lat <- ncvar_get(nc, "lat") # Breitengrad
lev <- ncvar_get(nc, "level") # Drucklevel
date_help <- ncvar_get(nc, "time")
nc_close(nc); rm(nc)

#' Umschreiben der Zeitachse in Posix-Format
dts <- as.POSIXct(date_help * 3600, origin = "1900-01-01 00:00", tz = "UTC")
dts_month <- as.character(month(dts, label = TRUE))
dts_year <- year(dts)

#' Auflösungen des Datensatzes
#' Räumliche Auflösung
n_lon <- length(lon)
n_lat <- length(lat)
n_lev <- length(lev)
#' Zeitliche Auflösung
n_dts <- length(dts)

#' Indizes der Matrizen für spätere Verarbeitung
dimnames(u) <- list(lon, lat, lev, dts)
dimnames(v) <- list(lon, lat, lev, dts)
dimnames(z) <- list(lon, lat, lev, dts)

#' Vektor mit Markern für warme und kalte Monate
#' Notwendig für Dijkstra-Methode
dts_cold_warm <- rep(NA, length.out = length(dts))
dts_cold_warm[which(dts_month == "Nov" | dts_month == "Dec" | dts_month == "Jan" | dts_month == "Feb" | dts_month == "Mar" | dts_month == "Apr")] <- "cold"
dts_cold_warm[which(dts_month == "May" | dts_month == "Jun" | dts_month == "Jul" | dts_month == "Aug" | dts_month == "Sep" | dts_month == "Oct")] <- "warm"

#' Unterscheidung von Jahreszeiten
dts_season <- rep(NA, length.out = length(dts))
dts_season[which(dts_month == "Dec" | dts_month == "Jan" | dts_month == "Feb")] <- "djf"
dts_season[which(dts_month == "Mar" | dts_month == "Apr" | dts_month == "May")] <- "mam"
dts_season[which(dts_month == "Jun" | dts_month == "Jul" | dts_month == "Aug")] <- "jja"
dts_season[which(dts_month == "Sep" | dts_month == "Oct" | dts_month == "Nov")] <- "son"


#' Umrechnen der Matrizen von Pazifik- auf Europazentrisches Gitter -----------------------------------------
#' [0,360] -> [-180,180]
#'
lon_help <- lon - 180
# lon[1]; lon_help[97];
u <- u[c(97:192, 1:96),,,]
dimnames(u) <- list(lon_help, lat, lev, dts)
v <- v[c(97:192, 1:96),,,]
dimnames(v) <- list(lon_help, lat, lev, dts)
z <- z[c(97:192, 1:96),,,]
dimnames(z) <- list(lon_help, lat, lev, dts)
lon <- lon_help


#' Festlegen des zu untersuchenden Druckniveaus 
pressure_level <- which(lev == geopotential)
