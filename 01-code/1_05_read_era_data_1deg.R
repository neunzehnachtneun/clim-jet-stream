#' ==========================================================================================================
#' EINLESEROUTINE FÜR NCDF-DATEN DER ERA40- UND ERA-INTERIM-DATENSÄTZE                                     ==
#' EINGELESEN WERDEN DER ZONALWIND U, DER MERIDIONALWIND V, DAS GEOPOTENZIAL Z SOWIE                       ==
#' DIE ACHSEN DES DATENSATZES, ALSO LÄNGEN- UND BREITENGRAD UND ZEITACHSE                                  ==
#' DIE ROHDATEN LIEGEN AUF EINEM PAZIFIKZENTRISCHEN GITTER VOR,                                            ==
#' DIESES WIRD AUF EIN EUROPAZENTRISCHES TRANSFORMIERT                                                     ==
#' ==========================================================================================================

## =============================================================================
## Einlesen der ERA-Interim-Datensätze =========================================
## =============================================================================

timestep <- c(1:120) ## schrittweises einlesen, um arbeitsspeicherlast zu minimieren 

#' Einlesen der Daten mit ncdf4-Routinen
nc <- nc_open("../../a-e4ei-1deg-1958-2017-uv.nc")
u <- ncvar_get(nc, "eastward_wind")[,,,timestep]
v <- ncvar_get(nc, "northward_wind")[,,,timestep]
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
lev <- ncvar_get(nc, "air_pressure")
tm_help <- ncvar_get(nc, "time")[timestep]
nc_close(nc); rm(nc)

#' Umschreiben der Zeitachse in Posix-Format
dts <- as_date(tm_help / 24, origin = "1900-01-01 UTC", tz = "UTC")
dts_month <- as.character(month(dts, label = TRUE))
dts_year <- year(dts)


## =============================================================================
## Eckdaten der Datensätze =====================================================
## =============================================================================


#' Auflösungen des Datensatzes
#' Räumliche Auflösung
n_lon <- dim(u)[1]
n_lat <- dim(u)[2]
n_lev <- dim(u)[3]
#' Zeitliche Auflösung
n_dts <- dim(u)[4]


## =============================================================================
## Definieren von kalten/warmen Jahreszeiten nach Molnos et al. ================
## =============================================================================

#' Vektor mit Markern für warme und kalte Monate
#' Notwendig für Dijkstra-Methode
dts_cold_warm <- rep(NA, length.out = length(dts))
dts_cold_warm[which(dts_month == "Nov" | dts_month == "Dec" | 
                      dts_month == "Jan" | dts_month == "Feb" | 
                      dts_month == "Mar" | dts_month == "Apr")] <- "cold"
dts_cold_warm[which(dts_month == "May" | dts_month == "Jun" | 
                      dts_month == "Jul" | dts_month == "Aug" | 
                      dts_month == "Sep" | dts_month == "Oct")] <- "warm"

#' Unterscheidung von Jahreszeiten
dts_season <- rep(NA, length.out = length(dts))
dts_season[which(dts_month == "Dec" | 
                   dts_month == "Jan" | dts_month == "Feb")] <- "djf"
dts_season[which(dts_month == "Mar" | 
                   dts_month == "Apr" | dts_month == "May")] <- "mam"
dts_season[which(dts_month == "Jun" | 
                   dts_month == "Jul" | dts_month == "Aug")] <- "jja"
dts_season[which(dts_month == "Sep" | 
                   dts_month == "Oct" | dts_month == "Nov")] <- "son"

## =============================================================================
## Festlegen des zu untersuchenden Druckniveaus ================================
## =============================================================================
pressure_level <- which(lev == geopotential)
