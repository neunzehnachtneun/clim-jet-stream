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

#' Einlesen der Daten mit ncdf4-Routinen
nc <- nc_open("../../a-ei-1deg-1979-2017-ztuv.nc")
u_ei <- ncvar_get(nc, "u")[,,,1:2] ## U-Wind-Komponente
v_ei <- ncvar_get(nc, "v")[,,,1:2] # V-Wind-Komponente
lon <- ncvar_get(nc, "longitude") # Längengrad
lat <- ncvar_get(nc, "latitude") # Breitengrad
lev <- ncvar_get(nc, "level") # Drucklevel
date_help <- ncvar_get(nc, "time")[1:2]
nc_close(nc); rm(nc)

#' Umschreiben der Zeitachse in Posix-Format
dts <- as.POSIXct(date_help * 3600, origin = "1900-01-01 00:00", tz = "UTC")
dts_month <- as.character(month(dts, label = TRUE))
dts_year <- year(dts)

#' Umrechnen der Matrizen von Pazifik- auf Europazentrisches Gitter -----------------------------------------
#' [0,360] -> [-180,180]
#'
lon_help <- lon - 180
# lon[1]; lon_help[97];
# lon[1]; lon_help[181];
u_ei <- u_ei[c(181:360, 1:180),,,]
dimnames(u_ei) <- list(lon_help, lat, lev, dts)
v_ei <- v_ei[c(181:360, 1:180),,,]
dimnames(v_ei) <- list(lon_help, lat, lev, dts)
lon <- lon_help


## =============================================================================
## Einlesen der ERA-40-Datensätze ==============================================
## =============================================================================

#' Einlesen der Daten mit ncdf4-Routinen
nc <- nc_open("../../a-e4-1deg-1958-1978-ztuv.nc")
u_e4 <- ncvar_get(nc, "u") ## U-Wind-Komponente
v_e4 <- ncvar_get(nc, "v") # V-Wind-Komponente
lon_e4 <- ncvar_get(nc, "longitude") # Längengrad
lat_e4 <- ncvar_get(nc, "latitude") # Breitengrad
lev_e4 <- ncvar_get(nc, "level") # Drucklevel
date_help_e4 <- ncvar_get(nc, "time")
nc_close(nc); rm(nc)

#' Umschreiben der Zeitachse in Posix-Format
dts_e4 <- as.POSIXct(date_help_e4 * 3600, origin = "1900-01-01 00:00", tz = "UTC")
dts_month_e4 <- as.character(month(dts_e4, label = TRUE))
dts_year_e4 <- year(dts_e4)

#' Umrechnen der Matrizen von Pazifik- auf Europazentrisches Gitter -----------------------------------------
#' [0,360] -> [-180,180]
#'
lon_help_e4 <- lon_e4 - 180
# lon[1]; lon_help[97];
u_e4 <- u_e4[c(181:360, 1:180),,,]
dimnames(u_e4) <- list(lon_help_e4, lat_e4, lev_e4, dts_e4)
v_e4 <- v_e4[c(181:360, 1:180),,,]
dimnames(v_e4) <- list(lon_help_e4, lat_e4, lev_e4, dts_e4)
lon_e4 <- lon_help_e4



## =============================================================================
## Zusammenfügen von ERA-40 und ERA-Interim ====================================
## =============================================================================

dts <- c(dts_e4, dts)

u <- 
  abind(abind(array(u_e4[,,1,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(u_e4[,,2,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(u_e4[,,3,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(u_e4[,,4,], dim = c(360, 91, 1, 252)), 
              array(u_e4[,,5,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(u_e4[,,6,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(u_e4[,,7,], dim = c(360, 91, 1, 252)), 
              along = 3),
        u_ei, along = 4)
v <- 
  abind(abind(array(v_e4[,,1,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(v_e4[,,2,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(v_e4[,,3,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(v_e4[,,4,], dim = c(360, 91, 1, 252)), 
              array(v_e4[,,5,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(v_e4[,,6,], dim = c(360, 91, 1, 252)), 
              array(dim = c(360,91,1,252)),
              array(v_e4[,,7,], dim = c(360, 91, 1, 252)), 
              along = 3),
        v_ei, along = 4)
rm(u_e4, u_ei, v_e4, v_ei)

#' Indizes der Matrizen für spätere Verarbeitung
dimnames(u) <- list(lon, lat, lev, dts)
dimnames(v) <- list(lon, lat, lev, dts)
# dimnames(z_e4) <- list(lon, lat, lev, dts)


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




#' Festlegen des zu untersuchenden Druckniveaus 
pressure_level <- which(lev == geopotential)
